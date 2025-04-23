# `hb_persistent.erl` Analysis

## Overview

`hb_persistent.erl` is an advanced process management module for HyperBEAM that creates and manages long-lived Converge resolution processes. It provides a mechanism for maintaining stateful processes that can handle expensive computations more efficiently by keeping large messages in memory and avoiding repeated serialization/deserialization operations. Additionally, it offers facilities for coordinating and serializing parallel executions to prevent redundant work.

This module bridges the gap between the storage subsystem and process management, functioning as a form of in-memory persistence that complements the disk-based storage implementations. It represents a higher-level approach to resource optimization by focusing on compute resources rather than storage resources.

## Key Characteristics

- **Long-Lived Process Management**: Creates and manages persistent processes for Converge resolution
- **Execution Deduplication**: Prevents redundant parallel execution of identical computations
- **Process Coordination**: Allows processes to register as leaders for specific executions
- **Distributed Group Management**: Built on Erlang's process group mechanism for distributed coordination
- **Configurable Execution Strategy**: Supports customizable worker, grouping, and await functions
- **Process Monitoring**: Includes utilities for monitoring process groups and their states
- **Result Notification**: Handles notification of waiting processes when results are available

## Dependencies

### Upstream Dependencies

- `hb_name`: For process registration and lookup (wraps Erlang's `pg` module)
- `hb_converge`: For execution of Converge resolution
- `hb_opts`: For accessing configuration options
- `erlang`: For process monitoring and management primitives
- `include/hb.hrl`: System-wide macros and definitions

## Implementation Details

### Process Coordination

The module implements a leader-follower pattern for coordinating parallel executions:

```erlang
find_or_register(GroupName, _Msg1, _Msg2, Opts) ->
    case hb_opts:get(await_inprogress, false, Opts) of
        false -> {leader, GroupName};
        _ ->
            Self = self(),
            case find_execution(GroupName, Opts) of
                {ok, Leader} when Leader =/= Self ->
                    ?event({found_leader, GroupName, {leader, Leader}}),
                    {wait, Leader};
                {ok, Leader} when Leader =:= Self ->
                    {infinite_recursion, GroupName};
                _ ->
                    ?event(
                        {
                            register_resolver,
                            {group, GroupName}
                        }
                    ),
                    register_groupname(GroupName, Opts),
                    {leader, GroupName}
            end
    end.
```

This function checks if a process is already handling a specific execution (identified by a group name). If so, it returns a directive to wait for that process; otherwise, it registers the current process as the leader for that execution.

### Process Grouping

The module includes a flexible strategy for grouping related executions:

```erlang
group(Msg1, Msg2, Opts) ->
    Grouper =
        maps:get(grouper, hb_converge:info(Msg1, Opts), fun default_grouper/3),
    apply(
        Grouper,
        hb_converge:truncate_args(Grouper, [Msg1, Msg2, Opts])
    ).
```

This allows customization of how executions are grouped, with a default implementation that uses a hash of the message pair:

```erlang
default_grouper(Msg1, Msg2, Opts) ->
    % Use Erlang's `phash2` to hash the result of the Grouper function.
    ?no_prod("Using a hash for group names is not secure."),
    case hb_opts:get(await_inprogress, true, Opts) of
        named -> ungrouped_exec;
        _ -> erlang:phash2({Msg1, Msg2})
    end.
```

### Worker Process Management

The module provides functions for starting and managing worker processes:

```erlang
start_worker(GroupName, Msg, Opts) ->
    start(),
    ?event(worker_spawns,
        {starting_worker, {group, GroupName}, {msg, Msg}, {opts, Opts}}
    ),
    WorkerPID = spawn(
        fun() ->
            % If the device's info contains a `worker` function we
            % use that instead of the default implementation.
            WorkerFun =
                maps:get(
                    worker,
                    hb_converge:info(Msg, Opts),
                    Def = fun default_worker/3
                ),
            % ... (initialization logic)
            register_groupname(GroupName, Opts),
            apply(
                WorkerFun,
                hb_converge:truncate_args(
                    WorkerFun,
                    [
                        GroupName,
                        Msg,
                        maps:merge(Opts, #{
                            is_worker => true,
                            spawn_worker => false,
                            allow_infinite => true
                        })
                    ]
                )
            )
        end
    ),
    WorkerPID.
```

The default worker implementation shows the main processing loop:

```erlang
default_worker(GroupName, Msg1, Opts) ->
    Timeout = hb_opts:get(worker_timeout, 10000, Opts),
    worker_event(GroupName, default_worker_waiting_for_req, Msg1, undefined, Opts),
    receive
        {resolve, Listener, GroupName, Msg2, ListenerOpts} ->
            % ... (process the request)
            Res =
                hb_converge:resolve(
                    Msg1,
                    Msg2,
                    maps:merge(ListenerOpts, Opts)
                ),
            send_response(Listener, GroupName, Msg2, Res),
            notify(GroupName, Msg2, Res, Opts),
            % ... (determine next action based on configuration)
    after Timeout ->
        % We have hit the in-memory persistence timeout. Check whether the
        % device has shutdown procedures (for example, writing in-memory
        % state to the cache).
        unregister(Msg1, undefined, Opts)
    end.
```

### Result Notification and Waiting

The module includes functions for notifying waiting processes about results and for waiting for results from other processes:

```erlang
notify(GroupName, Msg2, Msg3, Opts) ->
    % ... (debug logging)
    receive
        {resolve, Listener, GroupName, Msg2, _ListenerOpts} ->
            ?event({notifying_listener, {listener, Listener}, {group, GroupName}}),
            send_response(Listener, GroupName, Msg2, Msg3),
            notify(GroupName, Msg2, Msg3, Opts)
    after 0 ->
        ?event(finished_notify),
        ok
    end.
```

```erlang
await(Worker, Msg1, Msg2, Opts) ->
    % ... (get the device's await function)
    GroupName = group(Msg1, Msg2, Opts),
    % set monitor to a worker, so we know if it exits
    _Ref = erlang:monitor(process, Worker),
    Worker ! {resolve, self(), GroupName, Msg2, Opts},
    AwaitFun(Worker, GroupName, Msg1, Msg2, Opts).
```

The default await implementation shows how a process waits for a result and handles worker failures:

```erlang
default_await(Worker, GroupName, Msg1, Msg2, Opts) ->
    % Wait for the result.
    receive
        {resolved, _, GroupName, Msg2, Res} ->
            worker_event(GroupName, {resolved_await, Res}, Msg1, Msg2, Opts),
            Res;
        {'DOWN', _R, process, Worker, Reason} ->
            ?event(
                {leader_died,
                    {group, GroupName},
                    {leader, Worker},
                    {reason, Reason},
                    {request, Msg2}
                }
            ),
            {error, leader_died}
    end.
```

## Tests

The module includes comprehensive tests demonstrating its capabilities:

1. **Deduplicated Execution**: Shows how parallel requests for the same computation are deduplicated
2. **Persistent Worker**: Tests the creation and operation of a persistent worker
3. **Spawning After Execution**: Tests spawning new workers after execution completes

These tests verify that the module correctly handles parallel execution, maintains state, and manages the lifecycle of worker processes.

## Questions and Insights

### Questions

1. **Error Propagation**: How are errors from one worker process propagated to waiting processes? The code shows basic error handling, but the full strategy isn't clear.

2. **Scaling Limits**: What are the scaling limits of this approach in terms of the number of concurrent persistent processes? Could this become a bottleneck in large deployments?

3. **Memory Management**: How does the system handle memory pressure when many large messages are kept in memory by persistent processes?

4. **Process Recovery**: Is there a mechanism for recovering the state of a persistent process if it crashes unexpectedly?

5. **Group Name Collisions**: The default grouper uses a hash function that the code explicitly notes is not secure for production. What alternative strategies are used in production deployments?

### Insights

1. **Optimization Strategy**: The module represents a performance optimization strategy that trades memory for compute efficiency by keeping messages in memory to avoid serialization costs.

2. **Distributed Coordination**: The use of Erlang's process group mechanism suggests a focus on distributed coordination rather than just local optimization.

3. **Extensibility**: The customizable worker, grouper, and await functions provide great flexibility for different execution patterns and requirements.

4. **Error Handling**: The module includes explicit handling of worker failures through process monitoring, showing a focus on robustness.

5. **Performance Tuning**: The configurable timeout and static/dynamic worker options allow for performance tuning based on specific workload characteristics.

## Integration with Other Subsystems

### Integration with Converge Protocol

The module works closely with the Converge Protocol (`hb_converge`) for message resolution, showing how it fits into the core execution model.

### Integration with Process Management

The module leverages Erlang's process management primitives and the `pg` module (through `hb_name`) for distributed process coordination.

### Integration with Configuration System

The module uses `hb_opts` extensively to access configuration options, allowing its behavior to be customized based on system configuration.

## Recategorization Considerations

While currently categorized as part of the Storage Subsystem, this module sits at the intersection of storage and process management. It represents a form of "in-memory persistence" that complements the disk-based storage implementations, but its focus on process coordination and execution deduplication aligns more with a process management or compute optimization subsystem.

Considering its role in the system, it might be more accurately categorized as part of a "Compute Resource Management" or "Process Optimization" subsystem, alongside other components that focus on optimizing the execution of computations rather than the storage of data.

However, given its current usage primarily for optimizing storage-related operations (avoiding serialization/deserialization), its placement in the Storage Subsystem is reasonable, though it represents a higher-level abstraction than the core storage implementations.
