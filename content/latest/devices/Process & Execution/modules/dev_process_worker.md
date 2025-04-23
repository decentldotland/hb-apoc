# Module: dev_process_worker

## Basic Information
- **Source File:** dev_process_worker.erl
- **Module Type:** Device Core Processing
- **Purpose:** Long-lived process worker maintaining state between calls
- **Behavior:** Implements hb_ao interface for process computation

## Interface

### Public API
```erlang
% Core functionality
-export([server/3, stop/1]).

% Process grouping and coordination
-export([group/3, await/5, notify_compute/4]).
```

### Include Files
```erlang
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
```

## Runtime Options

1. **Process Workers Control**
   - Option: process_workers
   - Default: false
   - Purpose: Enables/disables worker process grouping

2. **Worker Timeout**
   - Option: process_worker_max_idle
   - Default: 300_000 (5 minutes)
   - Purpose: Maximum idle time before state snapshot

3. **Server Options**
   ```erlang
   ServerOpts = #{
       await_inprogress => false,
       spawn_worker => false,
       process_workers => false
   }
   ```

## Implementation Details

### 1. Process Grouping

```erlang
group(Msg1, Msg2, Opts) ->
    case hb_opts:get(process_workers, false, Opts) of
        false ->
            hb_persistent:default_grouper(Msg1, Msg2, Opts);
        true ->
            case Msg2 of
                undefined ->
                    hb_persistent:default_grouper(Msg1, undefined, Opts);
                _ ->
                    case hb_path:matches(<<"compute">>, hb_path:hd(Msg2, Opts)) of
                        true ->
                            process_to_group_name(Msg1, Opts);
                        _ ->
                            hb_persistent:default_grouper(Msg1, Msg2, Opts)
                    end
            end
    end
```

Key aspects:
- Groups by process ID for compute operations
- Falls back to default grouping for other operations
- Only active when process_workers=true

### 2. Worker Server

```erlang
server(GroupName, Msg1, Opts) ->
    ServerOpts = #{
        await_inprogress => false,
        spawn_worker => false,
        process_workers => false
    },
    Timeout = hb_opts:get(process_worker_max_idle, 300_000, Opts),
    receive
        {resolve, Listener, GroupName, Msg2, ListenerOpts} ->
            TargetSlot = hb_ao:get(<<"slot">>, Msg2, Opts),
            Res = hb_ao:resolve(Msg1, #{
                <<"path">> => <<"compute">>,
                <<"slot">> => TargetSlot
            }, maps:merge(ListenerOpts, ServerOpts)),
            send_notification(Listener, GroupName, TargetSlot, Res),
            server(GroupName, case Res of
                {ok, Msg3} -> Msg3;
                _ -> Msg1
            end, Opts);
        stop ->
            exit(normal)
    after Timeout ->
        hb_ao:resolve(Msg1, <<"snapshot">>,
            ServerOpts#{ <<"cache-control">> => [<<"store">>] }),
        {ok, Msg1}
    end
```

Key behaviors:
- Maintains state between computations
- Auto-snapshots on timeout
- Handles resolution requests
- Propagates computation results

### 3. Result Notification

```erlang
notify_compute(GroupName, SlotToNotify, Msg3, Opts) ->
    notify_compute(GroupName, SlotToNotify, Msg3, Opts, 0).
notify_compute(GroupName, SlotToNotify, Msg3, Opts, Count) ->
    receive
        {resolve, Listener, GroupName, #{ <<"slot">> := SlotToNotify }, _} ->
            send_notification(Listener, GroupName, SlotToNotify, Msg3),
            notify_compute(GroupName, SlotToNotify, Msg3, Opts, Count + 1);
        {resolve, Listener, GroupName, Msg, _}
                when is_map(Msg) andalso not is_map_key(<<"slot">>, Msg) ->
            send_notification(Listener, GroupName, SlotToNotify, Msg3),
            notify_compute(GroupName, SlotToNotify, Msg3, Opts, Count + 1)
    after 0 ->
        ok
    end
```

Key aspects:
- Notifies all waiting listeners
- Handles both slot-specific and general requests
- Tracks notification count
- Non-blocking (0 timeout)

## Event Logging

The module logs events at key points:

1. Worker Lifecycle
   ```erlang
   ?event(worker, {waiting_for_req, {group, GroupName}})
   ?event(worker, {work_received, {group, GroupName}, {slot, TargetSlot}})
   ?event(worker, {work_done, {group, GroupName}, {req, Msg2}, {res, Res}})
   ?event(worker, {stopping, {group, GroupName}, {msg1, Msg1}})
   ```

2. Computation Flow
   ```erlang
   ?event({awaiting_compute, {worker, Worker}, {group, GroupName}})
   ?event(compute_debug, {notified_of_resolution, {target, TargetSlot}})
   ?event(worker_short, {finished_notifying, {listeners, Count}})
   ```

## Test Coverage

The module includes tests for:

1. **Info Interface**
   ```erlang
   info_test() ->
       M1 = dev_process:test_wasm_process(...),
       Res = hb_ao:info(M1, #{}),
       ?assertEqual(fun dev_process_worker:group/3, maps:get(grouper, Res)).
   ```

2. **Grouping Logic**
   ```erlang
   grouper_test() ->
       M1 = dev_process:test_aos_process(),
       M2 = #{ <<"path">> => <<"compute">>, <<"v">> => 1 },
       M3 = #{ <<"path">> => <<"compute">>, <<"v">> => 2 },
       M4 = #{ <<"path">> => <<"not-compute">>, <<"v">> => 3 },
       % Verifies grouping behavior for compute vs non-compute paths
   ```

## Integration Points

### Direct Dependencies
- hb_persistent: Default grouping behavior
- hb_path: Path matching and handling
- hb_ao: Message resolution
- hb_opts: Configuration management
- dev_process: Process key management

### Usage Context
- Called by dev_process for worker management
- Integrates with hb_ao resolution system
- Coordinates with process cache system
- Manages long-lived worker processes
