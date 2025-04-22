# Message Push Device Analysis (`dev_push.erl`)

## Overview

The `dev_push.erl` module implements a sophisticated message propagation mechanism within HyperBEAM, enabling cross-process communication through recursive message delivery. With 0 downstream dependents, this utility device serves as a critical component in HyperBEAM's distributed messaging architecture, facilitating asynchronous communication between processes both locally and across network boundaries.

This module addresses a fundamental requirement in distributed systems: the ability for one process to trigger actions in other processes through message passing. By implementing a push-based messaging pattern, it allows processes to send messages to other processes, evaluate their results, and potentially trigger further message propagation - effectively creating message chains and enabling complex distributed workflows.

The module's design emphasizes versatility and resilience, with support for both synchronous and asynchronous operation modes, fallback encoding mechanisms, and robust error handling. Its integration with HyperBEAM's attestation, caching, and HTTP subsystems creates a cohesive framework for secure, reliable message propagation in a distributed environment.

## Key Characteristics

- **Recursive Propagation**: Recursively pushes messages to target processes until no more messages remain
- **Sync/Async Modes**: Supports both synchronous and asynchronous execution modes
- **Message Tracking**: Maintains contextual information about message origin and propagation paths
- **Cross-Node Communication**: Enables message pushing across different nodes via HTTP redirection
- **Format Negotiation**: Supports multiple message formats with automatic downgrading (httpsig and ans104)
- **Target Resolution**: Resolves target processes through IDs and hints
- **Comprehensive Logging**: Includes detailed event logging for debugging and tracking message flow
- **Error Handling**: Provides robust error handling for network failures, format issues, and missing targets
- **Process Integration**: Seamlessly integrates with the process device for slot-based message management
- **Cache Integration**: Uses the cache system for message storage and retrieval
- **HTTP Integration**: Leverages HTTP for remote message scheduling and pushing
- **Message Attestation**: Ensures messages are properly attested before propagation

## Dependencies

### Library Dependencies
- EUNIT library for testing

### Upstream Dependencies
- `hb_converge`: For message resolution and field access/modification
- `hb_message`: For message attestation and verification
- `hb_cache`: For message storage and retrieval
- `dev_process`: For process-related operations
- `hb_http`: For remote message pushing via HTTP
- `uri_string`: For parsing and manipulating URIs

## Implementation Details

### Core Push Function

The main entry point is the `push/3` function, which handles both initial messages and slot-based pushes:

```erlang
push(Base, Req, Opts) ->
    ModBase = dev_process:as_process(Base, Opts),
    ?event(push, {push_base, {base, ModBase}, {req, Req}}, Opts),
    case hb_converge:get(<<"slot">>, {as, <<"message@1.0">>, Req}, no_slot, Opts) of
        no_slot ->
            case schedule_initial_message(ModBase, Req, Opts) of
                {ok, Assignment} ->
                    case find_type(hb_converge:get(<<"body">>, Assignment, Opts), Opts) of
                        <<"Message">> ->
                            ?event(push,
                                {pushing_message,
                                    {base, ModBase},
                                    {assignment, Assignment}
                                },
                                Opts
                            ),
                            push_with_mode(ModBase, Assignment, Opts);
                        <<"Process">> ->
                            ?event(push,
                                {initializing_process,
                                    {base, ModBase},
                                    {assignment, Assignment}},
                                Opts
                            ),
                            {ok, Assignment}
                    end;
                {error, Res} -> {error, Res}
            end;
        _ -> push_with_mode(ModBase, Req, Opts)
    end.
```

This function:
1. Ensures the base is a process
2. Checks if a slot is provided
3. If no slot, schedules the message and processes based on type (Message or Process)
4. If a slot is provided, pushes the message with the appropriate mode

### Push Mode Selection

The module supports both synchronous and asynchronous pushing modes:

```erlang
push_with_mode(Base, Req, Opts) ->
    Mode = is_async(Base, Req, Opts),
    case Mode of
        <<"sync">> ->
            do_push(Base, Req, Opts);
        <<"async">> ->
            spawn(fun() -> do_push(Base, Req, Opts) end)
    end.
```

The mode is determined by checking various configuration sources:

```erlang
is_async(Base, Req, Opts) ->
    hb_converge:get_first(
        [
            {Req, <<"push-mode">>},
            {Base, <<"push-mode">>},
            {Base, <<"process/push-mode">>}
        ],
        <<"sync">>,
        Opts
    ).
```

### Core Push Processing

The actual push processing occurs in the `do_push/3` function:

```erlang
do_push(Base, Assignment, Opts) ->
    Slot = hb_converge:get(<<"slot">>, Assignment, Opts),
    ID = dev_process:process_id(Base, #{}, Opts),
    ?event(push, {push_computing_outbox, {process_id, ID}, {slot, Slot}}),
    Result = hb_converge:resolve(
        {as, <<"process@1.0">>, Base},
        #{ <<"path">> => <<"compute/results/outbox">>, <<"slot">> => Slot },
        Opts#{ hashpath => ignore }
    ),
    % ... process the results and push to downstream processes ...
```

This function:
1. Retrieves the slot number and process ID
2. Computes the outbox for the given slot
3. For each message in the outbox, pushes it to the target process
4. Collects and returns the results of all downstream pushes

### Message Pushing Logic

The core message pushing logic is in `push_result_message/5`:

```erlang
push_result_message(Base, FromSlot, Key, MsgToPush, Opts) ->
    case hb_converge:get(<<"target">>, MsgToPush, undefined, Opts) of
        undefined ->
            ?event(push, {skip_no_target, {key, Key}, MsgToPush}, Opts),
            #{};
        TargetID ->
            % ... schedule the message and push it to the target ...
            case schedule_result(Base, MsgToPush, Opts) of
                {ok, Assignment} ->
                    % ... process the assignment and recursively push ...
                    Resurse = hb_converge:resolve(
                        {as, <<"process@1.0">>, TargetAsProcess},
                        #{ <<"path">> => <<"push">>, <<"slot">> => NextSlotOnProc },
                        Opts#{ cache_control => <<"always">> }
                    ),
                    % ... handle the results ...
```

This function:
1. Checks if a target is specified
2. If a target exists, schedules the message on the target process
3. Recursively calls push on the target process with the new slot
4. Returns the results of the recursive push

### Message Scheduling

The module includes several functions for scheduling messages:

```erlang
schedule_result(Base, MsgToPush, Opts) ->
    schedule_result(Base, MsgToPush, <<"httpsig@1.0">>, Opts).
schedule_result(Base, MsgToPush, Codec, Opts) ->
    % ... prepare and attest the message ...
    SignedReq =
        #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>,
            <<"body">> =>
                SignedMsg = hb_message:attest(
                    additional_keys(Base, MsgToPush, Opts),
                    Opts,
                    Codec
                )
        },
    % ... resolve the request and handle the response ...
```

These functions:
1. Prepare the message with additional keys
2. Attest the message using the specified codec
3. Create a schedule request
4. Resolve the request on the target process
5. Handle the response, including potential redirects and format downgrades

### Cross-Node Communication

For messages that need to be pushed to remote nodes, the module includes `remote_schedule_result/3`:

```erlang
remote_schedule_result(Location, SignedReq, Opts) ->
    ?event(push, {remote_schedule_result, {location, Location}, {req, SignedReq}}, Opts),
    {Node, RedirectPath} = parse_redirect(Location),
    % ... prepare the request ...
    % Store a copy of the message for ourselves.
    hb_cache:write(SignedReq, Opts),
    case hb_http:post(Node, Path, maps:without([<<"path">>], SignedReq), Opts) of
        {ok, Res} ->
            % ... handle the response ...
        {error, Res} ->
            {error, Res}
    end.
```

This function:
1. Parses the redirect location to extract the node and path
2. Stores a copy of the message in the local cache
3. Posts the request to the remote node
4. Handles the response, including potential further redirects

## Integration with HyperBEAM

### Integration with Process Management

The module integrates with HyperBEAM's process management system:

1. **Process Conversion**: Uses `dev_process:as_process/2` to ensure a base is a process
   ```erlang
   ModBase = dev_process:as_process(Base, Opts)
   ```

2. **Process ID Retrieval**: Uses `dev_process:process_id/3` to get the process ID
   ```erlang
   ID = dev_process:process_id(Base, #{}, Opts)
   ```

3. **Process Key Handling**: Uses `dev_process:ensure_process_key/2` to ensure process keys are present
   ```erlang
   TargetAsProcess = dev_process:ensure_process_key(TargetBase, Opts)
   ```

### Integration with Cache System

The module integrates with HyperBEAM's cache system:

1. **Message Retrieval**: Uses `hb_cache:read/2` to retrieve messages
   ```erlang
   {ok, PushBase} = hb_cache:read(Target, Opts)
   ```

2. **Message Storage**: Uses `hb_cache:write/2` to store messages
   ```erlang
   hb_cache:write(SignedReq, Opts)
   ```

### Integration with HTTP System

The module integrates with HyperBEAM's HTTP system for remote operations:

1. **Remote Posting**: Uses `hb_http:post/4` to send messages to remote nodes
   ```erlang
   hb_http:post(Node, Path, maps:without([<<"path">>], SignedReq), Opts)
   ```

### Integration with Message System

The module integrates with HyperBEAM's message system:

1. **Message Attestation**: Uses `hb_message:attest/3` to attest messages
   ```erlang
   SignedMsg = hb_message:attest(additional_keys(Base, MsgToPush, Opts), Opts, Codec)
   ```

2. **Message Verification**: Uses `hb_message:verify/2` to verify messages
   ```erlang
   hb_message:verify(SignedMsg, signers)
   ```

3. **Message ID Retrieval**: Uses `hb_message:id/3` to get message IDs
   ```erlang
   hb_message:id(FromMsg, all, Opts)
   ```

## Testing Approach

The module includes several complex test functions:

### Full Push Test

```erlang
full_push_test_() ->
    {timeout, 30, fun() ->
        % ... set up test environment ...
        % Create a process
        Msg1 = dev_process:test_aos_process(Opts),
        % Schedule a ping-pong script on it
        Script = ping_pong_script(2),
        {ok, Msg2} = dev_process:schedule_aos_call(Msg1, Script),
        % Push the script and verify execution
        {ok, StartingMsgSlot} =
            hb_converge:resolve(Msg2, #{ <<"path">> => <<"slot">> }, Opts),
        Msg3 =
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => StartingMsgSlot
            },
        {ok, _} = hb_converge:resolve(Msg1, Msg3, Opts),
        % Check the final result
        ?assertEqual(
            {ok, <<"Done.">>},
            hb_converge:resolve(Msg1, <<"now/results/data">>, Opts)
        )
    end}.
```

This test:
1. Creates a test process
2. Schedules a ping-pong script that sends messages to itself
3. Pushes the initial message
4. Verifies that the script completes correctly

### Format Negotiation Test

```erlang
push_prompts_encoding_change_test() ->
    % ... set up test environment ...
    % Create a test message
    Msg = hb_message:attest(#{
        <<"path">> => <<"push">>,
        <<"method">> => <<"POST">>,
        <<"target">> => <<"QQiMcAge5ZtxcUV7ruxpi16KYRE8UBP0GAAqCIJPXz0">>,
        <<"action">> => <<"Eval">>,
        <<"data">> => <<"print(\"Please ignore!\")">>
    }, Opts),
    % Try to resolve it and verify the expected error
    Res =
        hb_converge:resolve_many(
            [
                <<"QQiMcAge5ZtxcUV7ruxpi16KYRE8UBP0GAAqCIJPXz0">>,
                {as, <<"process@1.0">>, <<>>},
                Msg
            ],
            Opts
        ),
    ?assertMatch({error, #{ <<"status">> := 422 }}, Res).
```

This test:
1. Creates a message with a specific format
2. Attempts to resolve it
3. Verifies that the expected error occurs due to format issues

The module also includes disabled tests for multi-process pushing and pushing with redirect hints, which provide additional test coverage but are not currently active.

## Observations and Insights

### Strengths

1. **Versatile Communication**: Provides a flexible mechanism for cross-process communication both locally and remotely.

2. **Recursive Propagation**: Enables complex message chains through recursive message pushing.

3. **Format Negotiation**: Supports multiple message formats with automatic downgrading for compatibility.

4. **Sync/Async Modes**: Offers both synchronous and asynchronous execution modes for different use cases.

5. **Comprehensive Logging**: Includes detailed event logging for debugging and monitoring message flow.

### Design Patterns

1. **Recursive Chain Pattern**: Implements recursive message chains for propagating messages through multiple processes.

2. **Format Negotiation Pattern**: Uses a fallback mechanism to try different formats when the preferred one fails.

3. **Async Processing Pattern**: Provides an option for asynchronous message pushing through process spawning.

4. **Idempotency Pattern**: Ensures messages are uniquely identified and can be safely retried.

5. **Redirect Handling Pattern**: Implements proper handling of HTTP redirects for remote operations.

### Challenges and Limitations

1. **Complex Error Handling**: The error handling logic is complex and spread across multiple functions.

2. **Network Dependency**: Heavily relies on network communication for remote pushing, which can introduce latency and reliability issues.

3. **Test Fragility**: Some tests are disabled due to potential issues, indicating fragility in the testing approach.

4. **Format Dependency**: Requires specific message formats, which can limit interoperability with external systems.

5. **Remote Node Trust**: Limited validation of remote nodes, potentially allowing security issues in untrusted environments.

### Future Opportunities

1. **Enhanced Error Recovery**: Adding more sophisticated error recovery mechanisms for network failures.

2. **Improved Format Negotiation**: Expanding format negotiation to handle more formats and be more flexible.

3. **Performance Optimization**: Optimizing the recursive pushing logic to reduce latency in long message chains.

4. **Security Enhancements**: Adding more rigorous validation of remote nodes and messages.

5. **Monitoring and Metrics**: Adding more comprehensive monitoring and metrics for message flow.

## Architectural Significance

The module has several points of architectural significance:

1. **Distributed Communication**: Enables distributed communication patterns across multiple processes and nodes.

2. **Message Propagation**: Provides a crucial mechanism for message propagation in a distributed system.

3. **Cross-Node Interoperability**: Facilitates interoperability between different nodes in a network.

4. **State Distribution**: Enables distributed state updates through message passing.

5. **Event Chaining**: Allows for event chaining across multiple processes and nodes.

## Conclusion

The `dev_push.erl` module is a sophisticated component of HyperBEAM's messaging architecture, enabling complex distributed communication patterns through recursive message propagation. Its support for both local and remote pushing, synchronous and asynchronous modes, and multiple message formats makes it a versatile tool for building distributed applications.

While the module's complexity introduces challenges in terms of error handling, testing, and maintenance, its comprehensive design provides a solid foundation for distributed message passing. The integration with HyperBEAM's process, cache, HTTP, and message systems creates a cohesive framework for secure, reliable communication in a distributed environment.

As HyperBEAM continues to evolve, this push mechanism will likely remain a critical component for enabling complex distributed workflows, cross-node communication, and event-driven architectures. Future improvements in error handling, format negotiation, and security will further enhance its utility in distributed systems.

## TO-DO Comments and Incomplete Aspects

This module contains a few explicit and implicit TODO items and incomplete aspects:

1. There's a TODO comment in the `add_attestations` function:
   ```erlang
   % TODO: Filter out attestations from the current node.
   ```
   This suggests the current implementation may include redundant attestations from the local node.

2. There's a note about an incomplete test case:
   ```erlang
   % Note: This test currently only gets a reply that the message was not
   % trusted by the process. To fix this, we would have to add another 
   % trusted authority to the `test_aos_process' call.
   ```
   This indicates that the `push_with_redirect_hint_test_disabled` test is incomplete and requires additional setup to fully test the functionality.

3. Several test functions are explicitly disabled (`push_with_redirect_hint_test_disabled` and `multi_process_push_test_disabled`), suggesting there are aspects of the functionality that cannot be reliably tested in the current implementation.

4. There's a comment about potential enhancements to attestation handling:
   ```erlang
   ?no_prod("Currently we only attest to the outbox and spawn items. Make it general?")
   ```
   This indicates a design consideration about whether the attestation mechanism should be generalized to handle more message types.

These items represent potential areas for improvement in future versions of the module, particularly around testing reliability, attestation handling, and optimizations for duplicate attestation filtering.
