# `dev_scheduler_server.erl` Analysis

## Overview

`dev_scheduler_server.erl` implements a server component for the HyperBEAM scheduler system that manages the assignment of messages to specific slots within a process. As noted in its documentation, it "acts as a deliberate 'bottleneck' to prevent the server accidentally assigning multiple messages to the same slot," highlighting its role in maintaining ordering guarantees within the system.

This module is designed as a long-lived Erlang process that maintains state for a specific process ID, including the current slot number and a cryptographic hash chain that links all assignments together. When a message is scheduled, the server assigns it to the next available slot, updates the hash chain, and creates an assignment message that is persisted to storage and potentially uploaded to the network.

The server supports different scheduling modes that provide different trade-offs between performance and confirmation guarantees, ranging from aggressive asynchronous scheduling to fully synchronized operations that wait for network confirmation.

## Key Characteristics

- **Sequential Assignment**: Ensures messages are assigned to sequential slots without gaps or duplicates
- **Hash Chain Management**: Maintains a cryptographic chain linking all assignments together
- **Multiple Scheduling Modes**: Supports different performance/reliability trade-offs through configurable modes
- **Process-Per-ID Model**: Creates a dedicated Erlang process for each HyperBEAM process ID
- **State Management**: Maintains and persists state between restarts
- **Erlang Message Passing**: Uses Erlang's message passing for communication
- **Blockchain Integration**: Includes Arweave blockchain metadata in assignments

## Dependencies

### Upstream Dependencies

- `hb_name`: For process registration
- `dev_scheduler_cache`: For persisting and retrieving assignments
- `hb_message`: For creating attested assignment messages
- `hb_client`: For uploading assignments to the network
- `hb_path`: For path extraction from messages
- `hb_opts`: For configuration options
- `ar_timestamp`: For obtaining blockchain timing information
- `hb`: For wallet access

## Implementation Details

### Server Initialization

The `start/2` function initializes a new scheduler server process:

```erlang
start(ProcID, Opts) ->
    ?event(scheduling, {starting_scheduling_server, {proc_id, ProcID}}),
    spawn_link(
        fun() ->
            case hb_opts:get(scheduling_mode, disabled, Opts) of
                disabled ->
                    throw({scheduling_disabled_on_node, {requested_for, ProcID}});
                _ -> ok
            end,
            hb_name:register({dev_scheduler, ProcID}),
            {CurrentSlot, HashChain} =
                case dev_scheduler_cache:latest(ProcID, Opts) of
                    not_found ->
                        ?event({starting_new_schedule, {proc_id, ProcID}}),
                        {-1, <<>>};
                    {Slot, Chain} ->
                        ?event({continuing_schedule, {proc_id, ProcID}, {current_slot, Slot}}),
                        {Slot, Chain}
                end,
            ?event(
                {scheduler_got_process_info,
                    {proc_id, ProcID},
                    {current, CurrentSlot},
                    {hash_chain, HashChain}
                }
            ),
            server(
                #{
                    id => ProcID,
                    current => CurrentSlot,
                    wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts),
                    hash_chain => HashChain,
                    opts => Opts
                }
            )
        end
    ).
```

This function:
1. Checks if scheduling is enabled for the node
2. Registers the process with the naming service
3. Retrieves the current state (slot and hash chain) from the cache if available, or initializes new state if not
4. Starts the server loop with the initial state

### Message Scheduling

The `schedule/2` function is the main interface for scheduling messages:

```erlang
schedule(AOProcID, Message) when is_binary(AOProcID) ->
    schedule(dev_scheduler_registry:find(AOProcID), Message);
schedule(ErlangProcID, Message) ->
    ErlangProcID ! {schedule, Message, self()},
    receive
        {scheduled, Message, Assignment} ->
            Assignment
    end.
```

This function:
1. Resolves the process ID to an Erlang PID if necessary
2. Sends a scheduling request to the scheduler process
3. Waits for a confirmation response with the assignment

### Assignment Creation

The core scheduling logic is in the `do_assign/3` function:

```erlang
do_assign(State, Message, ReplyPID) ->
    HashChain = next_hashchain(maps:get(hash_chain, State), Message),
    NextSlot = maps:get(current, State) + 1,
    % Run the signing of the assignment and writes to the disk in a separate
    % process.
    AssignFun =
        fun() ->
            {Timestamp, Height, Hash} = ar_timestamp:get(),
            Assignment = hb_message:attest(#{
                <<"path">> =>
                    case hb_path:from_message(request, Message) of
                        undefined -> <<"compute">>;
                        Path -> Path
                    end,
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"process">> => hb_util:id(maps:get(id, State)),
                <<"epoch">> => <<"0">>,
                <<"slot">> => NextSlot,
                <<"block-height">> => Height,
                <<"block-hash">> => hb_util:human_id(Hash),
                <<"block-timestamp">> => Timestamp,
                % Note: Local time on the SU, not Arweave
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"hash-chain">> => hb_util:id(HashChain),
                <<"body">> => Message
            }, maps:get(wallet, State)),
            % ... storage and reply logic ...
        end,
    % ... scheduling mode handling ...
    State#{
        current := NextSlot,
        hash_chain := HashChain
    }.
```

This function:
1. Creates the next hash chain link
2. Determines the next slot number
3. Creates an assignment message with blockchain metadata, process information, and the message itself
4. Handles storage, network upload, and client notification based on the scheduling mode
5. Updates and returns the server state

### Hash Chain Management

The `next_hashchain/2` function maintains the cryptographic chain of assignments:

```erlang
next_hashchain(HashChain, Message) ->
    ?event({creating_next_hashchain, {hash_chain, HashChain}, {message, Message}}),
    ID = hb_message:id(Message, all),
    crypto:hash(
        sha256,
        << HashChain/binary, ID/binary >>
    ).
```

This function:
1. Extracts the content-addressed ID of the message
2. Concatenates it with the previous hash chain
3. Computes a new SHA-256 hash of the combined data

### Scheduling Modes

The module supports different scheduling modes, implemented in the `maybe_inform_recipient/5` function:

```erlang
maybe_inform_recipient(Mode, ReplyPID, Message, Assignment, State) ->
    case hb_opts:get(scheduling_mode, remote_confirmation, maps:get(opts, State)) of
        Mode -> ReplyPID ! {scheduled, Message, Assignment};
        _ -> ok
    end.
```

This function:
1. Checks if the current scheduling mode matches the requested notification mode
2. Sends a confirmation message to the client if the modes match

The supported modes are:
- `aggressive`: Responds immediately and performs the assignment in a separate process
- `local_confirmation`: Responds after writing to local storage
- `remote_confirmation`: Responds after uploading to the network

## Questions and Insights

### Questions

1. **Concurrency Control**: How does the system handle concurrent scheduling requests for the same process? Is there a mechanism to prevent race conditions between multiple instances of scheduling servers?

2. **Failure Recovery**: What happens if the scheduling server crashes during an assignment operation? How is consistency maintained in such scenarios?

3. **Network Partition Handling**: How does the system handle network partitions, particularly in `remote_confirmation` mode where uploads are expected to succeed?

4. **Performance Implications**: What are the performance implications of the different scheduling modes? Are there benchmarks or guidelines for choosing between them?

5. **Backward Compatibility**: How does the server handle backward compatibility with older message formats or hash chain algorithms?

### Insights

1. **Deliberate Bottleneck**: The server is explicitly designed as a bottleneck, which is an interesting architectural choice. This indicates a deliberate trade-off between parallelism and sequential consistency.

2. **Cryptographic Continuity**: The hash chain mechanism ensures that each assignment is cryptographically linked to its predecessors, creating a verifiable history of assignments.

3. **Flexible Confirmation Models**: The three scheduling modes provide a spectrum of confirmation guarantees, allowing applications to choose the right balance between performance and reliability.

4. **State Persistence**: The server is designed to recover its state from persistent storage, allowing it to continue from the correct slot after restarts.

5. **Blockchain Integration**: The inclusion of Arweave blockchain metadata in assignments ties the scheduler to the blockchain timeline, potentially enabling external verification.

## Integration with Other Subsystems

### Integration with Device and Process Management Subsystem

- Works with `dev_scheduler_registry` for process discovery and creation
- Provides the core scheduling logic for `dev_scheduler`
- Maintains the state that other scheduler components refer to

### Integration with Storage Subsystem

- Uses `dev_scheduler_cache` for persisting and retrieving assignments
- Creates a permanent record of assignments for future reference

### Integration with Network Communication Subsystem

- Uses `hb_client` to upload assignments to the network
- Potentially waits for network confirmation in `remote_confirmation` mode

### Integration with Core Infrastructure

- Uses `hb_message` for message attestation
- Uses `hb_name` for process registration
- Uses `hb_opts` for configuration access

### Integration with Arweave Subsystem

- Uses `ar_timestamp` to obtain blockchain timing information
- Incorporates blockchain metadata into assignments

## Recategorization Considerations

This module is correctly categorized as part of the Device and Process Management Subsystem. Its primary responsibility is managing the scheduling of messages within processes, which is a core aspect of process management.

The module's tight integration with other scheduler components (`dev_scheduler_registry`, `dev_scheduler_cache`) and its focused responsibility of maintaining the sequential order of message execution further reinforce its categorization.

While it interacts with other subsystems such as storage and network, these interactions are in service of its primary process management responsibility. The deliberate design as a bottleneck for sequential processing also aligns with the process management paradigm rather than with the patterns typical of other subsystems.
