# Module: dev_scheduler_server

## Basic Information
- **Source File:** dev_scheduler_server.erl
- **Module Type:** Device Scheduling Control
- **Purpose:** Long-lived server that manages message scheduling for processes, acting as a deliberate bottleneck to prevent slot assignment conflicts

## Interface

### Public API
```erlang
% Server management
-export([start/2, stop/1]).
% Scheduling operations
-export([schedule/2, info/1]).
```

## Core Functionality

### 1. Server State Structure
```erlang
#{
    id => ProcessID,           % Process identifier
    current => CurrentSlot,    % Current slot number
    wallet => WalletKey,       % Server wallet for signing
    hash_chain => HashChain,   % Assignment hash chain
    opts => Options           % Configuration options
}
```

### 2. Server Initialization

```erlang
start(ProcID, Opts) ->
    spawn_link(fun() ->
        % Verify scheduling is enabled
        case hb_opts:get(scheduling_mode, disabled, Opts) of
            disabled -> throw({scheduling_disabled_on_node, ProcID});
            _ -> ok
        end,
        
        % Register server process
        hb_name:register({dev_scheduler, ProcID}),
        
        % Initialize state from cache or start fresh
        {CurrentSlot, HashChain} =
            case dev_scheduler_cache:latest(ProcID, Opts) of
                not_found -> {-1, <<>>};
                {Slot, Chain} -> {Slot, Chain}
            end,
            
        % Start server loop
        server(#{
            id => ProcID,
            current => CurrentSlot,
            wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts),
            hash_chain => HashChain,
            opts => Opts
        })
    end)
```

### 3. Message Assignment

```erlang
do_assign(State, Message, ReplyPID) ->
    % Generate next hash chain element
    HashChain = next_hashchain(maps:get(hash_chain, State), Message),
    NextSlot = maps:get(current, State) + 1,
    
    % Create assignment
    AssignFun = fun() ->
        % Get timestamp and block info
        {Timestamp, Height, Hash} = ar_timestamp:get(),
        
        % Create and sign assignment
        Assignment = hb_message:commit(#{
            <<"path">> => get_path(Message),
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"process">> => ProcessID,
            <<"slot">> => NextSlot,
            <<"block-height">> => Height,
            <<"block-hash">> => Hash,
            <<"timestamp">> => erlang:system_time(millisecond),
            <<"hash-chain">> => HashChain,
            <<"body">> => Message
        }, Wallet),
        
        % Store and notify based on mode
        maybe_inform_recipient(aggressive, ReplyPID, Message, Assignment, State),
        ok = dev_scheduler_cache:write(Assignment, Opts),
        maybe_inform_recipient(local_confirmation, ReplyPID, Message, Assignment, State),
        hb_client:upload(Assignment, Opts),
        maybe_inform_recipient(remote_confirmation, ReplyPID, Message, Assignment, State)
    end,
    
    % Execute based on scheduling mode
    case hb_opts:get(scheduling_mode, sync, maps:get(opts, State)) of
        aggressive -> spawn(AssignFun);
        _ -> AssignFun()
    end,
    
    % Update state
    State#{
        current := NextSlot,
        hash_chain := HashChain
    }
```

## Implementation Details

### 1. Hash Chain Management

```erlang
next_hashchain(HashChain, Message) ->
    ID = hb_message:id(Message, all),
    crypto:hash(sha256, <<HashChain/binary, ID/binary>>)
```

The hash chain links assignments together to maintain order integrity:
- Each assignment includes the hash of the previous assignment
- Message IDs are incorporated into the chain
- SHA-256 is used for hashing

### 2. Scheduling Modes

Three confirmation modes are supported:

1. **Aggressive**
   - Immediate response
   - Async storage and upload
   - Fastest but least reliable

2. **Local Confirmation**
   - Wait for local storage
   - Async upload
   - Balance of speed and reliability

3. **Remote Confirmation**
   - Wait for local storage
   - Wait for upload confirmation
   - Slowest but most reliable

### 3. Error Handling

```erlang
assign(State, Message, ReplyPID) ->
    try
        do_assign(State, Message, ReplyPID)
    catch
        _Class:Reason:Stack ->
            ?event({error_scheduling, Reason, Stack}),
            State
    end
```

## Event Logging

### 1. Server Lifecycle
```erlang
?event(scheduling, {starting_scheduling_server, {proc_id, ProcID}})
?event({starting_new_schedule, {proc_id, ProcID}})
?event({continuing_schedule, {proc_id, ProcID}, {current_slot, Slot}})
```

### 2. Assignment Operations
```erlang
?event(scheduling, {assigned, {proc_id, ProcID}, {slot, Slot}, {assignment, ID}})
?event(starting_message_write)
?event(writes_complete)
?event(uploading_assignment)
?event(uploads_complete)
```

### 3. Error Handling
```erlang
?event({error_scheduling, Reason, Stack})
?event({scheduling_mode, Mode})
```

## Testing Coverage

### 1. Basic Operations
```erlang
new_proc_test_() ->
    % Tests:
    % - Server initialization
    % - Message scheduling
    % - Slot progression
    % - State maintenance
```

### 2. Performance Testing
```erlang
benchmark_test() ->
    % Tests:
    % - Scheduling throughput
    % - State consistency
    % - Resource usage
```

## Integration Points

### Direct Dependencies
- dev_scheduler_cache: Assignment storage
- dev_scheduler_registry: Server registration
- hb_message: Message operations
- hb_client: Assignment upload
- ar_timestamp: Block information

### Usage Context
- Called by dev_scheduler
- Integrates with scheduler cache
- Manages assignment persistence
- Coordinates with registry

## Key Features

### 1. Concurrency Control
- Single server per process
- Ordered slot assignment
- Hash chain verification
- State consistency

### 2. Persistence
- Local caching
- Remote storage
- State recovery
- Assignment tracking

### 3. Configuration
- Multiple scheduling modes
- Customizable timeouts
- Flexible notification
- Error recovery

### 4. Performance
- Async operations
- Batched updates
- State caching
- Resource management

## Best Practices

### 1. Server Management
- Initialize with proper options
- Monitor server health
- Handle errors gracefully
- Clean up resources

### 2. Assignment Handling
- Verify message integrity
- Maintain hash chain
- Confirm persistence
- Track slot progression

### 3. Integration
- Check scheduling mode
- Handle timeouts
- Verify responses
- Monitor performance
