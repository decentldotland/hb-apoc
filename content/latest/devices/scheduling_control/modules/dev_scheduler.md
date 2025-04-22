# Module: dev_scheduler

## Basic Information
- **Source File:** dev_scheduler.erl
- **Module Type:** Device Core Processing
- **Purpose:** Implements AO process scheduling with support for local and remote execution

## Interface

### Public API
```erlang
% Core operations
-export([info/0]).
% Local scheduling
-export([schedule/3, router/4, register/3]).
% CU-flow operations
-export([slot/3, status/3, next/3]).
% System operations
-export([start/0, checkpoint/1]).
```

### Constants
```erlang
% Maximum assignments per query
-define(MAX_ASSIGNMENT_QUERY_LEN, 1000).
% Lookahead worker timeout
-define(LOOKAHEAD_TIMEOUT, 200).
```

## Core Functionality

### 1. Process Message Format
```erlang
#{
    <<"device">> => <<"scheduler@1.0">>,
    <<"device-stack">> => [<<"Cron@1.0">>, <<"WASM-64@1.0">>, <<"PODA@1.0">>],
    <<"image">> => <<"wasm-image-id">>,
    <<"type">> => <<"Process">>,
    <<"scheduler-location">> => Address,
    <<"test-random-seed">> => RandomSeed
}
```

### 2. Scheduling Operations

#### Schedule Message
```erlang
schedule(Msg1, Msg2, Opts) ->
    case hb_ao:get(<<"method">>, Msg2, <<"GET">>, Opts) of
        <<"POST">> -> post_schedule(Msg1, Msg2, Opts);
        <<"GET">> -> get_schedule(Msg1, Msg2, Opts)
    end
```

#### Get Current Slot
```erlang
slot(M1, M2, Opts) ->
    ProcID = find_target_id(M1, M2, Opts),
    case find_server(ProcID, M1, Opts) of
        {local, PID} -> 
            % Get local slot info
            {Timestamp, Hash, Height} = ar_timestamp:get(),
            #{ current := CurrentSlot } = dev_scheduler_server:info(PID),
            {ok, #{
                <<"process">> => ProcID,
                <<"current">> => CurrentSlot,
                <<"timestamp">> => Timestamp,
                <<"block-height">> => Height,
                <<"block-hash">> => Hash
            }};
        {redirect, Redirect} -> 
            % Handle remote slot
            remote_slot(ProcID, Redirect, Opts)
    end
```

### 3. Server Management

#### Server Location
```erlang
register(_Msg1, Req, Opts) ->
    % Validate operator signature
    Signers = hb_message:signers(OnlyCommitted),
    Operator = hb_util:human_id(
        ar_wallet:to_address(
            hb_opts:get(priv_wallet, hb:wallet(), Opts)
        )
    ),
    
    % Create scheduler location
    NewSchedulerLocation = #{
        <<"data-protocol">> => <<"ao">>,
        <<"variant">> => <<"ao.N.1">>,
        <<"type">> => <<"scheduler-location">>,
        <<"url">> => URL,
        <<"nonce">> => NewNonce,
        <<"time-to-live">> => TimeToLive,
        <<"codec-device">> => Codec
    }
```

## Implementation Details

### 1. Local Scheduling

#### Assignment Management
```erlang
get_local_assignments(ProcID, From, To, Opts) ->
    ComputedTo =
        case (RequestedTo - From) > ?MAX_ASSIGNMENT_QUERY_LEN of
            true -> From + ?MAX_ASSIGNMENT_QUERY_LEN;
            false -> RequestedTo
        end,
    {
        read_local_assignments(ProcID, From, ComputedTo, Opts),
        ComputedTo < RequestedTo
    }
```

#### Cache Integration
```erlang
cache_remote_schedule(Schedule, Opts) ->
    Cacher = fun() ->
        Assignments = hb_ao:get(<<"assignments">>, Schedule, Opts),
        lists:foreach(
            fun(Assignment) ->
                dev_scheduler_cache:write(Assignment, Opts)
            end,
            AssignmentList
        )
    end,
    case hb_opts:get(scheduler_async_remote_cache, true, Opts) of
        true -> spawn(Cacher);
        false -> Cacher()
    end
```

### 2. Remote Scheduling

#### Remote Slot Access
```erlang
remote_slot(<<"ao.N.1">>, ProcID, Node, Opts) ->
    % Mainnet AO-Core scheduler
    hb_http:get(Node, <<ProcID/binary, "/slot">>, Opts);

remote_slot(<<"ao.TN.1">>, ProcID, Node, Opts) ->
    % Testnet AO-Core scheduler
    Path = << ProcID/binary, "/latest?proc-id=", ProcID/binary>>,
    case hb_http:get(Node, Path, Opts#{ http_client => httpc }) of
        {ok, Res} ->
            % Convert response to standard format
            Body = hb_ao:get(<<"body">>, Res, Opts),
            JSON = hb_json:decode(Body),
            A = dev_scheduler_formats:aos2_to_assignment(JSON, Opts),
            {ok, #{
                <<"process">> => ProcID,
                <<"current">> => maps:get(<<"slot">>, A),
                <<"timestamp">> => maps:get(<<"timestamp">>, A),
                <<"block-height">> => maps:get(<<"block-height">>, A)
            }}
    end
```

### 3. Performance Optimization

#### Lookahead Workers
```erlang
spawn_lookahead_worker(ProcID, Slot, Opts) ->
    Caller = self(),
    spawn(fun() ->
        case dev_scheduler_cache:read(ProcID, Slot, Opts) of
            {ok, Assignment} ->
                Caller ! {assignment, ProcID, Slot, Assignment};
            not_found -> fail
        end
    end)
```

## Event Logging

### 1. Server Management
```erlang
?event({ensure_started, genesis_wasm, self()})
?event({registering_scheduler, {msg1, Msg1}, {req, Req}})
?event({uploading_signed_scheduler_location, Signed})
```

### 2. Assignment Operations
```erlang
?event({scheduling_message, {proc_id, ProcID}})
?event({got_assignments, length(Assignments), {more, More}})
?event({assignments_bundle_outbound, {format, Format}, {res, Res}})
```

### 3. Remote Operations
```erlang
?event({getting_remote_slot, {proc_id, ProcID}, {redirect, Redirect}})
?event({remote_schedule_result, {res, Res}})
?event({legacy_scheduler_not_found, {url, URL}, {resp, Resp}})
```

## Testing Coverage

### 1. Basic Operations
```erlang
status_test() ->
    % Tests status retrieval
register_new_process_test() ->
    % Tests process registration
schedule_message_and_get_slot_test() ->
    % Tests scheduling and slot retrieval
```

### 2. HTTP Integration
```erlang
http_post_schedule_test() ->
    % Tests HTTP schedule posting
http_get_schedule_test() ->
    % Tests schedule retrieval via HTTP
http_get_legacy_schedule_test() ->
    % Tests legacy schedule compatibility
```

### 3. Performance Tests
```erlang
benchmark_suite_test_() ->
    % Tests different store configurations:
    % - FS store with local confirmation
    % - FS store with aggressive confirmation
    % - RocksDB store with local confirmation
    % - RocksDB store with aggressive confirmation
    % - RocksDB with HTTP/3 and 100 workers
```

## Integration Points

### Direct Dependencies
- dev_scheduler_server: Server process management
- dev_scheduler_cache: Assignment caching
- dev_scheduler_formats: Format conversion
- dev_scheduler_registry: Process registration
- hb_http: Remote communication
- hb_ao: Message handling
- hb_cache: State persistence

### Usage Context
- Process scheduling coordination
- Assignment management
- Remote scheduler integration
- Cache optimization
- HTTP API support

## Key Features

### 1. Scheduling Capabilities
- Local process scheduling
- Remote scheduler integration
- Legacy system compatibility
- Assignment caching

### 2. Performance Optimization
- Lookahead workers
- Async caching
- Query size limits
- Connection pooling

### 3. Protocol Support
- AO Core protocol
- Legacy protocol adaptation
- HTTP/2 and HTTP/3 support
- Multiple store backends

### 4. Security
- Operator validation
- Message signing
- Access control
- Resource limits

## Best Practices

### 1. Development
- Use lookahead workers for performance
- Implement proper error handling
- Cache assignments effectively
- Handle protocol variations

### 2. Operations
- Monitor assignment processing
- Track cache effectiveness
- Handle remote failures
- Manage resource usage

### 3. Integration
- Validate scheduler locations
- Handle protocol differences
- Implement proper timeouts
- Manage state consistently
