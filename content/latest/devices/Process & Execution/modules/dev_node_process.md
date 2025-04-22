# Module: dev_node_process

## Basic Information
- **Source File:** dev_node_process.erl
- **Module Type:** Device Core Processing
- **Purpose:** Node-specific singleton process management with local name registration

## Interface

### Public API
```erlang
% Core operations
-export([info/1]).
```

### Include Files
```erlang
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
```

## Implementation Details

### 1. Device Configuration

```erlang
info(_Opts) ->
    #{
        default => fun lookup/4,
        excludes => [<<"set">>, <<"keys">>]
    }
```

### 2. Process Lookup

```erlang
lookup(Name, _Base, Req, Opts) ->
    % Try to find process in local registry
    case hb_ao:resolve(
        #{ <<"device">> => <<"local-name@1.0">> },
        #{ <<"path">> => <<"lookup">>,
           <<"key">> => Name,
           <<"load">> => true },
        Opts
    ) of
        {ok, ProcessID} ->
            % Process found, load from cache
            hb_cache:read(ProcessID, Opts);
            
        {error, not_found} ->
            % Process not found, spawn if allowed
            case hb_ao:get(<<"spawn">>, Req, true, Opts) of
                true -> spawn_register(Name, Opts);
                false -> {error, not_found}
            end
    end
```

### 3. Process Spawning

```erlang
spawn_register(Name, Opts) ->
    % Look up process definition in node config
    case hb_opts:get(node_processes, #{}, Opts) of
        #{ Name := BaseDef } ->
            % Create and sign process definition
            Signed = hb_message:commit(
                augment_definition(BaseDef, Opts),
                Opts),
            ID = hb_message:id(Signed, signed, Opts),
            
            % Schedule process execution
            {ok, _} = hb_ao:resolve(
                Signed,
                #{
                    <<"path">> => <<"schedule">>,
                    <<"method">> => <<"POST">>,
                    <<"body">> => Signed
                },
                Opts),
            
            % Register with local name service
            case dev_local_name:direct_register(
                #{ <<"key">> => Name,
                   <<"value">> => ID },
                Opts) of
                {ok, _} -> {ok, Signed};
                {error, Err} ->
                    {error, #{
                        <<"status">> => 500,
                        <<"body">> => <<"Failed to register process.">>,
                        <<"details">> => Err
                    }}
            end;
            
        _ -> {error, not_found}
    end
```

### 4. Definition Augmentation

```erlang
augment_definition(BaseDef, Opts) ->
    % Add node address to process definition
    Address = hb_util:human_id(
        ar_wallet:to_address(
            hb_opts:get(priv_wallet, no_viable_wallet, Opts)
        )
    ),
    hb_ao:set(BaseDef, #{
        <<"scheduler">> => Address
    })
```

## Process Definition Format

### Node Configuration
```erlang
#{
    node_processes => #{
        ProcessName => #{
            <<"device">> => <<"process@1.0">>,
            <<"execution-device">> => ExecutionDevice,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            % Process-specific configuration...
        }
    }
}
```

### Augmented Definition
```erlang
#{
    % Original process definition fields...
    <<"scheduler">> => NodeAddress
}
```

## Event Logging

The module logs events at key points:

1. Process Lookup
```erlang
?event(node_process, {lookup, {name, Name}})
```

2. Process Registration
```erlang
?event(node_process, {registering, {name, Name}, {base_def, BaseDef}})
?event(node_process, {spawned, {name, Name}, {process, Signed}})
?event(node_process, {initialized, {name, Name}, {assignment, Assignment}})
?event(node_process, {registered, {name, Name}, {process_id, ID}})
```

## Test Coverage

### 1. Basic Operations
```erlang
lookup_no_spawn_test() ->
    % Tests lookup without spawning
    % Verifies not_found handling

lookup_spawn_test() ->
    % Tests process spawning
    % Verifies process persistence
    % Checks process identity
```

### 2. Process Execution
```erlang
lookup_execute_test() ->
    % Tests full process lifecycle:
    % - Process spawning
    % - Message scheduling
    % - Result retrieval
```

### 3. Test Environment
```erlang
generate_test_opts() ->
    % Creates test environment with:
    % - Lua test script
    % - Process definition
    % - File system store
    % - Test wallet
```

## Integration Points

### Direct Dependencies
- dev_local_name: Local process registration
- hb_message: Message signing and ID generation
- hb_ao: Message resolution
- hb_cache: Process state storage
- ar_wallet: Node address generation

### Usage Context
- Manages node-specific singleton processes
- Integrates with local name service
- Handles process persistence
- Coordinates with scheduler

### Process Lifecycle
1. Definition Loading
   - From node configuration
   - With node address augmentation

2. Process Creation
   - Message signing
   - State initialization
   - Local registration

3. Execution Management
   - Scheduler integration
   - State persistence
   - Result handling

## Key Features

1. **Singleton Pattern**
   - Node-specific processes
   - Local name registration
   - Cross-reboot persistence

2. **Process Management**
   - Automatic spawning
   - Definition augmentation
   - State persistence

3. **Integration**
   - Local name service
   - Process scheduler
   - Message system

4. **Error Handling**
   - Missing definitions
   - Registration failures
   - Lookup failures
