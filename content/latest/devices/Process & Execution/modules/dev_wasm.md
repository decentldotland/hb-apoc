# Module: dev_wasm

## Basic Information
- **Source File:** dev_wasm.erl
- **Module Type:** Device Core Processing
- **Purpose:** WASM execution device using Memory-64 preview standard
- **Backend:** Uses beamr (Erlang wrapper for WAMR - WebAssembly Micro Runtime)

## Interface

### Public API
```erlang
% Core operations
-export([info/2, init/3, compute/3, import/3, terminate/3, snapshot/3, normalize/3]).

% Device API
-export([instance/3]).

% Test API
-export([cache_wasm_image/1, cache_wasm_image/2]).
```

### Include Files
```erlang
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
```

## Message Requirements

### Init Operation
```
M1/Init ->
    Assumes:
        M1/process
        M1/[Prefix]/image
    Generates:
        /priv/[Prefix]/instance
        /priv/[Prefix]/import-resolver
    Side-effects:
        Creates WASM executor in HyperBEAM node memory
```

### Compute Operation
```
M1/Compute ->
    Assumes:
        M1/priv/[Prefix]/instance
        M1/priv/[Prefix]/import-resolver
        M1/process
        M2/message
        M2/message/function OR M1/function
        M2/message/parameters OR M1/parameters
    Generates:
        /results/[Prefix]/type
        /results/[Prefix]/output
    Side-effects:
        Calls WASM executor with message and process
```

### State Operation
```
M1/[Prefix]/state ->
    Assumes:
        M1/priv/[Prefix]/instance
    Generates:
        Raw binary WASM state
```

## Implementation Details

### 1. WASM Image Loading

```erlang
init(M1, M2, Opts) ->
    % Get input prefix for parameters
    InPrefix = dev_stack:input_prefix(M1, M2, Opts),
    % Get prefix for state
    Prefix = dev_stack:prefix(M1, M2, Opts),
    
    % Load WASM image from various sources
    ImageBin = case hb_ao:get(<<InPrefix/binary, "/image">>, M1, Opts) of
        not_found -> 
            case hb_ao:get(<<"body">>, M1, Opts) of
                not_found -> throw({wasm_init_error, ...});
                Bin when is_binary(Bin) -> Bin
            end;
        ImageID when ?IS_ID(ImageID) ->
            {ok, ImageMsg} = hb_cache:read(ImageID, Opts),
            hb_ao:get(<<"body">>, ImageMsg, Opts);
        ImageMsg when is_map(ImageMsg) ->
            hb_ao:get(<<"body">>, ImageMsg, Opts);
        Image when is_binary(Image) -> Image
    end,
    
    % Start WASM executor
    {ok, Instance, _, _} = hb_beamr:start(ImageBin, Mode),
    
    % Set up instance and handlers
    {ok, hb_private:set(M1, #{
        <<Prefix/binary, "/write">> => fun write_handler/1,
        <<Prefix/binary, "/read">> => fun read_handler/1,
        <<Prefix/binary, "/instance">> => Instance,
        <<Prefix/binary, "/import-resolver">> => fun default_import_resolver/3
    }, Opts)}
```

### 2. Computation Execution

```erlang
compute(RawM1, M2, Opts) ->
    % Normalize message state
    {ok, M1} = normalize(RawM1, M2, Opts),
    
    % Extract function and parameters
    WASMFunction = hb_ao:get_first([
        {M2, <<"body/function">>},
        {M2, <<"function">>},
        {M1, <<"function">>}
    ], Opts),
    
    WASMParams = hb_ao:get_first([
        {M2, <<"body/parameters">>},
        {M2, <<"parameters">>},
        {M1, <<"parameters">>}
    ], Opts),
    
    % Execute WASM function
    {ResType, Res, MsgAfterExecution} = hb_beamr:call(
        instance(M1, M2, Opts),
        WASMFunction,
        WASMParams,
        ImportResolver,
        M1,
        Opts
    ),
    
    % Return results
    {ok, hb_ao:set(MsgAfterExecution, #{
        <<"results/", Prefix/binary, "/type">> => ResType,
        <<"results/", Prefix/binary, "/output">> => Res
    })}
```

### 3. State Management

```erlang
% Normalize state
normalize(RawM1, M2, Opts) ->
    case instance(RawM1, M2, Opts) of
        not_found ->
            % Load from snapshot if no instance
            Memory = hb_ao:get([<<"snapshot">>] ++ DeviceKey ++ [<<"body">>],
                             {as, dev_message, RawM1}, Opts),
            case Memory of
                not_found -> throw({error, no_wasm_instance_or_snapshot});
                State ->
                    {ok, M1} = init(RawM1, State, Opts),
                    hb_beamr:deserialize(instance(M1, M2, Opts), State),
                    M1
            end;
        _ -> RawM1
    end

% Create snapshot
snapshot(M1, M2, Opts) ->
    Instance = instance(M1, M2, Opts),
    {ok, Serialized} = hb_beamr:serialize(Instance),
    {ok, #{ <<"body">> => Serialized }}
```

## Event Logging

The module logs events at key points:

1. Initialization
```erlang
?event(running_init)
?event({in_prefix, InPrefix})
?event({getting_wasm_image, ImageID})
?event(wasm_image_message_directly_provided)
?event({setting_wasm_instance, Instance, {prefix, Prefix}})
```

2. Computation
```erlang
?event(running_compute)
?event({skipping_wasm_exec, {reason, wasm_function_not_provided}})
?event({calling_wasm_executor, {wasm_function, WASMFunction}})
```

3. State Management
```erlang
?event({normalize_raw_m1, RawM1})
?event(wasm_instance_found_not_deserializing)
?event(snapshot, generating_snapshot)
?event(terminate_called_on_dev_wasm)
```

## Test Coverage

1. **Initialization Tests**
```erlang
input_prefix_test() ->
    % Tests input prefix handling
process_prefixes_test() ->
    % Tests process prefix handling
init_test() ->
    % Tests basic initialization
```

2. **Execution Tests**
```erlang
basic_execution_test() ->
    % Tests basic WASM function execution
basic_execution_64_test() ->
    % Tests Memory-64 execution
imported_function_test() ->
    % Tests imported function handling
```

3. **Performance Tests**
```erlang
benchmark_test() ->
    % Tests execution performance
    % Verifies >5 iterations in 0.5s
```

4. **State Management Tests**
```erlang
state_export_and_restore_test() ->
    % Tests state serialization
    % Tests state restoration
    % Verifies computation results
```

## Integration Points

### Direct Dependencies
- hb_beamr: WASM runtime interface
- hb_beamr_io: WASM I/O operations
- dev_stack: Device stack management
- hb_ao: Message handling
- hb_cache: Image and state caching
- hb_private: Private state management

### Usage Context
- Called by dev_process for WASM execution
- Integrates with WAMR runtime
- Manages WASM state persistence
- Handles function imports/exports
