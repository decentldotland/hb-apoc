# Module: dev_lua

## Basic Information
- **Source File:** dev_lua.erl
- **Module Type:** Device Core Processing
- **Purpose:** Lua script execution device with sandboxing capabilities

## Interface

### Public API
```erlang
% Core operations
-export([info/1, init/3, snapshot/3, normalize/3]).
```

### Include Files
```erlang
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
```

## Sandboxing Configuration

### Default Sandbox Functions
```erlang
-define(DEFAULT_SANDBOX, [
    {['_G', io], <<"sandboxed">>},
    {['_G', file], <<"sandboxed">>},
    {['_G', os, execute], <<"sandboxed">>},
    {['_G', os, exit], <<"sandboxed">>},
    {['_G', os, getenv], <<"sandboxed">>},
    {['_G', os, remove], <<"sandboxed">>},
    {['_G', os, rename], <<"sandboxed">>},
    {['_G', os, tmpname], <<"sandboxed">>},
    {['_G', package], <<"sandboxed">>},
    {['_G', loadfile], <<"sandboxed">>},
    {['_G', require], <<"sandboxed">>},
    {['_G', dofile], <<"sandboxed">>},
    {['_G', load], <<"sandboxed">>},
    {['_G', loadfile], <<"sandboxed">>},
    {['_G', loadstring], <<"sandboxed">>}
]).
```

## Implementation Details

### 1. Script Loading & Initialization

```erlang
init(Base, Req, Opts) ->
    case find_script(Base, Opts) of
        {ok, Script} ->
            % Initialize Lua state
            State0 = luerl:init(),
            {ok, _, State1} = luerl:do_dec(Script, State0),
            
            % Apply sandboxing if configured
            State2 = case hb_ao:get(<<"sandbox">>, Base, false, Opts) of
                false -> State1;
                true -> sandbox(State1, ?DEFAULT_SANDBOX, Opts);
                Spec -> sandbox(State1, Spec, Opts)
            end,
            
            % Add AO Core resolver
            {ok, State3} = add_ao_core_resolver(Base, State2, Opts),
            
            % Return initialized state
            {ok, hb_private:set(Base, <<"state">>, State3, Opts)};
            
        Error -> Error
    end
```

### 2. Function Resolution

```erlang
compute(Key, RawBase, Req, Opts) ->
    % Initialize if needed
    {ok, Base} = ensure_initialized(RawBase, Req, Opts),
    State = hb_private:get(<<"state">>, Base, Opts),
    
    % Get function and parameters
    Function = hb_ao:get_first([
        {Req, <<"body/function">>},
        {Req, <<"function">>},
        {Base, <<"function">>}
    ], Key, Opts),
    
    Params = hb_ao:get_first([
        {Req, <<"body/parameters">>},
        {Req, <<"parameters">>},
        {Base, <<"parameters">>}
    ], [Base, Req, #{}], Opts),
    
    % Execute function
    case luerl:call_function_dec([Function], encode(Params), State) of
        {ok, [LuaResult], NewState} when is_map(LuaResult) ->
            Result = decode(LuaResult),
            {ok, Result#{
                <<"priv">> => #{<<"state">> => NewState}
            }};
        {ok, [LuaResult], _} ->
            {ok, LuaResult};
        {lua_error, Error, Details} ->
            {error, #{
                <<"status">> => 500,
                <<"body">> => Error,
                <<"details">> => Details
            }}
    end
```

### 3. State Management

```erlang
% Create snapshot
snapshot(Base, _Req, Opts) ->
    case hb_private:get(<<"state">>, Base, Opts) of
        not_found ->
            {error, <<"Cannot snapshot Lua state: state not initialized.">>};
        State ->
            {ok, #{
                <<"body">> => term_to_binary(luerl:externalize(State))
            }}
    end

% Restore from snapshot
normalize(Base, _Req, Opts) ->
    case hb_private:get(<<"state">>, Base, Opts) of
        not_found ->
            SerializedState = hb_ao:get(
                [<<"snapshot">>] ++ DeviceKey ++ [<<"body">>],
                {as, dev_message, Base},
                Opts
            ),
            case SerializedState of
                not_found -> throw({error, no_lua_state_snapshot_found});
                State ->
                    ExternalizedState = binary_to_term(State),
                    InternalizedState = luerl:internalize(ExternalizedState),
                    {ok, hb_private:set(Base, <<"state">>, 
                                      InternalizedState, Opts)}
            end;
        _ -> {ok, Base}
    end
```

## Event Logging

The module logs events at key points:

1. Initialization
```erlang
?event(debug_lua, lua_state_already_initialized)
?event(debug_lua, initializing_lua_state)
```

2. Function Resolution
```erlang
?event(debug_lua, compute_called)
?event(debug_lua, ensure_initialized_done)
?event(debug_lua, function_found)
?event(debug_lua, parameters_found)
```

3. Execution
```erlang
?event({calling_lua_func, {function, Function}, {args, Params}})
?event(debug_lua, calling_lua_func)
?event(debug_lua, got_lua_result)
?event(debug_lua, decoded_result)
```

## Test Coverage

### 1. Basic Operations
```erlang
simple_invocation_test() ->
    % Tests basic Lua function call
    % Verifies result retrieval

sandboxed_failure_test() ->
    % Tests sandbox restrictions
    % Verifies blocked operations
```

### 2. AO Core Integration
```erlang
ao_core_sandbox_test() ->
    % Tests device sandboxing
    % Verifies allowed/blocked devices

ao_core_resolution_from_lua_test() ->
    % Tests AO Core resolution from Lua
    % Verifies result handling
```

### 3. Performance Tests
```erlang
direct_benchmark_test() ->
    % Tests execution performance
    % Verifies >10 iterations in 3s

pure_lua_process_benchmark_test_() ->
    % Tests process execution performance
    % Measures execution time per message
```

### 4. Process Integration
```erlang
pure_lua_process_test() ->
    % Tests Lua process execution
    % Verifies process state handling

invoke_aos_test() ->
    % Tests AOS integration
    % Verifies command execution

aos_authority_not_trusted_test() ->
    % Tests authority validation
    % Verifies untrusted message handling
```

## Integration Points

### Direct Dependencies
- luerl: Lua VM implementation
- hb_ao: Message handling
- hb_private: Private state management
- hb_cache: Script caching
- hb_message: Message operations

### Usage Context
- Called by dev_process for Lua execution
- Integrates with AO Core resolution
- Manages Lua state persistence
- Provides sandboxed environment
- Supports HTTP preprocessing
