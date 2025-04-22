# Module: dev_genesis_wasm

## Basic Information
- **Source File:** dev_genesis_wasm.erl
- **Module Type:** Device Core Processing
- **Purpose:** Emulates legacynet AO process environment using HyperBEAM infrastructure

## Interface

### Public API
```erlang
% Core operations
-export([init/3, compute/3, normalize/3, snapshot/3]).
```

### Include Files
```erlang
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").
```

### Constants
```erlang
% Timeout for legacy CU status check
-define(STATUS_TIMEOUT, 100).
```

## Implementation Details

### 1. Server Management

```erlang
ensure_started(Opts) ->
    % Check if server is running
    IsRunning = is_genesis_wasm_server_running(Opts),
    IsCompiled = hb_features:genesis_wasm(),
    GenWASMProc = is_pid(hb_name:lookup(<<"genesis-wasm@1.0">>)),
    
    case IsRunning orelse (IsCompiled andalso GenWASMProc) of
        true -> true;
        false ->
            % Start server process
            PID = spawn(fun() ->
                % Set up directories
                NodeURL = "http://localhost:" ++ 
                         integer_to_list(hb_opts:get(port, no_port, Opts)),
                DBDir = filename:absname(hb_util:list(
                    hb_opts:get(genesis_wasm_db_dir,
                               "cache-mainnet/genesis-wasm",
                               Opts))),
                CheckpointDir = filename:absname(hb_util:list(
                    hb_opts:get(genesis_wasm_checkpoints_dir,
                               "cache-mainnet/genesis-wasm/checkpoints",
                               Opts))),
                DatabaseUrl = filename:absname(DBDir ++ "/genesis-wasm-db"),
                
                % Create directories
                filelib:ensure_path(DBDir),
                filelib:ensure_path(CheckpointDir),
                
                % Start server process
                Port = open_port({spawn_executable,
                    "_build/genesis-wasm-server/launch-monitored.sh"},
                    [binary, use_stdio, stderr_to_stdout,
                     {args, ["npm", "--prefix",
                            "_build/genesis-wasm-server",
                            "run", "dev"]},
                     {env, [
                         {"UNIT_MODE", "hbu"},
                         {"HB_URL", NodeURL},
                         {"PORT", integer_to_list(
                             hb_opts:get(genesis_wasm_port, 6363, Opts))},
                         {"DB_URL", DatabaseUrl},
                         {"NODE_CONFIG_ENV", "development"},
                         {"DEFAULT_LOG_LEVEL", hb_util:list(
                             hb_opts:get(genesis_wasm_log_level,
                                        "error", Opts))},
                         {"WALLET_FILE", filename:absname(hb_util:list(
                             hb_opts:get(priv_key_location,
                                        no_key, Opts)))},
                         {"DISABLE_PROCESS_FILE_CHECKPOINT_CREATION", "false"},
                         {"PROCESS_MEMORY_FILE_CHECKPOINTS_DIR", CheckpointDir}
                     ]}]),
                collect_events(Port)
            end),
            
            % Register process
            hb_name:register(<<"genesis-wasm@1.0">>, PID),
            
            % Wait for startup
            hb_util:until(fun() ->
                receive after 2000 -> ok end,
                Status = is_genesis_wasm_server_running(Opts),
                Status
            end),
            true
    end
```

### 2. Computation Handling

```erlang
compute(Msg, Msg2, Opts) ->
    % Validate genesis-wasm feature
    case ensure_started(Opts) of
        true ->
            % Execute via delegated-compute device
            case hb_ao:resolve(Msg,
                    {as, <<"delegated-compute@1.0">>, Msg2}, Opts) of
                {ok, Msg3} ->
                    % Apply patches via patch device
                    {ok, Msg4} = hb_ao:resolve(Msg3,
                        {as, <<"patch@1.0">>, Msg2}, Opts),
                    {ok, Msg4};
                {error, Error} -> {error, Error}
            end;
        false ->
            {error, #{
                <<"status">> => 500,
                <<"message">> =>
                    <<"HyperBEAM was not compiled with genesis-wasm@1.0 on "
                        "this node.">>
            }}
    end
```

### 3. Server Status Management

```erlang
% Check server status with caching
is_genesis_wasm_server_running(Opts) ->
    case get(genesis_wasm_pid) of
        undefined ->
            Parent = self(),
            PID = spawn(fun() ->
                Parent ! {ok, self(), status(Opts)}
            end),
            receive
                {ok, PID, Status} ->
                    put(genesis_wasm_pid, Status),
                    Status
            after ?STATUS_TIMEOUT ->
                erlang:exit(PID, kill),
                false
            end;
        _ -> true
    end

% Check server status via HTTP
status(Opts) ->
    ServerPort = integer_to_binary(
        hb_opts:get(genesis_wasm_port, 6363, Opts)),
    try hb_http:get(
        <<"http://localhost:", ServerPort/binary, "/status">>,
        Opts) of
        {ok, _} -> true;
        _ -> false
    catch
        _:_ -> false
    end
```

### 4. Event Collection

```erlang
% Collect and log server events
collect_events(Port) ->
    collect_events(Port, <<>>).
collect_events(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_events(Port,
                log_server_events(<<Acc/binary, Data/binary>>));
        stop ->
            port_close(Port),
            ok
    end

% Log server output lines
log_server_events(Bin) when is_binary(Bin) ->
    log_server_events(binary:split(Bin, <<"\n">>, [global]));
log_server_events([Remaining]) -> Remaining;
log_server_events([Line | Rest]) ->
    ?event(genesis_wasm_server, {server_logged, Line}),
    log_server_events(Rest)
```

## Configuration Options

### Server Settings
- **genesis_wasm_port**: Server port (default: 6363)
- **genesis_wasm_db_dir**: Database directory (default: "cache-mainnet/genesis-wasm")
- **genesis_wasm_checkpoints_dir**: Checkpoints directory (default: "cache-mainnet/genesis-wasm/checkpoints")
- **genesis_wasm_log_level**: Log level (default: "error")
- **priv_key_location**: Wallet key file location

### Feature Flags
- **genesis_wasm**: Must be compiled with genesis-wasm support
- **DISABLE_PROCESS_FILE_CHECKPOINT_CREATION**: Checkpoint creation control
- **UNIT_MODE**: Set to "hbu" for HyperBEAM unit mode

## Event Logging

The module logs events at key points:

1. Server Management
```erlang
?event({ensure_started, genesis_wasm, self()})
?event({genesis_wasm_booting, {pid, self()}})
?event({genesis_wasm_port_opened, {port, Port}})
?event({genesis_wasm_starting, {pid, PID}})
?event({genesis_wasm_started, {pid, PID}})
```

2. Status Checking
```erlang
?event(genesis_wasm_pinging_server)
?event({genesis_wasm_get_info_endpoint, {worker, self()}})
?event({genesis_wasm_received_status, Status})
?event({genesis_wasm_status_check, timeout})
```

3. Server Output
```erlang
?event(genesis_wasm_server, {server_logged, Line})
?event(genesis_wasm_stopped, {pid, self()})
```

## Integration Points

### Direct Dependencies
- hb_features: Feature flag checking
- hb_name: Process registration
- hb_http: Server status checking
- hb_ao: Message resolution
- hb_opts: Configuration management

### Usage Context
- Called by dev_process for legacy AO execution
- Integrates with delegated-compute device
- Manages genesis-wasm server process
- Handles legacy process migration
