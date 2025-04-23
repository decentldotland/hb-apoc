# Module: dev_delegated_compute

## Basic Information
- **Source File:** dev_delegated_compute.erl
- **Module Type:** Device Core Processing
- **Purpose:** Wrapper for remote machine computation using JSON-Iface

## Interface

### Public API
```erlang
% Core operations
-export([init/3, compute/3, normalize/3, snapshot/3]).
```

### Include Files
```erlang
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
```

## Implementation Details

### 1. State Management

```erlang
% Simple initialization - no special handling needed
init(Msg1, _Msg2, _Opts) ->
    {ok, Msg1}.

% State normalization - pass through
normalize(Msg1, _Msg2, _Opts) ->
    {ok, Msg1}.

% State snapshot - pass through
snapshot(Msg1, _Msg2, _Opts) ->
    {ok, Msg1}.
```

### 2. Computation Handling

```erlang
compute(Msg1, Msg2, Opts) ->
    % Get process ID from either message
    RawProcessID = dev_process:process_id(Msg1, #{}, Opts),
    ProcessID = case RawProcessID of
        not_found -> hb_ao:get(<<"process-id">>, Msg2, Opts);
        ProcID -> ProcID
    end,
    
    % Get output prefix for results
    OutputPrefix = dev_stack:prefix(Msg1, Msg2, Opts),
    
    % Execute computation
    case do_compute(ProcessID, Msg2, Opts) of
        {ok, JSONRes} ->
            % Convert JSON result to message
            {ok, Msg} = dev_json_iface:json_to_message(JSONRes, Opts),
            
            % Return results in both message and JSON formats
            {ok, hb_ao:set(Msg1, #{
                <<OutputPrefix/binary, "/results">> => Msg,
                <<OutputPrefix/binary, "/results/json">> => #{
                    <<"content-type">> => <<"application/json">>,
                    <<"body">> => JSONRes
                }
            }, Opts)};
            
        {error, Error} -> {error, Error}
    end
```

### 3. Remote Execution

```erlang
do_compute(ProcID, Msg2, Opts) ->
    % Get slot number
    Slot = hb_ao:get(<<"slot">>, Msg2, Opts),
    
    % Convert to AOS2 format
    {ok, AOS2 = #{ <<"body">> := Body }} =
        dev_scheduler_formats:assignments_to_aos2(
            ProcID,
            #{ Slot => Msg2 },
            false,
            Opts
        ),
    
    % Execute via relay device
    case hb_ao:resolve(
        #{ <<"device">> => <<"relay@1.0">>,
           <<"content-type">> => <<"application/json">> },
        AOS2#{
            <<"path">> => <<"call">>,
            <<"relay-method">> => <<"POST">>,
            <<"relay-body">> => Body,
            <<"relay-path">> =>
                <<"/result/",
                  (integer_to_binary(Slot))/binary,
                  "?process-id=", ProcID/binary>>,
            <<"content-type">> => <<"application/json">>
        },
        Opts#{
            hashpath => ignore,
            cache_control => [<<"no-store">>, <<"no-cache">>]
        }
    ) of
        {ok, Response} ->
            JSONRes = hb_ao:get(<<"body">>, Response, Opts),
            {ok, JSONRes};
        {Err, Error} when Err == error; Err == failure ->
            {error, Error}
    end
```

## Message Flow

### Input Message Requirements
1. Process Identification
   - Either in base message via dev_process:process_id/3
   - Or in request message as <<"process-id">>

2. Computation Details
   - Slot number in request message
   - Computation parameters in request message

### Output Message Structure
1. Results Path (`<prefix>/results`)
   - Converted JSON response as message

2. JSON Results Path (`<prefix>/results/json`)
   ```json
   {
     "content-type": "application/json",
     "body": "<JSON response>"
   }
   ```

## Event Logging

The module logs events at key points:

1. Computation Results
```erlang
?event({compute_lite_res,
    {process_id, ProcessID},
    {slot, Slot},
    {json_res, {string, JSONRes}},
    {req, Msg2}})
```

2. Remote Execution
```erlang
?event({do_compute_msg, {req, Msg2}})
?event({do_compute_msg, {aos2, {string, Body}}})
?event({delegated_compute_res_metadata,
    {req, maps:without([<<"body">>], Response)}})
```

## Integration Points

### Direct Dependencies
- dev_process: Process ID extraction
- dev_stack: Prefix management
- dev_json_iface: JSON conversion
- dev_scheduler_formats: AOS2 format conversion
- hb_ao: Message resolution

### Usage Context
- Used as standalone for trusted remote results
- Used as execution device for AO processes
- Integrates with relay device for remote calls
- Handles JSON-Iface protocol

### Remote Protocol
1. Request Format
   - HTTP POST to `/result/<slot>?process-id=<id>`
   - Body contains AOS2-formatted computation request
   - Content-Type: application/json

2. Response Format
   - JSON response body
   - No caching (no-store, no-cache)
   - Response converted to message format

## Key Features

1. **Dual Usage Modes**
   - Standalone for trusted remote results
   - AO process execution device

2. **Format Handling**
   - AOS2 format for requests
   - JSON-Iface for responses
   - Message format conversion

3. **State Management**
   - Stateless operation
   - Pass-through initialization
   - No snapshot requirements

4. **Error Handling**
   - Remote execution errors
   - JSON conversion errors
   - Process ID resolution
