# Module: dev_wasi

## Basic Information
- **Source File:** dev_wasi.erl
- **Module Type:** Device Core Processing
- **Purpose:** Virtual filesystem device implementing WASI-preview-1 standard

## Interface

### Public API
```erlang
% Core operations
-export([init/3, compute/1, stdout/1]).

% WASI-preview-1 functions
-export([path_open/3, fd_write/3, fd_read/3, clock_time_get/3]).
```

### Include Files
```erlang
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
```

## Virtual Filesystem Structure

### Initial VFS Setup
```erlang
-define(INIT_VFS,
    #{
        <<"dev">> => #{
            <<"stdin">> => <<>>,
            <<"stdout">> => <<>>,
            <<"stderr">> => <<>>
        }
    }
).
```

### Initial File Descriptors
```erlang
-define(INIT_FDS,
    #{
        <<"0">> => #{
            <<"filename">> => <<"/dev/stdin">>,
            <<"offset">> => 0
        },
        <<"1">> => #{
            <<"filename">> => <<"/dev/stdout">>,
            <<"offset">> => 0
        },
        <<"2">> => #{
            <<"filename">> => <<"/dev/stderr">>,
            <<"offset">> => 0
        }
    }
).
```

## Implementation Details

### 1. Initialization
```erlang
init(M1, _M2, Opts) ->
    % Set up WASI stdlib
    MsgWithLib = hb_ao:set(M1, #{
        <<"wasm/stdlib/wasi_snapshot_preview1">> =>
            #{ <<"device">> => <<"WASI@1.0">>}
    }, Opts),
    
    % Initialize file descriptors
    MsgWithFDs = hb_ao:set(MsgWithLib,
        <<"file-descriptors">>, ?INIT_FDS, Opts),
    
    % Initialize virtual filesystem
    CompleteMsg = hb_ao:set(MsgWithFDs,
        <<"vfs">>, ?INIT_VFS, Opts),
    
    {ok, CompleteMsg}
```

### 2. File Operations

#### Path Opening
```erlang
path_open(Msg1, Msg2, Opts) ->
    % Extract WASI arguments
    Instance = hb_private:get(<<"instance">>, Msg1, Opts),
    [FDPtr, LookupFlag, PathPtr|_] = hb_ao:get(<<"args">>, Msg2, Opts),
    
    % Get path from WASM memory
    Path = hb_beamr_io:read_string(Instance, PathPtr),
    
    % Create or get file descriptor
    FD = #{
        <<"index">> := Index
    } = case hb_ao:get(<<"vfs/", Path/binary>>, Msg1, Opts) of
        not_found -> #{
            <<"index">> => length(hb_ao:keys(FDs)) + 1,
            <<"filename">> => Path,
            <<"offset">> => 0
        };
        F -> F
    end,
    
    % Return file descriptor
    {ok, #{
        <<"state">> => hb_ao:set(Msg1, <<"vfs/", Path/binary>>, FD),
        <<"results">> => [0, Index]
    }}
```

#### File Reading
```erlang
fd_read(S, Instance, [FDNum, VecsPtr, NumVecs, RetPtr], BytesRead, Opts) ->
    % Get file info
    FDNumStr = integer_to_binary(FDNum),
    Filename = hb_ao:get(
        <<"file-descriptors/", FDNumStr/binary, "/filename">>,
        S, Opts),
    
    % Parse iovec
    {VecPtr, Len} = parse_iovec(Instance, VecsPtr),
    
    % Read data
    Data = hb_ao:get(<<"vfs/", Filename/binary>>, S, Opts),
    Offset = hb_ao:get(
        <<"file-descriptors/", FDNumStr/binary, "/offset">>,
        S, Opts),
    ReadSize = min(Len, byte_size(Data) - Offset),
    Bin = binary:part(Data, Offset, ReadSize),
    
    % Write to WASM memory
    ok = hb_beamr_io:write(Instance, VecPtr, Bin),
    
    % Update offset and continue
    fd_read(
        hb_ao:set(S,
            <<"file-descriptors/", FDNumStr/binary, "/offset">>,
            Offset + ReadSize, Opts),
        Instance,
        [FDNum, VecsPtr + 16, NumVecs - 1, RetPtr],
        BytesRead + ReadSize,
        Opts
    )
```

#### File Writing
```erlang
fd_write(S, Instance, [FDnum, Ptr, Vecs, RetPtr], BytesWritten, Opts) ->
    % Get file info
    FDNumStr = integer_to_binary(FDnum),
    FD = hb_ao:get(<<"file-descriptors/", FDNumStr/binary>>, S, Opts),
    Filename = hb_ao:get(<<"filename">>, FD, Opts),
    StartOffset = hb_ao:get(<<"offset">>, FD, Opts),
    
    % Read data from WASM memory
    {VecPtr, Len} = parse_iovec(Instance, Ptr),
    {ok, Data} = hb_beamr_io:read(Instance, VecPtr, Len),
    
    % Update file content
    Before = binary:part(OrigData, 0, StartOffset),
    After = binary:part(OrigData, StartOffset,
                       byte_size(OrigData) - StartOffset),
    
    % Update state and continue
    S1 = hb_ao:set(S,
        <<"file-descriptors/", FDNumStr/binary, "/offset">>,
        StartOffset + byte_size(Data), Opts),
    S2 = hb_ao:set(S1,
        <<"vfs/", Filename/binary>>,
        <<Before/binary, Data/binary, After/binary>>, Opts),
    
    fd_write(S2, Instance,
        [FD, Ptr + 16, Vecs - 1, RetPtr],
        BytesWritten + byte_size(Data), Opts)
```

### 3. WASI Support Functions

```erlang
% Parse WASI iovec structure
parse_iovec(Instance, Ptr) ->
    {ok, VecStruct} = hb_beamr_io:read(Instance, Ptr, 16),
    <<BinPtr:64/little-unsigned-integer,
      Len:64/little-unsigned-integer>> = VecStruct,
    {BinPtr, Len}

% Get stdout content
stdout(M) ->
    hb_ao:get(<<"vfs/dev/stdout">>, M)

% WASI clock implementation
clock_time_get(Msg1, _Msg2, Opts) ->
    State = hb_ao:get(<<"state">>, Msg1, Opts),
    {ok, #{ <<"state">> => State, <<"results">> => [1] }}
```

## Test Coverage

### 1. VFS Serialization
```erlang
vfs_is_serializable_test() ->
    % Tests VFS message serialization
    % Verifies message conversion and matching
```

### 2. WASI Stack Tests
```erlang
wasi_stack_is_serializable_test() ->
    % Tests WASI stack message serialization
    % Verifies message conversion and matching
```

### 3. AOS Integration
```erlang
basic_aos_exec_test() ->
    % Tests WASI integration with AOS
    % Verifies memory allocation and string handling
    % Tests command execution and result parsing
```

## Integration Points

### Direct Dependencies
- hb_beamr_io: WASM memory operations
- hb_ao: Message handling
- hb_private: Private state management
- hb_json: JSON encoding/decoding

### Usage Context
- Called by dev_wasm for WASI operations
- Provides filesystem abstraction
- Manages WASM memory interactions
- Implements WASI-preview-1 interface
