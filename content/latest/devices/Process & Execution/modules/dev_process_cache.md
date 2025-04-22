# Module: dev_process_cache

## Basic Information
- **Source File:** dev_process_cache.erl
- **Module Type:** Device Core Processing
- **Purpose:** Wrapper around hb_cache providing convenient interface for process result caching

## Interface

### Public API
```erlang
% Core operations
-export([latest/2, latest/3, latest/4]).  % Get latest process state
-export([read/2, read/3]).                % Read process state
-export([write/4]).                       % Write process state
```

### Include Files
```erlang
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
```

## Implementation Details

### 1. Process Result Reading

```erlang
read(ProcID, Opts) ->
    hb_util:ok(latest(ProcID, Opts)).

read(ProcID, SlotRef, Opts) ->
    ?event({reading_computed_result, ProcID, SlotRef}),
    Path = path(ProcID, SlotRef, Opts),
    hb_cache:read(Path, Opts).
```

Key aspects:
- Reads process state by slot number or message ID
- Uses hb_cache for actual storage operations
- Logs read operations with event system

### 2. Process Result Writing

```erlang
write(ProcID, Slot, Msg, Opts) ->
    % Write to cache root
    {ok, Root} = hb_cache:write(Msg, Opts),
    
    % Link by slot number
    SlotNumPath = path(ProcID, Slot, Opts),
    hb_cache:link(Root, SlotNumPath, Opts),
    
    % Link by message ID
    MsgIDPath = path(
        ProcID,
        ID = hb_util:human_id(hb_ao:get(id, Msg)),
        Opts
    ),
    ?event({linking_id, {proc_id, ProcID}, {slot, Slot}, {id, ID}, {path, MsgIDPath}}),
    hb_cache:link(Root, MsgIDPath, Opts),
    
    {ok, SlotNumPath}.
```

Key aspects:
- Writes process state to cache
- Creates links by both slot number and message ID
- Enables retrieval by either reference
- Logs linking operations

### 3. Latest State Retrieval

```erlang
latest(ProcID, Opts) -> 
    latest(ProcID, [], Opts).

latest(ProcID, RequiredPath, Opts) ->
    latest(ProcID, RequiredPath, undefined, Opts).

latest(ProcID, RawRequiredPath, Limit, Opts) ->
    % Convert path to binary keys
    RequiredPath = case RawRequiredPath of
        undefined -> [];
        [] -> [];
        _ -> hb_path:term_to_path_parts(RawRequiredPath, Opts)
    end,
    
    % Get all slots
    Path = path(ProcID, slot_root, Opts),
    AllSlots = hb_cache:list_numbered(Path, Opts),
    
    % Apply limit if specified
    CappedSlots = case Limit of
        undefined -> AllSlots;
        _ -> lists:filter(fun(Slot) -> Slot =< Limit end, AllSlots)
    end,
    
    % Find highest slot with required path
    case first_with_path(ProcID, RequiredPath, 
                        lists:reverse(lists:sort(CappedSlots)), 
                        Opts) of
        not_found -> not_found;
        SlotNum ->
            {ok, Msg} = hb_cache:read(path(ProcID, SlotNum, Opts), Opts),
            {ok, SlotNum, Msg}
    end.
```

Key aspects:
- Finds latest process state optionally filtered by:
  * Required path presence
  * Maximum slot number
- Returns both slot number and state
- Handles path conversion and validation

### 4. Path Management

```erlang
path(ProcID, Ref, Opts) ->
    path(ProcID, Ref, [], Opts).

path(ProcID, Ref, PathSuffix, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:path(Store, [
        <<"computed">>,
        hb_util:human_id(ProcID)
    ] ++ case Ref of
        Int when is_integer(Int) -> 
            ["slot", integer_to_binary(Int)];
        root -> [];
        slot_root -> ["slot"];
        _ -> [Ref]
    end ++ PathSuffix).
```

Key aspects:
- Constructs cache paths for process states
- Handles different reference types:
  * Integer slots
  * Root references
  * Message IDs
- Supports path suffixes for nested data

## Event Logging

The module logs events at key points:

1. Read Operations
```erlang
?event({reading_computed_result, ProcID, SlotRef})
```

2. Write Operations
```erlang
?event({linking_id, {proc_id, ProcID}, {slot, Slot}, {id, ID}, {path, MsgIDPath}})
```

3. Latest State Lookup
```erlang
?event({latest_called, {proc_id, ProcID}, {required_path, RawRequiredPath}, {limit, Limit}})
?event({required_path_converted, {proc_id, ProcID}, {required_path, RequiredPath}})
?event({all_slots, {proc_id, ProcID}, {slots, AllSlots}})
```

## Test Coverage

The module includes comprehensive tests:

1. **Write and Read Process Outputs**
```erlang
test_write_and_read_output(Opts) ->
    % Tests:
    % - Writing signed and unsigned outputs
    % - Reading by slot number
    % - Reading by message ID
    % - Verifying message matching
```

2. **Latest Output Finding**
```erlang
find_latest_outputs(Opts) ->
    % Tests:
    % - Finding latest slot without qualifiers
    % - Finding latest slot with required path
    % - Finding latest slot with deep path
    % - Finding latest slot with limit
```

## Integration Points

### Direct Dependencies
- hb_cache: Underlying cache operations
- hb_store: Storage path management
- hb_path: Path manipulation
- hb_util: Utility functions
- hb_ao: Message access
- hb_message: Message operations

### Usage Context
- Called by dev_process for state caching
- Integrates with hb_cache system
- Supports process state persistence
- Enables state retrieval by slot or ID
