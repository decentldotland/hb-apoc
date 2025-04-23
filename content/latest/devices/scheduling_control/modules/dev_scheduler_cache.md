# Module: dev_scheduler_cache

## Basic Information
- **Source File:** dev_scheduler_cache.erl
- **Module Type:** Device Scheduling Control
- **Purpose:** Manages caching of process assignments and scheduler locations with symlink-based organization

## Interface

### Public API
```erlang
% Assignment operations
-export([write/2, read/3, list/2, latest/2]).
% Location operations
-export([read_location/2, write_location/2]).
```

## Core Functionality

### 1. Assignment Management

#### Writing Assignments
```erlang
write(Assignment, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ProcID = hb_ao:get(<<"process">>, Assignment),
    Slot = hb_ao:get(<<"slot">>, Assignment),
    
    % Write to main cache
    case hb_cache:write(Assignment, Opts) of
        {ok, RootPath} ->
            % Create symlink for process-specific access
            hb_store:make_link(
                Store,
                RootPath,
                hb_store:path(Store, [
                    <<"assignments">>,
                    hb_util:human_id(ProcID),
                    hb_ao:normalize_key(Slot)
                ])
            ),
            ok;
        {error, Reason} -> {error, Reason}
    end
```

#### Reading Assignments
```erlang
read(ProcID, Slot, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ResolvedPath = hb_store:resolve(
        Store,
        hb_store:path(Store, [
            "assignments",
            hb_util:human_id(ProcID),
            Slot
        ])
    ),
    
    case hb_cache:read(ResolvedPath, Opts) of
        {ok, Assignment} ->
            % Handle AOS2 format conversion if needed
            case hb_ao:get(<<"slot">>, Assignment, Opts) of
                not_found ->
                    {ok, dev_scheduler_formats:aos2_normalize_types(Assignment)};
                _ ->
                    {ok, Assignment}
            end;
        not_found -> not_found
    end
```

### 2. Location Management

#### Writing Locations
```erlang
write_location(LocationMsg, Opts) ->
    case hb_cache:write(LocationMsg, Opts) of
        {ok, RootPath} ->
            % Create symlinks for each signer
            lists:foreach(
                fun(Signer) ->
                    hb_store:make_link(
                        Store,
                        RootPath,
                        hb_store:path(Store, [
                            "scheduler-locations",
                            hb_util:human_id(Signer)
                        ])
                    )
                end,
                hb_message:signers(LocationMsg)
            ),
            ok;
        {error, Reason} -> {error, Reason}
    end
```

#### Reading Locations
```erlang
read_location(Address, Opts) ->
    hb_cache:read(
        hb_store:path(Store, [
            "scheduler-locations",
            hb_util:human_id(Address)
        ]),
        Opts
    )
```

## Implementation Details

### 1. Storage Structure

#### Assignment Storage
```
assignments/
  ├── {process_id}/
  │   ├── 1  -> {root_path}/assignment1
  │   ├── 2  -> {root_path}/assignment2
  │   └── ...
  └── ...
```

#### Location Storage
```
scheduler-locations/
  ├── {signer1} -> {root_path}/location1
  ├── {signer2} -> {root_path}/location2
  └── ...
```

### 2. Assignment Listing

```erlang
list(ProcID, Opts) ->
    hb_cache:list_numbered(
        hb_store:path(Store, [
            "assignments",
            hb_util:human_id(ProcID)
        ]),
        Opts
    )
```

### 3. Latest Assignment Retrieval

```erlang
latest(ProcID, Opts) ->
    case list(ProcID, Opts) of
        [] -> not_found;
        Assignments ->
            AssignmentNum = lists:max(Assignments),
            {ok, Assignment} = read(ProcID, AssignmentNum, Opts),
            {
                AssignmentNum,
                hb_ao:get(<<"hash-chain">>, Assignment, #{hashpath => ignore})
            }
    end
```

## Event Logging

### 1. Assignment Operations
```erlang
?event({writing_assignment, {proc_id, ProcID}, {slot, Slot}, {assignment, Assignment}})
?event(error, {failed_to_write_assignment, {reason, Reason}})
?event({getting_assignments_from_cache, {proc_id, ProcID}, {opts, Opts}})
?event({no_assignments_in_cache, {proc_id, ProcID}})
```

### 2. Location Operations
```erlang
?event({read_location_msg, {address, Address}, {res, Res}})
?event({writing_location_msg, {signers, Signers}, {location_msg, LocationMsg}})
?event(warning, {failed_to_cache_location_msg, {reason, Reason}})
```

### 3. Path Resolution
```erlang
?event({resolved_path, {p1, P1}, {p2, P2}, {resolved, ResolvedPath}})
?event(debug_sched, {read_assignment, {res, not_found}})
```

## Integration Points

### Direct Dependencies
- hb_cache: Core caching functionality
- hb_store: Storage abstraction
- hb_ao: Message operations
- hb_message: Message handling
- dev_scheduler_formats: Format conversion

### Usage Context
- Called by dev_scheduler_server
- Integrates with storage system
- Manages assignment persistence
- Handles location caching

## Key Features

### 1. Storage Organization
- Symlink-based structure
- Process-specific paths
- Slot-based organization
- Signer-based location access

### 2. Format Handling
- AOS2 format support
- Type normalization
- Path resolution
- Key normalization

### 3. Performance
- Efficient lookups
- Quick latest retrieval
- Optimized listing
- Cached access

### 4. Reliability
- Error handling
- Path validation
- Format verification
- State consistency

## Best Practices

### 1. Storage Management
- Use proper paths
- Handle symlinks
- Validate formats
- Clean up resources

### 2. Error Handling
- Check store availability
- Validate paths
- Handle format errors
- Log failures

### 3. Integration
- Verify message format
- Handle conversions
- Maintain consistency
- Monitor operations
