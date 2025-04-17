# `hb_store.erl` Analysis

## Overview

`hb_store.erl` serves as an abstraction layer for key-value store operations in HyperBEAM, providing a unified interface across multiple storage backend implementations. As noted in the code comments, "This interface allows us to swap out the underlying store implementation(s) as desired."

With 14 downstream dependents according to our Stage 1 analysis, this module plays a critical role in the system's data persistence strategy. The module allows operations to be attempted across a list of storage modules, falling back if earlier modules fail.

## Key Characteristics

- **Storage Backend Abstraction**: Provides a uniform interface to different storage implementations
- **Behavior Definition**: Defines a callback behavior that storage modules must implement
- **Cascading Implementation**: Falls back to alternative implementations if a module fails
- **Hierarchical Paths**: Supports directory-like structures with groups and path components
- **Symbolic Link Support**: Provides link functionality between different paths
- **Scope-Based Filtering**: Allows filtering and sorting store modules by scope (e.g., local, remote)

## Dependencies

### Upstream Dependencies

`hb_store.erl` has minimal dependencies, which is appropriate for a foundational module:
- `include/hb.hrl`: System-wide macros and definitions
- `eunit/include/eunit.hrl`: Testing framework includes
- `hb_path`: Used for path manipulation (specifically the `to_binary/1` function)

### Downstream Dependents

According to our Stage 1 analysis, 14 other modules depend on this module, spanning several subsystems:

1. **Storage Implementations**:
   - `hb_store_fs.erl`
   - `hb_store_gateway.erl`
   - `hb_store_remote_node.erl`
   - `hb_store_rocksdb.erl`

2. **Cache System**:
   - `hb_cache.erl`
   - `hb_cache_control.erl`
   - `hb_cache_render.erl`

3. **Core Components**:
   - `hb_converge_test_vectors.erl`
   - `hb_http_server.erl`

4. **Device Components**:
   - `dev_process.erl`
   - `dev_process_cache.erl`
   - `dev_scheduler_cache.erl`

## Key Functions

### Behavior Definition

```erlang
behavior_info(callbacks) ->
    [
        {start, 1}, {stop, 1}, {reset, 1}, {make_group, 2}, {make_link, 3},
        {type, 2}, {read, 2}, {write, 3},
        {list, 2}, {path, 2}, {add_path, 3}
    ].
```

This defines the required callbacks that any storage implementation module must provide.

### Store Management

- `start/1`: Starts all storage modules in the list
- `stop/1`: Stops all storage modules in the list
- `reset/1`: Resets (clears) all storage modules

### Core Storage Operations

- `read/2`: Reads a value from a key
- `write/3`: Writes a value to a key
- `type/2`: Gets the type of element at a given path
- `list/2`: Lists keys in a group (with a warning about potential performance impact)

### Store Selection and Management

- `filter/2`: Filters store modules based on a predicate
- `scope/2`: Limits store modules to those with a specific scope
- `sort/2`: Orders store modules by a preference order of scopes

### Path and Structure Operations

- `path/1, path/2`: Creates a path from components
- `add_path/2, add_path/3`: Combines path components
- `join/1`: Joins a list of path components
- `make_group/2`: Creates a "group" (directory-like namespace)
- `make_link/3`: Creates a link from one path to another
- `resolve/2`: Follows links to resolve a path to its final target

### Implementation Helpers

- `call_function/3`: Tries to call a function on each store module until one succeeds
- `call_all/3`: Calls a function on all store modules

### Testing Support

- `test_stores/0`: Returns a list of test store configurations
- `generate_test_suite/1, generate_test_suite/2`: Generates test suites for store modules

## Implementation Details

### Store Module Format

Store modules are represented as maps with a required `<<"store-module">>` key that points to the Erlang module implementing the store behavior:

```erlang
Store = #{<<"store-module">> := Mod}
```

### Cascading Implementation Pattern

The module uses a cascading pattern where it attempts operations on each store in sequence until one succeeds:

```erlang
call_function([Store = #{<<"store-module">> := Mod} | Rest], Function, Args) ->
    try apply(Mod, Function, [Store | Args]) of
        not_found ->
            call_function(Rest, Function, Args);
        Result ->
            Result
    catch
        Class:Reason:Stacktrace ->
            ?event(error, {store_call_failed, {Class, Reason, Stacktrace}}),
            call_function(Rest, Function, Args)
    end.
```

This pattern provides robustness through fallbacks.

### Store Scoping

Stores have a concept of "scope" (e.g., local, remote) that can be used for filtering:

```erlang
filter(Modules, Filter) ->
    lists:filter(
        fun(Store) ->
            try Filter(get_store_scope(Store), Store)
            catch _:_ -> false
            end
        end,
        Modules
    ).
```

## Questions and Insights

### Questions

1. **Path resolution depth limits**: Are there safeguards against circular symbolic links that could cause infinite recursion during path resolution?

2. **Cross-store consistency**: How does the system ensure consistency when data might be distributed across multiple store implementations?

3. **Scope usage patterns**: What are the common scope values used in the system, and how are they leveraged in practice?

4. **Integration with transactions**: Does the system support transactions that span multiple storage operations?

5. **Error handling strategy**: What's the strategy for error handling and recovery when a storage operation fails across all available stores?

### Insights

1. **Pluggable Storage**: The design allows for pluggable storage backends, which is important for flexibility in distributed systems where different storage technologies might be appropriate in different contexts.

2. **Hierarchical Structure**: Despite being a key-value store abstraction, the system supports hierarchical paths and links, bringing filesystem-like semantics to the storage layer.

3. **Defensive Implementation**: The code is defensive, with careful error handling and fallbacks to alternative implementations.

4. **Warning About List Performance**: There's an explicit comment warning about the performance implications of listing keys, suggesting an awareness of potential scalability challenges.

5. **Symbolic Link Support**: The inclusion of symbolic link functionality (`make_link`, `resolve`) suggests a more sophisticated storage model than simple key-value.

## Integration with Other Subsystems

### Integration with Cache System

The cache system (`hb_cache.erl` and related modules) appears to build on top of the storage abstraction, likely providing an in-memory caching layer that falls back to persistent storage.

### Integration with Path System

The module closely integrates with the path manipulation system:

```erlang
join(Path) -> hb_path:to_binary(Path).
```

### Integration with Device System

Device components (e.g., `dev_process.erl`, `dev_scheduler_cache.erl`) depend on the storage subsystem, suggesting that device state and computation results are persisted through this abstraction.

### Integration with HTTP System

The dependency from `hb_http_server.erl` suggests that the HTTP system may use the storage abstraction for persisting API data or serving content.

## Recategorization Considerations

Based on the actual implementation, this module is correctly categorized as part of the Storage Subsystem. It serves as the core abstraction layer for storage operations, with multiple specialized implementations. The module's focus on abstract storage operations, path structures, and the pluggable backend system all align well with its categorization.

One interesting note is that the module includes both storage and path manipulation functionality. In a more strictly separated architecture, path manipulation might be entirely delegated to a separate module. However, the tight integration between storage operations and path handling makes this organization sensible in the current design.
