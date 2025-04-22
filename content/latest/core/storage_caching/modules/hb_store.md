# Module: hb_store

## Basic Information
- **Source File:** hb_store.erl
- **Module Type:** Core Storage & Caching
- **Behavior:** Yes (defines behavior_info/1)

## Purpose
Provides an abstraction layer for key-value store operations in HyperBEAM. This module allows swapping underlying store implementations by defining a common interface and behavior that store modules must implement. It manages store operations through a chain of responsibility pattern, trying each configured store module until one succeeds.

## Interface

### Core Operations
- `start/1` - Start store modules
- `stop/1` - Stop store modules
- `reset/1` - Delete all keys in store
- `read/2` - Read key from store
- `write/3` - Write key-value to store
- `list/2` - List keys in a group

### Path Operations
- `path/1,2` - Create path from components
- `add_path/2,3` - Combine path components
- `join/1` - Join path components
- `make_group/2` - Create namespace/directory
- `make_link/3` - Create link between paths
- `resolve/2` - Resolve path through links

### Store Management
- `filter/2` - Filter stores by criteria
- `scope/2` - Limit store scope
- `sort/2` - Order stores by preference
- `type/2` - Get element type

## Dependencies

### Direct Dependencies
- hb_path: Path manipulation
- hb_opts: Options handling
- hb_store_fs: Filesystem store
- hb_store_rocksdb: RocksDB store (optional)

### Inverse Dependencies
- Used by modules needing storage
- Core component for caching
- Essential for persistence

## Implementation Details

### Key Concepts

1. **Store Behavior**
   ```erlang
   behavior_info(callbacks) -> [
       {start, 1}, {stop, 1}, {reset, 1},
       {make_group, 2}, {make_link, 3},
       {type, 2}, {read, 2}, {write, 3},
       {list, 2}, {path, 2}, {add_path, 3}
   ]
   ```

2. **Store Configuration**
   ```erlang
   Store = #{
       <<"store-module">> => ModuleName,
       <<"prefix">> => PathPrefix,
       % Other store-specific options
   }
   ```

3. **Path Resolution**
   ```erlang
   % Example link resolution:
   write(Store, "file", "data")
   make_link(Store, "file", "link")
   read(Store, "link") % Returns "data"
   ```

### State Management

1. **Store State**
   - Module-specific state
   - Scoped operations
   - Path management
   - Link resolution

2. **Error Handling**
   - Store fallbacks
   - Operation retries
   - Error propagation
   - State recovery

3. **Path Management**
   - Path normalization
   - Link resolution
   - Group handling
   - Scope isolation

### Error Handling

1. **Store Failures**
   - Tries next store on failure
   - Reports store errors
   - Maintains consistency
   - Handles timeouts

2. **Path Errors**
   - Validates paths
   - Checks permissions
   - Handles missing links
   - Reports failures

## Integration Points

1. **Storage System**
   - Multiple store types
   - Pluggable interface
   - Common operations
   - State management

2. **Cache System**
   - Store selection
   - Path resolution
   - Link handling
   - State persistence

3. **Path System**
   - Path manipulation
   - Link resolution
   - Group management
   - Scope handling

## Analysis Insights

### Performance Considerations

1. **Store Selection**
   - Efficient filtering
   - Smart sorting
   - Quick fallbacks
   - Minimal overhead

2. **Path Resolution**
   - Fast link resolution
   - Efficient grouping
   - Path caching
   - Minimal recursion

### Security Implications

1. **Store Access**
   - Scope isolation
   - Path validation
   - Link verification
   - Access control

2. **Data Protection**
   - State isolation
   - Error handling
   - Safe fallbacks
   - Consistent state

### Best Practices

1. **Store Implementation**
   - Follow behavior spec
   - Handle errors gracefully
   - Maintain consistency
   - Document limitations

2. **Store Usage**
   - Use appropriate stores
   - Handle failures
   - Validate paths
   - Check scopes

3. **Path Handling**
   - Normalize paths
   - Resolve links
   - Manage groups
   - Validate access
