# Module: hb_store_rocksdb

## Basic Information
- **Source File:** hb_store_rocksdb.erl
- **Module Type:** Core Storage & Caching
- **Behaviors:** hb_store, gen_server

## Purpose
Provides a RocksDB-based implementation of the hb_store behavior, wrapping RocksDB storage in a process. Replicates functionality of hb_store_fs while using RocksDB as the backend storage engine. Uses prefixes to encode item types and supports hierarchical storage with links and groups.

## Interface

### Core Operations
- `start/1, start_link/1` - Initialize RocksDB store
- `stop/1` - Stop RocksDB store
- `reset/1` - Clear store data
- `enabled/0` - Check if RocksDB support is enabled

### Storage Operations
- `read/2` - Read key from store, following links
- `write/3` - Write value to store
- `list/2` - List contents of directory
- `type/2` - Get type of key (simple/composite)

### Path Operations
- `make_group/2` - Create directory in store
- `make_link/3` - Create symbolic link
- `resolve/2` - Resolve path through links
- `add_path/3` - Combine path components
- `path/2` - Create path from components

## Dependencies

### Direct Dependencies
- rocksdb: Storage engine
- gen_server: Process behavior
- hb_store: Store behavior
- hb_path: Path manipulation
- filelib: Directory utilities

### Inverse Dependencies
- Alternative store implementation
- Used when RocksDB is enabled
- Performance-focused storage

## Implementation Details

### Key Concepts

1. **Value Types**
   ```erlang
   % Value encoding with type prefixes
   encode_value(link, Value)  -> <<1, Value/binary>>
   encode_value(raw, Value)   -> <<2, Value/binary>>
   encode_value(group, Value) -> <<3, (term_to_binary(Value))/binary>>
   ```

2. **Store Configuration**
   ```erlang
   Store = #{
       <<"store-module">> => hb_store_rocksdb,
       <<"prefix">> => DataDir  % Base directory
   }
   ```

3. **Directory Structure**
   ```erlang
   % Groups store member sets
   {group, MemberSet} = decode_value(GroupValue)
   % Links point to other paths
   {link, Target} = decode_value(LinkValue)
   % Raw values store actual data
   {raw, Data} = decode_value(RawValue)
   ```

### State Management

1. **Process State**
   - RocksDB handle
   - Base directory
   - Connection status
   - Operation timeout

2. **Directory State**
   - Member sets
   - Path hierarchy
   - Link tracking
   - Type encoding

3. **Error Handling**
   - Connection failures
   - Invalid paths
   - Broken links
   - Timeout handling

### Error Handling

1. **Database Operations**
   - Handle connection loss
   - Manage timeouts
   - Validate operations
   - Report errors

2. **Path Resolution**
   - Handle missing paths
   - Follow valid links
   - Check permissions
   - Validate types

## Integration Points

1. **Storage System**
   - Implements hb_store
   - Uses RocksDB backend
   - Manages persistence
   - Handles encoding

2. **Process System**
   - gen_server behavior
   - State management
   - Message handling
   - Error recovery

3. **Path System**
   - Path resolution
   - Link handling
   - Group management
   - Type validation

## Analysis Insights

### Performance Considerations

1. **Database Operations**
   - Efficient encoding
   - Batch operations
   - Connection pooling
   - Cache utilization

2. **Path Resolution**
   - Link caching
   - Group optimization
   - Type inference
   - Prefix handling

### Security Implications

1. **Data Access**
   - Path validation
   - Type checking
   - Link verification
   - Group permissions

2. **Process Safety**
   - State isolation
   - Error handling
   - Recovery procedures
   - Connection management

### Best Practices

1. **Store Usage**
   - Check RocksDB status
   - Handle timeouts
   - Validate paths
   - Manage connections

2. **Path Handling**
   - Validate types
   - Check links
   - Manage groups
   - Handle errors

3. **Error Management**
   - Handle timeouts
   - Recover state
   - Report errors
   - Maintain consistency
