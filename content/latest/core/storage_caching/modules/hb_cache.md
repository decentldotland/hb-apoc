# Module: hb_cache

## Basic Information
- **Source File:** hb_cache.erl
- **Module Type:** Core Storage & Caching
- **Purpose:** Cache management for AO-Core protocol messages and compute results

## Purpose
Provides a sophisticated caching system for HyperBEAM that manages three layers of data representation:
1. Raw binary data stored at content hashes for deduplication
2. Hashpath-graph linking content through hashpaths and keys
3. Message storage with both committed and uncommitted IDs

## Interface

### Core Operations
- `read/2` - Read message from cache
- `write/2` - Write message to cache
- `read_resolved/3` - Read computation output
- `list/2` - List items under path

### Data Operations
- `write_binary/3` - Write raw binary data
- `write_hashpath/2` - Write message with hashpath
- `link/3` - Create link between paths
- `list_numbered/2` - List numbered items

## Dependencies

### Direct Dependencies
- hb_store: Storage abstraction
- hb_message: Message handling
- hb_path: Path manipulation
- dev_message: Message operations
- dev_codec_structured: Format conversion

### Inverse Dependencies
- Used by compute system
- Core caching component
- Message persistence layer

## Implementation Details

### Key Concepts

1. **Data Layers**
   ```erlang
   % Layer 1: Raw binary at content hash
   write_binary(HashPath, Data, Store)
   
   % Layer 2: Hashpath graph links
   write_hashpath(HashPath, Message, Store)
   
   % Layer 3: Message IDs and commitments
   write(Message, #{store => Store})
   ```

2. **Message Storage**
   ```erlang
   % Write message with commitments
   AllIDs = calculate_all_ids(Message, Opts),
   {ok, UncommittedID} = do_write_message(Message, AllIDs, Store)
   ```

3. **Type-Annotated Binary Messages (TABM)**
   ```erlang
   % Convert to TABM before storage
   Tabm = hb_message:convert(Msg, tabm, <<"structured@1.0">>, Opts)
   ```

### State Management

1. **Content Storage**
   - Hash-based deduplication
   - Binary content storage
   - Path management
   - Link resolution

2. **Message State**
   - Commitment tracking
   - ID management
   - Path resolution
   - Cache invalidation

3. **Hashpath Graph**
   - Path linking
   - Graph traversal
   - State tracking
   - Cycle detection

### Error Handling

1. **Storage Errors**
   - Missing content
   - Invalid paths
   - Broken links
   - Circular references

2. **Message Validation**
   - Format checking
   - Type validation
   - Path verification
   - State consistency

## Integration Points

1. **Storage System**
   - Multiple backends
   - Path resolution
   - Content storage
   - Link management

2. **Message System**
   - Format conversion
   - State tracking
   - ID management
   - Commitment handling

3. **Compute System**
   - Result caching
   - State persistence
   - Path resolution
   - Output management

## Analysis Insights

### Performance Considerations

1. **Content Storage**
   - Deduplication
   - Path optimization
   - Link efficiency
   - Cache utilization

2. **Message Handling**
   - Format conversion
   - State tracking
   - Path resolution
   - Graph traversal

### Security Implications

1. **Content Validation**
   - Path verification
   - Link checking
   - Type validation
   - State consistency

2. **Message Protection**
   - Commitment validation
   - ID verification
   - Path security
   - State isolation

### Best Practices

1. **Cache Usage**
   - Enable deduplication
   - Manage paths
   - Handle links
   - Track state

2. **Message Handling**
   - Validate formats
   - Check types
   - Resolve paths
   - Manage commitments

3. **Error Management**
   - Check paths
   - Validate links
   - Handle cycles
   - Maintain state

### Example Usage

```erlang
% Write binary content
{ok, Path} = hb_cache:write_binary(HashPath, Data, Opts),

% Write message with commitments
{ok, ID} = hb_cache:write(Message, #{
    store => Store,
    wallet => Wallet
}),

% Read message by ID
{ok, StoredMsg} = hb_cache:read(ID, Opts),

% Read computation result
{ok, Result} = hb_cache:read_resolved(Msg1ID, Msg2ID, Opts)
