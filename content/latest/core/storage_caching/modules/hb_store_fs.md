# Module: hb_store_fs

## Basic Information
- **Source File:** hb_store_fs.erl
- **Module Type:** Core Storage & Caching
- **Behavior:** hb_store

## Purpose
Provides a filesystem-based implementation of the hb_store behavior. This module serves as the default storage backend, implementing key-value store operations using the local filesystem with support for directories (groups) and symbolic links.

## Interface

### Core Operations
- `start/1` - Initialize filesystem store with data directory
- `stop/1` - Stop filesystem store (no-op)
- `reset/1` - Clear store by removing and recreating directory
- `scope/1` - Return store scope (always 'local')

### Storage Operations
- `read/2` - Read key from store, following symlinks
- `write/3` - Write value to path in store
- `list/2` - List contents of directory
- `type/2` - Get type of key (simple/composite)

### Path Operations
- `make_group/2` - Create directory in store
- `make_link/3` - Create symbolic link
- `resolve/2` - Resolve path through symlinks
- `add_prefix/2` - Add directory prefix to path
- `remove_prefix/2` - Remove directory prefix from path

## Dependencies

### Direct Dependencies
- file: Filesystem operations
- filelib: Directory utilities
- hb_store: Store behavior
- hb_path: Path manipulation
- hb_util: Utility functions

### Inverse Dependencies
- Used by modules needing local storage
- Default store implementation
- Core component for persistence

## Implementation Details

### Key Concepts

1. **Path Resolution**
   ```erlang
   % Example path resolution:
   /a/b/c: "Not the right data"
   /a/b -> /a/alt-b
   /a/alt-b/c: "Correct data"
   
   % Resolving "a/b/c" follows links:
   a/b -> a/alt-b
   a/alt-b/c -> "Correct data"
   ```

2. **Store Configuration**
   ```erlang
   Store = #{
       <<"prefix">> => DataDir  % Base directory for storage
   }
   ```

3. **File Types**
   - simple: Regular files
   - composite: Directories
   - link: Symbolic links

### State Management

1. **Directory Structure**
   - Uses filesystem hierarchy
   - Maintains symlinks
   - Handles nested paths
   - Preserves structure

2. **Error Handling**
   - Directory creation
   - Link resolution
   - File operations
   - Path validation

3. **Path Management**
   - Prefix handling
   - Link resolution
   - Path normalization
   - Directory creation

### Error Handling

1. **File Operations**
   - Handles missing files
   - Manages broken links
   - Creates directories
   - Validates paths

2. **Link Resolution**
   - Follows symlinks
   - Handles cycles
   - Validates targets
   - Reports errors

## Integration Points

1. **Storage System**
   - Implements hb_store behavior
   - Provides filesystem backend
   - Manages local storage
   - Handles persistence

2. **Path System**
   - Path manipulation
   - Link resolution
   - Directory structure
   - Prefix handling

3. **Error System**
   - Operation validation
   - Error reporting
   - State recovery
   - Path verification

## Analysis Insights

### Performance Considerations

1. **File Operations**
   - Efficient link following
   - Directory caching
   - Path resolution
   - Operation batching

2. **Path Resolution**
   - Link caching
   - Path normalization
   - Prefix handling
   - Directory structure

### Security Implications

1. **File Access**
   - Path validation
   - Link verification
   - Directory permissions
   - Access control

2. **Data Protection**
   - File permissions
   - Directory structure
   - Link validation
   - Path sanitization

### Best Practices

1. **Store Usage**
   - Validate paths
   - Handle errors
   - Follow links
   - Check permissions

2. **Path Handling**
   - Normalize paths
   - Validate links
   - Check existence
   - Handle prefixes

3. **Error Management**
   - Check operations
   - Handle failures
   - Validate state
   - Report errors
