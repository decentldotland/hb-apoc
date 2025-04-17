# `hb_store_fs.erl` Analysis

## Overview

`hb_store_fs.erl` is a filesystem-based implementation of the `hb_store` behavior, providing storage services through the local filesystem. It maps abstract storage operations defined in the `hb_store` behavior to concrete filesystem operations, demonstrating how HyperBEAM's storage abstraction layer works in practice.

This implementation uses the filesystem's native directory structure and symbolic links to represent the hierarchical path and link concepts defined in the HyperBEAM storage abstraction.

## Key Characteristics

- **Filesystem Mapping**: Maps abstract storage operations to filesystem operations
- **Symbolic Link Support**: Uses filesystem symbolic links to implement the storage links concept
- **Hierarchical Structure**: Maps "groups" to filesystem directories
- **Path Prefixing**: Prefixes all paths with a configured directory to isolate storage
- **Local Scoping**: Defines itself as having "local" scope in the storage ecosystem

## Dependencies

### Upstream Dependencies

- `hb_store`: Implements the behavior defined by this module
- `kernel/include/file.hrl`: Includes file information record definitions
- `include/hb.hrl`: System-wide macros and definitions
- `filelib`: Used for ensuring directories exist and other filesystem utilities
- `file`: Used for fundamental file operations
- `os`: Used for command execution (specifically for `reset` operation)
- `hb_util`: Used for path manipulation (specifically `remove_common`)
- `hb_path`: Used for path component manipulation

## Implementation Details

### Configuration

The module is configured through a map with a `<<"prefix">>` key that specifies the base directory for all storage operations:

```erlang
#{ <<"prefix">> := DataDir }
```

### Initialization and Management

```erlang
start(#{ <<"prefix">> := DataDir }) ->
    ok = filelib:ensure_dir(DataDir).

stop(#{ <<"prefix">> := _DataDir }) ->
    ok.

scope(_) -> local.

reset(#{ <<"prefix">> := DataDir }) ->
    os:cmd(binary_to_list(<< "rm -Rf ", DataDir/binary >>)),
    ?event({reset_store, {path, DataDir}}).
```

The initialization is straightforward, ensuring the base directory exists. The `reset` operation is more aggressive, using a shell command to completely remove and recreate the directory.

### Core Storage Operations

#### Reading

```erlang
read(Opts, Key) ->
    read(add_prefix(Opts, resolve(Opts, Key))).
read(Path) ->
    ?event({read, Path}),
    case file:read_file_info(Path) of
        {ok, #file_info{type = regular}} ->
            {ok, _} = file:read_file(Path);
        _ ->
            case file:read_link(Path) of
                {ok, Link} ->
                    ?event({link_found, Path, Link}),
                    read(Link);
                _ ->
                    not_found
            end
    end.
```

Reading first resolves the path (following any links) and then reads the file contents. If the target is a symlink, it follows the link recursively.

#### Writing

```erlang
write(Opts, PathComponents, Value) ->
    Path = add_prefix(Opts, PathComponents),
    ?event({writing, Path, byte_size(Value)}),
    filelib:ensure_dir(Path),
    ok = file:write_file(Path, Value).
```

Writing ensures the parent directory exists and then writes the binary content to the file.

#### Listing

```erlang
list(Opts, Path) ->
    file:list_dir(add_prefix(Opts, Path)).
```

Listing is a simple pass-through to the filesystem's directory listing functionality.

### Path Resolution and Manipulation

The implementation includes a sophisticated path resolution mechanism that handles symbolic links at different levels of the path:

```erlang
resolve(Opts, RawPath) ->
    Res = resolve(Opts, "", hb_path:term_to_path_parts(hb_store:join(RawPath))),
    ?event({resolved, RawPath, Res}),
    Res.
resolve(_, CurrPath, []) ->
    hb_store:join(CurrPath);
resolve(Opts, CurrPath, [Next|Rest]) ->
    PathPart = hb_store:join([CurrPath, Next]),
    ?event(
        {resolving,
            {accumulated_path, CurrPath},
            {next_segment, Next},
            {generated_partial_path_to_test, PathPart}
        }
    ),
    case file:read_link(add_prefix(Opts, PathPart)) of
        {ok, RawLink} ->
            Link = remove_prefix(Opts, RawLink),
            resolve(Opts, Link, Rest);
        _ ->
            resolve(Opts, PathPart, Rest)
    end.
```

This recursive approach resolves each segment of the path in sequence, following any symbolic links encountered along the way. This allows for complex hierarchical structures with links at different levels.

### Structural Operations

```erlang
make_group(Opts = #{ <<"prefix">> := _DataDir }, Path) ->
    P = add_prefix(Opts, Path),
    ?event({making_group, P}),
    filelib:ensure_dir(P),
   case file:make_dir(P) of
        ok -> ok;
        {error, eexist} -> ok
    end.

make_link(_, Link, Link) -> ok;
make_link(Opts, Existing, New) ->
    ?event({symlink,
        add_prefix(Opts, Existing),
        P2 = add_prefix(Opts, New)}),
    filelib:ensure_dir(P2),
    file:make_symlink(
        add_prefix(Opts, Existing),
        add_prefix(Opts, New)
    ).
```

Groups are implemented as directories, and links are implemented as filesystem symbolic links.

### Path Prefixing

```erlang
add_prefix(#{ <<"prefix">> := Prefix }, Path) ->
    hb_store:join([Prefix, Path]).

remove_prefix(#{ <<"prefix">> := Prefix }, Path) ->
    hb_util:remove_common(Path, Prefix).
```

These helper functions add or remove the storage prefix from paths. This isolation mechanism ensures that the storage system can't access files outside its designated area.

## Questions and Insights

### Questions

1. **Security Implications**: The `reset` operation uses `os:cmd` with concatenated strings, which could potentially be a security concern if the `DataDir` value isn't properly validated or sanitized.

2. **Symbolic Link Safety**: The implementation follows symbolic links without apparent depth checking, which could lead to infinite recursion if there are circular symbolic links.

3. **Concurrency Handling**: There's no explicit concurrency control in the filesystem operations, which might lead to race conditions in a multi-process environment.

4. **Error Handling Strategy**: Some operations have minimal error handling (e.g., `make_group` handles `eexist` but not other errors), which might lead to less robust behavior in edge cases.

5. **Performance Characteristics**: Filesystem operations can be slow, especially for deeply nested paths with multiple symbolic links to resolve. How does this impact overall system performance?

### Insights

1. **Simplicity vs. Robustness**: The implementation prioritizes simplicity and directness, which makes it easy to understand but potentially less robust in edge cases.

2. **Leveraging OS Capabilities**: By using native symbolic links, the implementation leverages existing OS capabilities rather than reinventing them.

3. **Path Resolution Flexibility**: The segment-by-segment path resolution allows for complex path structures with links at multiple levels, providing considerable flexibility.

4. **Isolation Through Prefixing**: The consistent use of path prefixing ensures that the storage system is isolated to its designated area, improving security.

5. **Event Tracing**: The implementation includes extensive event tracing (`?event` calls), suggesting a focus on observability and debugging.

## Integration with Other Subsystems

### Integration with Storage Abstraction

As an implementation of the `hb_store` behavior, this module seamlessly integrates with the storage abstraction layer. Any system component that uses the storage system can transparently use the filesystem implementation without specific knowledge of its inner workings.

### Integration with Path System

The implementation makes extensive use of the path manipulation utilities from `hb_store` and `hb_path`, showing how these subsystems work together to provide a cohesive storage solution.

## Recategorization Considerations

This module is correctly categorized as part of the Storage Subsystem. It directly implements the `hb_store` behavior and provides concrete filesystem-based storage services. Its focus on mapping abstract storage operations to filesystem operations and its tight integration with the storage abstraction layer confirm its placement.

The module doesn't show significant overlap with other subsystems beyond the expected dependencies, so there's no compelling reason to reconsider its categorization.
