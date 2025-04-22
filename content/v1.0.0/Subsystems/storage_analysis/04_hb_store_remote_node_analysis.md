# `hb_store_remote_node.erl` Analysis

## Overview

`hb_store_remote_node.erl` provides a store implementation that fetches data from other HyperBEAM nodes over HTTP. Unlike the previously analyzed storage implementations, this module explicitly provides only the read side of the store interface, functioning as a client adapter for remote HyperBEAM nodes.

This implementation enables a distributed storage architecture where nodes can access each other's data through the same unified storage abstraction, further extending HyperBEAM's ability to treat remote data sources as part of a cohesive storage system.

## Key Characteristics

- **Read-Only Implementation**: Only implements the read-side of the storage interface
- **HTTP Communication**: Uses HyperBEAM's HTTP client to fetch data from remote nodes
- **Remote Scope**: Explicitly marked as having a "remote" scope
- **Message Attestation**: Ensures only attested portions of messages are returned
- **Simplified Type System**: Considers all successfully retrieved data as "simple" type
- **Node-to-Node Communication**: Enables data sharing between different HyperBEAM nodes

## Dependencies

### Upstream Dependencies

- `hb_http`: Used to send HTTP requests to the remote node
- `hb_message`: Used for message validation and attestation checking
- `include/hb.hrl`: System-wide macros and definitions
- `eunit/include/eunit.hrl`: Testing framework includes

## Implementation Details

### Remote Scope Definition

```erlang
scope(_) -> remote.
```

As with the gateway implementation, this store is clearly marked as having a "remote" scope.

### Read Operation

```erlang
read(Opts = #{ <<"node">> := Node }, Key) ->
    ?event({reading, Key, Opts}),
    case hb_http:get(Node, Key, Opts) of
        {ok, Res} ->
            {ok, Msg} = hb_message:with_only_attested(Res),
            {ok, Msg};
        {error, Err} ->
            ?event({read_error, Key, Err}),
            not_found
    end.
```

The read operation:
1. Uses the configured node URL to form an HTTP request
2. Fetches the data using `hb_http:get/3`
3. Ensures that only the attested portions of the message are returned
4. Returns the message or `not_found` on error

### Type Determination

```erlang
type(Opts = #{ <<"node">> := Node }, Key) ->
    ?event({remote_type, Node, Key}),
    case read(Opts, Key) of
        not_found -> not_found;
        _ -> simple
    end.
```

The implementation provides a simplified type system - all data that can be successfully retrieved is classified as "simple" type. This is a pragmatic approach that avoids needing to analyze the structure of remote data.

### Path Resolution

```erlang
resolve(#{ <<"node">> := Node }, Key) ->
    ?event({resolving_to_self, Node, Key}),
    Key.
```

Similar to `hb_store_gateway.erl`, this implementation uses a no-op `resolve` function that simply returns the key unchanged, indicating that path resolution isn't implemented.

## Test

The module includes a single comprehensive test that demonstrates the intended data flow:

1. It creates a local filesystem store
2. Writes a message with random content to it
3. Starts an HTTP server node using that store as its backing store
4. Creates a remote store pointing to the HTTP server node
5. Reads the message through the remote store
6. Verifies that the content matches what was originally written

This test effectively shows how data can flow between different nodes in a distributed HyperBEAM setup.

## Questions and Insights

### Questions

1. **Write Support**: The module documentation mentions potential future support for write operations, possibly with attestations that the data has been written to the remote node. How would this be implemented and what security considerations would arise?

2. **Error Handling**: The implementation has minimal error handling. How does it manage timeouts, connection issues, or malformed responses?

3. **Attestation Requirements**: The implementation calls `hb_message:with_only_attested/1`, suggesting that all data must be properly attested. What happens if the remote node returns unattested data?

4. **Data Consistency**: How does the system handle data consistency across nodes? Are there mechanisms to ensure that data read from a remote node is up-to-date?

5. **Authorization**: Does the remote node implementation support any authorization or authentication mechanisms to control access to data?

### Insights

1. **Distributed Architecture**: This implementation is a key component for enabling distributed HyperBEAM deployments, allowing data to flow between nodes.

2. **Security Focus**: The use of `hb_message:with_only_attested/1` shows a focus on security, ensuring that only properly attested data is accepted from remote sources.

3. **Minimal but Functional**: Despite being much simpler than other storage implementations, this module provides the essential functionality needed for inter-node data access.

4. **HTTP as Transport**: The use of HTTP as the transport mechanism allows for standard networking components (load balancers, proxies, etc.) to be part of the architecture.

5. **Partial Interface Implementation**: This module demonstrates that storage implementations can implement only the parts of the interface they support, with the storage abstraction handling fallbacks to other implementations.

## Integration with Other Subsystems

### Integration with HTTP System

The module relies heavily on the `hb_http` module for communication, showing the tight integration between the storage and network subsystems.

### Integration with Message System

The use of `hb_message:with_only_attested/1` demonstrates integration with the messaging subsystem, particularly the security aspects of message handling.

### Integration with Cache System

While not directly shown in the code, the test demonstrates how this store implementation can be used with the cache system, allowing remote data to be accessed through the same cache interface as local data.

## Recategorization Considerations

Like `hb_store_gateway.erl`, this module bridges the Storage Subsystem and the Network Communication Subsystem. It belongs in the Storage Subsystem due to its implementation of the `hb_store` behavior, but it could also be considered part of a "Remote Data Access" or "Node Federation" category.

The module's focus on enabling data sharing between HyperBEAM nodes makes it a key component of the system's distributed architecture. In a more refined categorization, it might be grouped with other components that enable node-to-node communication and data sharing.

## Implementation Notes

The module documentation mentions that a write side "could be added" to the interface, suggesting that this is a potential area for future development. The documentation also hints at integration with Arweave bundlers for data persistence, indicating a planned extension of the distributed storage capabilities.

The simplicity of this implementation compared to others suggests that it may be a newer or less mature component, possibly representing an area of ongoing development in the HyperBEAM system.
