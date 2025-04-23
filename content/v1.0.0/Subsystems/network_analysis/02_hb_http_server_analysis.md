# `hb_http_server.erl` Analysis

## Overview

`hb_http_server.erl` implements the server-side component of HyperBEAM's HTTP functionality, exposing the Converge resolver to the web as an HTTP endpoint. This module acts as the bridge between HTTP clients and HyperBEAM's internal message-based processing, handling all aspects of the HTTP server lifecycle—from initialization and configuration to request processing and response generation.

The module builds upon the Cowboy web server library to provide HTTP/1.1, HTTP/2, and HTTP/3 support, while maintaining consistency with HyperBEAM's message-centric architecture. Rather than implementing custom HTTP request handlers for different endpoints, it transforms incoming HTTP requests into HyperBEAM messages, processes them through the Converge resolver, and converts the results back into proper HTTP responses.

This design allows developers to interact with HyperBEAM's functionality through standard HTTP requests, while internally maintaining the system's message-based paradigm.

## Key Characteristics

- **HTTP Server Management**: Handles initialization, configuration, and lifecycle of the HTTP server
- **Protocol Support**: Offers HTTP/1.1, HTTP/2, and HTTP/3 support through configurable options
- **Request Transformation**: Converts HTTP requests into HyperBEAM messages for processing
- **Integration with Converge**: Routes converted requests to the Converge resolver for processing
- **CORS Support**: Implements standard Cross-Origin Resource Sharing headers and preflight handling
- **Configuration Loading**: Supports loading server configuration from external files
- **Dynamic Options**: Allows updating server options at runtime
- **Metrics Integration**: Includes support for Prometheus metrics collection
- **Wallet Integration**: Associates a node wallet with the server for authentication purposes

## Dependencies

### Upstream Dependencies

- `hb_http`: For message/HTTP conversion and HTTP response generation
- `hb_converge`: For message resolution
- `hb_opts`: For configuration management
- `hb_util`: For utility functions
- `hb_store`: For storage operations
- `dev_meta`: For handling requests after HTTP conversion
- `cowboy`: For the underlying HTTP server implementation
- `cowboy_router`: For HTTP routing
- `cowboy_req`: For HTTP request handling
- `cowboy_static`: For serving static files
- `prometheus_cowboy2_handler`, `prometheus_cowboy2_instrumenter`: For metrics collection
- `ranch_server`: For low-level server management
- `ar_wallet`: For wallet operations
- `uri_string`: For URI manipulation

## Implementation Details

### Server Initialization

The module provides several functions for starting the HTTP server with different configuration options:

```erlang
start() ->
    % Load configuration, initialize wallet and store
    % Display ASCII art banner with server information
    % Start the server with the loaded configuration
    
start(Opts) ->
    % Ensure required applications are started
    application:ensure_all_started([
        kernel, stdlib, inets, ssl, ranch, cowboy, gun,
        prometheus, prometheus_cowboy2, os_mon, rocksdb
    ]),
    hb:init(),
    BaseOpts = set_default_opts(Opts),
    {ok, Listener, _Port} = new_server(BaseOpts),
    {ok, Listener}.
```

The initialization process:
1. Loads configuration from a file if specified
2. Sets up a wallet for the node
3. Initializes the required storage
4. Starts all dependent applications
5. Creates and starts the HTTP server with the specified options

### Protocol Support

The module supports multiple HTTP protocol versions through dedicated startup functions:

```erlang
new_server(RawNodeMsg) ->
    % Prepare server configuration
    % Determine which protocol to use
    case Protocol = hb_opts:get(protocol, no_proto, NodeMsg) of
        http3 ->
            start_http3(ServerID, ProtoOpts, NodeMsg);
        Pro when Pro =:= http2; Pro =:= http1 ->
            % The HTTP/2 server has fallback mode to 1.1 as necessary
            start_http2(ServerID, ProtoOpts, NodeMsg);
        _ -> {error, {unknown_protocol, Protocol}}
    end.
```

The implementation includes specialized functions for HTTP/2 and HTTP/3:

```erlang
start_http3(ServerID, ProtoOpts, _NodeMsg) ->
    % Set up QUIC transport for HTTP/3
    % Returns port and listener PID
    
start_http2(ServerID, ProtoOpts, NodeMsg) ->
    % Set up TCP transport for HTTP/2 (with HTTP/1.1 fallback)
    % Returns port and listener PID
```

### Request Handling

The core functionality is implemented in the Cowboy handler callbacks:

```erlang
init(Req, ServerID) ->
    case cowboy_req:method(Req) of
        <<"OPTIONS">> -> cors_reply(Req, ServerID);
        _ ->
            {ok, Body} = read_body(Req),
            handle_request(Req, Body, ServerID)
    end.
```

For non-OPTIONS requests, the module:
1. Reads the complete request body
2. Retrieves the server options using the server ID
3. Converts the HTTP request to a HyperBEAM message using `hb_http:req_to_tabm_singleton`
4. Determines the appropriate codec for the response
5. Processes the request through `dev_meta:handle`
6. Converts the result back to an HTTP response using `hb_http:reply`

```erlang
handle_request(Req, Body, ServerID) ->
    NodeMsg = get_opts(#{ http_server => ServerID }),
    % Parse the HTTP request into HyperBEAM's message format
    ReqSingleton = hb_http:req_to_tabm_singleton(Req, Body, NodeMsg),
    AttestationCodec = hb_http:accept_to_codec(ReqSingleton, NodeMsg),
    % Process the request through the Meta device
    {ok, Res} =
        dev_meta:handle(
            NodeMsg#{ attestation_device => AttestationCodec },
            ReqSingleton
        ),
    % Convert the result back to an HTTP response
    hb_http:reply(Req, ReqSingleton, Res, NodeMsg).
```

### Configuration Management

The module provides functions for setting and retrieving server options:

```erlang
set_opts(Opts) ->
    ServerRef = hb_opts:get(http_server, no_server_ref, Opts),
    ok = cowboy:set_env(ServerRef, node_msg, Opts).

get_opts(NodeMsg) ->
    ServerRef = hb_opts:get(http_server, no_server_ref, NodeMsg),
    cowboy:get_env(ServerRef, node_msg, no_node_msg).
```

It also includes a function for setting default options if none are provided:

```erlang
set_default_opts(Opts) ->
    % Create a temporary opts map that does not include the defaults
    TempOpts = Opts#{ only => local },
    % Generate a random port number if none is specified
    % Create a wallet if none is provided
    % Set up a store if none is configured
    % Return the updated options
```

### CORS Support

The module includes specific handling for CORS preflight requests:

```erlang
cors_reply(Req, _ServerID) ->
    Req2 = cowboy_req:reply(204, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-headers">> => <<"*">>,
        <<"access-control-allow-methods">> =>
            <<"GET, POST, PUT, DELETE, OPTIONS, PATCH">>
    }, Req),
    {ok, Req2, no_state}.
```

### Testing Support

The module includes functions for starting a test node:

```erlang
start_node() ->
    start_node(#{}).
start_node(Opts) ->
    % Initialize the required applications
    % Start the supervisor
    % Start the server with default options
    % Return the URL for the node
```

## Questions and Insights

### Questions

1. **Load Testing**: How does the server handle high load situations? Are there any explicit rate limiting or backpressure mechanisms?

2. **Security Considerations**: What security measures are in place beyond signature verification? Are there provisions for DDoS protection or malformed request handling?

3. **Error Handling**: How are server-side errors handled and presented to clients? Is there a consistent error format?

4. **Configuration Reloading**: Can configuration be reloaded at runtime without restarting the server?

5. **TLS Configuration**: For production deployments, how is TLS configured? The code references test certificates, but production would require proper certificate management.

### Insights

1. **Protocol Flexibility**: The support for multiple HTTP protocol versions (including HTTP/3) shows a forward-looking approach to web standards.

2. **Unified Message Handling**: Rather than implementing separate handlers for different endpoints, the module converts everything to messages and relies on the Converge resolver, maintaining consistency with the system's message-centric design.

3. **Configuration Adaptability**: The server can adapt its configuration based on the environment, using reasonable defaults when explicit configuration is not provided.

4. **Metrics Integration**: Built-in support for Prometheus metrics indicates a focus on observability and monitoring.

5. **Identity Integration**: The server is associated with a wallet identity, potentially enabling authentication and authorization mechanisms based on HyperBEAM's cryptographic identity system.

## Integration with Other Subsystems

### Integration with Core Infrastructure

- Uses `hb_converge` for message resolution
- Uses `hb_opts` for configuration management
- Uses `hb:init()` for system initialization
- Uses `hb:wallet()` for wallet management

### Integration with Storage Subsystem

- Uses `hb_store:start` to initialize the storage subsystem
- Configures storage options for the server

### Integration with Device Subsystem

- Uses `dev_meta:handle` to process requests after HTTP conversion
- Integrates with the device system for message handling

### Integration with Arweave Subsystem

- Uses `ar_wallet` for wallet operations
- Integrates with Arweave for identity and cryptography

## Recategorization Considerations

This module is correctly categorized as part of the Network Communication Subsystem. Its primary purpose is to expose HyperBEAM's functionality over HTTP, which is a core networking concern.

The module serves as the entry point for external HTTP requests, converting them into internal messages and routing them through the system. It also handles the conversion of results back to HTTP responses. This bidirectional protocol translation is a key aspect of the Network Communication Subsystem.

While the module has dependencies on other subsystems like Storage and Devices, its primary responsibility is managing HTTP communication, making the Network Communication Subsystem the appropriate categorization.
