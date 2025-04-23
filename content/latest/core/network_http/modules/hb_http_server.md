# Module: hb_http_server

## Basic Information
- **Source File:** hb_http_server.erl
- **Module Type:** Core Network & HTTP
- **Purpose:** HTTP server router that integrates with AO-Core resolver

## Purpose
Provides a Cowboy-based HTTP server that acts as a router, converting HTTP requests into AO-Core messages and routing them through the resolver. The server supports both HTTP/2 and HTTP/3 protocols, handles CORS, and provides comprehensive configuration options through an Opts message system.

## Interface

### Server Management
- `start/0,1` - Start HTTP server with optional configuration
- `start_node/0,1` - Start server in test/development mode
- `set_opts/1` - Update server configuration
- `get_opts/1` - Retrieve server configuration

### Request Handling
- `init/2` - Cowboy request entry point
- `handle_request/3` - Process non-CORS requests
- `allowed_methods/2` - Define allowed HTTP methods

### Configuration
- `set_default_opts/1` - Set default server options
- `new_server/1` - Create new server instance

## Dependencies

### Direct Dependencies
- cowboy: HTTP server framework
- ranch: Connection handling
- quicer: HTTP/3 support
- prometheus: Metrics (optional)
- hb_http: HTTP request/response handling
- dev_meta: Request processing
- hb_tracer: Request tracing

### Inverse Dependencies
- Used by HyperBEAM node system
- Core HTTP entry point
- Message routing system

## Implementation Details

### Key Concepts

1. **Server Configuration**
   ```erlang
   % Server initialization with options
   start(Opts) ->
       % Ensure required applications
       application:ensure_all_started([
           kernel, stdlib, inets, ssl,
           ranch, cowboy, gun, os_mon
       ]),
       % Initialize server
       BaseOpts = set_default_opts(Opts),
       {ok, Listener, _Port} = new_server(BaseOpts)
   ```

2. **Protocol Support**
   ```erlang
   % Protocol selection logic
   case Protocol = hb_opts:get(protocol, DefaultProto, NodeMsg) of
       http3 -> start_http3(ServerID, ProtoOpts, NodeMsg);
       Pro when Pro =:= http2; Pro =:= http1 ->
           start_http2(ServerID, ProtoOpts, NodeMsg)
   end
   ```

3. **Request Processing**
   ```erlang
   % Convert HTTP to AO-Core message
   ReqSingleton = hb_http:req_to_tabm_singleton(Req, Body, NodeMsg),
   % Process through meta device
   {ok, Res} = dev_meta:handle(NodeMsg, ReqSingleton),
   % Convert response back to HTTP
   hb_http:reply(Req, ReqSingleton, Res, NodeMsg)
   ```

### State Management

1. **Server State**
   - Configuration options
   - Protocol selection
   - Connection handling
   - Resource management

2. **Request State**
   - Message conversion
   - Request tracing
   - Error handling
   - Response generation

3. **Protocol State**
   - HTTP/2 support
   - HTTP/3 support
   - CORS handling
   - Connection management

### Error Handling

1. **Server Errors**
   - Startup failures
   - Configuration errors
   - Protocol errors
   - Resource errors

2. **Request Errors**
   - Processing failures
   - Conversion errors
   - Tracing errors
   - Response errors

## Integration Points

1. **HTTP Framework**
   - Cowboy integration
   - Protocol handling
   - Connection management
   - Request routing

2. **Message System**
   - Request conversion
   - Response handling
   - State management
   - Error handling

3. **Monitoring System**
   - Prometheus metrics
   - Request tracing
   - Error tracking
   - Performance monitoring

## Analysis Insights

### Performance Considerations

1. **Protocol Selection**
   - HTTP/2 vs HTTP/3
   - Connection handling
   - Resource usage
   - State management

2. **Request Processing**
   - Message conversion
   - State tracking
   - Error handling
   - Resource cleanup

### Security Implications

1. **Server Security**
   - Protocol security
   - Connection handling
   - State protection
   - Error isolation

2. **Request Security**
   - Input validation
   - State verification
   - Error handling
   - Resource protection

### Best Practices

1. **Server Configuration**
   - Protocol selection
   - Resource limits
   - Error handling
   - State management

2. **Request Handling**
   - Message validation
   - State tracking
   - Error handling
   - Resource cleanup

3. **Integration**
   - Framework usage
   - Protocol support
   - Error handling
   - Resource management

### Example Usage

```erlang
% Start server with default configuration
{ok, _Listener} = hb_http_server:start(),

% Start with custom configuration
{ok, _Listener} = hb_http_server:start(#{
    port => 8080,
    protocol => http2,
    prometheus => true,
    force_signed => true
}),

% Update server configuration
hb_http_server:set_opts(#{
    idle_timeout => 60000,
    max_connections => 1000
}),

% Start development node
URL = hb_http_server:start_node(#{
    port => 8734,
    store => #{
        <<"store-module">> => hb_store_fs,
        <<"prefix">> => <<"cache-dev">>
    }
})
