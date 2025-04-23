# Module: hb_http_client

## Basic Information
- **Source File:** hb_http_client.erl
- **Module Type:** Core Network & HTTP
- **Purpose:** HTTP client implementation with connection management and metrics

## Purpose
A sophisticated HTTP client that wraps both Gun and httpc libraries, providing connection pooling, metrics tracking, and robust error handling. Originally from the Arweave project and modified for HyperBEAM, it manages persistent connections and provides detailed monitoring capabilities through Prometheus.

## Interface

### Core Operations
- `start_link/1` - Start client gen_server
- `req/2` - Make HTTP request with options
- `init/1` - Initialize gen_server state

### Connection Management
- `get_connection/3` - Get or establish connection
- `open_connection/2` - Create new connection
- `parse_peer/2` - Parse peer information

### Request Handling
- `gun_req/3` - Handle Gun-based request
- `httpc_req/3` - Handle httpc-based request
- `request/3` - Process request with metrics

## Dependencies

### Direct Dependencies
- gun: HTTP/2 client
- httpc: HTTP/1.1 client
- prometheus: Metrics collection
- gen_server: OTP behavior
- ar_rate_limiter: Rate limiting

### Inverse Dependencies
- Used by hb_http
- Used by hb_gateway_client
- Core HTTP client functionality

## Implementation Details

### Key Concepts

1. **Connection Management**
   ```erlang
   % Connection state record
   -record(state, {
       pid_by_peer = #{},      % Map of peer to connection PID
       status_by_pid = #{},    % Map of PID to connection status
       opts = #{}             % Configuration options
   }).
   ```

2. **Request Processing**
   ```erlang
   % Request handling with metrics
   gun_req(Args, ReestablishedConnection, Opts) ->
       StartTime = erlang:monotonic_time(),
       Response = handle_request(Args, ReestablishedConnection, Opts),
       EndTime = erlang:monotonic_time(),
       record_metrics(Method, Path, Response, EndTime - StartTime)
   ```

3. **Protocol Support**
   ```erlang
   % Protocol selection
   case hb_opts:get(http_client, gun, Opts) of
       gun -> gun_req(Args, ReestablishedConnection, Opts);
       httpc -> httpc_req(Args, ReestablishedConnection, Opts)
   end
   ```

### State Management

1. **Connection State**
   - Connection tracking
   - Status management
   - Peer mapping
   - Error handling

2. **Request State**
   - Request tracking
   - Response handling
   - Metrics collection
   - Error recovery

3. **Protocol State**
   - Protocol selection
   - Connection handling
   - State verification
   - Error management

### Error Handling

1. **Connection Errors**
   - Connection failures
   - Timeout handling
   - Protocol errors
   - State recovery

2. **Request Errors**
   - Response errors
   - Protocol errors
   - Timeout handling
   - State verification

## Integration Points

1. **HTTP Libraries**
   - Gun integration
   - httpc integration
   - Protocol handling
   - State management

2. **Metrics System**
   - Prometheus metrics
   - Request tracking
   - Performance monitoring
   - Error tracking

3. **Rate Limiting**
   - Request throttling
   - Resource management
   - Error handling
   - State tracking

## Analysis Insights

### Performance Considerations

1. **Connection Management**
   - Connection pooling
   - State tracking
   - Resource usage
   - Error recovery

2. **Request Handling**
   - Protocol selection
   - Response handling
   - Metrics collection
   - Resource cleanup

### Security Implications

1. **Connection Security**
   - Protocol security
   - State protection
   - Error isolation
   - Resource management

2. **Request Security**
   - Input validation
   - Response verification
   - Error handling
   - State protection

### Best Practices

1. **Connection Usage**
   - Pool management
   - Error handling
   - State tracking
   - Resource cleanup

2. **Request Handling**
   - Protocol selection
   - Error management
   - Metrics tracking
   - Resource cleanup

3. **Integration**
   - Library usage
   - Protocol support
   - Error handling
   - Resource management

### Example Usage

```erlang
% Start HTTP client
{ok, _Pid} = hb_http_client:start_link(#{
    http_connect_timeout => 5000,
    http_request_send_timeout => 30000,
    http_keepalive => 60000
}),

% Make HTTP request with Gun
{ok, Status, Headers, Body} = hb_http_client:req(#{
    peer => <<"http://localhost:8080">>,
    path => <<"/api/data">>,
    method => <<"GET">>,
    headers => #{<<"accept">> => <<"application/json">>}
}, #{http_client => gun}),

% Make HTTP request with httpc
{ok, Status, Headers, Body} = hb_http_client:req(#{
    peer => <<"http://localhost:8080">>,
    path => <<"/api/data">>,
    method => <<"POST">>,
    headers => #{<<"content-type">> => <<"application/json">>},
    body => JsonData
}, #{http_client => httpc})
