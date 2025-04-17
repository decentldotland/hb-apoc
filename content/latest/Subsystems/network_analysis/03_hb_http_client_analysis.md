# `hb_http_client.erl` Analysis

## Overview

`hb_http_client.erl` implements the client-side component of HyperBEAM's HTTP functionality, managing outbound HTTP connections and requests to external services. This module provides a layer of abstraction over lower-level HTTP client libraries, supporting both `gun` (an Erlang HTTP client based on the BEAM socket interface) and `httpc` (the standard OTP HTTP client).

The module is implemented as a `gen_server` that maintains connection pools, handles connection failures and retries, and provides instrumentation for metrics collection. It focuses on connection management and request handling, providing a reliable interface for other modules (particularly `hb_http.erl`) to make HTTP requests.

As noted in the module's documentation, it originated from the Arweave project and was modified for use in HyperBEAM, showing the system's integration with and adaptation of external components.

## Key Characteristics

- **Connection Pooling**: Manages a pool of HTTP connections for efficient resource usage
- **Dual Client Support**: Can use either `gun` or `httpc` as the underlying HTTP client
- **Connection Management**: Handles connection establishment, monitoring, and cleanup
- **Request Processing**: Sends HTTP requests and processes responses
- **Error Handling**: Detects and handles various network and protocol errors
- **Retry Logic**: Supports connection re-establishment on certain types of failures
- **Metrics Collection**: Integrates with Prometheus for performance monitoring
- **Protocol Flexibility**: Supports HTTP, HTTPS, and multiple HTTP versions
- **Streaming Support**: Handles chunked responses and streaming data
- **Rate Limiting**: Integrates with `ar_rate_limiter` for request throttling

## Dependencies

### Upstream Dependencies

- `gen_server`: For the OTP server behavior
- `prometheus_*`: For metrics collection and reporting
- `gun`: For the primary HTTP client implementation
- `httpc`: For the alternative HTTP client implementation
- `ar_rate_limiter`: For request throttling
- `hb_opts`: For configuration access
- `hb_util`: For utility functions
- `uri_string`: For URI parsing
- `ar_util`: For peer formatting utilities
- `inet`: For timeout handling

## Implementation Details

### Connection Management

The module maintains a pool of connections, tracked by peer address:

```erlang
-record(state, {
    pid_by_peer = #{},
    status_by_pid = #{},
    opts = #{}
}).
```

Connections are established on demand and are reused for subsequent requests to the same peer:

```erlang
handle_call({get_connection, Args}, From,
        #state{ pid_by_peer = PIDPeer, status_by_pid = StatusByPID } = State) ->
    Peer = maps:get(peer, Args),
    case maps:get(Peer, PIDPeer, not_found) of
        not_found ->
            {ok, PID} = open_connection(Args, State#state.opts),
            MonitorRef = monitor(process, PID),
            PIDPeer2 = maps:put(Peer, PID, PIDPeer),
            StatusByPID2 =
                maps:put(
                    PID,
                    {{connecting, [{From, Args}]}, MonitorRef, Peer},
                    StatusByPID
                ),
            {
                reply,
                {ok, PID},
                State#state{
                    pid_by_peer = PIDPeer2,
                    status_by_pid = StatusByPID2
                }
            };
        PID ->
            % Connection exists...
```

Connections are monitored for failures, and the module handles connection lifecycle events such as `gun_up`, `gun_error`, and `gun_down`:

```erlang
handle_info({gun_up, PID, _Protocol}, #state{ status_by_pid = StatusByPID } = State) ->
    % Handle connection established
    
handle_info({gun_error, PID, Reason}, #state{ ... } = State) ->
    % Handle connection error
    
handle_info({gun_down, PID, Protocol, Reason, _KilledStreams, _UnprocessedStreams}, #state{ ... } = State) ->
    % Handle connection down
```

### Request Handling

The module provides two main request functions: `gun_req` for using the `gun` client and `httpc_req` for using the standard `httpc` client:

```erlang
req(Args, Opts) -> req(Args, false, Opts).
req(Args, ReestablishedConnection, Opts) ->
    case hb_opts:get(http_client, gun, Opts) of
        gun -> gun_req(Args, ReestablishedConnection, Opts);
        httpc -> httpc_req(Args, ReestablishedConnection, Opts)
    end.
```

For `gun_req`, the function:
1. Gets a connection from the connection pool
2. Throttles the request if needed
3. Sends the request and waits for a response
4. Handles various response formats and errors
5. Collects metrics about the request

```erlang
gun_req(Args, ReestablishedConnection, Opts) ->
    StartTime = erlang:monotonic_time(),
    #{ peer := Peer, path := Path, method := Method } = Args,
    Response =
        case catch gen_server:call(?MODULE, {get_connection, Args}, infinity) of
            {ok, PID} ->
                ar_rate_limiter:throttle(Peer, Path, Opts),
                case request(PID, Args, Opts) of
                    % Handle various response scenarios...
                end;
            % Handle errors...
        end,
    % Record metrics...
    Response.
```

For `httpc_req`, the function directly uses the standard OTP HTTP client to make the request:

```erlang
httpc_req(Args, _, Opts) ->
    #{
        peer := Peer,
        path := Path,
        method := RawMethod,
        headers := Headers,
        body := Body
    } = Args,
    % Prepare the request...
    case httpc:request(Method, Request, [], HTTPCOpts) of
        {ok, {{_, Status, _}, RawRespHeaders, RespBody}} ->
            % Process response...
        {error, Reason} ->
            % Handle error...
    end.
```

### Response Processing

The module handles various response formats, including chunked responses:

```erlang
await_response(Args, Opts) ->
    #{ pid := PID, stream_ref := Ref, timer := Timer, limit := Limit,
            counter := Counter, acc := Acc, method := Method, path := Path } = Args,
    case gun:await(PID, Ref, inet:timeout(Timer)) of
        {response, fin, Status, Headers} ->
            % Complete response with no body
            
        {response, nofin, Status, Headers} ->
            % Response headers received, awaiting body
            
        {data, nofin, Data} ->
            % Partial body chunk received
            
        {data, fin, Data} ->
            % Final body chunk received
            
        % Handle various error conditions...
    end.
```

For chunked responses, the module accumulates the chunks until the complete response is received, with optional size limits:

```erlang
{data, nofin, Data} ->
    case Limit of
        infinity ->
            await_response(Args#{ acc := [Acc | Data] }, Opts);
        Limit ->
            Counter2 = size(Data) + Counter,
            case Limit >= Counter2 of
                true ->
                    await_response(
                        Args#{
                            counter := Counter2,
                            acc := [Acc | Data]
                        },
                        Opts
                    );
                false ->
                    % Size limit exceeded
                    {error, too_much_data}
            end
    end;
```

### Metrics Collection

The module initializes and collects several Prometheus metrics for monitoring HTTP client performance:

```erlang
init(Opts) ->
    prometheus_counter:new([
        {name, gun_requests_total},
        {labels, [http_method, route, status_class]},
        {
            help,
            "The total number of GUN requests."
        }
    ]),
    prometheus_gauge:new([{name, outbound_connections},
        {help, "The current number of the open outbound network connections"}]),
    prometheus_histogram:new([
        {name, http_request_duration_seconds},
        {buckets, [0.01, 0.1, 0.5, 1, 5, 10, 30, 60]},
        {labels, [http_method, route, status_class]},
        {
            help,
            "The total duration of an hb_http_client:req call."
        }
    ]),
    % Additional metrics...
```

These metrics are updated throughout the request lifecycle:

```erlang
% When a connection is established
prometheus_gauge:inc(outbound_connections);

% When a connection is closed
prometheus_gauge:dec(outbound_connections);

% When a request completes
prometheus_histogram:observe(http_request_duration_seconds, [
        method_to_list(Method),
        Path,
        get_status_class(Response)
    ], EndTime - StartTime);
    
% For download metrics
prometheus_counter:inc(
    http_client_downloaded_bytes_total,
    [Path],
    byte_size(Data)
);

% For upload metrics
prometheus_counter:inc(
    http_client_uploaded_bytes_total,
    [Path],
    byte_size(Body)
);
```

## Questions and Insights

### Questions

1. **Connection Reuse Policy**: How aggressively does the system reuse connections? Are there limits on the number of connections per peer or total connections?

2. **Connection Timeout Management**: How does the system decide when to recycle or purge idle connections? Is there a keepalive mechanism for long-lived connections?

3. **Retry Strategy**: What is the system's policy for retrying failed requests? Are there exponential backoff mechanisms?

4. **Protocol Negotiation**: How does the system handle protocol negotiation for HTTP/2 and HTTP/3? Is there a fallback mechanism for servers that don't support newer protocols?

5. **Rate Limiting Strategy**: What specific rate limiting strategies are implemented by `ar_rate_limiter`? Is this peer-specific, global, or adaptive?

### Insights

1. **Dual Client Architecture**: The support for both `gun` and `httpc` provides flexibility and fallback options, showing a pragmatic approach to HTTP client implementation.

2. **Connection State Management**: The careful tracking and management of connection states (connecting, connected, down) demonstrates a robust approach to connection handling.

3. **Comprehensive Metrics**: The extensive metrics collection shows a focus on observability and performance monitoring, which is essential for distributed systems.

4. **Legacy Integration**: The adaptation of code from Arweave demonstrates HyperBEAM's pragmatic approach to reusing existing components while adapting them to its architecture.

5. **Error Classification**: The detailed error status classification (e.g., "connect_timeout", "econnrefused") provides valuable diagnostic information for network issues.

## Integration with Other Subsystems

### Integration with Core Infrastructure

- Uses `hb_opts` for configuration options
- Uses `hb_util` for utility functions

### Integration with Network Communication Subsystem

- Called by `hb_http` for making outbound HTTP requests
- Works with `hb_http_client_sup` for supervision

### Integration with Arweave Subsystem

- Uses `ar_rate_limiter` for request throttling
- Uses `ar_util` for formatting peer addresses

## Recategorization Considerations

This module is correctly categorized as part of the Network Communication Subsystem. It specifically handles the client-side aspects of HTTP communication, complementing `hb_http.erl` and `hb_http_server.erl`.

While it has connections to the Arweave subsystem (through the rate limiter and its origins in Arweave code), its primary purpose is to manage outbound HTTP connections, which is squarely within the Network Communication domain.

The module's focused scope on HTTP client functionality makes it a clear fit for this subsystem. It doesn't have strong interdependencies with other subsystems beyond the expected connections to configuration and utility functions.
