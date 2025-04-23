# Web-to-Core Integration

## Overview

Web-to-Core integration is a critical integration point in HyperBEAM that enables external web clients to interact with the platform's internal processing capabilities. This analysis examines how the external HTTP and WebSocket interfaces are integrated with HyperBEAM's internal message and device ecosystem, focusing on the mechanisms, data flows, and architectural significance of this integration point.

HyperBEAM's architecture serves as both a web server and a distributed processing platform, requiring sophisticated integration between traditional web protocols and its internal message-centric processing model. This integration enables web clients to initiate processes, retrieve results, interact with the blockchain, and participate in distributed computation workflows.

Understanding the Web-to-Core integration reveals critical aspects of HyperBEAM's external-facing architecture, security model, and extensibility approach, as this integration point forms the primary gateway through which external systems interact with the platform.

## Involved Subsystems

Web-to-Core integration involves several key subsystems:

### Web-Side Subsystems

- **HTTP Server**: Receives and responds to HTTP requests from clients
- **WebSocket Handler**: Maintains bidirectional connections with clients
- **Content Negotiation**: Determines appropriate response formats
- **Authentication and Authorization**: Validates client credentials and permissions

### Core-Side Subsystems

- **Message Processing**: Processes client requests as internal messages
- **Device Ecosystem**: Executes business logic and processing operations
- **Process and Scheduler**: Manages long-running computation processes
- **Storage and Caching**: Persists and retrieves data for web requests

### Integration Subsystems

- **Codec and Format Conversion**: Translates between web and internal formats
- **HTTP-to-Message Adaptation**: Converts HTTP concepts to message concepts
- **Security Boundary**: Provides security controls at the integration boundary
- **Error Handling**: Translates internal errors to web-appropriate responses

## Integration Mechanisms

Several mechanisms enable Web-to-Core integration:

### 1. HTTP Message Conversion

The HTTP server converts HTTP requests to internal messages:

```erlang
% Example from hb_http.erl
message_from_request(Req, Opts) ->
    % Extract HTTP components
    Method = maps:get(method, Req),
    Path = maps:get(path, Req),
    Headers = maps:get(headers, Req, #{}),
    Body = maps:get(body, Req, undefined),
    
    % Create internal message
    Message = #{
        <<"method">> => Method,
        <<"path">> => Path,
        <<"headers">> => Headers
    },
    
    % Add body if present
    MessageWithBody = case Body of
        undefined -> Message;
        _ -> Message#{<<"body">> => Body}
    end,
    
    % Add request metadata
    FinalMessage = add_request_metadata(MessageWithBody, Req, Opts),
    
    {ok, FinalMessage}.
```

This mechanism handles:
- **Method Mapping**: HTTP methods to internal operation types
- **Path Translation**: URL paths to internal resource paths
- **Header Processing**: HTTP headers to message metadata
- **Body Handling**: Request bodies to message content

### 2. Singleton Integration

The `hb_singleton` module serves as a RESTful API gateway:

```erlang
% Example based on hb_singleton.erl
handle_request(Message, Opts) ->
    % Extract API path components
    Path = hb_converge:get(Message, <<"path">>, <<>>, Opts),
    PathComponents = binary:split(Path, <<"/">>, [global, trim_all]),
    
    % Route to appropriate handler
    case PathComponents of
        [<<"api">>, <<"v1">>, <<"processes">>] ->
            handle_processes_request(Message, Opts);
        [<<"api">>, <<"v1">>, <<"processes">>, ProcessId] ->
            handle_process_request(ProcessId, Message, Opts);
        [<<"api">>, <<"v1">>, <<"schedulers">>] ->
            handle_schedulers_request(Message, Opts);
        % ... other routes
        _ ->
            {error, {not_found, Path}}
    end.
```

This mechanism provides:
- **RESTful Routing**: Mapping URL patterns to internal operations
- **API Versioning**: Supporting multiple API versions
- **Resource Mapping**: Exposing internal resources as API resources
- **Operation Translation**: Converting API operations to device operations

### 3. WebSocket Integration

WebSocket handlers integrate real-time communication:

```erlang
% Example based on websocket handling
handle_websocket_message(Frame, State, Opts) ->
    % Parse WebSocket message
    case parse_frame(Frame) of
        {ok, Message} ->
            % Convert to internal message format
            case websocket_to_internal(Message, Opts) of
                {ok, InternalMessage} ->
                    % Process message
                    case process_message(InternalMessage, Opts) of
                        {ok, Result} ->
                            % Send response back through WebSocket
                            Response = internal_to_websocket(Result, Opts),
                            {reply, Response, State};
                        {error, Error} ->
                            % Format error response
                            ErrorResponse = format_error_for_websocket(Error, Opts),
                            {reply, ErrorResponse, State}
                    end;
                {error, Error} ->
                    % Format conversion error
                    ErrorResponse = format_error_for_websocket(Error, Opts),
                    {reply, ErrorResponse, State}
            end;
        {error, Error} ->
            % Frame parsing error
            ErrorResponse = format_error_for_websocket(Error, Opts),
            {reply, ErrorResponse, State}
    end.
```

This mechanism enables:
- **Real-time Communication**: Bidirectional client-server interaction
- **Event Streaming**: Server-to-client event notifications
- **Subscription Model**: Clients subscribing to resource updates
- **Process Monitoring**: Real-time visibility into process execution

### 4. HTTP Response Generation

Internal results are converted to HTTP responses:

```erlang
% Example from hb_http.erl
message_to_response(Message, Status, Opts) ->
    % Extract response components
    Headers = hb_converge:get(Message, <<"headers">>, #{}, Opts),
    Body = hb_converge:get(Message, <<"body">>, <<>>, Opts),
    
    % Create HTTP response
    #{
        status => Status,
        headers => Headers,
        body => Body
    }.
```

This mechanism handles:
- **Status Mapping**: Internal results to HTTP status codes
- **Header Generation**: Adding appropriate HTTP headers
- **Content Formatting**: Formatting response bodies appropriately
- **Error Translation**: Converting internal errors to HTTP errors

## Message and Data Flow

The Web-to-Core integration involves several distinct data flows:

### 1. Inbound Request Flow

HTTP requests flow from clients to internal processing:

```
HTTP Request → HTTP Server → Message Conversion →
Authentication → API Routing → Device Resolution →
Message Processing
```

Key aspects of this flow:
- **Entry Point**: Requests enter through the HTTP server
- **Format Translation**: HTTP concepts are mapped to message concepts
- **Security Checks**: Requests are authenticated and authorized
- **Routing Decision**: Requests are routed to appropriate handlers
- **Processing Initiation**: Device processing is triggered

### 2. Outbound Response Flow

Processing results flow back to clients:

```
Processing Result → Response Formatting → Content Negotiation →
Status Code Selection → Header Generation → HTTP Response
```

Key aspects of this flow:
- **Result Preparation**: Internal results are prepared for external consumption
- **Format Selection**: Results are formatted based on client preferences
- **Status Determination**: Appropriate HTTP status codes are selected
- **Response Assembly**: Complete HTTP responses are assembled
- **Client Delivery**: Responses are delivered to clients

### 3. WebSocket Bidirectional Flow

WebSocket connections enable bidirectional communication:

```
WebSocket Connection →
  Client Message → Message Conversion → Processing → Response → Client
  Server Event → Event Conversion → Event Notification → Client
```

Key aspects of this flow:
- **Session Maintenance**: Long-lived connections are maintained
- **Bidirectional Communication**: Both client-initiated and server-initiated messages
- **Real-time Updates**: Immediate notification of state changes
- **Subscription Management**: Managing client interest in specific events
- **Connection Lifecycle**: Handling connection establishment and termination

### 4. Long-Running Process Flow

Long-running processes involve multi-stage interaction:

```
Initiation Request → Process Creation → Immediate Response →
Background Processing → Client Polling/WebSocket Updates →
Result Retrieval → Final Response
```

Key aspects of this flow:
- **Asynchronous Processing**: Non-blocking request handling
- **Process Tracking**: Maintaining process state for later retrieval
- **Progress Updates**: Communicating processing progress to clients
- **Result Persistence**: Storing results for client retrieval
- **Completion Notification**: Informing clients of process completion

## Configuration Aspects

Web-to-Core integration can be configured in several ways:

### 1. HTTP Server Configuration

HTTP server behavior is configured through options:

```erlang
% Example HTTP server configuration
http_server_options() ->
    #{
        port => 8080,
        max_connections => 1000,
        timeout => 30000,
        ssl => #{
            enabled => true,
            certfile => "cert.pem",
            keyfile => "key.pem"
        },
        websocket => #{
            enabled => true,
            timeout => 60000
        }
    }.
```

This configuration controls:
- **Port Selection**: What port the server listens on
- **Connection Limits**: Maximum concurrent connections
- **Timeout Values**: Connection and request timeouts
- **SSL Settings**: Certificate and encryption settings
- **WebSocket Settings**: WebSocket-specific configurations

### 2. API Endpoint Configuration

API endpoints can be configured:

```erlang
% Example API endpoint configuration
api_endpoints() ->
    #{
        <<"api/v1">> => #{
            enabled => true,
            rate_limit => 100,
            authentication => true
        },
        <<"api/v2">> => #{
            enabled => true,
            rate_limit => 200,
            authentication => true
        },
        <<"public">> => #{
            enabled => true,
            rate_limit => 50,
            authentication => false
        }
    }.
```

This configuration controls:
- **API Versions**: What API versions are available
- **Rate Limiting**: Request rate limits for endpoints
- **Authentication Requirements**: Whether authentication is required
- **Feature Flags**: Enabling/disabling specific API features
- **Documentation**: API documentation settings

### 3. Content Negotiation Configuration

Content negotiation can be configured:

```erlang
% Example content negotiation configuration
content_negotiation_options() ->
    #{
        default_format => <<"application/json">>,
        supported_formats => [
            <<"application/json">>,
            <<"application/xml">>,
            <<"text/plain">>,
            <<"application/x-www-form-urlencoded">>
        ],
        charset => <<"utf-8">>
    }.
```

This configuration controls:
- **Default Format**: Format used when none specified
- **Supported Formats**: Formats the server can produce/consume
- **Charset Settings**: Character encoding options
- **Quality Preferences**: Format preference rankings
- **Format Parameters**: Format-specific configuration

### 4. Authentication Configuration

Authentication can be configured:

```erlang
% Example authentication configuration
authentication_options() ->
    #{
        methods => [<<"bearer">>, <<"basic">>, <<"api_key">>],
        token_validation => #{
            enabled => true,
            cache_expiry => 300,
            public_key => "public_key.pem"
        },
        session => #{
            enabled => true,
            expiry => 3600,
            max_sessions => 10
        }
    }.
```

This configuration controls:
- **Auth Methods**: Supported authentication methods
- **Token Validation**: How tokens are validated
- **Session Management**: Session lifecycle settings
- **Key Management**: Cryptographic key settings
- **Caching Settings**: Authentication caching behavior

## Security Implications

Web-to-Core integration has several security implications:

### 1. Attack Surface

The web interface presents an attack surface:

- **Public Exposure**: Directly accessible to potential attackers
- **Protocol Vulnerabilities**: Subject to HTTP-specific vulnerabilities
- **Input Validation**: Must handle potentially malicious input
- **Rate Limiting**: Must prevent abuse through request flooding
- **Resource Consumption**: Must prevent resource exhaustion attacks

### 2. Authentication and Authorization

Security controls must be implemented:

- **Identity Verification**: Validating client identities
- **Permission Checking**: Enforcing access control policies
- **Session Management**: Securely managing user sessions
- **Credential Protection**: Protecting authentication credentials
- **Principle of Least Privilege**: Minimizing client capabilities

### 3. Data Protection

Data must be protected during web interaction:

- **Transport Security**: Encrypting data in transit
- **Input Sanitization**: Cleaning potentially dangerous inputs
- **Output Encoding**: Preventing injection attacks in responses
- **Sensitive Data Handling**: Protecting sensitive information
- **Error Information Leakage**: Preventing information disclosure in errors

### 4. Security Headers

Web responses should include security headers:

- **Content-Security-Policy**: Controlling resource loading
- **X-Content-Type-Options**: Preventing MIME-type sniffing
- **X-Frame-Options**: Preventing clickjacking
- **Strict-Transport-Security**: Enforcing HTTPS
- **X-XSS-Protection**: Browser XSS filtering

## Error Handling

Error handling in Web-to-Core integration follows several patterns:

### 1. HTTP Error Mapping

Internal errors are mapped to HTTP status codes:

```erlang
% Example HTTP error mapping
http_status_for_error(Error) ->
    case Error of
        {not_found, _} -> 404;
        {unauthorized, _} -> 401;
        {forbidden, _} -> 403;
        {bad_request, _} -> 400;
        {validation_error, _} -> 422;
        {conflict, _} -> 409;
        {rate_limited, _} -> 429;
        {timeout, _} -> 504;
        {internal_error, _} -> 500;
        _ -> 500
    end.
```

### 2. Error Response Formatting

Error responses are formatted for clients:

```erlang
% Example error response formatting
format_error_response(Error, Opts) ->
    Status = http_status_for_error(Error),
    ErrorCode = error_code_for_error(Error),
    Message = error_message_for_error(Error, Opts),
    
    % Create error response body
    Body = #{
        error => #{
            code => ErrorCode,
            message => Message,
            details => error_details_for_client(Error, Opts)
        }
    },
    
    % Create HTTP response
    #{
        status => Status,
        headers => #{<<"content-type">> => <<"application/json">>},
        body => jiffy:encode(Body)
    }.
```

### 3. WebSocket Error Handling

WebSocket connections require special error handling:

```erlang
% Example WebSocket error handling
handle_websocket_error(Error, State, Opts) ->
    % Format error for client
    ErrorResponse = format_error_for_websocket(Error, Opts),
    
    % Determine if connection should terminate
    case should_terminate_on_error(Error) of
        true ->
            % Send error and close connection
            {reply, ErrorResponse, State, close};
        false ->
            % Send error but keep connection
            {reply, ErrorResponse, State}
    end.
```

### 4. Error Logging

Errors are logged for diagnostic purposes:

```erlang
% Example error logging
log_web_error(Error, Request, Opts) ->
    % Prepare log entry
    LogEntry = #{
        timestamp => os:system_time(millisecond),
        error => Error,
        request => sanitize_request_for_logging(Request),
        client_info => extract_client_info(Request)
    },
    
    % Log error based on severity
    Severity = severity_for_error(Error),
    case Severity of
        critical -> hb_logger:critical("Web API Error", LogEntry);
        error -> hb_logger:error("Web API Error", LogEntry);
        warning -> hb_logger:warning("Web API Error", LogEntry);
        _ -> hb_logger:info("Web API Error", LogEntry)
    end.
```

## Performance Considerations

Web-to-Core integration has several performance implications:

### 1. Request Handling Efficiency

Efficient request handling is essential:

- **Connection Pooling**: Reusing connections for multiple requests
- **Worker Pools**: Processing requests with worker pools
- **Efficient Parsing**: Optimizing request parsing
- **Zero-Copy**: Minimizing data copying during processing
- **Asynchronous Processing**: Non-blocking request handling

### 2. Content Negotiation Optimization

Content negotiation can be optimized:

- **Format Caching**: Caching parsed Accept headers
- **Default Fast Paths**: Optimized handling for common formats
- **Lazy Conversion**: Delaying format conversion until necessary
- **Streaming Responses**: Streaming large responses incrementally
- **Compression**: Compressing responses for efficient transfer

### 3. Connection Management

Connection handling affects performance:

- **Keep-Alive**: Maintaining connections for multiple requests
- **Connection Limits**: Preventing resource exhaustion
- **Timeout Management**: Appropriate timeout settings
- **Backpressure**: Throttling requests during high load
- **HTTP/2 Multiplexing**: Using multiplexed connections

### 4. Caching Strategies

Caching improves performance:

- **Response Caching**: Caching complete responses
- **Partial Result Caching**: Caching intermediate results
- **Conditional Requests**: Supporting If-Modified-Since/ETag
- **Cache Invalidation**: Properly invalidating cached responses
- **Cache Control Headers**: Guiding client caching behavior

## Examples

Let's examine concrete examples of Web-to-Core integration from the codebase:

### HTTP API Handler

```erlang
% Example based on HTTP API handling
handle_process_request(ProcessId, Message, Opts) ->
    % Extract HTTP method
    Method = hb_converge:get(Message, <<"method">>, <<"GET">>, Opts),
    
    % Handle based on HTTP method
    case Method of
        <<"GET">> ->
            % Retrieve process
            get_process(ProcessId, Message, Opts);
        <<"POST">> ->
            % Update process
            update_process(ProcessId, Message, Opts);
        <<"DELETE">> ->
            % Delete process
            delete_process(ProcessId, Message, Opts);
        _ ->
            % Method not allowed
            {error, {method_not_allowed, Method}}
    end.

% Process retrieval implementation
get_process(ProcessId, Message, Opts) ->
    % Create process device request
    ProcessRequest = #{
        <<"action">> => <<"get">>,
        <<"process_id">> => ProcessId
    },
    
    % Resolve through process device
    case hb_converge:resolve(Message, {as, <<"process@1.0">>, ProcessRequest}, Opts) of
        {ok, Result} ->
            % Format for HTTP response
            format_process_response(Result, Opts);
        {error, Error} ->
            % Pass through error
            {error, Error}
    end.
```

This example demonstrates:
- **RESTful API Pattern**: HTTP methods mapping to operations
- **Resource Identification**: URL paths identifying resources
- **Device Integration**: Delegating to appropriate devices
- **Response Formatting**: Formatting results for external consumption

### WebSocket Handler

```erlang
% Example based on WebSocket handling
handle_websocket_frame({text, Data}, State, Opts) ->
    % Parse JSON data
    case jiffy:decode(Data, [return_maps]) of
        {ok, Message} ->
            % Process based on message type
            Type = maps:get(<<"type">>, Message, undefined),
            case Type of
                <<"subscribe">> ->
                    % Handle subscription request
                    handle_subscription(Message, State, Opts);
                <<"unsubscribe">> ->
                    % Handle unsubscription request
                    handle_unsubscription(Message, State, Opts);
                <<"process_request">> ->
                    % Handle process operation
                    handle_process_operation(Message, State, Opts);
                <<"ping">> ->
                    % Handle ping message
                    {reply, {text, jiffy:encode(#{<<"type">> => <<"pong">>})}, State};
                _ ->
                    % Unknown message type
                    {reply, {text, error_response(<<"unknown_message_type">>, <<"Unknown message type">>)}, State}
            end;
        {error, _} ->
            % JSON parsing error
            {reply, {text, error_response(<<"invalid_json">>, <<"Invalid JSON">>)}, State}
    end.

% Subscription handling
handle_subscription(Message, State = #{subscriptions := Subs}, Opts) ->
    % Extract subscription details
    Topic = maps:get(<<"topic">>, Message, undefined),
    if
        Topic == undefined ->
            % Missing topic
            {reply, {text, error_response(<<"missing_topic">>, <<"Missing topic">>)}, State};
        true ->
            % Register subscription
            NewSubs = maps:put(Topic, true, Subs),
            % Confirm subscription
            Response = jiffy:encode(#{
                <<"type">> => <<"subscription_confirmed">>,
                <<"topic">> => Topic
            }),
            {reply, {text, Response}, State#{subscriptions := NewSubs}}
    end.
```

This example demonstrates:
- **WebSocket Protocol**: Handling text and binary frames
- **Command Pattern**: Message type determining operation
- **Subscription Management**: Topic-based subscription
- **State Management**: Maintaining client-specific state
- **Real-time Communication**: Immediate client notification

### Content Negotiation

```erlang
% Example content negotiation
negotiate_response_format(Request, Result, Opts) ->
    % Extract Accept header
    Headers = hb_converge:get(Request, <<"headers">>, #{}, Opts),
    Accept = maps:get(<<"accept">>, Headers, <<"application/json">>),
    
    % Parse Accept header
    AcceptedTypes = parse_accept_header(Accept),
    
    % Find best matching format
    case find_matching_format(AcceptedTypes, Opts) of
        {ok, <<"application/json">>} ->
            % Format as JSON
            {ok, format_json_response(Result, Opts)};
        {ok, <<"application/xml">>} ->
            % Format as XML
            {ok, format_xml_response(Result, Opts)};
        {ok, <<"text/plain">>} ->
            % Format as plain text
            {ok, format_text_response(Result, Opts)};
        {ok, Format} ->
            % Unsupported format
            {error, {unsupported_format, Format}};
        {error, Error} ->
            % Negotiation error
            {error, Error}
    end.
```

This example demonstrates:
- **Content Negotiation**: Selecting appropriate response format
- **Accept Header Parsing**: Processing client format preferences
- **Format Selection**: Finding best matching format
- **Response Formatting**: Formatting responses in selected format
- **Error Handling**: Handling unsupported formats

### Authentication Integration

```erlang
% Example authentication integration
authenticate_request(Request, Opts) ->
    % Extract Authorization header
    Headers = hb_converge:get(Request, <<"headers">>, #{}, Opts),
    Authorization = maps:get(<<"authorization">>, Headers, undefined),
    
    if
        Authorization == undefined ->
            % No credentials provided
            {error, {unauthorized, <<"Missing credentials">>}};
        true ->
            % Parse authorization header
            case parse_authorization_header(Authorization) of
                {bearer, Token} ->
                    % Validate JWT token
                    validate_jwt_token(Token, Opts);
                {basic, Username, Password} ->
                    % Validate username/password
                    validate_basic_auth(Username, Password, Opts);
                {api_key, Key} ->
                    % Validate API key
                    validate_api_key(Key, Opts);
                {error, Error} ->
                    % Invalid authorization header
                    {error, {unauthorized, Error}}
            end
    end.
```

This example demonstrates:
- **Authentication Extraction**: Getting credentials from request
- **Multi-method Support**: Supporting different auth methods
- **Token Validation**: Validating authentication tokens
- **Credential Verification**: Verifying provided credentials
- **Security Integration**: Integrating with security subsystems

## Architectural Significance

Web-to-Core integration is architecturally significant for several reasons:

### 1. External Interface

This integration serves as the primary external interface:

- **System Boundary**: Defines the boundary between external and internal
- **Public API**: Provides the public programming interface
- **Integration Point**: Enables integration with external systems
- **User Experience**: Directly impacts user experience
- **Ecosystem Participation**: Enables participation in web ecosystem

### 2. Security Perimeter

This integration forms a critical security perimeter:

- **Attack Surface**: Represents the most exposed attack surface
- **Defense in Depth**: First layer of defense for internal systems
- **Authentication Boundary**: Point of identity verification
- **Authorization Enforcement**: Point of access control enforcement
- **Input Validation**: First validation point for external data

### 3. Abstraction Layer

This integration provides an abstraction over internal complexity:

- **Implementation Hiding**: Hiding internal implementation details
- **API Stability**: Providing stable interface despite internal changes
- **Consistency Layer**: Ensuring consistent client experience
- **Protocol Translation**: Translating between external and internal protocols
- **Domain Alignment**: Aligning technical systems with domain concepts

### 4. Evolution Support

This integration facilitates system evolution:

- **Versioning**: Supporting multiple API versions during transition
- **Feature Flags**: Allowing gradual feature rollout
- **Backward Compatibility**: Maintaining compatibility with clients
- **Progressive Enhancement**: Adding capabilities while maintaining base functionality
- **Documentation**: Providing self-documenting capabilities

## Conclusion

Web-to-Core integration is a fundamental integration point in HyperBEAM that enables external web clients to interact with the platform's internal processing capabilities. This integration creates a bridge between the web's request-response model and HyperBEAM's message-centric processing architecture, enabling a wide range of interactions from simple data retrieval to complex distributed computation.

The integration patterns reveal key architectural principles in HyperBEAM:

1. **Protocol Translation**: Bridging between web protocols and internal messaging
2. **Security Boundary**: Creating a secure perimeter around internal systems
3. **API Abstraction**: Providing stable interfaces over complex internals
4. **Content Negotiation**: Supporting diverse client requirements
5. **Progressive Interaction**: Enabling both simple and complex interaction patterns

Understanding this integration point is essential for working with HyperBEAM's externally-facing capabilities, diagnosing issues that cross the web boundary, and extending the system with new web-based interfaces. The robust integration between web protocols and internal processing demonstrates the elegant architectural foundation that enables HyperBEAM's flexibility as both a web server and a distributed processing platform.
