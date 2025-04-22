# Module: hb_http

## Basic Information
- **Source File:** hb_http.erl
- **Module Type:** Core Network & HTTP
- **Purpose:** Core HTTP request/reply functionality for HyperBEAM

## Purpose
Provides fundamental HTTP communication capabilities for HyperBEAM, handling message-based requests and responses. The module converts between HTTP and HyperBEAM's internal message formats, manages content negotiation, and handles protocol-specific features like CORS and message signing.

## Interface

### Core Operations
- `get/2,3` - HTTP GET request with message response
- `post/3,4` - HTTP POST request with message response
- `request/2,4,5` - Generic HTTP request handling
- `reply/4` - Generate HTTP response from message

### Protocol Support
- `accept_to_codec/2` - Content negotiation
- `req_to_tabm_singleton/3` - Convert HTTP to TABM format
- `encode_reply/3` - Generate HTTP response format

### Codec Management
- `mime_to_codec/2` - Convert MIME types to codecs
- `codec_to_content_type/2` - Get content type for codec
- `default_codec/1` - Get default codec for options

## Dependencies

### Direct Dependencies
- httpc: HTTP client operations
- cowboy: HTTP server handling
- hb_message: Message processing
- hb_ao: Core operations
- dev_codec_httpsig: HTTPSig codec
- dev_codec_httpsig_conv: HTTPSig conversion
- ar_bundles: ANS-104 bundle handling

### Inverse Dependencies
- Used by hb_http_client
- Used by hb_http_server
- Used by hb_gateway_client
- Core HTTP functionality provider

## Implementation Details

### Key Concepts

1. **Message-Based Communication**
   ```erlang
   % HTTP GET with message response
   get(Node, Message, Opts) ->
       request(<<"GET">>, Node, Path, Message, Opts)
   
   % HTTP POST with message handling
   post(Node, Message, Opts) ->
       request(<<"POST">>, Node, Path, Message, Opts)
   ```

2. **Content Negotiation**
   ```erlang
   % Codec selection priority:
   % 1. accept-codec in message
   % 2. accept header in request
   % 3. default codec
   accept_to_codec(TABMReq, Opts) ->
       AcceptCodec = maps:get(
           <<"accept-codec">>,
           TABMReq,
           mime_to_codec(maps:get(<<"accept">>, TABMReq, <<"*/*">>), Opts)
       )
   ```

3. **Protocol Conversion**
   ```erlang
   % Convert HTTP request to TABM format
   req_to_tabm_singleton(Req, Body, Opts) ->
       case cowboy_req:header(<<"codec-device">>, Req) of
           <<"httpsig@1.0">> -> 
               httpsig_to_tabm_singleton(Req, Body, Opts);
           <<"ans104@1.0">> ->
               ans104_to_tabm_singleton(Body, Opts)
       end
   ```

### State Management

1. **Request State**
   - Method handling
   - Path resolution
   - Header management
   - Body processing

2. **Response State**
   - Status codes
   - Header generation
   - Body encoding
   - CORS handling

3. **Protocol State**
   - Codec selection
   - Format conversion
   - Message signing
   - State verification

### Error Handling

1. **HTTP Errors**
   - Status codes
   - Error responses
   - Protocol errors
   - State validation

2. **Protocol Errors**
   - Codec failures
   - Format errors
   - Signature validation
   - State verification

## Integration Points

1. **HTTP Client**
   - Request handling
   - Response processing
   - State management
   - Error handling

2. **HTTP Server**
   - Request processing
   - Response generation
   - Protocol handling
   - State management

3. **Message System**
   - Format conversion
   - State tracking
   - Protocol handling
   - Error management

## Analysis Insights

### Performance Considerations

1. **Request Handling**
   - Efficient conversion
   - State management
   - Resource usage
   - Error handling

2. **Response Generation**
   - Format optimization
   - Header management
   - Body encoding
   - Resource usage

### Security Implications

1. **Message Validation**
   - Signature verification
   - Format validation
   - State verification
   - Error handling

2. **Protocol Security**
   - CORS handling
   - Header validation
   - State protection
   - Error isolation

### Best Practices

1. **Request Handling**
   - Validate inputs
   - Handle errors
   - Manage state
   - Clean resources

2. **Response Generation**
   - Set proper headers
   - Handle CORS
   - Validate state
   - Clean resources

3. **Protocol Usage**
   - Use appropriate codecs
   - Handle signatures
   - Validate formats
   - Manage state

### Example Usage

```erlang
% Basic GET request
{ok, Response} = hb_http:get(Node, #{
    <<"path">> => <<"/api/data">>,
    <<"accept">> => <<"application/json">>
}, Opts),

% POST with message
{ok, Response} = hb_http:post(Node, #{
    <<"path">> => <<"/api/compute">>,
    <<"body">> => Data,
    <<"codec-device">> => <<"httpsig@1.0">>
}, Opts),

% Handle HTTP response
{ok, Response} = hb_http:reply(Req, TABMReq, #{
    <<"status">> => 200,
    <<"body">> => Result
}, Opts)
