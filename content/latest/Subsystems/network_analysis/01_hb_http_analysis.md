# `hb_http.erl` Analysis

## Overview

`hb_http.erl` serves as HyperBEAM's core HTTP request/reply functionality, providing a crucial bridge between the message-based internal architecture and external HTTP communication. The module abstracts HTTP operations into message transformations, allowing HyperBEAM components to interact with external systems through a unified interface that maintains the message-centric design philosophy.

Rather than exposing raw HTTP mechanics, the module transforms HTTP interactions into message operations, maintaining consistency with the system's broader architecture. It handles the complex task of encoding and decoding between HyperBEAM's internal message format and the HTTP protocol, with support for different content formats through pluggable codecs.

## Key Characteristics

- **Message-Centric Design**: Treats HTTP as a transport for messages rather than a separate protocol
- **Bidirectional Conversion**: Converts between internal message formats and HTTP requests/responses
- **Content Negotiation**: Supports multiple content formats through codec selection
- **Signature Verification**: Validates message signatures for secure communication
- **Routing Support**: Handles complex routing decisions through integration with `dev_router`
- **Multi-Node Requests**: Capable of dispatching requests to multiple nodes and aggregating results
- **CORS Support**: Automatically handles Cross-Origin Resource Sharing headers
- **Deep Integration**: Works closely with HyperBEAM's message and device systems

## Dependencies

### Upstream Dependencies

- `hb_converge`: For message resolution and key lookup
- `hb_message`: For message conversion and signature verification
- `hb_http_client`: For making actual HTTP requests
- `hb_opts`: For accessing configuration options
- `hb_path`: For path normalization
- `hb_util`: For utility functions
- `hb_cache`: For storing signed messages
- `dev_router`: For message routing decisions
- `dev_codec_httpsig`: For HTTP signature handling
- `dev_codec_httpsig_conv`: For HTTP signature conversions
- `ar_bundles`: For ANS-104 message serialization/deserialization
- `cowboy_req`: For HTTP request/response handling
- `httpc`: For HTTP client configuration
- `uri_string`: For URI parsing

## Implementation Details

### HTTP Request Handling

The module provides several functions for making HTTP requests, with varying levels of abstraction:

```erlang
% High-level functions
get(Node, Opts) -> get(Node, <<"/">>, Opts).
get(Node, PathBin, Opts) when is_binary(PathBin) ->
    get(Node, #{ <<"path">> => PathBin }, Opts);
get(Node, Message, Opts) ->
    request(
        <<"GET">>,
        Node,
        hb_converge:get(<<"path">>, Message, <<"/">>, Opts),
        Message,
        Opts
    ).

post(Node, Message, Opts) ->
    post(Node,
        hb_converge:get(
            <<"path">>,
            Message,
            <<"/">>,
            Opts#{ topic => converge_internal }
        ),
        Message,
        Opts
    ).
```

These functions ultimately call `request/5`, which handles the actual HTTP request processing:

```erlang
request(Method, Peer, Path, RawMessage, Opts) ->
    Req = prepare_request(
        hb_converge:get(
            <<"codec-device">>,
            RawMessage,
            <<"httpsig@1.0">>,
            Opts
        ),
        Method,
        Peer,
        Path,
        RawMessage,
        Opts
    ),
    {_ErlStatus, Status, Headers, Body} = hb_http_client:req(Req, Opts),
    % Process the response...
```

This function prepares the request with the appropriate codec, sends it via `hb_http_client`, and then processes the response into a HyperBEAM message format.

### Content Negotiation

The module implements content negotiation through codec selection:

```erlang
accept_to_codec(TABMReq, Opts) ->
    AcceptCodec =
        maps:get(
            <<"accept-codec">>,
            TABMReq,
            mime_to_codec(maps:get(<<"accept">>, TABMReq, <<"*/*">>), Opts)
        ),
    case AcceptCodec of
        not_specified -> default_codec(Opts);
        _ -> AcceptCodec
    end.

mime_to_codec(<<"application/", Mime/binary>>, Opts) ->
    Name =
        case binary:match(Mime, <<"@">>) of
            nomatch -> << Mime/binary, "@1.0" >>;
            _ -> Mime
        end,
    try hb_converge:message_to_device(#{ <<"device">> => Name }, Opts)
    catch _:Error -> default_codec(Opts)
    end;
% Other cases...
```

This allows clients to specify their preferred content format, with the system selecting an appropriate codec for encoding and decoding messages.

### Multi-Node Requests

The module can handle requests to multiple nodes concurrently:

```erlang
multirequest(Config, Method, Path, Message, Opts) ->
    MultiOpts = #{
        nodes := Nodes,
        responses := Responses,
        stop_after := StopAfter,
        accept_status := Statuses,
        parallel := Parallel
    } = multirequest_opts(Config, Message, Opts),
    AllResults =
        if Parallel ->
            parallel_multirequest(
                Nodes, Responses, StopAfter, Method, Path, Message, Statuses, Opts);
        true ->
            serial_multirequest(
                Nodes, Responses, Method, Path, Message, Statuses, Opts)
        end,
    % Process results...
```

This functionality allows for:
- Parallel or serial execution
- Configurable success criteria (how many responses are needed)
- Early termination options
- Status code filtering

### HTTP Response Generation

For server-side HTTP responses, the module provides the `reply` function:

```erlang
reply(Req, TABMReq, Message, Opts) ->
    Status =
        case hb_converge:get(<<"status">>, Message, Opts) of
            not_found -> 200;
            S-> S
        end,
    reply(Req, TABMReq, Status, Message, Opts).

reply(Req, TABMReq, Status, RawMessage, Opts) ->
    Message = hb_converge:normalize_keys(RawMessage),
    {ok, HeadersBeforeCors, EncodedBody} = encode_reply(TABMReq, Message, Opts),
    % Add CORS headers and send response...
```

This function converts a HyperBEAM message into an HTTP response, handling:
- Status code selection
- CORS header addition
- Content encoding via the appropriate codec
- Cookie handling

### Message Security

The module includes security features for message verification:

```erlang
http_sig_to_tabm_singleton(Req = #{ headers := RawHeaders }, Body, Opts) ->
    Msg = dev_codec_httpsig_conv:from(
        RawHeaders#{ <<"body">> => Body }
    ),
    {ok, SignedMsg} =
        dev_codec_httpsig:reset_hmac(
            hb_util:ok(remove_unsigned_fields(Msg, Opts))
        ),
    ForceSignedRequests = hb_opts:get(force_signed_requests, false, Opts),
    case (not ForceSignedRequests) orelse hb_message:verify(SignedMsg) of
        true ->
            % Signature verified...
            maybe_add_unsigned(Req, SignedMsg, Opts);
        false ->
            % Signature verification failed...
            throw({invalid_signature, SignedMsg})
    end.
```

This ensures that signed messages are properly verified, with an option to require signatures for all requests.

## Tests

The module includes extensive tests that verify:

1. **Basic Functionality**: Simple HTTP request/response cycles
2. **Signature Verification**: Both signed and unsigned message handling
3. **Nested Path Resolution**: Accessing nested data through path expressions
4. **WASM Integration**: Running WebAssembly computations via HTTP
5. **CORS Support**: Proper handling of CORS headers
6. **Content Negotiation**: Using different content formats (ANS-104, HTTPSig)

These tests demonstrate the module's capabilities and ensure its correct operation in various scenarios.

## Questions and Insights

### Questions

1. **Error Handling Strategy**: How are transient network errors handled? Is there a retry mechanism or is error handling left to the caller?

2. **Performance Optimization**: Are there mechanisms for connection pooling or keep-alive optimization beyond the basic `httpc:set_options([{max_keep_alive_length, 0}])` setup?

3. **Timeout Management**: How are request timeouts managed, particularly for parallel multi-node requests?

4. **Streaming Support**: Does the system support streaming for large payloads, or is it primarily designed for message-sized data?

5. **HTTP/2 Support**: Is there support for HTTP/2 features like multiplexing and server push?

### Insights

1. **Message Abstraction**: The module successfully abstracts HTTP behind a message interface, maintaining the system's message-centric design even when communicating over HTTP.

2. **Protocol Bridging**: Rather than exposing a traditional HTTP client API, the module bridges between HTTP and HyperBEAM's message protocol, allowing components to stay within the message paradigm.

3. **Codec Flexibility**: The pluggable codec system allows for different content formats without changing the core HTTP handling logic.

4. **Security Integration**: Signature verification is tightly integrated with the HTTP layer, ensuring security from the edge of the system.

5. **Distributed Design**: The multi-node request functionality reflects HyperBEAM's distributed architecture, allowing for communication with multiple nodes as a unified operation.

## Integration with Other Subsystems

### Integration with Core Infrastructure

- Uses `hb_converge` for message resolution and key lookup
- Uses `hb_message` for message conversion and signature verification
- Uses `hb_opts` for configuration access
- Uses `hb_path` for path normalization

### Integration with Storage Subsystem

- Uses `hb_cache` for storing signed messages received over HTTP

### Integration with Device Subsystem

- Uses `dev_router` for message routing decisions
- Uses `dev_codec_httpsig` and `dev_codec_httpsig_conv` for HTTP signature handling

### Integration with Arweave Subsystem

- Uses `ar_bundles` for ANS-104 message serialization/deserialization

## Recategorization Considerations

This module is correctly categorized as part of the Network Communication Subsystem. Its primary purpose is to handle HTTP communication, bridging between HyperBEAM's internal message format and external HTTP interactions.

However, it's worth noting that the module represents more than just an HTTP client—it's a protocol adapter that maps between HTTP and HyperBEAM's message protocol. This makes it a critical part of the system's external communication infrastructure, responsible for maintaining the message paradigm even when crossing system boundaries.
