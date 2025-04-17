# `dev_codec_httpsig_conv.erl` Analysis

## Overview

`dev_codec_httpsig_conv.erl` implements a specialized codec that marshals messages between HyperBEAM's internal Type-Annotated-Binary-Message (TABM) format and HTTP message structures. This module handles the complex task of transforming between HyperBEAM's rich data model and the constraints of HTTP's wire format, with particular attention to preserving type information, handling nested structures, and maintaining cryptographic attestations.

The module serves as the implementation companion to `dev_codec_httpsig.erl`, which provides the primary interface. While `dev_codec_httpsig.erl` focuses on HTTP Message Signatures and attestation operations, this module performs the actual conversion logic, managing the transformation of data between formats in a way that preserves hierarchical relationships and security properties.

A key feature of this module is its use of HTTP multipart messages as defined in RFC-7578, which enables it to handle complex nested message structures and large message values that exceed HTTP header size limitations.

## Key Characteristics

- **HTTP Multipart Support**: Uses the multipart/form-data format to encode complex message structures
- **Adaptive Field Placement**: Intelligently places message fields in either HTTP headers or body parts based on size constraints
- **Nested Structure Preservation**: Maintains hierarchical relationships between message components
- **Signature Integration**: Seamlessly extracts and embeds HTTP Message Signatures during conversion
- **Protocol Compliance**: Follows HTTP standards for multipart encoding and header formats
- **Content-Disposition Handling**: Uses Content-Disposition headers to identify field types and relationships
- **Size-Aware Processing**: Adjusts encoding strategies based on field sizes and HTTP limitations
- **Body Part Management**: Special handling for the main message body with inline fields
- **Hashpath Preservation**: Maintains cryptographic hashpaths across conversion boundaries

## Dependencies

### Upstream Dependencies

- `dev_codec_httpsig`: For attestation handling and signature operations
- `dev_codec_flat`: For message flattening and restoration
- `hb_structured_fields`: For parsing and formatting HTTP Structured Fields
- `hb_converge`: For key normalization and message resolution
- `hb_util`: For cryptographic operations and utility functions
- `ar_wallet`: For cryptographic address calculation

## Implementation Details

### Message Conversion: TABM to HTTP (to/1)

The `to/1` function converts a TABM message into an HTTP message structure:

```erlang
to(TABM) -> to(TABM, []).
to(TABM, Opts) when is_map(TABM) ->
    Stripped = maps:without(
        [<<"attestations">>, <<"signature">>, <<"signature-input">>, <<"priv">>],
        TABM
    ),
    {InlineFieldHdrs, InlineKey} = inline_key(TABM),
    Intermediate = do_to(Stripped, Opts ++ [{inline, InlineFieldHdrs, InlineKey}]),
    % Add signatures if present
    case maps:get(<<"attestations">>, TABM, not_found) of
        #{ <<"hmac-sha256">> := #{ <<"signature">> := Sig, <<"signature-input">> := SigInput } } ->
            HPs = hashpaths_from_message(TABM),
            EncWithHPs = maps:merge(Intermediate, HPs),
            Res = EncWithHPs#{
                <<"signature">> => Sig,
                <<"signature-input">> => SigInput
            },
            Res;
        _ -> Intermediate
    end.
```

The implementation:
1. Strips attestation and private fields from the message
2. Determines the inline body key for the message
3. Processes the message body and transforms it to HTTP format
4. Adds signatures and hashpaths if present

### Message Conversion: HTTP to TABM (from/1)

The `from/1` function converts an HTTP message into a TABM:

```erlang
from(HTTP) ->
    Body = maps:get(<<"body">>, HTTP, <<>>),
    {_, InlinedKey} = inline_key(HTTP),
    Headers = maps:without([<<"body">>, <<"body-keys">>], HTTP),
    ContentType = maps:get(<<"content-type">>, Headers, undefined),
    MsgWithoutSigs = maps:without(
        [<<"signature">>, <<"signature-input">>, <<"attestations">>],
        from_body(Headers, InlinedKey, ContentType, Body)
    ),
    HPs = extract_hashpaths(HTTP),
    {ok, MsgWithSigs} = attestations_from_signature(
        maps:without(maps:keys(HPs), MsgWithoutSigs),
        HPs,
        maps:get(<<"signature">>, Headers, not_found),
        maps:get(<<"signature-input">>, Headers, not_found)
    ),
    maps:without(maps:keys(HPs) ++ [<<"content-digest">>], MsgWithSigs)
```

This function:
1. Extracts the body and content type from the HTTP message
2. Determines the inline body key for the message
3. Processes the body content, potentially parsing multipart structures
4. Extracts hashpaths from the message
5. Builds attestations from signatures and signature inputs
6. Removes temporary fields from the final message

### Multipart Body Handling

A significant part of the implementation deals with HTTP multipart message handling:

```erlang
from_body(TABM, _InlinedKey, _ContentType, <<>>) -> TABM;
from_body(TABM, InlinedKey, ContentType, Body) ->
    Params = case ContentType of
        undefined -> [];
        _ ->
            {item, {_, _XT}, XParams} = hb_structured_fields:parse_item(ContentType),
            XParams
    end,
    case lists:keyfind(<<"boundary">>, 1, Params) of
        false ->
            % Not multipart, just add body to the inlined key
            maps:put(InlinedKey, Body, TABM);
        {_, {_Type, Boundary}} ->
            % Parse the multipart body
            BegPat = <<"--", Boundary/binary, ?CRLF/binary>>,
            EndPat = <<?CRLF/binary, "--", Boundary/binary, "--">>,
            {Start, SL} = binary:match(Body, BegPat),
            {End, _} = binary:match(Body, EndPat),
            BodyPart = binary:part(Body, Start + SL, End - (Start + SL)),
            Parts = binary:split(BodyPart, [<<?CRLF/binary, "--", Boundary/binary>>], [global]),
            {ok, GroupedTABM} = from_body_parts(TABM, InlinedKey, Parts),
            dev_codec_flat:from(GroupedTABM)
    end.
```

This code:
1. Handles empty bodies by returning the unchanged TABM
2. For non-multipart bodies, adds the body to the inlined key
3. For multipart bodies, extracts the boundary and parts
4. Parses each part individually and reconstructs the message structure

### Map Grouping Strategy

The module uses a smart grouping strategy to maintain hierarchical relationships:

```erlang
group_maps(Map, Parent, Top) when is_map(Map) ->
    {Flattened, NewTop} = maps:fold(
        fun(Key, Value, {CurMap, CurTop}) ->
            NormKey = hb_converge:normalize_key(Key),
            FlatK = case Parent of
                <<>> -> NormKey;
                _ -> <<Parent/binary, "/", NormKey/binary>>
            end,
            case Value of
                _ when is_map(Value) ->
                    NewTop = group_maps(Value, FlatK, CurTop),
                    {CurMap, NewTop};
                _ ->
                    case byte_size(Value) > ?MAX_HEADER_LENGTH of
                        true ->
                            NewTop = maps:put(FlatK, Value, CurTop),
                            {CurMap, CurTop};
                        false ->
                            NewCurMap = maps:put(NormKey, Value, CurMap),
                            {NewCurMap, CurTop}
                    end
            end
        end,
        {#{}, Top},
        Map
    ),
    % Combine results based on context
    case maps:size(Flattened) of
        0 -> NewTop;
        _ -> case Parent of
            <<>> -> maps:merge(NewTop, Flattened);
            _ -> NewTop#{ Parent => Flattened }
        end
    end.
```

This function:
1. Traverses the map structure recursively
2. Builds path-based keys for nested structures
3. Decides whether to place values in headers or the body based on size
4. Preserves the hierarchical relationship through path encoding

### Signature and Attestation Handling

The module carefully extracts and embeds signature information:

```erlang
attestations_from_signature(Map, HPs, RawSig, RawSigInput) ->
    SfSigsKV = hb_structured_fields:parse_dictionary(RawSig),
    SfInputs = maps:from_list(hb_structured_fields:parse_dictionary(RawSigInput)),
    Attestations = maps:from_list(lists:map(
        fun ({SigName, Signature}) ->
            {list, SigInputs, ParamsKVList} = maps:get(SigName, SfInputs, #{}),
            % Find hashpaths from signature
            Hashpath = lists:filtermap(
                fun ({item, BareItem, _}) ->
                    case hb_structured_fields:from_bare_item(BareItem) of
                        HP = <<"hash", _/binary>> -> {true, HP};
                        _ -> false
                    end;
                (_) -> false
                end,
                SigInputs
            ),
            Hashpaths = maps:from_list(lists:map(
                fun (HP) -> {HP, maps:get(HP, HPs, <<>>)} end,
                Hashpath
            )),
            Params = maps:from_list(ParamsKVList),
            {string, EncPubKey} = maps:get(<<"keyid">>, Params),
            PubKey = hb_util:decode(EncPubKey),
            Address = hb_util:human_id(ar_wallet:to_address(PubKey)),
            SerializedSig = iolist_to_binary(
                hb_structured_fields:dictionary(#{ SigName => Signature })
            ),
            {item, {binary, UnencodedSig}, _} = Signature,
            {Address, Hashpaths#{
                <<"signature">> => SerializedSig,
                <<"signature-input">> => iolist_to_binary(
                    hb_structured_fields:dictionary(
                        #{ SigName => maps:get(SigName, SfInputs) }
                    )
                ),
                <<"id">> => hb_util:human_id(crypto:hash(sha256, UnencodedSig)),
                <<"attestation-device">> => <<"httpsig@1.0">>
            }}
        end,
        SfSigsKV
    )),
    Msg = Map#{ <<"attestations">> => Attestations },
    % Reset HMAC if necessary
    case maps:get(<<"hmac-sha256">>, Attestations, not_found) of
        not_found -> dev_codec_httpsig:reset_hmac(Msg);
        _ -> Msg
    end.
```

This function:
1. Parses the signature and signature input fields
2. Extracts relevant hashpaths from the message
3. Identifies the signer's address from the public key
4. Builds structured attestations with necessary metadata
5. Ensures the message HMAC is properly maintained

## Questions and Insights

### Questions

1. **Performance Considerations**: How does the multipart encoding and body part extraction impact performance, especially for large messages with complex hierarchies?

2. **Error Resilience**: How does the system handle malformed HTTP messages, especially when signatures or multipart boundaries are corrupted?

3. **Content Type Handling**: The module seems to focus primarily on multipart/form-data. How well does it handle other Content-Types, especially binary formats?

4. **Size Constraints**: The module enforces a 4KB limit for header values. Are there operational scenarios where this limit is problematic?

5. **Integration with Edge Proxies**: How does this encoding scheme interact with CDNs, proxies, and other middleboxes that might modify HTTP headers?

### Insights

1. **Structure-Preserving Design**: The module shows careful attention to preserving hierarchical structure across format boundaries, essential for maintaining message semantics.

2. **Cryptographic Integration**: Signature information is tightly integrated into the conversion process, ensuring security properties survive transformation.

3. **Protocol-Aware Processing**: The implementation shows deep understanding of HTTP protocol details, particularly multipart handling and Content-Disposition semantics.

4. **Adaptive Encoding Strategy**: The size-aware field placement demonstrates pragmatic engineering, balancing standard compliance with practical constraints.

5. **Clean Separation of Concerns**: The module focuses exclusively on format conversion, delegating cryptographic operations to `dev_codec_httpsig.erl`, reflecting good design principles.

## Integration with Other Subsystems

### Integration with Network Communication Subsystem

- Provides the format bridge between HyperBEAM's internal message format and HTTP wire format
- Enables HTTP servers and clients to work with HyperBEAM's message model
- Supports proper handling of HTTP headers, multipart bodies, and content types

### Integration with Codec and Data Format Subsystem

- Works closely with `dev_codec_httpsig.erl` to provide a complete HTTP message handling solution
- Leverages `hb_structured_fields.erl` for structured field parsing and generation
- Uses `dev_codec_flat.erl` for handling nested message structures

### Integration with Core Infrastructure

- Supports the attestation model through signature extraction and embedding
- Maintains cryptographic properties across format boundaries
- Preserves message relationships through hierarchical path encoding

## Recategorization Considerations

This module is correctly categorized as part of the Codec and Data Format Subsystem. Its primary responsibility is format conversion between HyperBEAM's internal TABM format and HTTP message structures, which aligns precisely with the subsystem's focus on data representation and conversion.

While it has significant interactions with networking aspects, these are focused on the data format aspects of HTTP rather than network protocols or connections. Similarly, its security-related functions are specifically about preserving security information during format conversion rather than implementing security mechanisms.

The module works in tandem with other codec modules like `dev_codec_httpsig.erl` and `dev_codec_flat.erl` to provide a comprehensive solution for format interoperability, firmly placing it within the Codec and Data Format Subsystem.
