# `dev_codec_httpsig.erl` Analysis

## Overview

`dev_codec_httpsig.erl` implements HTTP Message Signatures as described in RFC-9421 as a Converge device in HyperBEAM. This module serves as both a codec (message format converter) and an attestation mechanism, providing cryptographic message verification capabilities that form a critical part of HyperBEAM's security infrastructure.

The module effectively bridges the gap between HTTP-based protocols and HyperBEAM's internal message format, enabling secure, verifiable communication across network boundaries. It implements the complex rules of HTTP Message Signatures, including component derivation, signature base construction, and cryptographic verification, while maintaining compatibility with HTTP standards.

As noted in its documentation, the module divides its functionality into two main areas:
1. **Attestation functions** (`id/3`, `attest/3`, `verify/3`, etc.) implemented directly in this module
2. **Codec functions** (`to/1`, `from/1`) which are relayed to the `dev_codec_httpsig_conv` module

## Key Characteristics

- **HTTP Standards Compliant**: Implements RFC-9421 (HTTP Message Signatures) with precise attention to specification details
- **Dual-Role Functionality**: Provides both message format conversion and cryptographic attestation
- **Component-Based Signing**: Enables fine-grained control over which message components are included in signatures
- **Multiple Signature Support**: Accommodates multiple attestors signing the same message
- **Format-Aware Processing**: Handles HTTP Structured Fields (RFC 8941) with careful parsing and serialization
- **Cryptographic Algorithms**: Supports RSA-PSS-SHA512 for signatures and HMAC-SHA256 for integrity
- **Derived Component Support**: Handles HTTP-specific derived components like `@method`, `@target-uri`, etc.

## Dependencies

### Upstream Dependencies

- `hb_structured_fields`: For parsing and serializing HTTP Structured Fields
- `hb_converge`: For message resolution and normalization
- `hb_message`: For message format conversion
- `hb_util`: For encoding/decoding and utility functions
- `hb_crypto`: For cryptographic operations
- `hb_opts`: For configuration access
- `ar_wallet`: For Arweave wallet integration and signing

## Implementation Details

### Message Signing Process

The `attest/3` function implements the core message signing process:

```erlang
attest(MsgToSign, _Req, Opts) ->
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    NormMsg = hb_converge:normalize_keys(MsgToSign),
    % ... (handling hashpath and preparing the message) ...
    EncWithoutBodyKeys = maps:without(
        [<<"signature">>, <<"signature-input">>, <<"body-keys">>, <<"priv">>],
        hb_message:convert(MsgWithoutHP, <<"httpsig@1.0">>, Opts)
    ),
    Enc = add_content_digest(EncWithoutBodyKeys),
    Authority = authority(lists:sort(maps:keys(Enc)), SigParams, Wallet),
    {ok, {SignatureInput, Signature}} = sign_auth(Authority, #{}, Enc),
    % ... (creating attestation and updating the message) ...
    reset_hmac(MsgWithoutHP#{<<"attestations">> =>
        OldAttestations#{ Attestor => Attestation }
    }).
```

This function:
1. Obtains the wallet for signing
2. Normalizes and converts the message to HTTP format
3. Adds a content digest for the message body
4. Creates an "authority" with component identifiers and signature parameters
5. Generates the signature base and signs it
6. Creates an attestation with the signature and signature input
7. Updates the message with the new attestation

### Signature Verification

The `verify/3` function handles signature verification:

```erlang
verify(MsgToVerify, #{ <<"attestor">> := <<"hmac-sha256">> }, _Opts) ->
    % ... (HMAC verification logic) ...
verify(MsgToVerify, Req, _Opts) ->
    % Validate a signed attestation.
    Attestor = maps:get(<<"attestor">>, Req),
    SigName = address_to_sig_name(Attestor),
    % ... (parsing signature parameters) ...
    case Alg of
        {string, <<"rsa-pss-sha512">>} ->
            {string, KeyID} = maps:get(<<"keyid">>, Params),
            PubKey = hb_util:decode(KeyID),
            % ... (preparing the message) ...
            Res = verify_auth(
                #{
                    key => {{rsa, 65537}, PubKey},
                    sig_name => address_to_sig_name(Address)
                },
                EncWithDigest
            ),
            {ok, Res};
        _ -> {error, {unsupported_alg, Alg}}
    end.
```

The function:
1. Distinguishes between HMAC and signature verification
2. For signatures, extracts the public key from the signature parameters
3. Prepares the message in the same format as was used for signing
4. Generates the signature base and verifies the signature

### Signature Base Construction

The construction of the signature base is a critical part of the implementation:

```erlang
signature_base(Authority, Req, Res) when is_map(Authority) ->
    ComponentIdentifiers = maps:get(component_identifiers, Authority),
    ComponentsLine = signature_components_line(ComponentIdentifiers, Req, Res),
    ParamsLine = signature_params_line(
        ComponentIdentifiers,
        maps:get(sig_params, Authority)),
    SignatureBase = join_signature_base(ComponentsLine, ParamsLine),
    {ParamsLine, SignatureBase}.
```

This follows the RFC-9421 specification for creating a signature base, which consists of:
1. Component lines for each field or derived component being signed
2. A signature parameters line with the list of components and parameters

### Component Extraction

The module handles extracting component values from HTTP messages:

```erlang
extract_field({item, {_Kind, IParsed}, IParams}, Req, Res) ->
    % ... (parameter parsing) ...
    Lowered = lower_bin(IParsed),
    NormalizedItem = hb_structured_fields:item(
        {item, {string, Lowered}, IParams}
    ),
    IsRequestIdentifier = find_request_param(IParams),
    case maps:get(Lowered, if IsRequestIdentifier -> Req; true -> Res end, not_found) of
        not_found -> {field_not_found_error, ...};
        FieldValue -> 
            % ... (field value extraction) ...
    end.
```

This function carefully:
1. Normalizes field names (case-insensitive)
2. Determines whether to extract from request or response
3. Handles error cases according to the specification
4. Applies special processing for structured fields and dictionary keys

### Derived Components

The module supports HTTP-specific derived components:

```erlang
derive_component(Identifier, Req, Res) ->
    % ... 
    case Lowered of
        <<"@method">> -> {ok, upper_bin(maps:get(<<"method">>, Req, <<>>))};
        <<"@target-uri">> -> {ok, bin(maps:get(<<"path">>, Req, <<>>))};
        <<"@authority">> -> ... 
        % ... (more derived components) ...
    end
```

These derived components follow RFC-9421's specifications for extracting standard HTTP message components like method, target URI, and status.

## Questions and Insights

### Questions

1. **Algorithm Support**: The module currently only supports RSA-PSS-SHA512 for signatures. Are there plans to extend support to other signature algorithms mentioned in RFC-9421?

2. **Performance Considerations**: How does the complexity of HTTP Message Signatures impact performance, especially for large messages with many signed components?

3. **Integration with HTTP Pipeline**: How is this module integrated into the HTTP request/response pipeline? Is signing automatic or conditional?

4. **Error Handling**: What happens if a message contains malformed signatures or incompatible signature parameters?

5. **Key Management**: How are the keys used for signing and verification managed and secured?

### Insights

1. **Standard Conformance with Pragmatism**: The module shows careful attention to RFC-9421 specifications while making practical engineering choices for integration with HyperBEAM.

2. **Defensive Implementation**: The code exhibits defensive programming practices, with careful type checking, normalization, and error handling throughout.

3. **Dual-Layer Signatures**: The module implements both HMAC (for integrity) and RSA signatures (for authentication), providing multiple security layers.

4. **Format Awareness**: The implementation shows deep understanding of HTTP's structural complexities, particularly with Structured Fields and component derivation.

5. **Security by Design**: The module demonstrates security-first thinking, with careful handling of signature inputs and verification steps.

## Integration with Other Subsystems

### Integration with Network Communication Subsystem

- Provides signature capabilities for HTTP messages sent and received through the network stack
- Handles verification of incoming signed messages
- Enables content integrity verification through content digests

### Integration with Core Infrastructure

- Uses the Converge protocol for message resolution
- Leverages message conversion capabilities for format translation
- Integrates with Arweave wallet for cryptographic operations

### Integration with Device and Process Management Subsystem

- Functions as a device within the device system
- Supports the attestation model used by process management

## Recategorization Considerations

This module is correctly categorized as part of the Codec and Data Format Subsystem, as its primary purpose is to handle the conversion and verification of HTTP Message Signatures format. While it has security aspects that might suggest categorization in a Security Subsystem, its focus on a specific protocol format (HTTP Message Signatures) and format conversion aligns it well with codec functionality.

The dual nature of the module—handling both format conversion and cryptographic operations—exemplifies how HyperBEAM integrates security directly into its data formats rather than treating security as a separate layer.
