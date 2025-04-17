# SNP System Analysis (`dev_snp.erl` and `dev_snp_nif.erl`)

## Overview

The SNP System in HyperBEAM provides critical hardware-based security capabilities through AMD's Secure Encrypted Virtualization-Secure Nested Paging (SEV-SNP) technology. With 1 downstream dependent, this subsystem enables cryptographic attestation of node integrity, creating a foundation for trust in distributed and confidential computing scenarios.

The implementation is divided into two complementary modules:

1. `dev_snp.erl`: The device interface module that handles the high-level logic for generating and verifying attestation reports, integrating with HyperBEAM's message system.

2. `dev_snp_nif.erl`: A Native Implemented Function (NIF) module that interfaces with native code (likely Rust-based) to perform the low-level cryptographic operations required for SNP attestation.

This system allows HyperBEAM nodes to generate cryptographic proof that they are running in a secure environment with trusted software components, and to verify similar proofs from other nodes. By leveraging hardware-based security features, it provides stronger security guarantees than purely software-based solutions.

## Key Characteristics

- **Hardware-Based Security**: Utilizes AMD SEV-SNP for hardware-level security attestation
- **Attestation Generation**: Creates attestation reports that prove the integrity of the node's environment
- **Attestation Verification**: Validates attestation reports from other nodes against security requirements
- **Software Component Validation**: Verifies firmware, kernel, and other components against trusted values
- **Non-Debug Enforcement**: Ensures nodes are running in non-debug mode for production security
- **Measurement Verification**: Validates launch measurements against expected values
- **Nonce-Based Authentication**: Uses address and node message ID to create unique nonces for attestation
- **Trust Management**: Provides mechanisms for defining and checking trusted software configurations
- **Native Code Integration**: Uses NIFs to interface with hardware-specific functionality
- **Signature Verification**: Validates report signatures using hardware root of trust

## Dependencies

### Library Dependencies
- `jiffy`: For JSON encoding and decoding of attestation reports
- Rust-based native library (loaded via `?load_nif_from_crate` macro)

### Upstream Dependencies
- `hb_opts`: For accessing node configuration
- `hb_converge`: For message resolution and field access
- `hb_http_server`: For managing node configuration
- `hb_message`: For message attestation, verification, and ID management
- `hb_private`: For handling private message fields
- `hb_util`: For utility functions including ID handling and encoding
- `dev_message`: For message ID extraction
- `ar_wallet`: For wallet address handling

## Implementation Details: High-Level Interface (`dev_snp.erl`)

### Initialization

The module initializes with trusted software hashes:

```erlang
init(M1, _M2, Opts) ->
    case {hb_opts:get(trusted, #{}, Opts), hb_opts:get(operator, undefined, Opts)} of
        {#{snp_hashes := _}, _} ->
            {error, <<"Already initialized.">>};
        {_, Addr} when is_binary(Addr) ->
            {error, <<"Cannot enable SNP if operator is already set.">>};
        _ ->
            SnpHashes = hb_converge:get(<<"body">>, M1, Opts),
            SNPDecoded = jiffy:decode(SnpHashes, [return_maps]),
            Hashes = maps:get(<<"snp_hashes">>, SNPDecoded),
            ok = hb_http_server:set_opts(Opts#{
                trusted => maps:merge(hb_opts:get(trusted, #{}, Opts), Hashes),
                snp_hashes => Hashes
            }),
            {ok, <<"SNP node initialized successfully.">>}
    end.
```

This initialization:
1. Checks that SNP is not already initialized
2. Ensures the node does not already have an operator set
3. Extracts trusted hashes from the message
4. Stores the hashes in the node's configuration

### Attestation Report Generation

The module generates attestation reports:

```erlang
generate(_M1, _M2, Opts) ->
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    {ok, PublicNodeMsgID} =
        dev_message:id(
            NodeMsg = hb_private:reset(Opts),
            #{ <<"attestors">> => <<"none">> },
            Opts
        ),
    RawPublicNodeMsgID = hb_util:native_id(PublicNodeMsgID),
    ReportData = generate_nonce(Address, RawPublicNodeMsgID),
    {ok, ReportJSON} = dev_snp_nif:generate_attestation_report(ReportData, 1),
    LocalHashes = hb_opts:get(snp_hashes, {error, not_configured}, Opts),
    ReportMsg = hb_message:attest(LocalHashes#{
        <<"nonce">> => hb_util:encode(ReportData),
        <<"address">> => Address,
        <<"node-message">> => NodeMsg,
        <<"report">> => ReportJSON
    }, Wallet),
    {ok, ReportMsg}.
```

This process:
1. Gets the node's wallet and address
2. Creates a public version of the node message (without private fields)
3. Generates a nonce using the address and node message ID
4. Calls the NIF to generate an attestation report with this nonce
5. Creates a complete attestation message with the report and supporting data
6. Signs the message with the node's wallet

### Attestation Report Verification

The module verifies attestation reports through a multi-step process:

```erlang
verify(M1, M2, NodeOpts) ->
    {ok, MsgWithJSONReport} = hb_message:find_target(M1, M2, NodeOpts),
    ReportJSON = hb_converge:get(<<"report">>, MsgWithJSONReport, NodeOpts),
    Report = jiffy:decode(ReportJSON, [return_maps]),
    Msg = maps:merge(
        maps:without([<<"report">>], MsgWithJSONReport),
        Report
    ),
    % Step 1: Verify the nonce.
    Address = hb_converge:get(<<"address">>, Msg, NodeOpts),
    NodeMsgID = extract_node_message_id(Msg, NodeOpts),
    Nonce = hb_util:decode(hb_converge:get(<<"nonce">>, Msg, NodeOpts)),
    NonceMatches = report_data_matches(Address, NodeMsgID, Nonce),
    % Step 2: Verify the address and the signature.
    Signers = hb_message:signers(MsgWithJSONReport),
    SigIsValid = hb_message:verify(MsgWithJSONReport, Signers),
    AddressIsValid = lists:member(Address, Signers),
    % Step 3: Verify that the debug flag is disabled.
    DebugDisabled = not is_debug(Msg),
    % Step 4: Verify measurement data (firmware, kernel, OS image) is trusted.
    IsTrustedSoftware = execute_is_trusted(M1, Msg, NodeOpts),
    % Step 5: Verify the measurement against the report's measurement.
    Args = extract_measurement_args(Msg),
    {ok, Expected} = dev_snp_nif:compute_launch_digest(Args),
    Measurement = hb_converge:get(<<"measurement">>, Msg, NodeOpts),
    {ok, MeasurementIsValid} = dev_snp_nif:verify_measurement(ReportJSON, list_to_binary(Expected)),
    % Step 6: Check the report's integrity.
    {ok, ReportIsValid} = dev_snp_nif:verify_signature(ReportJSON),
    Valid = all_checks_pass([
        NonceMatches, SigIsValid, AddressIsValid, DebugDisabled,
        IsTrustedSoftware, MeasurementIsValid, ReportIsValid
    ]),
    {ok, Valid}.
```

The verification process performs multiple checks:
1. Verifies the nonce matches the expected value
2. Validates message signatures and signing address
3. Ensures the debug flag is disabled (production mode)
4. Verifies all software components are trusted
5. Validates the measurement against expected launch digest
6. Verifies the report's signature against hardware root of trust

### Trust Verification

The module implements a trust verification mechanism:

```erlang
trusted(_Msg1, Msg2, NodeOpts) ->
    Key = hb_converge:get(<<"key">>, Msg2, NodeOpts),
    Body = hb_converge:get(<<"body">>, Msg2, not_found, NodeOpts),
    TrustedSoftware = hb_opts:get(trusted, #{}, NodeOpts),
    PropertyName = hb_converge:get(Key, TrustedSoftware, not_found, NodeOpts),
    {ok, PropertyName == Body}.
```

This allows verifying individual software components against a list of trusted values.

## Implementation Details: Native Interface (`dev_snp_nif.erl`)

### NIF Interface

The module defines the interface to native functions:

```erlang
-export([generate_attestation_report/2, compute_launch_digest/1, check_snp_support/0]).
-export([verify_measurement/2, verify_signature/1]).

check_snp_support() -> ?NOT_LOADED.
generate_attestation_report(_UniqueData, _VMPL) -> ?NOT_LOADED.
compute_launch_digest(_Args) -> ?NOT_LOADED.
verify_measurement(_Report, _Expected) -> ?NOT_LOADED.
verify_signature(_Report) -> ?NOT_LOADED.

init() ->
    ?load_nif_from_crate(dev_snp_nif, 0).
```

These functions provide:
1. Checking if SNP is supported on the current hardware
2. Generating attestation reports with unique data
3. Computing expected launch digests from configuration
4. Verifying measurements against expected values
5. Verifying report signatures

### Native Implementation

While the actual native code isn't included here (it's likely in a Rust crate), the interface suggests it provides:

1. **Hardware Access**: Direct access to AMD SEV-SNP hardware features
2. **Cryptographic Operations**: Signature generation and verification
3. **Measurement Computation**: Launch digest calculation
4. **Report Validation**: Attestation report integrity checks

The tests in `dev_snp_nif.erl` provide insights into the expected behavior:

```erlang
compute_launch_digest_test() ->
    ArgsMap = #{ 
        vcpus => 1,
        vcpu_type => 5, 
        vmm_type => 1,
        guest_features => 16#1,
        firmware => "b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510",
        kernel => "69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576",
        initrd => "02e28b6c718bf0a5260d6f34d3c8fe0d71bf5f02af13e1bc695c6bc162120da1",
        append => "56e1e5190622c8c6b9daa4fe3ad83f3831c305bb736735bf795b284cb462c9e7"
    },
    {ok, Result} = dev_snp_nif:compute_launch_digest(ArgsMap),
    EncTestVector = <<"Lhgbg_pneEf5Ebaj1ru3lIFu7RXHY4jBVnjSd-Yk7D0jIryZ3aLdks4YOWfjajKW">>,
    ?assertMatch(EncTestVector, hb_util:encode(Result)).
```

This shows that the native code implements deterministic launch digest computation from a set of VM and software parameters.

## Integration with HyperBEAM

### Integration with Message System

The SNP system integrates with HyperBEAM's message system:

1. **Message Attestation**: Creates signed attestation messages using `hb_message:attest`
2. **Message Verification**: Verifies signatures and attestors using `hb_message:verify`
3. **Message ID**: Uses `dev_message:id` to extract and verify node message IDs
4. **Field Access**: Uses `hb_converge` for resolving fields in messages

### Integration with Configuration System

The system integrates with HyperBEAM's configuration system:

1. **Trusted Hashes**: Stores trusted software hashes in node configuration
2. **Option Access**: Uses `hb_opts:get` to access configuration values
3. **Configuration Updates**: Uses `hb_http_server:set_opts` to update configuration
4. **Trust Management**: Uses configuration to store and retrieve trusted software details

### Integration with NIF System

The integration between Erlang and native code is managed through:

1. **NIF Loading**: Uses `?load_nif_from_crate` to load the native library
2. **Function Mapping**: Maps Erlang functions to native implementations
3. **Data Conversion**: Handles data conversion between Erlang and native code
4. **Error Handling**: Provides error handling for NIF failures

## Testing Approach

The testing approach includes:

1. **Conditional Testing**: Conditionally tests based on hardware support:
   ```erlang
   real_node_test() ->
       if ?TEST_NODE == undefined ->
           {skip, <<"Test node not set.">>};
       true ->
           % Test against a real node
       end.
   ```

2. **Launch Digest Verification**: Tests the deterministic computation of launch digests:
   ```erlang
   compute_launch_digest_test() ->
       ArgsMap = #{ /* parameters */ },
       {ok, Result} = dev_snp_nif:compute_launch_digest(ArgsMap),
       ?assertMatch(ExpectedResult, hb_util:encode(Result)).
   ```

3. **Measurement Verification**: Tests the verification of measurements against expected values:
   ```erlang
   verify_measurement_test() ->
       {ok, MockReport} = file:read_file("test/snp-measurement.json"),
       ExpectedMeasurement = <</* binary data */>>
       Result = dev_snp_nif:verify_measurement(MockReport, ExpectedMeasurement),
       ?assertMatch({ok, true}, Result).
   ```

4. **Signature Verification**: Tests the verification of attestation report signatures:
   ```erlang
   verify_signature_test() ->
       {ok, MockAttestation} = file:read_file("test/snp-attestation.json"),
       Result = dev_snp_nif:verify_signature(MockAttestation),
       ?assertMatch({ok, true}, Result).
   ```

## Observations and Insights

### Strengths

1. **Hardware Root of Trust**: Leverages hardware-based security for stronger trust guarantees than purely software solutions.

2. **Comprehensive Verification**: Implements multiple verification steps to ensure the integrity and authenticity of attestation reports.

3. **Configurable Trust**: Allows flexible configuration of trusted software components through the `trusted` mechanism.

4. **Native Integration**: Uses NIFs for efficient integration with hardware-specific functionality.

5. **Nonce-Based Security**: Uses a combination of address and node message ID to create unique nonces for attestation, preventing replay attacks.

### Design Patterns

1. **Modular Architecture**: Separates high-level logic (`dev_snp.erl`) from low-level cryptographic operations (`dev_snp_nif.erl`).

2. **Multi-Step Verification**: Implements a series of verification steps that must all pass for an attestation to be considered valid.

3. **Trust Configuration**: Uses a configurable list of trusted software components that can be verified individually.

4. **Native Interface Pattern**: Uses the NIF pattern to interface with native code for hardware access and cryptographic operations.

5. **Message-Based Communication**: Leverages HyperBEAM's message system for attestation report transmission and verification.

### Challenges and Limitations

1. **Hardware Dependency**: Requires AMD SEV-SNP hardware support, limiting compatibility to specific platforms.

2. **Complexity**: The multi-step verification process introduces complexity that could be difficult to maintain and debug.

3. **Security Parameter Management**: Managing trusted software hashes securely could be challenging in large deployments.

4. **NIF Error Handling**: Error handling for NIF failures appears minimal, potentially leading to unexpected behavior if native code fails.

5. **Test Coverage**: Testing is challenging due to hardware dependencies, potentially leading to incomplete coverage.

### Future Opportunities

1. **Enhanced Trust Management**: Developing more sophisticated mechanisms for managing trusted software configurations.

2. **Extended Hardware Support**: Expanding support to other hardware-based security technologies beyond AMD SEV-SNP.

3. **Remote Attestation Infrastructure**: Building a more comprehensive remote attestation infrastructure around the core SNP functionality.

4. **Attestation Policies**: Implementing more granular attestation policies beyond the current all-or-nothing approach.

5. **Performance Optimization**: Optimizing the verification process for better performance in high-throughput scenarios.

## Architectural Significance

The SNP system is architecturally significant for several reasons:

1. **Security Foundation**: It provides a hardware-based security foundation for the entire system, enabling stronger trust guarantees.

2. **Confidential Computing**: It enables confidential computing scenarios where sensitive data can be processed in trusted environments.

3. **Trust Establishment**: It solves the problem of establishing trust between distributed nodes in an untrusted network.

4. **Hardware Integration**: It demonstrates how HyperBEAM integrates with hardware-specific security features.

5. **Node Validation**: It enables validation of node integrity before allowing sensitive operations or data access.

## Conclusion

The SNP System (`dev_snp.erl` and `dev_snp_nif.erl`) represents an advanced security component within HyperBEAM, leveraging AMD's SEV-SNP technology to provide hardware-based attestation capabilities. By enabling nodes to cryptographically prove their integrity and verify the integrity of other nodes, it creates a foundation for trust in distributed and confidential computing scenarios.

The implementation demonstrates a thoughtful approach to integrating hardware security features with HyperBEAM's message-based architecture. The separation between high-level logic and native cryptographic operations provides a clean design while enabling efficient access to hardware capabilities. The comprehensive verification process, covering everything from nonce validation to software component verification, ensures robust security guarantees.

While there are challenges related to hardware dependencies and complexity, the system provides significant value for security-sensitive applications. As confidential computing continues to grow in importance, components like the SNP system will likely become increasingly central to secure distributed systems.
