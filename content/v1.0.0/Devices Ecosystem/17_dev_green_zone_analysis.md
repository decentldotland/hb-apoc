# Green Zone System Analysis (`dev_green_zone.erl`)

## Overview

The Green Zone system provides a secure communication and identity management framework for trusted HyperBEAM nodes. With 0 downstream dependents, this module enables nodes to establish secure enclaves with hardware-attested trust, cryptographically secured communication, and identity management capabilities.

At its core, the Green Zone system implements a secure node collaboration mechanism that allows nodes to form trusted networks where configuration, cryptographic material, and identity can be safely shared. It uses a combination of AMD SEV-SNP hardware attestation, RSA-based key exchange, and AES-256-GCM symmetric encryption to create a robust security foundation for node interactions.

The system allows nodes to initialize their own green zones, join existing zones (subject to hardware attestation), securely exchange cryptographic keys, and even temporarily adopt the identity of other nodes in the network. These capabilities enable advanced distributed deployment scenarios with strong security guarantees based on hardware root of trust.

## Key Characteristics

- **Hardware-Based Security**: Leverages AMD SEV-SNP for hardware-level security attestation
- **Secure Identity Management**: Enables cryptographic identity management with public-key infrastructure
- **Network Trust Establishment**: Creates networks of trusted nodes with verified configuration
- **Configuration Enforcement**: Ensures compliant configuration across all participating nodes
- **Secure Key Exchange**: Implements secure exchange of cryptographic material using RSA encryption
- **Symmetric Encryption**: Uses AES-256-GCM for efficient secure communication
- **Identity Cloning**: Supports temporary identity adoption between trusted nodes
- **Attestation Verification**: Requires hardware attestation for joining green zones
- **Configuration Compliance**: Enforces required configuration settings across the zone
- **Trusted Node Registry**: Maintains a registry of trusted nodes with their attestation data

## Dependencies

### Library Dependencies
- `jiffy`: For JSON encoding and decoding
- `crypto`: For cryptographic operations (AES-GCM, random number generation)
- `public_key`: For RSA operations
- `base64`: For encoding and decoding binary data

### Upstream Dependencies
- `hb_opts`: For accessing node configuration
- `hb_converge`: For message resolution and field access
- `hb_http_server`: For managing node configuration
- `hb_message`: For message attestation, verification, and signature handling
- `hb_http`: For making HTTP requests to other nodes
- `hb_util`: For utility functions
- `dev_snp`: For hardware attestation generation and verification
- `dev_meta`: For node message management
- `ar_wallet`: For wallet address handling

## Implementation Details

### Green Zone Initialization

The module initializes a green zone, establishing the cryptographic foundation for secure communication:

```erlang
init(_M1, M2, Opts) ->
    RequiredConfig = hb_converge:get(
        <<"required-config">>, M2, default_zone_required_opts(Opts), Opts
    ),
    % Check if a wallet exists; create one if absent.
    NodeWallet = case hb_opts:get(priv_wallet, undefined, Opts) of
        undefined -> hb:wallet();
        ExistingWallet -> ExistingWallet
    end,
    % Generate a new 256-bit AES key if we have not already joined a green zone.
    GreenZoneAES = case hb_opts:get(priv_green_zone_aes, undefined, Opts) of
        undefined -> crypto:strong_rand_bytes(32);
        ExistingAES -> ExistingAES
    end,
    % Store the wallet, AES key, and an empty trusted nodes map.
    ok = hb_http_server:set_opts(Opts#{
        priv_wallet => NodeWallet,
        priv_green_zone_aes => GreenZoneAES,
        trusted_nodes => #{},
        green_zone_required_opts => RequiredConfig
    }),
    {ok, <<"Green zone initialized successfully.">>}.
```

This process:
1. Sets up the required configuration for the green zone
2. Ensures a wallet (RSA keypair) exists, creating one if necessary
3. Generates a new AES-256 key for symmetric encryption within the zone
4. Initializes an empty list of trusted nodes
5. Stores these elements in the node's configuration

### Joining a Green Zone

The module implements a sophisticated join process that allows nodes to securely enter existing green zones:

```erlang
join(M1, M2, Opts) ->
    PeerLocation = hb_converge:get(<<"peer-location">>, M1, undefined, Opts),
    PeerID = hb_converge:get(<<"peer-id">>, M1, undefined, Opts),
    if (PeerLocation =:= undefined) or (PeerID =:= undefined) ->
        validate_join(M1, M2, Opts);
    true ->
        join_peer(PeerLocation, PeerID, M1, M2, Opts)
    end.
```

The join process has two flows:
1. **Requesting to join** (`join_peer`): When a node wants to join an existing green zone, it:
   - Generates a hardware attestation report
   - Sends the report and its public key to the target node
   - Receives an encrypted AES key
   - Decrypts the AES key using its private key
   - Updates its configuration with the shared AES key

2. **Validating a join request** (`validate_join`): When a node receives a join request, it:
   - Verifies the hardware attestation report
   - Validates the configuration compliance
   - Adds the new node to its trusted nodes list
   - Encrypts its AES key with the requester's public key
   - Returns the encrypted key to the requester

### Identity Management

The module supports secure identity retrieval and cloning operations:

```erlang
key(_M1, _M2, Opts) ->
    GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
    {{KeyType, Priv, Pub}, _PubKey} = hb_opts:get(priv_wallet, undefined, Opts),
    case GreenZoneAES of
        undefined -> {error, <<"Node not part of a green zone.">>};
        _ ->
            IV = crypto:strong_rand_bytes(16),
            {EncryptedKey, Tag} = crypto:crypto_one_time_aead(
                aes_256_gcm, GreenZoneAES, IV,
                term_to_binary({KeyType, Priv, Pub}), <<>>, true
            ),
            {ok, #{
                <<"status">> => 200,
                <<"encrypted_key">> => base64:encode(<<EncryptedKey/binary, Tag/binary>>),
                <<"iv">> => base64:encode(IV)
            }}
    end.

become(_M1, M2, Opts) ->
    NodeLocation = hb_converge:get(<<"peer-location">>, M2, Opts),
    NodeID = hb_converge:get(<<"peer-id">>, M2, Opts),
    GreenZoneAES = hb_opts:get(priv_green_zone_aes, undefined, Opts),
    case GreenZoneAES of
        undefined -> {error, <<"Node not part of a green zone.">>};
        _ ->
            {ok, KeyResp} = hb_http:get(NodeLocation, <<"/~greenzone@1.0/key">>, Opts),
            Signers = hb_message:signers(KeyResp),
            case hb_message:verify(KeyResp, Signers) and lists:member(NodeID, Signers) of
                false -> {error, <<"Received incorrect response from peer!">>};
                true -> finalize_become(KeyResp, NodeLocation, NodeID, GreenZoneAES, Opts)
            end
    end.
```

These functions enable:
1. **Key Retrieval** (`key`): Encrypts a node's private key with the shared AES key for secure transmission
2. **Identity Adoption** (`become`): Allows a node to temporarily adopt another node's identity by:
   - Retrieving the target node's encrypted private key
   - Decrypting it using the shared AES key
   - Updating its local wallet with the target's keypair

### Configuration Enforcement

The module implements robust configuration enforcement for nodes joining a green zone:

```erlang
validate_peer_opts(Req, Opts) ->
    RequiredConfig = hb_converge:normalize_keys(
        hb_opts:get(green_zone_required_opts, #{}, Opts)
    ),
    PeerOpts = hb_converge:normalize_keys(
        hb_converge:get(<<"node-message">>, Req, undefined, Opts)
    ),
    FullRequiredOpts = RequiredConfig#{
        green_zone_required_opts => RequiredConfig
    },
    NodeHistory = hb_converge:get(<<"node_history">>, PeerOpts, [], Opts),
    HistoryCheck = case is_list(NodeHistory) of
        true -> length(NodeHistory) =< 1;
        false -> {error, not_a_list}
    end,
    MatchCheck = hb_message:match(PeerOpts, FullRequiredOpts, only_present),
    MatchCheck andalso (HistoryCheck =:= true).
```

This enforcement:
1. Extracts the required configuration from the local node
2. Retrieves the joining node's configuration
3. Verifies that the joining node's configuration matches the required settings
4. Ensures the joining node has minimal configuration history
5. Rejects nodes that do not comply with the green zone's requirements

## Integration with HyperBEAM

### Integration with Security Infrastructure

The Green Zone system integrates with HyperBEAM's security infrastructure:

1. **Hardware Attestation**: Uses `dev_snp` for hardware-based security attestation
2. **Message Signing**: Leverages `hb_message` for attestation and signature verification
3. **Cryptographic Integration**: Works with `ar_wallet` for RSA key management
4. **Configuration Security**: Uses `hb_http_server` for secure configuration updates

### Integration with Network Infrastructure

The system integrates with HyperBEAM's network infrastructure:

1. **HTTP Communication**: Uses `hb_http` for secure communication between nodes
2. **Node Discovery**: Supports node location and addressing for green zone formation
3. **Request/Response Patterns**: Follows HyperBEAM's standard request/response patterns
4. **Message Verification**: Verifies message signatures to prevent MITM attacks

### Integration with Configuration System

The system deeply integrates with HyperBEAM's configuration system:

1. **Required Configuration**: Enforces specific configuration across green zone nodes
2. **Configuration Adoption**: Supports adopting configuration from joining nodes
3. **Configuration Security**: Prevents unauthorized configuration changes
4. **Node Message Integration**: Works with `dev_meta` to manage node messages

## Testing Approach

The module includes testing for critical cryptographic operations:

```erlang
rsa_wallet_integration_test() ->
    % Create a new wallet using ar_wallet
    Wallet = ar_wallet:new(),
    {{KeyType, Priv, Pub}, {KeyType, Pub}} = Wallet,
    % Create test message
    PlainText = <<"HyperBEAM integration test message.">>,
    % Create RSA public key record for encryption
    RsaPubKey = #'RSAPublicKey'{
        publicExponent = 65537,
        modulus = crypto:bytes_to_integer(Pub)
    },
    % Encrypt using public key
    Encrypted = public_key:encrypt_public(PlainText, RsaPubKey),
    % Create RSA private key record for decryption
    RSAPrivKey = #'RSAPrivateKey'{
        publicExponent = 65537,
        modulus = crypto:bytes_to_integer(Pub),
        privateExponent = crypto:bytes_to_integer(Priv)
    },
    % Verify decryption works
    Decrypted = public_key:decrypt_private(Encrypted, RSAPrivKey),
    % Verify roundtrip
    ?assertEqual(PlainText, Decrypted),
    % Verify wallet structure
    ?assertEqual(KeyType, {rsa, 65537}).
```

This test:
1. Verifies RSA encryption and decryption functionality
2. Ensures compatibility with HyperBEAM's wallet structure
3. Confirms cryptographic operations work correctly

## Observations and Insights

### Strengths

1. **Hardware-Based Security**: Leverages hardware attestation for strong security guarantees that go beyond software-based approaches.

2. **Comprehensive Security Model**: Implements multiple security layers including hardware attestation, asymmetric encryption for key exchange, and symmetric encryption for communication.

3. **Identity Management**: Provides sophisticated identity management capabilities, including secure identity retrieval and adoption.

4. **Configuration Enforcement**: Ensures consistent configuration across all nodes in the green zone, reducing security risks from misconfigurations.

5. **Secure Key Exchange**: Implements secure key exchange protocols that prevent man-in-the-middle attacks through signature verification.

### Design Patterns

1. **Two-Way Verification**: Uses mutual verification where both joining and existing nodes verify each other's identity and attestations.

2. **Hybrid Encryption**: Combines asymmetric encryption (RSA) for key exchange with symmetric encryption (AES) for efficient secure communication.

3. **Configuration Propagation**: Enforces configuration requirements throughout the zone, ensuring consistency in security settings.

4. **Trust Registry**: Maintains a registry of trusted nodes with their attestation data and public keys.

5. **Secure Handshake Protocol**: Implements a multi-step handshake protocol for secure zone joining.

### Challenges and Limitations

1. **Hardware Dependency**: Requires AMD SEV-SNP hardware support, limiting compatibility to specific platforms.

2. **Complexity**: The multi-step join and identity adoption processes introduce complexity that could complicate troubleshooting.

3. **Node Synchronization**: Ensuring all nodes in a green zone maintain synchronized trusted node lists could be challenging.

4. **Key Management**: Managing encryption keys securely across nodes presents ongoing challenges, particularly for key rotation.

5. **Configuration Rigidity**: The strict configuration enforcement might limit flexibility in heterogeneous environments.

### Future Opportunities

1. **Key Rotation Mechanisms**: Implementing secure key rotation for long-lived green zones.

2. **Distributed Trust Management**: Developing more sophisticated mechanisms for managing trusted node lists across the zone.

3. **Alternative Attestation Methods**: Supporting additional hardware attestation technologies beyond AMD SEV-SNP.

4. **Scalability Enhancements**: Optimizing the green zone protocol for larger node clusters.

5. **Advanced Identity Management**: Implementing more granular identity and permission models within green zones.

## Architectural Significance

The Green Zone system has significant architectural importance:

1. **Security Foundation**: It provides a hardware-based security foundation for node collaboration within HyperBEAM.

2. **Trust Networks**: It enables the formation of trusted node networks with strong security guarantees.

3. **Configuration Standardization**: It enforces configuration standards across participating nodes, ensuring security consistency.

4. **Identity Framework**: It serves as an identity management framework for secure node operations.

5. **Secure Communication**: It establishes secure communication channels between trusted nodes.

## Conclusion

The Green Zone system (`dev_green_zone.erl`) represents a sophisticated security component within HyperBEAM, enabling the formation of trusted node networks with hardware-based security guarantees. By combining hardware attestation, asymmetric and symmetric encryption, and configuration enforcement, it creates a comprehensive security framework for node collaboration.

The implementation demonstrates a thoughtful approach to secure node communication and identity management. The multi-layered security model, including hardware attestation, RSA key exchange, and AES-GCM encryption, provides robust protection against various threats while enabling advanced distributed deployment scenarios.

While the system faces challenges related to hardware dependencies and complexity, its architecture provides a solid foundation for secure multi-node deployments. As distributed and confidential computing continues to evolve, components like the Green Zone system will play an increasingly important role in establishing trust between collaborating nodes in untrusted environments.
