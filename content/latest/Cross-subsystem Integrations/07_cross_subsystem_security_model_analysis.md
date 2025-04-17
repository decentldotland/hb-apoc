# Cross-Subsystem Security Model

## Overview

The Cross-Subsystem Security Model in HyperBEAM is a comprehensive framework that ensures security guarantees are maintained as data and operations traverse subsystem boundaries. This analysis examines how security properties such as authentication, authorization, integrity, confidentiality, and non-repudiation are preserved across subsystem transitions, focusing on the mechanisms, patterns, and architectural approaches that enable end-to-end security.

HyperBEAM's architecture implements security as a cross-cutting concern that spans all subsystems, requiring careful design to maintain security guarantees as operations cross subsystem boundaries. The system addresses this challenge through attestation chains, cryptographic verification, explicit trust boundaries, and consistent security policy enforcement.

Understanding the Cross-Subsystem Security Model reveals critical aspects of HyperBEAM's overall security architecture, illuminating how the system maintains a consistent security posture despite the diversity and complexity of its subsystems.

## Security Properties Across Boundaries

Several key security properties must be maintained across subsystem boundaries:

### 1. Authentication

Authentication guarantees must cross boundaries:

- **Identity Preservation**: Entity identities must be preserved across boundaries
- **Credential Transfer**: Authentication credentials must transfer appropriately
- **Authentication Context**: Authentication context must propagate with operations
- **Re-authentication Points**: Points where re-authentication occurs must be defined
- **Multi-Factor Integration**: Multiple authentication factors must be coordinated

### 2. Authorization

Authorization controls must span boundaries:

- **Permission Propagation**: Permissions must propagate with operations
- **Authority Delegation**: Authority must be delegated across boundaries
- **Principle of Least Privilege**: Minimal necessary privileges must be maintained
- **Context-Based Access**: Access based on context must be consistent
- **Capability Transfer**: Security capabilities must transfer appropriately

### 3. Integrity

Integrity guarantees must be preserved:

- **Data Integrity**: Data must maintain integrity across transformations
- **Control Integrity**: Control flows must maintain integrity
- **Cryptographic Chaining**: Cryptographic proofs must maintain continuity
- **Verification Points**: Verification must occur at boundary crossings
- **Tamper Evidence**: Evidence of tampering must be detectable

### 4. Confidentiality

Confidentiality must be maintained:

- **Data Protection**: Protected data must remain protected
- **Encryption Boundaries**: Encryption state must be managed at boundaries
- **Information Flow Control**: Information flows must be controlled
- **Need-to-Know Enforcement**: Need-to-know principles must be enforced
- **Containment Boundaries**: Sensitive data must be contained appropriately

### 5. Non-repudiation

Non-repudiation must extend across boundaries:

- **Evidence Preservation**: Evidence of actions must be preserved
- **Signature Chaining**: Digital signatures must be chained appropriately
- **Audit Continuity**: Audit trails must maintain continuity
- **Attribution Preservation**: Attribution must be preserved
- **Witness Mechanisms**: Witnessing of operations must be maintained

## Security Boundary Types

HyperBEAM implements several types of security boundaries:

### 1. Trust Domain Boundaries

Boundaries between different trust domains:

- **Green Zone Boundary**: Boundary between trusted Green Zone nodes and others
- **External API Boundary**: Boundary between external clients and internal systems
- **Third-Party Integration Boundary**: Boundary with external services
- **Untrusted Code Boundary**: Boundary around WebAssembly execution
- **Hardware Trust Boundary**: Boundary between hardware and software trust

### 2. Privilege Boundaries

Boundaries between different privilege levels:

- **Administrator/User Boundary**: Boundary between admin and user operations
- **Device Privilege Boundary**: Boundary between device privilege levels
- **Attestation Level Boundary**: Boundary between attestation requirement levels
- **Resource Access Boundary**: Boundary between resource access levels
- **Payment Tier Boundary**: Boundary between different payment/subscription tiers

### 3. Subsystem Boundaries

Boundaries between functional subsystems:

- **Storage/Processing Boundary**: Boundary between storage and processing
- **Network/Application Boundary**: Boundary between network and application
- **Blockchain/Local Boundary**: Boundary between blockchain and local systems
- **Device/Framework Boundary**: Boundary between devices and framework
- **Security/Application Boundary**: Boundary between security and application logic

### 4. Component Boundaries

Boundaries between individual components:

- **Module Interface Boundary**: Boundary at module interfaces
- **Process Boundary**: Boundary between Erlang processes
- **Message Boundary**: Boundary at message exchanges
- **API Contract Boundary**: Boundary at API contracts
- **Protocol Boundary**: Boundary at protocol transitions

## Security Transition Mechanisms

Several mechanisms ensure security in cross-boundary transitions:

### 1. Attestation Chain Mechanism

Attestation chains preserve security across boundaries:

```erlang
% Example based on attestation chain propagation
process_with_attestation_chain(Message, Device, Opts) ->
    % Verify existing attestation chain
    case verify_attestation_chain(Message, Opts) of
        {ok, VerifiedMessage} ->
            % Process with device
            case hb_converge:resolve(VerifiedMessage, {as, Device, {}}, Opts) of
                {ok, Result} ->
                    % Extend attestation chain to result
                    {ok, extend_attestation_chain(Result, VerifiedMessage, Opts)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, {attestation_verification_failed, Error}}
    end.

extend_attestation_chain(Result, InputMessage, Opts) ->
    % Get existing chain
    Chain = hb_converge:get(InputMessage, [<<"attestation">>, <<"chain">>], [], Opts),
    
    % Create new attestation entry
    NewEntry = create_attestation_entry(InputMessage, Result, Opts),
    
    % Add to chain
    NewChain = [NewEntry | Chain],
    
    % Add chain to result
    hb_converge:set(Result, [<<"attestation">>, <<"chain">>], NewChain, Opts).
```

This mechanism provides:
- **Continuous Attestation**: Maintaining attestation through processing chains
- **Cryptographic Binding**: Binding each step to its predecessors
- **Verification Points**: Allowing verification at boundary crossings
- **Provenance Tracking**: Tracking the origin and processing history
- **Trust Transfer**: Transferring trust from inputs to outputs

### 2. Security Context Propagation

Security context propagates across boundaries:

```erlang
% Example based on security context propagation
propagate_security_context(Message, Context, Opts) ->
    % Extract security attributes from context
    Principal = maps:get(principal, Context, undefined),
    Permissions = maps:get(permissions, Context, []),
    AuthLevel = maps:get(auth_level, Context, <<"none">>),
    TrustChain = maps:get(trust_chain, Context, []),
    
    % Add security context to message
    SecurityContext = #{
        <<"principal">> => Principal,
        <<"permissions">> => Permissions,
        <<"auth_level">> => AuthLevel,
        <<"trust_chain">> => TrustChain,
        <<"timestamp">> => os:system_time(millisecond)
    },
    
    % Add security information to message
    hb_converge:set(Message, [<<"security_context">>], SecurityContext, Opts).
```

This mechanism enables:
- **Context Transfer**: Transferring security context across boundaries
- **Implicit Authentication**: Carrying authentication information
- **Authorization Carriage**: Carrying authorization information
- **Trust Level Indication**: Indicating trust level
- **Security Metadata**: Providing security-related metadata

### 3. Cryptographic Envelopes

Cryptographic envelopes secure cross-boundary transitions:

```erlang
% Example based on cryptographic envelope creation
create_secure_envelope(Message, Recipient, Opts) ->
    % Extract message content
    Content = hb_converge:get(Message, <<"content">>, Opts),
    
    % Get recipient's public key
    case get_recipient_public_key(Recipient, Opts) of
        {ok, PublicKey} ->
            % Generate random symmetric key
            SymmetricKey = crypto:strong_rand_bytes(32),
            
            % Encrypt content with symmetric key
            EncryptedContent = encrypt_symmetric(Content, SymmetricKey, Opts),
            
            % Encrypt symmetric key with recipient's public key
            EncryptedKey = encrypt_asymmetric(SymmetricKey, PublicKey, Opts),
            
            % Create envelope
            Envelope = #{
                <<"encrypted_content">> => EncryptedContent,
                <<"encrypted_key">> => EncryptedKey,
                <<"recipient">> => Recipient,
                <<"timestamp">> => os:system_time(millisecond),
                <<"format">> => <<"AES-256-GCM+RSA">>
            },
            
            % Sign the envelope
            {ok, sign_envelope(Envelope, Opts)};
        {error, Error} ->
            {error, {recipient_key_error, Error}}
    end.
```

This mechanism provides:
- **End-to-End Encryption**: Protecting data across boundaries
- **Targeted Protection**: Protecting data for specific recipients
- **Integrity Protection**: Ensuring data integrity
- **Authentication**: Authenticating the sender
- **Non-repudiation**: Preventing sender repudiation

### 4. Policy Enforcement Points

Policy enforcement points secure boundary crossings:

```erlang
% Example based on policy enforcement at boundaries
enforce_boundary_policy(Message, BoundaryType, Direction, Opts) ->
    % Get applicable policies for this boundary
    Policies = get_boundary_policies(BoundaryType, Direction, Opts),
    
    % Evaluate message against all policies
    EvaluationResults = lists:map(
        fun(Policy) -> evaluate_policy(Message, Policy, Opts) end,
        Policies
    ),
    
    % Check if any policy denied the message
    case lists:any(
        fun({Decision, _}) -> Decision =:= deny end,
        EvaluationResults
    ) of
        true ->
            % Get first denial reason
            {_, DenyReason} = lists:keyfind(deny, 1, EvaluationResults),
            {error, {policy_violation, DenyReason}};
        false ->
            % All policies passed, apply any transformations
            TransformedMessage = apply_policy_transformations(Message, EvaluationResults, Opts),
            {ok, TransformedMessage}
    end.
```

This mechanism enables:
- **Boundary Guarding**: Controlling what crosses boundaries
- **Policy Evaluation**: Evaluating policies at boundaries
- **Transformation Application**: Applying required transformations
- **Decision Enforcement**: Enforcing access decisions
- **Audit Recording**: Recording policy decisions

## Security Patterns for Boundary Crossing

Several patterns ensure security across boundaries:

### 1. Cryptographic Re-Attestation Pattern

This pattern re-attests data at boundary crossings:

```
Original Attestation → Boundary Crossing →
Verification → Data Extraction → Transformation →
New Attestation → Attested Result
```

Key aspects of this pattern:
- **Attestation Verification**: Verifying incoming attestation
- **Attestation Break**: Breaking the attestation chain
- **Data Transformation**: Transforming data as needed
- **Re-Attestation**: Creating new attestation for transformed data
- **Chain Linking**: Linking to previous attestation

### 2. Security Gateway Pattern

This pattern mediates between security domains:

```
Source Domain → Security Gateway → Protocol Conversion →
Security Validation → Authorization →
Transformation → Target Domain
```

Key aspects of this pattern:
- **Domain Isolation**: Isolating security domains
- **Protocol Adaptation**: Adapting between domain protocols
- **Security Validation**: Validating security properties
- **Transformation Mediation**: Mediating transformations
- **Policy Enforcement**: Enforcing cross-domain policies

### 3. Privilege Reduction Pattern

This pattern reduces privileges at boundaries:

```
High Privilege Context → Boundary Crossing →
Privilege Calculation → Capability Restriction →
Minimal Privilege Assignment → Low Privilege Context
```

Key aspects of this pattern:
- **Privilege Evaluation**: Evaluating required privileges
- **Principle of Least Privilege**: Assigning minimal privileges
- **Capability Restriction**: Restricting capabilities
- **Context Transformation**: Transforming security context
- **Permission Narrowing**: Narrowing permissions

### 4. Trusted Intermediary Pattern

This pattern uses trusted components for transitions:

```
Source Component → Trusted Intermediary →
Validation → Transformation → Attestation →
Target Component
```

Key aspects of this pattern:
- **Trust Anchoring**: Anchoring trust in the intermediary
- **Validation Logic**: Validating security properties
- **Transformation Logic**: Performing trusted transformations
- **Attestation Creation**: Creating attestation of transformation
- **Trust Transfer**: Transferring trust to the result

## Configuration Aspects

Cross-Subsystem Security can be configured in several ways:

### 1. Boundary Configuration

Security boundaries can be configured:

```erlang
% Example boundary configuration
boundary_configuration() ->
    #{
        boundaries => [
            {<<"storage_processing">>, #{
                enforcement_level => strict,
                required_attestation => true,
                required_authorization => true,
                audit_level => detailed
            }},
            {<<"network_application">>, #{
                enforcement_level => moderate,
                required_attestation => false,
                required_authorization => true,
                audit_level => basic
            }}
        ],
        default_enforcement => moderate,
        audit_storage => local,
        policy_conflict_resolution => deny_overrides
    }.
```

This configuration controls:
- **Boundary Definitions**: Defining security boundaries
- **Enforcement Levels**: Setting enforcement strictness
- **Security Requirements**: Setting security requirements
- **Audit Settings**: Configuring boundary audit
- **Default Behavior**: Setting default behavior

### 2. Trust Configuration

Trust relationships can be configured:

```erlang
% Example trust configuration
trust_configuration() ->
    #{
        trust_domains => [
            {<<"internal">>, #{
                trust_level => high,
                authentication_requirement => strong,
                attestation_requirement => hardware
            }},
            {<<"partner">>, #{
                trust_level => medium,
                authentication_requirement => two_factor,
                attestation_requirement => software
            }},
            {<<"public">>, #{
                trust_level => low,
                authentication_requirement => basic,
                attestation_requirement => none
            }}
        ],
        cross_domain_rules => [
            {<<"internal">>, <<"partner">>, #{
                allowed_operations => [<<"read">>, <<"limited_write">>],
                data_classification_limit => <<"confidential">>
            }},
            {<<"partner">>, <<"public">>, #{
                allowed_operations => [<<"read">>],
                data_classification_limit => <<"public">>
            }}
        ],
        default_domain => <<"public">>,
        unknown_domain_policy => deny
    }.
```

This configuration controls:
- **Trust Domain Definitions**: Defining trust domains
- **Trust Levels**: Setting trust levels
- **Authentication Requirements**: Setting authentication requirements
- **Cross-Domain Rules**: Defining rules between domains
- **Default Policies**: Setting default policies

### 3. Attestation Configuration

Attestation behavior can be configured:

```erlang
% Example attestation configuration
attestation_configuration() ->
    #{
        attestation_chains => #{
            enabled => true,
            max_chain_length => 10,
            verification_depth => full,
            signature_algorithm => <<"ed25519">>,
            include_timestamps => true
        },
        attestation_requirements => [
            {<<"storage_operations">>, #{
                level => <<"hardware">>,
                freshness_requirement => 3600,
                required_attributes => [<<"operation">>, <<"data_hash">>]
            }},
            {<<"payment_operations">>, #{
                level => <<"hardware">>,
                freshness_requirement => 300,
                required_attributes => [<<"amount">>, <<"recipient">>, <<"timestamp">>]
            }}
        ],
        attestation_verification => #{
            cache_results => true,
            cache_ttl => 600,
            lazy_verification => false
        }
    }.
```

This configuration controls:
- **Chain Management**: Managing attestation chains
- **Attestation Requirements**: Setting operation requirements
- **Verification Settings**: Configuring verification behavior
- **Performance Settings**: Optimizing attestation performance
- **Algorithm Selection**: Selecting attestation algorithms

### 4. Audit Configuration

Audit behavior can be configured:

```erlang
% Example audit configuration
audit_configuration() ->
    #{
        cross_boundary_audit => #{
            enabled => true,
            detail_level => high,
            include_data => false,
            storage_location => <<"both">>,
            retention_period => 90
        },
        audit_events => [
            {<<"authentication">>, #{
                record => true,
                alert_on_failure => true,
                include_context => true
            }},
            {<<"authorization">>, #{
                record => true,
                alert_on_failure => true,
                include_context => true
            }},
            {<<"data_access">>, #{
                record => true,
                alert_on_failure => false,
                include_context => false
            }}
        ],
        audit_format => <<"structured">>,
        audit_batching => #{
            enabled => true,
            max_batch_size => 100,
            max_batch_interval => 60
        }
    }.
```

This configuration controls:
- **Boundary Audit**: Configuring boundary audit
- **Event Selection**: Selecting events to audit
- **Detail Settings**: Setting audit detail level
- **Storage Settings**: Configuring audit storage
- **Performance Settings**: Optimizing audit performance

## Security Implications

Cross-Subsystem Security has several implications:

### 1. Attack Surface Management

Security model affects attack surface:

- **Boundary Minimization**: Minimizing the number of boundaries
- **Interface Narrowing**: Narrowing interfaces at boundaries
- **Validation Depth**: Deep validation at boundaries
- **Privilege Separation**: Separating privileges across boundaries
- **Isolation Enforcement**: Enforcing isolation between components

### 2. Trust Management

Trust must be managed across boundaries:

- **Trust Chains**: Building and maintaining chains of trust
- **Trust Derivation**: Deriving trust from trusted sources
- **Trust Verification**: Verifying trust at boundaries
- **Trust Revocation**: Revoking trust when needed
- **Trust Establishment**: Establishing trust between components

### 3. Consistency of Protection

Protection must be consistent:

- **Protection Continuity**: Maintaining continuous protection
- **Consistent Enforcement**: Enforcing policies consistently
- **Security Level Matching**: Matching security levels across boundaries
- **Gap Prevention**: Preventing security gaps at boundaries
- **Overlapping Protections**: Creating overlapping protections

### 4. Security Decomposition

Security is decomposed across boundaries:

- **Responsibility Allocation**: Allocating security responsibilities
- **Control Distribution**: Distributing security controls
- **Layered Defense**: Implementing layered defenses
- **Coordinated Protection**: Coordinating protection mechanisms
- **Defense in Depth**: Implementing defense in depth

## Security Challenges

Cross-Subsystem Security poses several challenges:

### 1. Security Composition

Composing security across boundaries is challenging:

- **Composition Correctness**: Ensuring correct security composition
- **Emergent Properties**: Managing emergent security properties
- **Interference Detection**: Detecting security mechanism interference
- **Compatibility Assurance**: Ensuring security mechanism compatibility
- **Complete Coverage**: Ensuring complete security coverage

### 2. Security Abstraction Leakage

Security abstractions may leak across boundaries:

- **Abstraction Breaking**: Preventing abstraction breaking
- **Implementation Exposure**: Preventing implementation exposure
- **Model Consistency**: Maintaining consistent security model
- **Abstraction Level Matching**: Matching abstraction levels
- **Security Representation**: Managing security representation

### 3. Security State Management

Security state must be managed across boundaries:

- **State Synchronization**: Synchronizing security state
- **State Consistency**: Maintaining consistent security state
- **State Transfer**: Transferring security state properly
- **State Protection**: Protecting security state
- **State Recovery**: Recovering from security state inconsistencies

### 4. Security Tradeoffs

Security involves tradeoffs across boundaries:

- **Performance vs. Security**: Balancing performance and security
- **Usability vs. Security**: Balancing usability and security
- **Flexibility vs. Security**: Balancing flexibility and security
- **Compatibility vs. Security**: Balancing compatibility and security
- **Cost vs. Security**: Balancing cost and security

## Examples

Let's examine concrete examples of Cross-Subsystem Security from the codebase:

### Attestation Chain Across Process and Storage

```erlang
% Example based on process/storage boundary crossing
store_process_result(ProcessResult, SourceMessage, Opts) ->
    % Verify attestation chain from process
    case verify_attestation_chain(ProcessResult, Opts) of
        {ok, VerifiedResult} ->
            % Extract data to store
            StorableData = extract_storable_data(VerifiedResult, Opts),
            
            % Create storage message
            StorageMessage = create_storage_message(StorableData, VerifiedResult, Opts),
            
            % Extend attestation chain
            StorageMessageWithAttestation = extend_attestation_chain(
                StorageMessage, 
                VerifiedResult,
                <<"process_to_storage">>,
                Opts
            ),
            
            % Store with attestation
            hb_converge:resolve(
                StorageMessageWithAttestation,
                {as, <<"store@1.0">>, #{<<"action">> => <<"put">>}},
                Opts
            );
        {error, Error} ->
            {error, {attestation_verification_failed, Error}}
    end.

extend_attestation_chain(Message, SourceMessage, BoundaryType, Opts) ->
    % Get attestation chain from source
    SourceChain = hb_converge:get(SourceMessage, [<<"attestation">>, <<"chain">>], [], Opts),
    
    % Create boundary crossing attestation
    CrossingAttestation = create_boundary_crossing_attestation(
        Message,
        SourceMessage,
        BoundaryType,
        Opts
    ),
    
    % Add to chain
    NewChain = [CrossingAttestation | SourceChain],
    
    % Add chain to message
    hb_converge:set(Message, [<<"attestation">>, <<"chain">>], NewChain, Opts).
```

This example demonstrates:
- **Chain Verification**: Verifying attestation before crossing
- **Chain Extension**: Extending attestation across boundary
- **Boundary Context**: Adding boundary context to attestation
- **Data Extraction**: Extracting appropriate data for crossing
- **Storage Integration**: Integrating with storage subsystem

### Security Context Propagation in HTTP-to-Core

```erlang
% Example based on HTTP-to-Core boundary crossing
process_authenticated_request(HttpRequest, AuthInfo, Opts) ->
    % Convert HTTP request to internal message
    case hb_http:message_from_request(HttpRequest, Opts) of
        {ok, InternalMessage} ->
            % Create security context from authentication
            SecurityContext = create_security_context_from_auth(AuthInfo, Opts),
            
            % Add security context to message
            MessageWithSecurity = add_security_context(InternalMessage, SecurityContext, Opts),
            
            % Process message with security context
            case hb_converge:resolve(MessageWithSecurity, Opts) of
                {ok, Result} ->
                    % Convert result back to HTTP response
                    hb_http:message_to_response(Result, 200, Opts);
                {error, {security_error, Error}} ->
                    % Security error
                    create_security_error_response(Error, Opts);
                {error, Error} ->
                    % Other error
                    create_error_response(Error, Opts)
            end;
        {error, Error} ->
            % Conversion error
            create_conversion_error_response(Error, Opts)
    end.

create_security_context_from_auth(AuthInfo, Opts) ->
    % Extract authentication components
    Principal = maps:get(principal, AuthInfo),
    AuthMethod = maps:get(method, AuthInfo),
    AuthLevel = maps:get(level, AuthInfo),
    
    % Lookup permissions for principal
    Permissions = lookup_permissions(Principal, Opts),
    
    % Create security context
    #{
        <<"principal">> => Principal,
        <<"auth_method">> => AuthMethod,
        <<"auth_level">> => AuthLevel,
        <<"permissions">> => Permissions,
        <<"source">> => <<"http_boundary">>,
        <<"timestamp">> => os:system_time(millisecond)
    }.
```

This example demonstrates:
- **Context Creation**: Creating security context from authentication
- **Context Propagation**: Propagating context across boundary
- **Permission Resolution**: Resolving permissions for authenticated user
- **Error Handling**: Handling security errors at boundary
- **Protocol Adaptation**: Adapting between HTTP and internal protocols

### Green Zone Trust Boundary Enforcement

```erlang
% Example based on Green Zone boundary enforcement
send_to_node(Message, TargetNode, Opts) ->
    % Check if target is in the Green Zone
    case is_node_in_green_zone(TargetNode, Opts) of
        true ->
            % Node is trusted, check message classification
            case get_message_classification(Message, Opts) of
                {ok, Classification} ->
                    % Check if classification can be sent to Green Zone
                    case can_send_classification_to_green_zone(Classification, Opts) of
                        true ->
                            % Send through Green Zone secure channel
                            send_through_green_zone(Message, TargetNode, Opts);
                        false ->
                            % Classification too high for Green Zone
                            {error, {classification_too_high_for_green_zone, Classification}}
                    end;
                {error, Error} ->
                    % Cannot determine classification
                    {error, {classification_error, Error}}
            end;
        false ->
            % Node is not in Green Zone, check if external sending is allowed
            case can_send_to_external_node(Message, TargetNode, Opts) of
                true ->
                    % Send through external channel with protections
                    send_to_external_node(Message, TargetNode, Opts);
                false ->
                    % Cannot send to external node
                    {error, {cannot_send_to_external_node, TargetNode}}
            end
    end.
```

This example demonstrates:
- **Trust Boundary Check**: Checking trust boundary membership
- **Classification Control**: Controlling data by classification
- **Channel Selection**: Selecting appropriate channel based on trust
- **External Protection**: Adding protection for external transmission
- **Policy Enforcement**: Enforcing transmission policy

### WebAssembly Execution Sandbox Boundary

```erlang
% Example based on WebAssembly sandbox boundary
execute_in_wasm_sandbox(Message, Module, Opts) ->
    % Extract code and data
    Code = hb_converge:get(Message, [<<"code">>], undefined, Opts),
    InputData = hb_converge:get(Message, [<<"input">>], #{}, Opts),
    
    % Verify code attestation if present
    CodeAttestation = hb_converge:get(Message, [<<"code_attestation">>], undefined, Opts),
    case verify_code_attestation(Code, CodeAttestation, Opts) of
        {ok, verified} ->
            % Prepare sandbox environment
            SandboxOpts = prepare_sandbox_options(Message, Opts),
            
            % Convert input to JSON for WASM boundary crossing
            case dev_json_iface:message_to_json(InputData, Opts) of
                {ok, JsonInput} ->
                    % Execute in isolated WASM environment
                    case dev_wasm:execute_code(Module, Code, JsonInput, SandboxOpts) of
                        {ok, JsonResult} ->
                            % Convert result back across boundary
                            case dev_json_iface:json_to_message(JsonResult, Opts) of
                                {ok, ResultMessage} ->
                                    % Add sandbox attestation
                                    {ok, add_sandbox_attestation(ResultMessage, Code, InputData, Opts)};
                                {error, Error} ->
                                    {error, {result_conversion_error, Error}}
                            end;
                        {error, Error} ->
                            {error, {wasm_execution_error, Error}}
                    end;
                {error, Error} ->
                    {error, {input_conversion_error, Error}}
            end;
        {error, Error} ->
            {error, {code_attestation_error, Error}}
    end.
```

This example demonstrates:
- **Code Verification**: Verifying code before execution
- **Sandbox Preparation**: Preparing isolated execution environment
- **Format Adaptation**: Adapting data formats across boundary
- **Resource Limitation**: Limiting resources in sandboxed execution
- **Result Attestation**: Attesting execution results

## Architectural Significance

Cross-Subsystem Security is architecturally significant for several reasons:

### 1. Security Decomposition

The security model enables effective decomposition:

- **Modular Security**: Security decomposed into modules
- **Responsibility Allocation**: Clear allocation of security responsibilities
- **Interface Contracts**: Clear security contracts between components
- **Reusable Mechanisms**: Reusable security mechanisms
- **Consistent Model**: Consistent security model across decomposition

### 2. End-to-End Security

The security model enables end-to-end security:

- **Continuous Protection**: Continuous protection across boundaries
- **Property Preservation**: Preservation of security properties
- **Chain of Trust**: Unbroken chain of trust
- **Attestation Chaining**: Chained attestations across operations
- **Cryptographic Continuity**: Continuous cryptographic protection

### 3. Defense in Depth

The security model implements defense in depth:

- **Multiple Layers**: Multiple security layers
- **Diverse Mechanisms**: Diverse security mechanisms
- **Progressive Protection**: Progressive protection through system
- **Overlapping Controls**: Overlapping security controls
- **Independent Verification**: Independent verification mechanisms

### 4. Evolution Support

The security model supports evolution:

- **Security Abstraction**: Security abstracted from implementation
- **Standard Interfaces**: Standard security interfaces
- **Pluggable Mechanisms**: Pluggable security mechanisms
- **Configuration-Driven**: Configuration-driven security behavior
- **Versioned Security**: Support for multiple security versions

## Conclusion

The Cross-Subsystem Security Model in HyperBEAM represents a comprehensive approach to maintaining security guarantees across subsystem boundaries. By implementing attestation chains, security context propagation, cryptographic envelopes, and policy enforcement points, the system ensures that security properties such as authentication, authorization, integrity, confidentiality, and non-repudiation are preserved as operations traverse subsystem boundaries.

The security model reveals key architectural principles in HyperBEAM:

1. **Security Continuity**: Continuous security across subsystem boundaries
2. **Explicit Boundaries**: Well-defined and explicitly secured boundaries
3. **Attestation Chains**: Cryptographic attestation chains for verification
4. **Trust Architecture**: Clear trust relationships between components
5. **Defense in Depth**: Multiple security layers throughout the system

Understanding this security model is essential for working with HyperBEAM's security capabilities, diagnosing security issues that span subsystem boundaries, and extending the system with new security mechanisms. The sophisticated approach to cross-subsystem security demonstrates the elegant architectural foundation that enables HyperBEAM to function as a secure distributed computing platform.
