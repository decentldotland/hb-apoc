# Delegation and Composition Across Subsystems

## Overview

Delegation and composition are foundational integration patterns in HyperBEAM that enable complex functionality to be built from simpler components. This analysis examines how these patterns are implemented across subsystem boundaries, revealing critical aspects of HyperBEAM's architectural approach to extensibility, modularity, and functional decomposition.

HyperBEAM's architecture leverages delegation and composition extensively to create a system where complex behaviors emerge from the interaction of simpler components. These patterns appear at multiple layers, from device-level composition to subsystem-level integration, creating a coherent yet flexible system architecture.

## Involved Subsystems

Delegation and composition patterns cross multiple subsystem boundaries:

### Primary Composition Subsystems

- **Core Infrastructure**: The `hb_converge` module and `dev_stack` device provide fundamental composition capabilities
- **Device Ecosystem**: Multiple devices implement delegation patterns, particularly `dev_process` and `dev_meta`
- **Execution Environment**: WebAssembly integration uses delegation to bridge execution contexts
- **Security Infrastructure**: Security features delegate to hardware attestation and verification

### Consuming Subsystems

- **Storage Subsystem**: Uses composition for backend selection and caching layers
- **Network Communication**: Uses delegation for protocol adaptation and routing
- **Scheduler System**: Delegates slot assignments and state management
- **Payment Infrastructure**: Composes pricing and ledger components

## Delegation Patterns

HyperBEAM implements several distinct delegation patterns across subsystem boundaries:

### 1. Device-Swapping Delegation

The primary delegation pattern in HyperBEAM involves temporary device swapping:

```erlang
delegate_operation(Message, SubDevice, Opts) ->
    % Store current device
    Message2 = hb_converge:set(Message, [?DEVICE_STACK_PATH, CurrentPos], ThisDevice, Opts),
    % Set delegated device
    Message3 = hb_converge:set(Message2, ?DEVICE_PATH, SubDevice, Opts),
    % Execute with delegated device
    case hb_converge:resolve(Message3, Opts) of
        {ok, Result} ->
            % Restore original device
            {ok, hb_converge:set(Result, ?DEVICE_PATH, ThisDevice, Opts)};
        Error ->
            Error
    end.
```

This pattern enables:
- **Functional Specialization**: Devices delegate specific operations to specialized devices
- **Clean Separation of Concerns**: Each device handles a specific aspect of functionality
- **Composable Behavior**: Complex behavior emerges from composition of simpler devices
- **Preserved Context**: The delegation context is maintained in the message structure

### 2. Service Delegation

Service-level delegation occurs when one subsystem delegates operations to another:

```erlang
% Example from dev_relay.erl
execute({<<"POST">>, _} = Msg, Opts) ->
    case hb_http:request(Msg, Opts) of
        {ok, Response} -> {ok, Response};
        {error, Error} -> {error, Error}
    end;
```

This pattern enables:
- **Subsystem Specialization**: Each subsystem focuses on its core capabilities
- **Reusable Services**: Common services can be leveraged by multiple subsystems
- **Interface Stability**: Subsystems interact through stable interfaces
- **Implementation Flexibility**: Service implementations can change without affecting clients

### 3. Extension Delegation

Extension delegation allows for pluggable extensions without modifying core code:

```erlang
% Example from dev_p4.erl
price_message(Msg, Opts) ->
    case get_pricing_device(Msg, Opts) of
        {ok, PricingDevice} ->
            hb_converge:resolve(Msg, {as, PricingDevice, price_request()}, Opts);
        Error ->
            Error
    end,
```

This pattern enables:
- **Runtime Extension**: Behavior can be extended without code modification
- **Pluggable Components**: New components can be added to extend functionality
- **Configuration-Driven Behavior**: System behavior can change through configuration
- **Isolated Extensions**: Extensions operate in isolated contexts

### 4. Cross-Boundary Function Delegation

Function-level delegation occurs when specific operations cross subsystem boundaries:

```erlang
% Example from hb_client.erl
post(Endpoint, Headers, Body, Opts) ->
    hb_http_client:post(Endpoint, Headers, Body, Opts).
```

This pattern enables:
- **Interface Simplification**: Complex operations are simplified through delegation
- **Implementation Hiding**: Implementation details are hidden behind interfaces
- **Cross-Subsystem Calls**: Functionality can be invoked across subsystem boundaries
- **Layered Abstraction**: Abstract operations are built on concrete implementations

## Composition Patterns

HyperBEAM implements several composition patterns across subsystem boundaries:

### 1. Device Stack Composition

The primary composition pattern involves stacks of devices:

```erlang
% Example from dev_stack.erl
execute(Message, Opts) ->
    Devices = get_stack_devices(Message, Opts),
    StackMode = get_stack_mode(Message, Opts),
    case StackMode of
        <<"fold">> -> execute_fold(Message, Devices, Opts);
        <<"map">> -> execute_map(Message, Devices, Opts);
        _ -> {error, {invalid_stack_mode, StackMode}}
    end.
```

This pattern enables:
- **Sequential Processing**: Messages flow through a sequence of devices
- **Functional Composition**: Each device applies a transformation
- **Processing Pipelines**: Complex workflows are built from simpler steps
- **Declarative Configuration**: Pipelines are defined through configuration

### 2. Pipeline Composition

Processing pipelines compose multiple operations in sequence:

```erlang
% Example from dev_meta.erl
execute(Message, Opts) ->
    % Apply preprocessing pipeline
    case apply_pipeline(Message, preprocessing_pipeline(Message, Opts), Opts) of
        {ok, ProcessedMessage} ->
            % Process the message
            case process_message(ProcessedMessage, Opts) of
                {ok, Result} ->
                    % Apply postprocessing pipeline
                    apply_pipeline(Result, postprocessing_pipeline(Result, Opts), Opts);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.
```

This pattern enables:
- **Multi-Stage Processing**: Messages undergo multiple processing stages
- **Aspect-Oriented Behavior**: Cross-cutting concerns are applied consistently
- **Extensible Processing**: Pipelines can be extended without modifying core code
- **Pre/Post Processing**: Operations can happen before and after core processing

### 3. Layer Composition

Layer-based composition stacks abstractions on top of each other:

```erlang
% Example from storage subsystem
read(Key, Opts) ->
    % Try cache first
    case hb_cache:get(Key, Opts) of
        {ok, Value} ->
            {ok, Value};
        _ ->
            % Fall back to persistent storage
            case hb_store:get(Key, Opts) of
                {ok, Value} ->
                    % Update cache and return
                    hb_cache:put(Key, Value, Opts),
                    {ok, Value};
                Error ->
                    Error
            end
    end.
```

This pattern enables:
- **Abstraction Layers**: Higher-level abstractions build on lower-level ones
- **Progressive Enhancement**: Functionality is enhanced at each layer
- **Separation of Concerns**: Each layer focuses on specific concerns
- **Implementation Hiding**: Lower-level details are hidden from higher layers

### 4. Adapter Composition

Adapter-based composition bridges between different interfaces:

```erlang
% Example from dev_json_iface.erl
execute(Message, Opts) ->
    % Convert message to JSON
    case message_to_json(Message, Opts) of
        {ok, Json} ->
            % Process in WASM
            case process_json_in_wasm(Json, Opts) of
                {ok, ResultJson} ->
                    % Convert result back to message
                    json_to_message(ResultJson, Opts);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.
```

This pattern enables:
- **Interface Bridging**: Different interface styles are bridged
- **Format Adaptation**: Data formats are converted between subsystems
- **Protocol Translation**: Communication protocols are adapted
- **Legacy Integration**: New and old systems can interoperate

## Core Composition Mechanisms

HyperBEAM implements several mechanisms that enable delegation and composition:

### 1. Device Resolution

The `hb_converge:resolve/3` function is the primary composition mechanism:

```erlang
hb_converge:resolve(Message, {as, DeviceName, Request}, Options)
```

This mechanism provides:
- **Dynamic Dispatch**: Messages are routed to appropriate handlers
- **Temporary Device Swapping**: Devices can temporarily delegate to others
- **Message Context**: Context is maintained through the message structure
- **Device Selection**: Devices are selected based on message content or configuration

### 2. Device Stack

The `dev_stack` device provides explicit device composition:

```erlang
{
  "device": "stack@1.0",
  "devices": [
    "device1@1.0",
    "device2@1.0",
    "device3@1.0"
  ],
  "mode": "fold"
}
```

This mechanism provides:
- **Declarative Composition**: Device stacks are defined declaratively
- **Sequential Processing**: Messages flow through devices in sequence
- **Fold and Map Modes**: Different composition modes for different needs
- **Configuration-Driven**: Stacks can be defined through configuration

### 3. Pipeline Functions

The pipeline pattern is implemented through function sequences:

```erlang
apply_pipeline(Message, Pipeline, Opts) ->
    lists:foldl(
        fun(Step, {ok, Msg}) -> Step(Msg, Opts);
           (_, Error) -> Error
        end,
        {ok, Message},
        Pipeline
    ).
```

This mechanism provides:
- **Function Composition**: Functions are composed in sequence
- **Error Short-Circuiting**: Errors short-circuit the pipeline
- **Stateless Functions**: Each function transforms the message
- **Functional Programming Model**: Pure functional approach to composition

### 4. Extension Registration

Extension systems use registration for runtime composition:

```erlang
register_extension(Name, Module, Function, Opts) ->
    Extensions = get_extensions(Opts),
    set_extensions([{Name, {Module, Function}} | Extensions], Opts).
```

This mechanism provides:
- **Dynamic Extension**: System behavior can be extended at runtime
- **Registry-Based Dispatch**: Extensions are registered and dispatched
- **Named Extensions**: Extensions are identified by name
- **Configuration Integration**: Extension registration interacts with configuration

## Cross-Subsystem Composition Examples

To illustrate delegation and composition across subsystems, let's examine several key examples:

### Web API to Process Execution

```
HTTP Request → hb_http_server → hb_singleton → dev_meta →
  dev_stack(preprocess_devices) → dev_process → 
  dev_process:DELEGATE→dev_wasm → dev_json_iface →
  WebAssembly Module → dev_json_iface → dev_process →
  dev_stack(postprocess_devices) → HTTP Response
```

This example demonstrates:
- **Multi-layer Composition**: Multiple composition layers (HTTP, device stack, delegation)
- **Cross-Subsystem Delegation**: Process subsystem delegates to WebAssembly subsystem
- **Protocol Adaptation**: HTTP is adapted to internal message format
- **Pipeline Processing**: Pre/post processing pipelines handle cross-cutting concerns

### Blockchain Data Storage

```
Arweave Transaction → dev_codec_ans104 → Internal Message →
  hb_cache → hb_store_DELEGATE→hb_store_fs →
  File System → hb_store → hb_cache → Application
```

This example demonstrates:
- **Service Composition**: Storage service composed of multiple layers
- **Backend Selection**: Storage delegates to specific backends
- **Caching Integration**: Cache layer integrates with storage layer
- **Format Adaptation**: Blockchain format is adapted to internal format

### Security Attestation Flow

```
Message → dev_meta → dev_snp → Hardware Attestation →
  dev_green_zone → Cryptographic Verification →
  dev_process → Application Logic
```

This example demonstrates:
- **Security Layer Composition**: Security layers are composed with application logic
- **Hardware Integration**: System delegates to hardware for attestation
- **Trust Chain**: Attestation flows through multiple security components
- **Cross-Cutting Concern**: Security is applied as a cross-cutting concern

## Configuration Aspects

Delegation and composition are influenced by configuration in several ways:

### 1. Device Selection Configuration

Configuration determines which devices are used in delegation:

```erlang
get_pricing_device(Msg, Opts) ->
    % Try from message first
    case hb_converge:get(Msg, [<<"pricing">>, <<"device">>], undefined, Opts) of
        undefined ->
            % Fall back to configuration
            case hb_opts:get([<<"p4">>, <<"pricing_device">>], undefined, Opts) of
                undefined -> {error, no_pricing_device};
                Device -> {ok, Device}
            end;
        Device ->
            {ok, Device}
    end.
```

### 2. Stack Configuration

Device stacks are defined through configuration:

```json
{
  "devices": {
    "message-stack": {
      "device": "stack@1.0",
      "devices": [
        "message@1.0",
        "snp@1.0",
        "green-zone@1.0",
        "p4@1.0",
        "process@1.0"
      ],
      "mode": "fold"
    }
  }
}
```

### 3. Pipeline Configuration

Processing pipelines can be configured:

```erlang
preprocessing_pipeline(Msg, Opts) ->
    % Get from configuration
    case hb_opts:get([<<"meta">>, <<"preprocessing">>], [], Opts) of
        [] -> default_preprocessing_pipeline(Msg, Opts);
        Pipeline -> Pipeline
    end.
```

### 4. Backend Selection

Storage and service backends are selected through configuration:

```erlang
get_store_backend(Opts) ->
    hb_opts:get([<<"store">>, <<"backend">>], <<"fs">>, Opts).
```

## Security Implications

Delegation and composition have several security implications:

### 1. Trust Boundaries

Delegation often crosses trust boundaries:

- **Privilege Escalation**: Delegation could enable privilege escalation if not controlled
- **Trust Verification**: Delegated components must verify trust
- **Attestation Chain**: Attestation must be maintained across delegation
- **Authorization Checks**: Authority to delegate must be verified

### 2. Component Isolation

Composition requires proper component isolation:

- **Sandboxing**: Composed components should be properly isolated
- **Message Validation**: Messages must be validated at composition boundaries
- **State Isolation**: Component state should be isolated
- **Error Containment**: Errors should be contained within components

### 3. Configuration Security

Configuration-driven composition has security implications:

- **Configuration Validation**: Composition configuration must be validated
- **Secure Defaults**: Default composition should be secure
- **Configuration Protection**: Composition configuration must be protected
- **Trusted Configuration Source**: Configuration source must be trusted

### 4. Attestation Preservation

Delegation must preserve attestation properties:

- **Signature Verification**: Signatures must be verified before delegation
- **Re-attestation**: Results must be re-attested after delegation
- **Attestation Chain**: Delegation chain must be cryptographically verifiable
- **Revocation Checking**: Delegated component attestations must be checked for revocation

## Error Handling

Error handling in delegation and composition follows consistent patterns:

### 1. Error Propagation

Errors propagate through composition chains:

```erlang
case delegate_to_component(Message, Opts) of
    {ok, Result} -> process_result(Result, Opts);
    {error, _} = Error -> Error
end
```

### 2. Composition Abort

Errors abort composition chains:

```erlang
apply_pipeline(Message, Pipeline, Opts) ->
    lists:foldl(
        fun(Step, {ok, Msg}) -> Step(Msg, Opts);
           (_, Error) -> Error
        end,
        {ok, Message},
        Pipeline
    ).
```

### 3. Error Context

Delegation errors include context:

```erlang
delegate(Message, Component, Opts) ->
    case hb_converge:resolve(Message, {as, Component, Request}, Opts) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, {delegation_error, Component, Reason}}
    end.
```

### 4. Fallback Mechanisms

Composition often includes fallback mechanisms:

```erlang
% Try primary component first
case delegate_to_primary(Message, Opts) of
    {ok, Result} -> {ok, Result};
    {error, _} ->
        % Fall back to secondary
        delegate_to_secondary(Message, Opts)
end
```

## Performance Considerations

Delegation and composition have several performance implications:

### 1. Delegation Overhead

- **Resolution Cost**: Device resolution adds overhead
- **Context Switching**: Switching between devices has cost
- **Message Copying**: Message transformations can be expensive
- **Deep Delegation Chains**: Multiple delegations compound overhead

### 2. Optimization Strategies

Several strategies optimize delegation performance:

- **Delegation Caching**: Caching delegation results
- **Resolution Caching**: Caching resolution mappings
- **Message Reuse**: Reusing message structures where possible
- **Minimizing Delegation Depth**: Keeping delegation chains short

### 3. Composition Efficiency

Composition efficiency depends on several factors:

- **Composition Depth**: Deeper composition stacks have higher overhead
- **Component Granularity**: Fine-grained components have higher composition overhead
- **Message Size**: Larger messages increase composition cost
- **Composition Frequency**: Frequent recomposition increases overhead

## Examples

Let's examine concrete examples of delegation and composition from the codebase:

### Device Swapping in Process Device

From `dev_process.erl`:

```erlang
process_operation(<<"get">>, BaseMsg, Req, Opts) ->
    % Delegate to dev_process_cache
    hb_converge:resolve(BaseMsg, {as, <<"process-cache@1.0">>, Req}, Opts);

process_operation(<<"compute">>, BaseMsg, Req, Opts) ->
    % Get current slot
    case hb_converge:get(BaseMsg, <<"slot">>, undefined, Opts) of
        undefined -> {error, missing_slot};
        Slot ->
            % Get scheduler
            case get_scheduler(BaseMsg, Opts) of
                {ok, Scheduler} ->
                    % Verify slot assignment
                    VerifyReq = #{<<"action">> => <<"verify">>, <<"slot">> => Slot},
                    case hb_converge:resolve(BaseMsg, {as, Scheduler, VerifyReq}, Opts) of
                        {ok, Verified} ->
                            % Actually perform computation
                            perform_computation(Verified, Req, Opts);
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end
    end;
```

This example demonstrates:
- **Operation-Specific Delegation**: Different operations delegate to different devices
- **Multi-step Delegation**: Complex operations involve multiple delegation steps
- **Service Verification**: Services verify prerequisites before delegation
- **Composed Workflow**: Complete workflow emerges from multiple delegations

### Pipeline Composition in Meta Device

From `dev_meta.erl`:

```erlang
execute(Message, Opts) ->
    % Apply preprocessing pipeline
    case apply_pipeline(Message, preprocessing_pipeline(Message, Opts), Opts) of
        {ok, ProcessedMessage} ->
            % Process the message
            case process_message(ProcessedMessage, Opts) of
                {ok, Result} ->
                    % Apply postprocessing pipeline
                    apply_pipeline(Result, postprocessing_pipeline(Result, Opts), Opts);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.
```

This example demonstrates:
- **Pipeline Composition**: Processing is composed of multiple pipelines
- **Pre/Post Processing**: Cross-cutting concerns are applied through pipelines
- **Error Handling**: Errors short-circuit the pipeline
- **Three-Phase Processing**: Processing is divided into pre, main, and post phases

### Layer Composition in Storage

From the storage subsystem:

```erlang
put(Key, Value, Opts) ->
    % Get appropriate backend
    Backend = get_backend(Opts),
    % Store in persistent storage
    case Backend:put(Key, Value, Opts) of
        ok ->
            % Update cache
            hb_cache:put(Key, Value, Opts),
            ok;
        Error ->
            Error
    end.
```

This example demonstrates:
- **Layer Composition**: Storage consists of cache and persistent layers
- **Backend Selection**: Storage delegates to selected backend
- **Multi-level Storage**: Operations affect multiple storage levels
- **Cache Integration**: Cache is integrated with persistent storage

## Architectural Significance

Delegation and composition patterns are architecturally significant for several reasons:

### 1. Extensibility Model

These patterns form the core of HyperBEAM's extensibility model:

- **Plugin Architecture**: New devices can be added without modifying core code
- **Device Composition**: Complex behavior emerges from device composition
- **Extension Points**: Clear extension points for new functionality
- **Configuration-Driven Extension**: Extensions can be added through configuration

### 2. Modularity and Reuse

Delegation and composition enable modularity and reuse:

- **Separation of Concerns**: Each component handles specific concerns
- **Component Reuse**: Components can be reused in different contexts
- **Functional Decomposition**: Complex functionality is decomposed into simpler parts
- **Interface Consistency**: Consistent interfaces enable composition

### 3. System Evolution

These patterns facilitate system evolution:

- **Component Replacement**: Components can be replaced without affecting others
- **Gradual Enhancement**: System can be enhanced incrementally
- **Migration Support**: Multiple versions can coexist during migration
- **Backward Compatibility**: New components can interoperate with old ones

### 4. Security Architecture

These patterns are integral to the security architecture:

- **Security Checkpoints**: Composition boundaries provide security checkpoints
- **Trust Verification**: Delegation includes trust verification
- **Attestation Chain**: Cryptographic attestation is maintained through composition
- **Privilege Containment**: Delegation operates within privilege boundaries

## Conclusion

Delegation and composition are fundamental integration patterns in HyperBEAM that enable complex functionality to be built from simpler components while maintaining security, modularity, and extensibility. These patterns appear across subsystem boundaries, creating a coherent architectural approach to system integration.

The key principles revealed by this analysis include:

1. **Component Specialization**: Each component focuses on specific concerns
2. **Functional Composition**: Complex behavior emerges from component composition
3. **Delegation Transparency**: Delegation preserves context and error information
4. **Security Integration**: Security is integrated through composition boundaries
5. **Configuration-Driven Behavior**: Composition is largely driven by configuration

Understanding these patterns is essential for extending the system, diagnosing cross-subsystem issues, and maintaining architectural integrity as the system evolves. The consistent delegation and composition model, despite the diversity of subsystems and components, demonstrates the elegant architectural foundation that enables HyperBEAM's flexibility and extensibility.
