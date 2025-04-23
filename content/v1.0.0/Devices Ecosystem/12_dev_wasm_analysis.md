# WebAssembly Runtime Analysis (`dev_wasm.erl` and `dev_wasi.erl`)

## Overview

HyperBEAM's WebAssembly runtime implementation provides a sophisticated sandboxed execution environment for WebAssembly modules within the HyperBEAM ecosystem. With 3 downstream dependents, this subsystem serves as a critical extensibility mechanism, enabling the safe execution of user-provided code in a contained environment while maintaining integration with HyperBEAM's message-based architecture.

The implementation is divided into two primary modules:

1. `dev_wasm.erl`: The core WebAssembly execution engine that manages module loading, function invocation, state management, and integration with the broader HyperBEAM message system.

2. `dev_wasi.erl`: The WebAssembly System Interface (WASI) implementation that provides virtualized system capabilities like filesystem access and standard I/O operations to WebAssembly modules.

Together, these modules create a flexible yet secure environment for executing WebAssembly code, leveraging the sandboxed nature of WebAssembly while providing controlled access to system resources through WASI. The implementation utilizes `beamr`, an Erlang wrapper for WAMR (WebAssembly Micro Runtime), as the underlying execution engine.

## Key Characteristics

- **Message-Based Integration**: Integrates WebAssembly execution seamlessly with HyperBEAM's message system
- **Stateful Execution**: Maintains WebAssembly module state across invocations
- **State Serialization**: Supports serializing and deserializing WebAssembly state for persistence
- **WASI Support**: Implements the WebAssembly System Interface for virtualized system access
- **Virtual Filesystem**: Provides a message-based virtual filesystem for WebAssembly modules
- **Import Resolution**: Supports dynamic resolution of imported functions
- **Standard Library Integration**: Enables extension through a standard library interface
- **Memory-64 Support**: Handles both standard WebAssembly and Memory-64 preview standard
- **Device-Oriented Design**: Follows HyperBEAM's device pattern for consistent interface
- **AOT Compilation**: Optional ahead-of-time compilation for performance optimization
- **Prefixing Support**: Flexible input/output path prefixing for integration with other devices
- **Snapshot Capabilities**: State snapshot and restoration functionality
- **Comprehensive Testing**: Extensive test suite for functionality verification

## Dependencies

### Library Dependencies
- `beamr`: Erlang wrapper for the WebAssembly Micro Runtime (WAMR)
- `jiffy`: JSON encoding/decoding (used in tests for AOS integration)

### Upstream Dependencies
- `hb_converge`: For message resolution and field access
- `hb_private`: For accessing private message fields
- `hb_cache`: For reading/writing WebAssembly modules
- `hb_beamr`: HyperBEAM's interface to the BEAMR library
- `dev_stack`: For handling device stacking and prefix management
- `dev_message`: For message manipulation and validation

## Implementation Details: `dev_wasm.erl`

### Module Initialization

The module initializes a WebAssembly environment from a provided image:

```erlang
init(M1, M2, Opts) ->
    ?event(running_init),
    % Where we should read initial parameters from.
    InPrefix = dev_stack:input_prefix(M1, M2, Opts),
    % Where we should read/write our own state to.
    Prefix = dev_stack:prefix(M1, M2, Opts),
    ?event({in_prefix, InPrefix}),
    ImageBin = get_image_binary_from_various_sources(),
    % Start the WASM executor.
    {ok, Instance, _Imports, _Exports} = hb_beamr:start(ImageBin, Mode),
    % Set the WASM Instance, handler, and standard library invokation function.
    {ok,
        hb_private:set(M1,
            #{
                <<Prefix/binary, "/instance">> => Instance,
                <<Prefix/binary, "/import-resolver">> =>
                    fun default_import_resolver/3
            },
            Opts
        )
    }.
```

The initialization process:
1. Determines input and output prefixes for path resolution
2. Obtains the WebAssembly binary from various possible sources (direct binary, message field, or cached image)
3. Configures execution mode (standard or AOT)
4. Initializes the WebAssembly runtime instance
5. Sets up import resolution functionality
6. Stores the instance and resolver in private message fields

### Function Execution

The module executes WebAssembly functions with provided parameters:

```erlang
compute(RawM1, M2, Opts) ->
    % Normalize the message to have an open WASM instance
    {ok, M1} = normalize(RawM1, M2, Opts),
    ?event(running_compute),
    Prefix = dev_stack:prefix(M1, M2, Opts),
    case hb_converge:get(pass, M1, Opts) of
        X when X == 1 orelse X == not_found ->
            % Extract function and parameters
            WASMFunction = get_function_from_various_sources(),
            WASMParams = get_params_from_various_sources(),
            case WASMFunction of
                not_found -> {ok, M1};
                _ ->
                    % Execute the function
                    {ResType, Res, MsgAfterExecution} =
                        hb_beamr:call(
                            instance(M1, M2, Opts),
                            WASMFunction,
                            WASMParams,
                            import_resolver,
                            M1,
                            Opts
                        ),
                    % Store results
                    {ok,
                        hb_converge:set(MsgAfterExecution,
                            #{
                                <<"results/", Prefix/binary, "/type">> => ResType,
                                <<"results/", Prefix/binary, "/output">> => Res
                            }
                        )
                    }
            end;
        _ -> {ok, M1}
    end.
```

The execution process:
1. Normalizes the message to ensure a valid WebAssembly instance
2. Extracts the function name and parameters from various possible sources
3. Calls the WebAssembly function through the BEAMR interface
4. Stores the result and result type in the message

### State Management

The module supports state serialization and deserialization:

```erlang
snapshot(M1, M2, Opts) ->
    ?event(snapshot, generating_snapshot),
    Instance = instance(M1, M2, Opts),
    {ok, Serialized} = hb_beamr:serialize(Instance),
    {ok,
        #{
            <<"body">> => Serialized
        }
    }.

normalize(RawM1, M2, Opts) ->
    case instance(RawM1, M2, Opts) of
        not_found ->
            Memory = get_snapshot_from_message(),
            case Memory of
                not_found -> throw({error, no_wasm_instance_or_snapshot});
                State ->
                    {ok, M1} = init(RawM1, State, Opts),
                    Res = hb_beamr:deserialize(instance(M1, M2, Opts), State),
                    M1
            end;
        _ -> RawM1
    end,
    dev_message:set(M3, #{ <<"snapshot">> => unset }, Opts).
```

The state management includes:
1. Serializing WebAssembly instance state to a binary
2. Deserializing state when restoring from a snapshot
3. Initializing a new instance when needed
4. Normalizing messages to handle both direct instances and state restoration

### Import Resolution

The module implements dynamic import resolution:

```erlang
import(Msg1, Msg2, Opts) ->
    % 1. Adjust the path to the stdlib.
    ModName = hb_converge:get(<<"module">>, Msg2, Opts),
    FuncName = hb_converge:get(<<"func">>, Msg2, Opts),
    Prefix = dev_stack:prefix(Msg1, Msg2, Opts),
    AdjustedPath = build_stdlib_path(),
    StatePath = build_state_path(),
    
    % 2. Add state to message and resolve
    AdjustedMsg1 = add_state_to_message(),
    case hb_converge:resolve(AdjustedMsg1, AdjustedMsg2, Opts) of
        {ok, Res} -> {ok, Res};
        {error, not_found} ->
            undefined_import_stub(Msg1, Msg2, Opts)
    end.
```

The import resolution:
1. Extracts the import module and function name
2. Builds paths for standard library resolution
3. Attempts to resolve the function through HyperBEAM's converge resolution
4. Falls back to a stub handler for undefined imports

## Implementation Details: `dev_wasi.erl`

### Virtual Filesystem Initialization

The module initializes a virtual filesystem with standard IO devices:

```erlang
init(M1, _M2, Opts) ->
    ?event(running_init),
    MsgWithLib =
        hb_converge:set(
            M1,
            #{
                <<"wasm/stdlib/wasi_snapshot_preview1">> =>
                    #{ <<"device">> => <<"WASI@1.0">>}
            },
            Opts
        ),
    MsgWithFDs =
        hb_converge:set(
            MsgWithLib,
            <<"file-descriptors">>,
            ?INIT_FDS,
            Opts
        ),
    CompleteMsg =
        hb_converge:set(
            MsgWithFDs,
            <<"vfs">>,
            ?INIT_VFS,
            Opts
        ),
    {ok, CompleteMsg}.
```

The initialization creates:
1. A virtual filesystem structure with stdin, stdout, and stderr
2. File descriptors for standard I/O
3. Registration of WASI standard library functions

### WASI Function Implementation

The module implements WASI API functions:

```erlang
fd_write(Msg1, Msg2, Opts) ->
    State = hb_converge:get(<<"state">>, Msg1, Opts),
    Instance = hb_private:get(<<"wasm/instance">>, State, Opts),
    [FD, Ptr, Vecs, RetPtr|_] = hb_converge:get(<<"args">>, Msg2, Opts),
    
    % Implementation details omitted for brevity
    
    {ok, #{ <<"state">> => S, <<"results">> => [0] }}.
```

The implementations include:
1. File descriptor operations (path_open, fd_read, fd_write)
2. Clock operations (clock_time_get)
3. Memory management for I/O buffers
4. Virtual filesystem manipulation

## Integration with HyperBEAM

### Integration with Message System

The WebAssembly runtime integrates with HyperBEAM's message system in several ways:

1. **Device Pattern**: Both modules follow HyperBEAM's device pattern, implementing standardized interfaces like `init/3` and `compute/3`.

2. **Message-Based State**: WebAssembly state is stored within HyperBEAM messages, allowing for seamless integration with the broader system.

3. **Content-Addressable Storage**: WebAssembly modules can be stored in HyperBEAM's content-addressable cache for efficient retrieval.

4. **Path-Based Access**: The virtual filesystem is implemented as a hierarchical message structure, accessible through path-based resolution.

5. **Import Resolution**: WebAssembly imports are resolved through HyperBEAM's message resolution system, enabling flexible extension.

### Integration with Device System

The WebAssembly runtime integrates well with HyperBEAM's device system:

1. **Device Stacking**: Supports the device stacking pattern, with examples in tests showing WASI and WASM stacked for full functionality.

2. **Prefix Management**: Uses `dev_stack` for managing input and output prefixes, enabling flexible path management.

3. **Device Registration**: The `info/2` function exposes the supported operations while excluding internal functions.

4. **Message Transformation**: Follows HyperBEAM's message transformation patterns, creating, modifying, and returning messages according to device conventions.

## Performance Considerations

The WebAssembly runtime incorporates several performance optimizations:

1. **AOT Compilation**: Optional ahead-of-time compilation for performance-critical modules (when allowed by configuration).

2. **State Preservation**: Maintains WebAssembly instance state across invocations, avoiding expensive reinitialization.

3. **Efficient I/O**: The WASI implementation directly interfaces with WebAssembly memory for efficient I/O operations.

4. **Cached Modules**: Uses HyperBEAM's cache for WebAssembly module storage, enabling efficient retrieval.

5. **Benchmark Testing**: Includes benchmark tests to evaluate and monitor performance.

## Security Considerations

The WebAssembly runtime implements several security measures:

1. **Sandboxed Execution**: Leverages WebAssembly's inherent sandboxing to isolate execution.

2. **Controlled Imports**: Implements a controlled import resolution mechanism to restrict module capabilities.

3. **Virtual Resources**: Provides virtualized system resources through WASI rather than direct system access.

4. **Configuration Control**: The `wasm_allow_aot` configuration flag controls whether AOT compilation is permitted, providing a security control point.

5. **Stub Handling**: Unimplemented imports are handled with stubs, preventing crashes from undefined functions.

## Extensibility Mechanisms

The WebAssembly runtime offers several extensibility points:

1. **Standard Library Integration**: The stdlib path mechanism allows extending available functionality.

2. **Import Resolution**: The import resolver can be extended or replaced for custom import handling.

3. **Virtual Filesystem**: The message-based virtual filesystem can be populated with custom content.

4. **Multiple Module Support**: The content-addressable storage of modules enables referring to multiple modules.

5. **State Serialization**: The snapshot and restore mechanism enables complex stateful processing across sessions.

## Testing Approach

The WebAssembly runtime includes comprehensive testing:

1. **Functional Testing**: Tests basic execution, import resolution, and error handling.

2. **WASI Testing**: Verifies virtual filesystem operations and WASI standard library functions.

3. **Serialization Testing**: Ensures state can be correctly serialized and deserialized.

4. **Edge Cases**: Tests error conditions and unusual input formats.

5. **Performance Benchmarking**: Measures execution speed and efficiency.

6. **AOS Integration**: Tests integration with AOS (presumably an application running on WebAssembly).

## Observations and Insights

### Strengths

1. **Integration Design**: The WebAssembly runtime is exceptionally well-integrated with HyperBEAM's message and device systems, enabling seamless operation within the broader ecosystem.

2. **Flexible State Management**: The state serialization and restoration capabilities provide powerful options for stateful processing across sessions.

3. **Virtual Resource Management**: The WASI implementation provides a clean abstraction for system resources without exposing the underlying system.

4. **Multiple Entry Points**: The design supports various ways to provide WebAssembly modules and parameters, enhancing flexibility.

5. **Error Resilience**: Careful error handling and fallbacks enhance system reliability even when user-provided code has issues.

### Limitations

1. **Limited WASI Implementation**: The WASI implementation appears to cover only a subset of the full WASI API, potentially limiting compatibility with some WebAssembly modules.

2. **Implicit Dependencies**: The relationship between `dev_wasm.erl` and `dev_wasi.erl` is somewhat implicit, requiring careful coordination when using them together.

3. **Serialization Complexity**: The state serialization approach, while powerful, introduces complexity in managing WebAssembly state across sessions.

4. **Performance Overhead**: The message-based virtual filesystem likely introduces some performance overhead compared to more direct implementations.

5. **Documentation Gaps**: While the code includes comments, more comprehensive documentation about WebAssembly module requirements and limitations would be beneficial.

### Future Opportunities

1. **Extended WASI Support**: Expanding the WASI implementation to cover more of the standard would enhance compatibility.

2. **Performance Optimization**: Further optimization of the memory and I/O operations could improve performance for I/O-intensive applications.

3. **Structured Error Handling**: More structured error reporting from WebAssembly execution could aid in debugging and resilience.

4. **Module Validation**: Additional validation of WebAssembly modules before execution could enhance security and reliability.

5. **Memory Management Controls**: Introducing limits and monitoring for WebAssembly memory usage could prevent resource exhaustion.

## Connection with `dev_genesis_wasm.erl`

The WebAssembly runtime has an important connection with the `dev_genesis_wasm.erl` module, which serves as a compatibility layer for running "legacynet" Autonomous Object (AO) processes within HyperBEAM. This connection represents a practical application of the WebAssembly capabilities for maintaining compatibility with existing systems.

### `dev_genesis_wasm.erl` Overview

`dev_genesis_wasm.erl` is a specialized device that creates an environment for legacy AO processes to run within HyperBEAM. While its implementation is minimal, its role is significant:

```erlang
compute(Msg, Msg2, Opts) ->
    case hb_converge:resolve(Msg, {as, <<"delegated-compute@1.0">>, Msg2}, Opts) of
        {ok, Msg3} ->
            {ok, Msg4} = hb_converge:resolve(Msg3, {as, <<"patch@1.0">>, Msg2}, Opts),
            {ok, Msg4};
        {error, Error} ->
            {error, Error}
    end.
```

The module works by:
1. Delegating computation to the `delegated-compute@1.0` device (likely WebAssembly-based)
2. Applying any state patches using the `patch@1.0` device
3. Providing the necessary environment and state lifecycle for AO processes

### Integration Points

The connection between `dev_genesis_wasm.erl` and the WebAssembly runtime appears to involve these key integration points:

1. **Execution Delegation**: `dev_genesis_wasm.erl` likely delegates WebAssembly execution to the core WebAssembly runtime.

2. **State Management**: The lifecycle operations (init, compute, normalize, snapshot) in `dev_genesis_wasm.erl` parallel those in `dev_wasm.erl`, suggesting aligned state management approaches.

3. **Legacy Compatibility**: The WebAssembly runtime likely provides the execution environment needed by legacy AO processes, while `dev_genesis_wasm.erl` provides the necessary adaptation layer.

4. **Virtual Environment**: The WASI implementation in `dev_wasi.erl` likely provides the filesystem and I/O capabilities expected by the legacy processes.

### AOS Integration

The relationship is further reinforced by the presence of AOS (Arweave Operating System) testing in `dev_wasi.erl`:

```erlang
basic_aos_exec_test() ->
    Init = generate_wasi_stack("test/aos-2-pure-xs.wasm", <<"handle">>, []),
    Msg = gen_test_aos_msg("return 1 + 1"),
    Env = gen_test_env(),
    % ... test execution ...
```

This suggests that the WebAssembly runtime, particularly with WASI support, is designed to support AOS-based applications, which aligns with the purpose of `dev_genesis_wasm.erl` to support legacy AO processes.

### Architectural Implications

This connection has several architectural implications:

1. **Layered Execution**: The system uses a layered approach to WebAssembly execution, with `dev_genesis_wasm.erl` providing a higher-level interface tailored to specific needs.

2. **Compatibility Strategy**: HyperBEAM uses WebAssembly as a strategy for maintaining compatibility with legacy components, leveraging its sandboxed execution capabilities.

3. **Device Composition**: The approach demonstrates the compositional nature of HyperBEAM's device system, where specialized devices can leverage more general-purpose devices.

4. **Evolutionary Path**: This suggests an evolutionary path where legacy components can be integrated while the system evolves, potentially allowing for future migration.

## Conclusion

The WebAssembly runtime in HyperBEAM represents a sophisticated system that successfully bridges the gap between the WebAssembly ecosystem and HyperBEAM's message-based architecture. By implementing both the core execution environment and the system interface layer, it provides a complete solution for safely executing WebAssembly code within HyperBEAM.

The implementation demonstrates thoughtful design in its state management, integration patterns, and security considerations. Its connection with `dev_genesis_wasm.erl` highlights the practical application of WebAssembly for maintaining compatibility with legacy systems, demonstrating how HyperBEAM leverages WebAssembly to balance innovation with backward compatibility.

While there are opportunities for enhancement in areas like WASI coverage and performance optimization, the current implementation provides a solid foundation for WebAssembly-based extensibility within HyperBEAM, supporting both new applications and legacy systems.
