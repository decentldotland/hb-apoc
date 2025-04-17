# `dev_scheduler_registry.erl` Analysis

## Overview

`dev_scheduler_registry.erl` is a registry module for the HyperBEAM system that manages the lifecycle and discovery of scheduler processes. As described in its documentation, it serves as "a simple registry for local services in AO, using pg," with a current focus on scheduler processes (referred to as "SU processes" in the code comments).

This module provides a centralized mechanism for mapping between process IDs (which are typically content-addressed identifiers) and the actual Erlang processes that handle these identifiers. It also offers process creation capabilities when requested processes don't exist, making it both a registry and a factory for scheduler processes.

The registry uses the `hb_name` service, which provides a distributed naming capability, enabling processes to be located across the system. This is essential for the distributed nature of HyperBEAM operations.

## Key Characteristics

- **Process Registration**: Maps between process IDs and Erlang PIDs
- **Process Discovery**: Enables lookup of running scheduler processes
- **Process Creation**: Can optionally create processes that don't exist
- **Wallet Management**: Provides access to the wallet used for scheduler operations
- **Process Enumeration**: Supports listing all registered processes
- **Naming Service Integration**: Uses `hb_name` for distributed process registration

## Dependencies

### Upstream Dependencies

- `hb_name`: For process registration and lookup
- `hb`: For wallet access
- `dev_scheduler_server`: For starting new scheduler processes

## Implementation Details

### Registry Initialization

The module initializes the registry system by starting the naming service:

```erlang
start() ->
    hb_name:start(),
    ok.
```

This ensures that the underlying name service is available for process registration and lookup.

### Process Lookup

The module provides a set of overloaded `find` functions for looking up processes:

```erlang
find(ProcID) -> find(ProcID, false).
find(ProcID, GenIfNotHosted) ->
    find(ProcID, GenIfNotHosted, #{ priv_wallet => hb:wallet() }).
find(ProcID, GenIfNotHosted, Opts) ->
    case hb_name:lookup({dev_scheduler, ProcID}) of
        undefined -> maybe_new_proc(ProcID, GenIfNotHosted, Opts);
        Pid -> Pid
    end.
```

These functions attempt to find a process by its ID, with the option to create a new process if one doesn't exist. The `GenIfNotHosted` parameter controls whether a new process should be created when not found:
- When `GenIfNotHosted` is `false`, the function returns `not_found` for non-existent processes
- When `GenIfNotHosted` is `true`, it creates a new process via `dev_scheduler_server:start/2`

The lookup is performed using the `hb_name:lookup/1` function, with process IDs prefixed with `dev_scheduler` to create a namespaced identifier.

### Process Creation

The `maybe_new_proc/3` function handles the conditional creation of new processes:

```erlang
maybe_new_proc(_ProcID, false, _Opts) -> not_found;
maybe_new_proc(ProcID, _GenIfNotHosted, Opts) -> 
    dev_scheduler_server:start(ProcID, Opts).
```

This function either returns `not_found` or delegates to `dev_scheduler_server:start/2` to create a new scheduler process for the given ID.

### Process Enumeration

The `get_processes/0` function retrieves a list of all registered process IDs:

```erlang
get_processes() ->
    ?event({getting_processes, hb_name:all()}),
    [ ProcID || {{dev_scheduler, ProcID}, _} <- hb_name:all() ].
```

This function filters the complete set of registered names from `hb_name:all()` to extract only those that are prefixed with `dev_scheduler`, returning just the process IDs.

### Wallet Access

The `get_wallet/0` function provides access to the wallet used for authentication:

```erlang
get_wallet() ->
    % TODO: We might want to use a different wallet per SU later.
    hb:wallet().
```

As noted in the code comment, there's a consideration for potentially using different wallets per scheduler unit in the future.

## Questions and Insights

### Questions

1. **Scalability Considerations**: How does the registry handle scenarios with a large number of processes? Are there any performance bottlenecks in the lookup or enumeration operations?

2. **Wallet Isolation**: The comment about potentially using different wallets per scheduler unit suggests a security or isolation consideration. What are the implications of using a single wallet versus multiple wallets?

3. **Process Lifecycle Management**: How are process terminations handled in the registry? Is there a mechanism to clean up entries for processes that have ended?

4. **Distributed Registry**: How does the registry behave in a distributed environment with multiple nodes? How is consistency maintained across nodes?

5. **Lookup Performance**: Are there any optimizations for frequent lookups of the same process ID? Is there any caching mechanism?

### Insights

1. **Centralized Discovery**: The registry provides a centralized point of discovery for scheduler processes, simplifying the interaction model for other components that need to work with these processes.

2. **Factory Pattern**: The module implements a factory pattern through its conditional process creation capability, combining discovery and creation in a convenient interface.

3. **Simple Interface**: The API is designed to be straightforward, with sensible defaults and overloaded functions to accommodate different use cases.

4. **Naming Convention**: The use of a consistent prefix (`dev_scheduler`) for registered processes creates a namespace that allows for easy filtering and organization.

5. **Testing Focus**: The comprehensive test suite indicates a focus on reliability, covering different scenarios including non-existent processes, process creation, and enumeration.

## Integration with Other Subsystems

### Integration with Device and Process Management Subsystem

- Serves as a critical component for the scheduler subsystem, enabling process discovery and creation
- Works closely with `dev_scheduler_server` to instantiate new scheduler processes
- Provides the wallet access needed for scheduler operations

### Integration with Core Infrastructure

- Uses `hb_name` for process registration and lookup
- Relies on `hb` for wallet access

## Recategorization Considerations

This module is correctly categorized as part of the Device and Process Management Subsystem. Its primary responsibility is managing the lifecycle and discovery of scheduler processes, which is a fundamental aspect of process management.

The module's tight integration with `dev_scheduler_server` and its focus on process mapping and creation align it firmly with the process management domain. While it depends on core infrastructure components like `hb_name`, its purpose is specifically to support the process management aspects of the system.

The registry pattern implemented by this module is a common design pattern in distributed systems, particularly for service discovery and lifecycle management. This further reinforces its categorization as a process management component rather than an infrastructure or utility component.
