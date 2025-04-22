# Module: hb_http_client_sup

## Basic Information
- **Source File:** hb_http_client_sup.erl
- **Module Type:** Core Network & HTTP
- **Purpose:** Supervisor for the Gun HTTP client wrapper

## Purpose
Provides supervision for the HTTP client (hb_http_client) using OTP supervisor behavior. This module ensures proper lifecycle management and fault tolerance for the HTTP client process, with configurable shutdown timeouts based on build environment.

## Interface

### Core Operations
- `start_link/1` - Start supervisor with configuration options
- `init/1` - Initialize supervisor with child specifications

### Configuration
- `SHUTDOWN_TIMEOUT` - Configurable timeout (30s prod, 10s debug)
- `CHILD` macro - Child specification template

## Dependencies

### Direct Dependencies
- supervisor: OTP supervisor behavior
- hb_http_client: HTTP client worker

### Inverse Dependencies
- Used by HyperBEAM supervision tree
- Core HTTP client lifecycle management

## Implementation Details

### Key Concepts

1. **Supervision Strategy**
   ```erlang
   % One-for-one restart strategy with limits
   {ok, {{one_for_one, 5, 10}, [
       % Single HTTP client worker
       ?CHILD(hb_http_client, worker, Opts)
   ]}}
   ```

2. **Child Specification**
   ```erlang
   % Child specification macro
   -define(CHILD(I, Type, Opts), {
       I,                          % Child ID
       {I, start_link, Opts},      % Start function
       permanent,                  % Restart strategy
       ?SHUTDOWN_TIMEOUT,         % Shutdown timeout
       Type,                      % Process type
       [I]                        % Modules
   }).
   ```

3. **Environment Awareness**
   ```erlang
   % Debug vs Production timeout
   -ifdef(DEBUG).
   -define(SHUTDOWN_TIMEOUT, 10000).
   -else.
   -define(SHUTDOWN_TIMEOUT, 30000).
   -endif.
   ```

### State Management

1. **Supervisor State**
   - Child specifications
   - Restart strategy
   - Process monitoring
   - Error handling

2. **Child State**
   - Worker process
   - Configuration options
   - Lifecycle management
   - Error recovery

3. **System State**
   - Process registration
   - Resource management
   - Error handling
   - State recovery

### Error Handling

1. **Supervisor Errors**
   - Child process failures
   - Restart limits
   - Resource cleanup
   - State recovery

2. **Child Errors**
   - Process crashes
   - Shutdown handling
   - Resource cleanup
   - State verification

## Integration Points

1. **Supervision Tree**
   - Process hierarchy
   - Lifecycle management
   - Error propagation
   - State coordination

2. **HTTP Client**
   - Process supervision
   - Configuration management
   - Error handling
   - Resource cleanup

3. **System Integration**
   - Process registration
   - Resource management
   - Error handling
   - State coordination

## Analysis Insights

### Performance Considerations

1. **Process Management**
   - Restart strategies
   - Resource usage
   - State handling
   - Error recovery

2. **Configuration Impact**
   - Timeout settings
   - Restart limits
   - Resource allocation
   - Error handling

### Security Implications

1. **Process Isolation**
   - Error containment
   - Resource protection
   - State isolation
   - Access control

2. **System Protection**
   - Resource limits
   - Error boundaries
   - State protection
   - Process isolation

### Best Practices

1. **Supervision Strategy**
   - Use one_for_one for independent processes
   - Configure appropriate restart limits
   - Set proper shutdown timeouts
   - Handle errors gracefully

2. **Configuration Management**
   - Use environment-specific settings
   - Configure appropriate timeouts
   - Set resource limits
   - Handle errors properly

3. **Integration**
   - Follow OTP principles
   - Use proper process registration
   - Handle errors appropriately
   - Manage resources carefully

### Example Usage

```erlang
% Start supervisor with configuration
{ok, Pid} = hb_http_client_sup:start_link(#{
    http_connect_timeout => 5000,
    http_request_send_timeout => 30000,
    http_keepalive => 60000
}),

% Supervisor automatically manages HTTP client:
% - Starts hb_http_client worker
% - Monitors process for failures
% - Restarts on crashes (up to 5 times in 10 seconds)
% - Ensures proper shutdown with timeout
% - Maintains process registration
