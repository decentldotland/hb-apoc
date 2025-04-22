# Module: hb_persistent

## Basic Information
- **Source File:** hb_persistent.erl
- **Module Type:** Core Storage & Caching
- **Purpose:** Management of long-lived AO-Core resolution processes

## Purpose
Creates and manages persistent processes for AO-Core resolution, particularly useful for large messages that are expensive to serialize/deserialize or when executions need to be serialized to avoid parallel computation. Uses Erlang's `pg` module for distributed process group management.

## Interface

### Process Management
- `start_monitor/0,1` - Start process group monitor
- `stop_monitor/1` - Stop process group monitor
- `start_worker/2,3` - Start persistent worker process

### Registration Operations
- `find_or_register/3` - Find or register as execution leader
- `unregister_notify/4` - Unregister and notify waiters
- `await/4` - Wait for execution result
- `notify/4` - Notify waiting processes

### Group Operations
- `group/3` - Calculate group name for messages
- `forward_work/2` - Forward requests to new process
- `default_grouper/3` - Default group name calculator
- `default_worker/3` - Default worker implementation

## Dependencies

### Direct Dependencies
- pg: Process group management
- hb_name: Name registration
- hb_ao: Core resolution
- hb_opts: Options handling

### Inverse Dependencies
- Used by resolution system
- Performance optimization
- State management

## Implementation Details

### Key Concepts

1. **Process Groups**
   ```erlang
   % Register process in group
   register_groupname(GroupName, Opts) ->
       hb_name:register(GroupName)
   
   % Find existing process
   find_execution(GroupName, _Opts) ->
       hb_name:lookup(GroupName)
   ```

2. **Worker Management**
   ```erlang
   % Start worker process
   start_worker(GroupName, Msg, Opts) ->
       spawn(fun() ->
           register_groupname(GroupName, Opts),
           WorkerFun(GroupName, Msg, Opts)
       end)
   ```

3. **Message Resolution**
   ```erlang
   % Default worker behavior
   default_worker(GroupName, Msg1, Opts) ->
       receive
           {resolve, Listener, GroupName, Msg2, ListenerOpts} ->
               Res = hb_ao:resolve(Msg1, Msg2, Opts),
               send_response(Listener, GroupName, Msg2, Res)
       end
   ```

### State Management

1. **Process State**
   - Group registration
   - Worker lifecycle
   - Message handling
   - Timeout management

2. **Group State**
   - Process tracking
   - Name resolution
   - State persistence
   - Error handling

3. **Message State**
   - Resolution tracking
   - Result caching
   - State updates
   - Error recovery

### Error Handling

1. **Process Errors**
   - Worker crashes
   - Timeout handling
   - State recovery
   - Group cleanup

2. **Resolution Errors**
   - Message validation
   - State verification
   - Error propagation
   - Recovery strategies

## Integration Points

1. **Process System**
   - Group management
   - Worker lifecycle
   - Message passing
   - State tracking

2. **Resolution System**
   - Message handling
   - State management
   - Result caching
   - Error handling

3. **Name System**
   - Process registration
   - Group management
   - Name resolution
   - State tracking

## Analysis Insights

### Performance Considerations

1. **Process Management**
   - Group efficiency
   - Worker lifecycle
   - Message passing
   - State handling

2. **Resolution Optimization**
   - Result caching
   - State persistence
   - Message deduplication
   - Resource management

### Security Implications

1. **Process Safety**
   - Group isolation
   - Message validation
   - State protection
   - Error handling

2. **State Protection**
   - Process isolation
   - Group security
   - Message validation
   - Error containment

### Best Practices

1. **Worker Management**
   - Handle timeouts
   - Validate messages
   - Track state
   - Clean up resources

2. **Group Handling**
   - Manage registration
   - Handle errors
   - Track processes
   - Clean up state

3. **Error Management**
   - Handle crashes
   - Recover state
   - Propagate errors
   - Clean up resources

### Example Usage

```erlang
% Start a persistent worker
Msg1 = #{<<"device">> => Device},
WorkerPid = hb_persistent:start_worker(Msg1, #{
    static_worker => true,
    worker_timeout => 10000
}),

% Find or register for execution
case hb_persistent:find_or_register(Msg1, Msg2, Opts) of
    {leader, GroupName} ->
        % Handle execution
        Result = handle_execution(Msg1, Msg2),
        hb_persistent:unregister_notify(GroupName, Msg2, Result, Opts);
    {wait, Leader} ->
        % Wait for result
        hb_persistent:await(Leader, Msg1, Msg2, Opts)
end
