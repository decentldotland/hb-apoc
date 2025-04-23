# Module: hb_process_monitor

## Basic Information
- **Source File:** hb_process_monitor.erl
- **Module Type:** Core Process Management
- **Purpose:** Process monitoring and cron-based task execution

## Purpose
Provides a monitoring system for processes with periodic task execution capabilities. The module creates a monitor process that periodically checks and executes cron tasks for a given process ID, with configurable rates and cursor-based pagination support.

## Interface

### Core Operations
- `start/1,2,3` - Start monitor with various configuration options
- `stop/1` - Stop a running monitor
- `server/1` - Internal server loop
- `handle_crons/1` - Process cron tasks

## Dependencies

### Direct Dependencies
- hb_opts: Configuration management
- hb_client: Cron task management
- hb_logger: Logging functionality
- dev_mu: Message pushing
- timer: Erlang timer functionality

### Inverse Dependencies
- Used by process management system
- Used by task scheduling system
- Core monitoring functionality provider

## Implementation Details

### Key Concepts

1. **Monitor State**
   ```erlang
   -record(state, {
       proc_id,    % Process ID being monitored
       cursor,     % Pagination cursor for cron tasks
       logger      % Logger process reference
   }).
   ```

2. **Process Monitoring**
   ```erlang
   % Start monitor with default rate
   start(ProcID) ->
       start(ProcID, hb_opts:get(default_cron_rate)).
   
   % Start monitor with custom rate
   start(ProcID, Rate, Cursor) ->
       Logger = hb_logger:start(),
       Monitor = spawn(fun() -> server(#state{...}) end),
       Ticker = spawn(fun() -> ticker(Monitor, Rate) end),
       {Monitor, Logger}
   ```

3. **Task Execution**
   ```erlang
   % Handle cron tasks with pagination
   handle_crons(State) ->
       case hb_client:cron(State#state.proc_id, State#state.cursor) of
           {ok, HasNextPage, Results, Cursor} ->
               lists:map(fun(Res) -> 
                   dev_mu:push(#{ message => Res }, State)
               end, Results),
               NS = State#state{cursor = Cursor},
               case HasNextPage of
                   true -> NS;
                   false -> handle_crons(NS)
               end;
           Error ->
               hb_logger:log(State#state.logger, Error),
               State
       end
   ```

### State Management

1. **Monitor State**
   - Process tracking
   - Cursor management
   - Logger reference
   - Error handling

2. **Task State**
   - Cron execution
   - Result handling
   - Pagination state
   - Error recovery

3. **System State**
   - Process registration
   - Timer management
   - Resource tracking
   - Error handling

### Error Handling

1. **Monitor Errors**
   - Process failures
   - Timer errors
   - State recovery
   - Resource cleanup

2. **Task Errors**
   - Execution failures
   - State recovery
   - Resource cleanup
   - Error logging

## Integration Points

1. **Process System**
   - Process monitoring
   - State tracking
   - Error handling
   - Resource management

2. **Task System**
   - Cron execution
   - Result handling
   - State management
   - Error recovery

3. **Logging System**
   - Error logging
   - State tracking
   - Event recording
   - Resource monitoring

## Analysis Insights

### Performance Considerations

1. **Process Management**
   - Monitor overhead
   - Timer accuracy
   - Resource usage
   - State tracking

2. **Task Execution**
   - Cron scheduling
   - Result handling
   - Resource usage
   - Error recovery

### Security Implications

1. **Process Isolation**
   - Monitor separation
   - Resource protection
   - State isolation
   - Error containment

2. **Task Security**
   - Execution isolation
   - Resource limits
   - State protection
   - Error handling

### Best Practices

1. **Monitor Usage**
   - Configure appropriate rates
   - Handle errors properly
   - Manage resources
   - Track state

2. **Task Management**
   - Handle pagination
   - Process results
   - Manage resources
   - Handle errors

3. **Integration**
   - Use proper logging
   - Handle errors
   - Manage state
   - Clean resources

### Example Usage

```erlang
% Start monitor with default rate
{Monitor, Logger} = hb_process_monitor:start(ProcID),

% Start monitor with custom rate
{Monitor, Logger} = hb_process_monitor:start(ProcID, 5000),

% Start monitor with cursor
{Monitor, Logger} = hb_process_monitor:start(
    ProcID,
    5000,
    <<"initial_cursor">>
),

% Stop monitor
hb_process_monitor:stop(Monitor)
