# Module: hb_event

## Basic Information
- **Source File:** hb_event.erl
- **Module Type:** Core Process Management
- **Purpose:** Event logging and Prometheus counter management

## Purpose
Provides a unified interface for event logging and metric collection in HyperBEAM. The module handles both debug logging and Prometheus counter increments, with configurable behavior based on module attributes and runtime options. It also includes tracing capabilities for specific topics.

## Interface

### Core Operations
- `log/1,2,3,4,5,6` - Multi-arity logging functions with varying levels of detail
- `increment/3` - Increment Prometheus counters for specific topics
- `handle_tracer/3` - Handle trace recording for specific topics
- `parse_name/1` - Convert various types to binary names

## Dependencies

### Direct Dependencies
- prometheus_counter: Metric counters
- hb_util: Debug printing
- hb_opts: Configuration options
- hb_name: Process registration
- hb_tracer: Event tracing

### Inverse Dependencies
- Used by system components for logging
- Used by monitoring systems
- Core event tracking provider

## Implementation Details

### Key Concepts

1. **Event Logging**
   ```erlang
   % Multi-arity logging with defaults
   log(X) -> log(global, X).
   log(Topic, X) -> log(Topic, X, "").
   log(Topic, X, Mod) -> log(Topic, X, Mod, undefined).
   log(Topic, X, Mod, Func) -> log(Topic, X, Mod, Func, undefined).
   log(Topic, X, Mod, Func, Line) -> log(Topic, X, Mod, Func, Line, #{}).
   ```

2. **Debug Control**
   ```erlang
   % Module attribute based control
   case lists:member({hb_debug, [print]}, ModAtom:module_info(attributes)) of
       true -> hb_util:debug_print(X, atom_to_list(ModAtom), Func, Line);
       false -> 
           case lists:keyfind(hb_debug, 1, ModAtom:module_info(attributes)) of
               {hb_debug, [no_print]} -> X;
               _ -> log(Topic, X, atom_to_list(ModAtom), Func, Line, Opts)
           end
   end
   ```

3. **Prometheus Integration**
   ```erlang
   % Counter declaration
   prometheus_counter:declare([
       {name, <<"event">>},
       {help, <<"AO-Core execution events">>},
       {labels, [topic, event]}
   ])
   ```

### State Management

1. **Event State**
   - Topic tracking
   - Counter management
   - Queue monitoring
   - Error handling

2. **Debug State**
   - Print control
   - Module attributes
   - Option handling
   - Error tracking

3. **Trace State**
   - Topic filtering
   - Step recording
   - Error handling
   - Resource cleanup

### Error Handling

1. **Queue Overload**
   ```erlang
   case erlang:process_info(self(), message_queue_len) of
       {message_queue_len, Len} when Len > ?OVERLOAD_QUEUE_LENGTH ->
           ?debug_print({warning, prometheus_event_queue_overloading,
               {queue, Len}, {current_message, EventName}});
       _ -> ignored
   end
   ```

2. **Startup Handling**
   ```erlang
   await_prometheus_started() ->
       receive
           Msg ->
               case application:get_application(prometheus) of
                   undefined -> await_prometheus_started();
                   _ -> self() ! Msg, ok
               end
       end
   ```

## Integration Points

1. **Prometheus System**
   - Counter registration
   - Metric increments
   - Label management
   - Error handling

2. **Debug System**
   - Print control
   - Module attributes
   - Option handling
   - Error tracking

3. **Trace System**
   - Topic filtering
   - Step recording
   - Error handling
   - Resource cleanup

## Analysis Insights

### Performance Considerations

1. **Queue Management**
   - Overload detection
   - Message handling
   - Resource usage
   - Error recovery

2. **Counter Impact**
   - Increment timing
   - Label handling
   - Resource usage
   - Error handling

### Security Implications

1. **Event Privacy**
   - Topic filtering
   - Debug control
   - State protection
   - Error isolation

2. **Resource Protection**
   - Queue limits
   - Counter protection
   - State security
   - Error handling

### Best Practices

1. **Event Usage**
   - Use appropriate topics
   - Handle debug properly
   - Manage resources
   - Handle errors

2. **Counter Management**
   - Monitor queue size
   - Handle overload
   - Clean resources
   - Track errors

3. **Integration**
   - Use proper topics
   - Handle errors
   - Manage state
   - Clean resources

### Example Usage

```erlang
% Basic logging
hb_event:log({info, "Process started"}),

% Topic-specific logging
hb_event:log(http, {request_received, Path}),

% Detailed logging
hb_event:log(
    http,
    {request_complete, Status},
    ?MODULE,
    ?FUNCTION_NAME,
    ?LINE
),

% Logging with options
hb_event:log(
    http,
    {request_failed, Reason},
    ?MODULE,
    ?FUNCTION_NAME,
    ?LINE,
    #{debug_print => true}
),

% Counter increment
hb_event:increment(http, request_complete, #{})

% The events will appear in Prometheus as:
# HELP event AO-Core execution events
# TYPE event counter
event{topic="http",event="request_complete"} 42
