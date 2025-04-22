# Module: hb_metrics_collector

## Basic Information
- **Source File:** hb_metrics_collector.erl
- **Module Type:** Core Process Management
- **Purpose:** Prometheus metrics collector for system monitoring

## Purpose
Implements a Prometheus collector that gathers and exposes system metrics including process uptime and system load. The module follows the prometheus_collector behavior to integrate with Prometheus monitoring systems, providing standardized metrics for system observation.

## Interface

### Core Operations
- `deregister_cleanup/1` - Cleanup when collector is deregistered
- `collect_mf/2` - Collect metric families
- `collect_metrics/2` - Generate specific metrics

## Dependencies

### Direct Dependencies
- prometheus_collector: Collector behavior
- prometheus_model_helpers: Metric creation helpers
- cpu_sup: System load statistics
- erlang: System statistics

### Inverse Dependencies
- Used by Prometheus monitoring system
- Used by system monitoring tools
- Core metrics provider

## Implementation Details

### Key Concepts

1. **Metric Types**
   ```erlang
   % Process uptime gauge
   create_gauge(
       process_uptime_seconds,
       "The number of seconds the Erlang process has been up.",
       Uptime
   )

   % System load gauge
   create_gauge(
       system_load,
       "The load values are proportional to how long time a runnable Unix process has to spend in the run queue before it is scheduled.",
       SystemLoad
   )
   ```

2. **Metric Collection**
   ```erlang
   collect_mf(_Registry, Callback) ->
       % Get process uptime
       {Uptime, _} = erlang:statistics(wall_clock),
       Callback(create_gauge(...)),

       % Get system load
       SystemLoad = cpu_sup:avg5(),
       Callback(create_gauge(...)),

       ok.
   ```

3. **Metric Generation**
   ```erlang
   collect_metrics(system_load, SystemLoad) ->
       prometheus_model_helpers:gauge_metrics([{[], SystemLoad}]);
   
   collect_metrics(process_uptime_seconds, Uptime) ->
       UptimeSeconds = Uptime / 1000,
       prometheus_model_helpers:gauge_metrics([{[], UptimeSeconds}]).
   ```

### State Management

1. **Metric State**
   - Gauge values
   - Collection timing
   - Value conversion
   - Error handling

2. **System State**
   - Process uptime
   - System load
   - Resource usage
   - Error tracking

3. **Collection State**
   - Metric families
   - Callback handling
   - Value tracking
   - Error management

### Error Handling

1. **Collection Errors**
   - Metric failures
   - State recovery
   - Resource cleanup
   - Error tracking

2. **System Errors**
   - Load calculation
   - Uptime tracking
   - Resource errors
   - State recovery

## Integration Points

1. **Prometheus System**
   - Metric collection
   - Value reporting
   - State tracking
   - Error handling

2. **System Monitoring**
   - Load tracking
   - Uptime monitoring
   - Resource tracking
   - Error detection

3. **Process Management**
   - State tracking
   - Resource monitoring
   - Error handling
   - Cleanup management

## Analysis Insights

### Performance Considerations

1. **Metric Collection**
   - Collection timing
   - Resource usage
   - Value caching
   - Error handling

2. **System Impact**
   - Load calculation
   - Resource usage
   - State tracking
   - Error recovery

### Security Implications

1. **System Access**
   - Load monitoring
   - Resource tracking
   - State protection
   - Error isolation

2. **Data Protection**
   - Metric privacy
   - State security
   - Resource limits
   - Error handling

### Best Practices

1. **Collector Usage**
   - Regular collection
   - Proper cleanup
   - Error handling
   - Resource management

2. **Metric Management**
   - Value validation
   - State tracking
   - Error handling
   - Resource cleanup

3. **Integration**
   - Prometheus setup
   - Error handling
   - State management
   - Resource cleanup

### Example Usage

```erlang
% The collector is automatically registered with Prometheus
% and will be called periodically to collect metrics

% Metrics will be exposed in Prometheus format:
# HELP process_uptime_seconds The number of seconds the Erlang process has been up.
# TYPE process_uptime_seconds gauge
process_uptime_seconds 3600.0

# HELP system_load The load values are proportional to how long time a runnable Unix process has to spend in the run queue before it is scheduled.
# TYPE system_load gauge
system_load 1.23

% The metrics can then be queried through Prometheus:
rate(process_uptime_seconds[5m])
avg_over_time(system_load[1h])
