# Module: hb_cache_render

## Basic Information
- **Source File:** hb_cache_render.erl
- **Module Type:** Core Storage & Caching
- **Purpose:** Visualization tool for cache key graphs

## Purpose
Provides functionality to render cache key graphs into DOT and SVG formats, enabling visual inspection of cache structure and relationships. This module helps developers understand and debug cache hierarchies by generating visual representations of the cache's internal organization.

## Interface

### Core Operations
- `render/1,2` - Render key graph to SVG and display
- `cache_path_to_dot/2,3` - Convert cache path to DOT format
- `dot_to_svg/1` - Convert DOT graph to SVG format

### Test Data Generation
- `prepare_unsigned_data/0` - Create test unsigned data
- `prepare_signed_data/0` - Create test signed data
- `prepare_deeply_nested_complex_message/0` - Create complex test data

## Dependencies

### Direct Dependencies
- hb_store: Storage access
- hb_util: Utility functions
- hb_message: Message handling
- file: File operations
- os: System commands

### Inverse Dependencies
- Used by debugging tools
- Cache visualization
- Development utilities

## Implementation Details

### Key Concepts

1. **Graph Generation**
   ```erlang
   % Convert cache structure to graph
   cache_path_to_graph(ToRender, GraphOpts, Store) ->
       % Collect nodes and arcs recursively
       traverse_store(Store, Key, Parent, Graph)
   ```

2. **Node Types**
   ```erlang
   % Simple nodes (leaf nodes)
   process_simple_node(Store, Key, Parent, Path, Graph)
   
   % Composite nodes (directories)
   process_composite_node(Store, Key, Parent, Path, Graph)
   ```

3. **Graph Visualization**
   ```erlang
   % Generate DOT format
   graph_to_dot(Graph) ->
       % Create nodes and arcs
       [Header, Nodes, Arcs, Footer]
   ```

### State Management

1. **Graph Building**
   - Node tracking
   - Arc management
   - Cycle detection
   - Path resolution

2. **Visualization State**
   - DOT generation
   - SVG conversion
   - File handling
   - Display management

3. **Test Data**
   - Message creation
   - Data structure
   - Store population
   - State verification

### Error Handling

1. **Graph Generation**
   - Cycle detection
   - Invalid paths
   - Missing nodes
   - State validation

2. **Visualization**
   - DOT errors
   - SVG conversion
   - File operations
   - Display failures

## Integration Points

1. **Storage System**
   - Path resolution
   - Type detection
   - Content access
   - State tracking

2. **Visualization System**
   - DOT generation
   - SVG conversion
   - File handling
   - Display management

3. **Test System**
   - Data generation
   - Structure creation
   - State population
   - Verification

## Analysis Insights

### Performance Considerations

1. **Graph Generation**
   - Cycle avoidance
   - Path optimization
   - State tracking
   - Memory management

2. **Visualization**
   - DOT efficiency
   - SVG conversion
   - File handling
   - Display speed

### Security Implications

1. **File Operations**
   - Path validation
   - File permissions
   - Command execution
   - State protection

2. **Data Access**
   - Store access
   - Path validation
   - Content protection
   - State isolation

### Best Practices

1. **Graph Generation**
   - Handle cycles
   - Validate paths
   - Track state
   - Manage memory

2. **Visualization**
   - Use appropriate formats
   - Handle errors
   - Validate output
   - Clean up files

3. **Testing**
   - Create varied data
   - Validate structures
   - Check output
   - Clean up state

### Example Usage

```erlang
% Basic rendering of all cache paths
hb_cache_render:render(Store),

% Render specific path
hb_cache_render:render(<<"specific/path">>, Store),

% Generate DOT without rendering
Dot = hb_cache_render:cache_path_to_dot(Path, Store),

% Custom rendering options
Opts = #{render_data => false},
hb_cache_render:cache_path_to_dot(Path, Opts, Store),

% Test data generation
hb_cache_render:prepare_deeply_nested_complex_message(),
hb_cache_render:render(Store)  % View the complex structure
