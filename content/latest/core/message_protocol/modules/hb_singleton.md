# Module: hb_singleton

## Basic Information
- **Source File:** hb_singleton.erl
- **Module Type:** Core Message Protocol
- **Behavior:** None

## Purpose
Parser that translates AO-Core HTTP API requests in TABM format into an ordered list of messages to evaluate. Handles complex path syntax and message transformations.

## Interface

### Core Functions
- `from/1` - Convert singleton TABM message to list of executable AO-Core messages
- `to/1` - Convert list of AO-Core messages into TABM message

## Dependencies

### Direct Dependencies
- hb_message: Message handling
- hb_util: Utility functions
- cowboy_req: HTTP request parsing
- uri_string: URL decoding

### Inverse Dependencies
- Used by HTTP API handlers
- Core component for API requests
- Essential for message routing

## Implementation Details

### Key Concepts

1. **Path Syntax**
   ```
   /Part1/Part2/.../PartN/    => [Part1, Part2, ..., PartN]
   /ID/Part2/.../PartN        => [ID, Part2, ..., PartN]
   ```

2. **Part Syntax**
   ```
   Part                       => #{path => Part}
   Part&Key=Value            => #{path => Part, Key => Value}
   Part&Key                  => #{path => Part, Key => true}
   Part&k1=v1&k2=v2         => #{path => Part, k1 => "v1", k2 => "v2"}
   Part~Device              => {as, Device, #{path => Part}}
   Part~D&K1=V1            => {as, D, #{path => Part, K1 => "v1"}}
   pt&k1+int=1             => #{path => pt, k1 => 1}
   pt~d&k1+int=1           => {as, d, #{path => pt, k1 => 1}}
   (/nested/path)          => Resolution of /nested/path
   (/nested/path&k1=v1)    => (resolve /nested/path)#{k1 => v1}
   pt&k1+res=(/a/b/c)     => #{path => pt, k1 => (resolve /a/b/c)}
   ```

3. **Key Syntax**
   ```
   key: "value"             => #{key => "value"} for all messages
   n.key: "value"          => #{key => "value"} for Nth message
   key+Int: 1              => #{key => 1}
   key+Res: /nested/path   => #{key => (resolve /nested/path)}
   N.Key+Res=(/a/b/c)     => #{Key => (resolve /a/b/c)}
   ```

### State Management

1. **Path Processing**
   - Parses relative references
   - Handles query parameters
   - Manages path segments
   - Validates segment length

2. **Message Building**
   - Merges base messages
   - Applies scoped modifications
   - Handles device specifiers
   - Manages typed values

3. **Type Handling**
   - Integer conversion
   - Binary handling
   - Resolution types
   - Value validation

### Error Handling

1. **Path Validation**
   - Checks segment length
   - Validates syntax
   - Handles malformed paths
   - Manages encoding errors

2. **Type Conversion**
   - Validates type specifiers
   - Handles conversion errors
   - Manages invalid values
   - Scope validation

## Integration Points

1. **HTTP System**
   - Request parsing
   - Path handling
   - Query parameters
   - Message conversion

2. **Message System**
   - TABM format
   - AO-Core messages
   - Message transformation
   - State management

3. **Device System**
   - Device specifiers
   - Message routing
   - Resolution handling
   - Type conversion

## Analysis Insights

### Performance Considerations

1. **Path Processing**
   - Efficient parsing
   - Minimal allocations
   - Optimized splitting
   - Cached results

2. **Message Building**
   - Efficient merging
   - Smart type handling
   - Minimal copying
   - Optimized conversion

### Security Implications

1. **Path Validation**
   - Length limits
   - Syntax checking
   - Encoding validation
   - Safe decoding

2. **Type Safety**
   - Value validation
   - Safe conversion
   - Scope checking
   - Error handling

### Best Practices

1. **Path Construction**
   - Use clear paths
   - Validate segments
   - Handle encoding
   - Check lengths

2. **Message Handling**
   - Validate types
   - Check scopes
   - Handle errors
   - Manage state
