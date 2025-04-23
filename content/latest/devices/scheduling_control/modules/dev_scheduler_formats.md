# Module: dev_scheduler_formats

## Basic Information
- **Source File:** dev_scheduler_formats.erl
- **Module Type:** Device Scheduling Control
- **Purpose:** Handles format conversion and compatibility between different AO client formats for scheduler responses

## Interface

### Public API
```erlang
% Format conversion
-export([assignments_to_bundle/4, assignments_to_aos2/4]).
-export([aos2_to_assignments/3, aos2_to_assignment/2]).
% Type normalization
-export([aos2_normalize_types/1]).
```

## Core Functionality

### 1. Format Types

#### HTTP Bundle Format
```erlang
assignments_to_bundle(ProcID, Assignments, More, Opts) ->
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    {ok, #{
        <<"type">> => <<"schedule">>,
        <<"process">> => ProcID,
        <<"continues">> => More,
        <<"timestamp">> => Timestamp,
        <<"block-height">> => Height,
        <<"block-hash">> => Hash,
        <<"assignments">> => AssignmentMap
    }}
```

#### AOS2 JSON Format
```erlang
assignments_to_aos2(ProcID, Assignments, More, Opts) ->
    {ok, #{
        <<"content-type">> => <<"application/json">>,
        <<"body">> => #{
            <<"page_info">> => #{
                <<"process">> => ProcID,
                <<"has_next_page">> => More,
                <<"timestamp">> => Timestamp,
                <<"block-height">> => Height,
                <<"block-hash">> => Hash
            },
            <<"edges">> => AssignmentEdges
        }
    }}
```

### 2. Format Conversion

#### Bundle to AOS2
```erlang
assignment_to_aos2(Assignment, Opts) ->
    Message = hb_ao:get(<<"body">>, Assignment, Opts),
    AssignmentWithoutBody = maps:without([<<"body">>], Assignment),
    #{
        <<"message">> => dev_json_iface:message_to_json_struct(Message),
        <<"assignment">> => dev_json_iface:message_to_json_struct(AssignmentWithoutBody)
    }
```

#### AOS2 to Bundle
```erlang
aos2_to_assignments(ProcID, Body, Opts) ->
    % Extract assignments from edges
    Assignments = maps:get(<<"edges">>, Body),
    ParsedAssignments = lists:map(
        fun(A) -> aos2_to_assignment(A, Opts) end,
        Assignments
    ),
    
    % Get time info from last assignment or use defaults
    TimeInfo = get_time_info(ParsedAssignments),
    
    % Convert to bundle format
    assignments_to_bundle(ProcID, ParsedAssignments, false, TimeInfo, Opts)
```

### 3. Type Normalization

```erlang
aos2_normalize_types(Msg) ->
    % Convert timestamp from binary to integer
    normalize_timestamp(Msg),
    
    % Convert nonce to slot if needed
    normalize_nonce(Msg),
    
    % Convert epoch from binary to integer
    normalize_epoch(Msg),
    
    % Convert slot from binary to integer
    normalize_slot(Msg),
    
    % Ensure block hash exists
    ensure_block_hash(Msg)
```

## Implementation Details

### 1. Format Options
```erlang
format_opts(Opts) ->
    Opts#{
        hashpath => ignore,
        cache_control => [<<"no-cache">>, <<"no-store">>],
        await_inprogress => false
    }
```

### 2. Cursor Generation
```erlang
cursor(Assignment, Opts) ->
    % Use slot number for ao.N.1
    % Use assignment ID for ao.TN.1
    hb_ao:get(<<"slot">>, Assignment, Opts)
```

### 3. Data Normalization
```erlang
aos2_normalize_data(JSONStruct) ->
    % Ensure data field exists
    case JSONStruct of
        #{<<"data">> := _} -> JSONStruct;
        _ -> JSONStruct#{ <<"data">> => <<>> }
    end
```

## Event Logging

### 1. Format Conversion
```erlang
?event({body_struct, BodyStruct})
?event({encoded, {explicit, Encoded}})
?event({raw_assignments, Assignments})
?event({parsed_assignments, ParsedAssignments})
```

### 2. Error Handling
```erlang
?event(error, {scheduler_did_not_provide_message, MessageID})
?event({missing_block_hash, Msg})
```

### 3. Type Normalization
```erlang
?event({aos2_normalized_types, {msg, Msg}, {anchor, Anchor}})
```

## Integration Points

### Direct Dependencies
- hb_ao: Message operations
- hb_json: JSON encoding/decoding
- hb_gateway_client: Message conversion
- dev_json_iface: JSON structure conversion
- ar_timestamp: Block information

### Usage Context
- Called by dev_scheduler
- Used by dev_scheduler_cache
- Integrates with gateway client
- Handles format compatibility

## Key Features

### 1. Format Support
- HTTP bundle format
- AOS2 JSON format
- Legacy compatibility
- Format conversion

### 2. Type Handling
- Binary to integer conversion
- Field normalization
- Type verification
- Default values

### 3. Performance
- Efficient conversion
- Minimal copying
- Cached options
- Optimized structures

### 4. Compatibility
- Legacy support
- Format detection
- Type coercion
- Field mapping

## Best Practices

### 1. Format Conversion
- Validate input format
- Preserve data integrity
- Handle missing fields
- Maintain compatibility

### 2. Type Normalization
- Check field types
- Convert consistently
- Handle edge cases
- Provide defaults

### 3. Integration
- Follow format specs
- Handle errors gracefully
- Log conversions
- Maintain compatibility
