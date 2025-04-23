# Module: hb_store_gateway

## Basic Information
- **Source File:** hb_store_gateway.erl
- **Module Type:** Core Storage & Caching
- **Behavior:** hb_store

## Purpose
Provides a gateway-based implementation of the hb_store behavior that reads data from Arweave gateway and GraphQL routes. Acts as a bridge between local storage and remote Arweave network data, with optional local caching capabilities.

## Interface

### Core Operations
- `scope/1` - Return store scope (always 'remote')
- `resolve/2` - Resolve path (identity function)
- `read/2` - Read data from gateway
- `type/2` - Get type of data at key

### Data Operations
- `list/2` - List keys in a message
- `maybe_cache/2` - Cache data if enabled

## Dependencies

### Direct Dependencies
- hb_gateway_client: Gateway communication
- hb_message: Message handling
- hb_private: Private data management
- hb_path: Path manipulation
- hb_cache: Cache operations

### Inverse Dependencies
- Used by modules needing remote data
- Core component for Arweave access
- Essential for distributed storage

## Implementation Details

### Key Concepts

1. **Gateway Configuration**
   ```erlang
   Store = #{
       <<"store-module">> => hb_store_gateway,
       <<"store">> => LocalStore,  % Optional local cache
       <<"routes">> => Routes      % Optional route list
   }
   ```

2. **Caching Strategy**
   ```erlang
   % Cache data in local store if enabled
   maybe_cache(StoreOpts, Data) ->
       case get_store_option(StoreOpts) of
           false -> do_nothing;
           Store -> hb_cache:write(Data, #{store => Store})
       end
   ```

3. **Type Detection**
   ```erlang
   % Determine if data is simple or composite
   IsFlat = lists:all(
       fun({_, Value}) -> not is_map(Value) end,
       maps:to_list(CleanData)
   ),
   Type = if IsFlat -> simple; true -> composite end
   ```

### State Management

1. **Remote Data**
   - Gateway access
   - GraphQL queries
   - Response handling
   - Error management

2. **Local Cache**
   - Optional caching
   - Cache invalidation
   - Store selection
   - Data persistence

3. **Type System**
   - Simple values
   - Composite data
   - Type inference
   - Cache optimization

### Error Handling

1. **Gateway Errors**
   - Connection failures
   - Invalid responses
   - Timeout handling
   - Cache fallback

2. **Data Validation**
   - Message format
   - Type checking
   - Path validation
   - Cache consistency

## Integration Points

1. **Gateway System**
   - Arweave gateway
   - GraphQL endpoints
   - HTTP communication
   - Response parsing

2. **Cache System**
   - Local storage
   - Cache policies
   - Data persistence
   - Store selection

3. **Message System**
   - Data formatting
   - Type handling
   - Path resolution
   - State management

## Analysis Insights

### Performance Considerations

1. **Gateway Access**
   - Connection pooling
   - Response caching
   - Request batching
   - Error recovery

2. **Cache Strategy**
   - Selective caching
   - Cache invalidation
   - Store optimization
   - Memory management

### Security Implications

1. **Remote Access**
   - Gateway validation
   - Route restrictions
   - Data verification
   - Access control

2. **Cache Security**
   - Data isolation
   - Store permissions
   - Cache validation
   - State protection

### Best Practices

1. **Gateway Usage**
   - Configure routes
   - Handle timeouts
   - Validate responses
   - Manage connections

2. **Cache Management**
   - Enable when needed
   - Configure stores
   - Handle failures
   - Validate data

3. **Error Handling**
   - Check responses
   - Handle timeouts
   - Validate data
   - Manage state

### Example Usage

```erlang
% Basic gateway store setup
Gateway = #{
    <<"store-module">> => hb_store_gateway,
    <<"store">> => false  % No local cache
},

% Read data from gateway
{ok, Data} = hb_store:read(Gateway, <<"transaction-id">>),

% Gateway with local cache
CachedGateway = #{
    <<"store-module">> => hb_store_gateway,
    <<"store">> => LocalStore,  % Enable local cache
    <<"routes">> => [<<"graphql">>, <<"gateway">>]
},

% Data will be cached locally after read
{ok, CachedData} = hb_store:read(CachedGateway, <<"transaction-id">>)
