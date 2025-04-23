# Module: hb_gateway_client

## Basic Information
- **Source File:** hb_gateway_client.erl
- **Module Type:** Core Network & HTTP
- **Purpose:** Arweave GraphQL API client for data retrieval

## Purpose
Implements Arweave's GraphQL API to access data items stored on the network. This module is essential for retrieving full HyperBEAM `structured@1.0` form messages from Arweave, as gateways currently don't expose all necessary fields outside the GraphQL API. The module will be deprecated once gateways integrate serving in `httpsig@1.0` form.

## Interface

### Raw Access Operations
- `read/2` - Get data item by ID including data and tags
- `data/2` - Get raw data associated with transaction ID
- `result_to_message/2` - Convert GraphQL result to message format

### Application Operations
- `scheduler_location/2` - Find scheduler location by address

## Dependencies

### Direct Dependencies
- hb_http: HTTP request handling
- hb_json: JSON encoding/decoding
- hb_ao: Core operations
- ar_bundles: ANS-104 bundle handling
- dev_codec_ans104: ANS-104 codec
- dev_codec_structured: Structured codec

### Inverse Dependencies
- Used by gateway integration components
- Used by scheduler location services
- Core Arweave data access provider

## Implementation Details

### Key Concepts

1. **GraphQL Schema**
   ```graphql
   type Transaction {
     id: ID!
     anchor: String!
     signature: String!
     recipient: String!
     owner: Owner { address: String! key: String! }!
     fee: Amount!
     quantity: Amount!
     data: MetaData!
     tags: [Tag { name: String! value: String! }!]!
   }
   ```

2. **Data Retrieval**
   ```erlang
   % GraphQL query execution
   query(Query, Opts) ->
       hb_http:request(#{
           <<"method">> => <<"POST">>,
           <<"path">> => <<"/graphql">>,
           <<"content-type">> => <<"application/json">>,
           <<"body">> => hb_json:encode(Query)
       }, Opts)
   ```

3. **Message Conversion**
   ```erlang
   % Convert GraphQL result to ANS-104 format
   result_to_message(ExpectedID, Item, Opts) ->
       % Extract data and tags
       Data = get_data(ExpectedID, Item, Opts),
       Tags = get_tags(Item, Opts),
       
       % Create ANS-104 transaction
       TX = create_ans104_tx(ExpectedID, Item, Data, Tags),
       
       % Convert to structured format
       TABM = dev_codec_ans104:from(TX),
       Structured = dev_codec_structured:to(TABM)
   ```

### State Management

1. **Query State**
   - GraphQL queries
   - Response handling
   - Data validation
   - Error management

2. **Data State**
   - Raw data retrieval
   - Format conversion
   - State verification
   - Error handling

3. **Message State**
   - Format conversion
   - State tracking
   - Verification
   - Error handling

### Error Handling

1. **GraphQL Errors**
   - Query failures
   - Response validation
   - Data verification
   - State recovery

2. **Gateway Errors**
   - Connection failures
   - Data retrieval errors
   - Format errors
   - State verification

## Integration Points

1. **Arweave Network**
   - GraphQL API
   - Gateway access
   - Data retrieval
   - State verification

2. **Message System**
   - Format conversion
   - State tracking
   - Error handling
   - Data validation

3. **Scheduler System**
   - Location lookup
   - State tracking
   - Error handling
   - Data validation

## Analysis Insights

### Performance Considerations

1. **Data Retrieval**
   - Query optimization
   - Response caching
   - State management
   - Error handling

2. **Message Processing**
   - Format conversion
   - State tracking
   - Resource usage
   - Error recovery

### Security Implications

1. **Data Verification**
   - Message signing
   - State validation
   - Trust management
   - Error isolation

2. **Gateway Trust**
   - Trust configuration
   - Data validation
   - State verification
   - Error handling

### Best Practices

1. **GraphQL Usage**
   - Query optimization
   - Response handling
   - Error management
   - State tracking

2. **Data Handling**
   - Format validation
   - State verification
   - Error handling
   - Resource cleanup

3. **Integration**
   - API usage
   - Error handling
   - State management
   - Resource cleanup

### Example Usage

```erlang
% Read data item by ID
{ok, Message} = hb_gateway_client:read(
    <<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>,
    #{}
),

% Get scheduler location
{ok, Location} = hb_gateway_client:scheduler_location(
    <<"fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY">>,
    #{}
),

% Get raw data by ID
{ok, Data} = hb_gateway_client:data(
    <<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>,
    #{}
),

% Convert GraphQL result to message
{ok, Message} = hb_gateway_client:result_to_message(
    <<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>,
    GraphQLResult,
    #{}
)
