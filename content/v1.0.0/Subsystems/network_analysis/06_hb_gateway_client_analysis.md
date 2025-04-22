# `hb_gateway_client.erl` Analysis

## Overview

`hb_gateway_client.erl` provides specialized client functionality for interacting with Arweave's blockchain network through both its GraphQL API and direct gateway endpoints. The module serves as a bridge between HyperBEAM and Arweave data storage, enabling the retrieval and conversion of blockchain transactions into HyperBEAM's internal message format.

As noted in its documentation, this module is considered transitional and may be deprecated once "Arweave gateways integrate serving in `httpsig@1.0` form." This suggests that its current implementation addresses a temporary gap in the Arweave gateway functionality, specifically the ability to retrieve complete transactions in a format compatible with HyperBEAM's message system.

The module focuses on two primary tasks:
1. Retrieving transaction data from Arweave through GraphQL queries and REST endpoints
2. Converting the retrieved data into HyperBEAM's structured message format

## Key Characteristics

- **GraphQL Integration**: Implements queries to Arweave's GraphQL API to retrieve transaction metadata
- **Raw Data Retrieval**: Fetches transaction data from Arweave gateways' raw endpoints
- **Format Conversion**: Transforms Arweave transaction data into HyperBEAM messages
- **Verification Logic**: Includes verification mechanisms for confirming data integrity
- **Trust Configuration**: Provides options for handling unverifiable transactions
- **Specialized Queries**: Includes application-specific queries like scheduler location lookup
- **Dual-API Approach**: Combines GraphQL (for metadata) with REST (for content) to build complete messages

## Dependencies

### Upstream Dependencies

- `hb_http`: For making HTTP requests to Arweave gateways and GraphQL endpoints
- `hb_converge`: For message navigation and key operations
- `hb_util`: For utility functions, particularly encoding and decoding
- `hb_opts`: For configuration access
- `jiffy`: For JSON encoding and decoding
- `dev_codec_ans104`: For converting Arweave transactions to TABM format
- `dev_codec_structured`: For converting TABM to structured format
- `ar_bundles`: For transaction verification

## Implementation Details

### GraphQL Interaction

The module implements GraphQL queries to retrieve transaction metadata:

```erlang
read(ID, Opts) ->
    Query =
        #{
            <<"query">> =>
                <<
                    "query($transactionIds: [ID!]!) { ",
                        "transactions(ids: $transactionIds, first: 1){ ",
                            "edges { ", (item_spec())/binary , " } ",
                        "} ",
                    "} "
                >>,
            <<"variables">> =>
                #{
                    <<"transactionIds">> => [hb_util:human_id(ID)]
                }
        },
    % Execute query and process results...
```

The GraphQL schema includes fields needed to reconstruct an Arweave transaction:

```erlang
item_spec() ->
    <<"node { ",
        "id ",
        "anchor ",
        "signature ",
        "recipient ",
        "owner { key } ",
        "fee { winston } ",
        "quantity { winston } ",
        "tags { name value } ",
        "data { size } "
    "}">>.
```

The `query/2` function handles the actual GraphQL request:

```erlang
query(Query, Opts) ->
    Res = hb_http:request(
        #{
            % Add options for the HTTP request...
            <<"method">> => <<"POST">>,
            <<"path">> => <<"/graphql">>,
            <<"content-type">> => <<"application/json">>,
            <<"body">> => jiffy:encode(Query)
        },
        Opts
    ),
    % Process response...
```

### Raw Data Retrieval

The module retrieves transaction data from Arweave gateways' raw endpoints:

```erlang
data(ID, Opts) ->
    Req = #{
        <<"multirequest-accept-status">> => 200,
        <<"multirequest-responses">> => 1,
        <<"path">> => <<"/raw/", ID/binary>>,
        <<"method">> => <<"GET">>
    },
    case hb_http:request(Req, Opts) of
        {ok, Res} ->
            % Process successful response...
        Res ->
            % Handle error...
    end.
```

### Message Conversion

The module converts Arweave transaction data to HyperBEAM's message format:

```erlang
result_to_message(ExpectedID, Item, Opts) ->
    GQLOpts = Opts#{ hashpath => ignore },
    % Get the transaction data...
    
    % Convert to ANS-104 message format
    TX =
        #tx {
            format = ans104,
            id = hb_util:decode(ExpectedID),
            last_tx = normalize_null(hb_converge:get(<<"anchor">>, Item, GQLOpts)),
            signature =
                hb_util:decode(hb_converge:get(<<"signature">>, Item, GQLOpts)),
            % Additional fields...
        },
    
    % Convert ANS-104 to TABM format
    TABM = dev_codec_ans104:from(TX),
    
    % Convert TABM to structured format
    Structured = dev_codec_structured:to(TABM),
    
    % Verify and potentially modify the message...
```

The conversion process involves:
1. Retrieving transaction metadata from GraphQL results
2. Fetching the transaction data from gateway endpoints
3. Constructing an ANS-104 format transaction record
4. Converting the transaction to TABM format
5. Converting TABM to HyperBEAM's structured format
6. Verifying the transaction and handling trust decisions

### Verification and Trust Handling

The module includes verification logic and trust configuration:

```erlang
Embedded =
    case ar_bundles:verify_item(TX) of
        true ->
            % Transaction verifies successfully...
            Structured;
        _ ->
            % Transaction doesn't verify, check trust configuration...
            case hb_opts:get(ans104_trust_gql, false, Opts) of
                false ->
                    % Don't trust unverified transactions...
                    Structured;
                true ->
                    % Trust GraphQL results despite verification failure...
                    % Add trusted keys to the attestation...
            end
    end,
```

This allows for handling cases where the transaction doesn't verify cryptographically, with a configuration option to still trust GraphQL results if needed.

### Specialized Queries

The module includes application-specific queries:

```erlang
scheduler_location(Address, Opts) ->
    Query =
        #{
            <<"query">> =>
                <<"query($SchedulerAddrs: [String!]!) { ",
                    "transactions(owners: $SchedulerAddrs, tags: { name: \"Type\" values: [\"Scheduler-Location\"] }, first: 1){ ",
                        "edges { ",
                            (item_spec())/binary ,
                        " } ",
                    "} ",
                "}">>,
            % Additional query parameters...
        },
    % Execute query and process results...
```

This specialized query locates scheduler information based on an address, demonstrating how the module is used for specific application needs.

## Questions and Insights

### Questions

1. **Deprecation Timeline**: What is the timeline for Arweave gateways to implement `httpsig@1.0` support, and what preparations are being made for the transition?

2. **Error Handling Strategy**: How does the system handle gateway unavailability or partial responses? Is there a fallback mechanism or retry strategy?

3. **Trust Decisions**: What criteria inform the `ans104_trust_gql` configuration setting? Are there cases where this setting should be enabled or disabled?

4. **Transaction Caching**: Does the system cache retrieved transactions to reduce redundant gateway queries, or is this handled at a different level?

5. **Gateway Selection**: How are gateways selected and managed? Is there a health-checking or scoring mechanism to prefer more reliable gateways?

### Insights

1. **API Complementarity**: The module cleverly combines GraphQL for metadata and direct gateway access for content, leveraging the strengths of each API.

2. **Transitional Design**: The module's documentation explicitly acknowledges its transitional nature, suggesting architectural awareness and forward planning.

3. **Verification Flexibility**: The trust configuration provides flexibility for handling real-world blockchain imperfections, balancing security with practicality.

4. **Format Layering**: The multi-step conversion process (ANS-104 -> TABM -> Structured) demonstrates the layered architecture of HyperBEAM's message system.

5. **Application-Specific Extensions**: The `scheduler_location` function shows how the base functionality is extended for specific application needs.

## Integration with Other Subsystems

### Integration with Network Communication Subsystem

- Uses `hb_http` to make HTTP requests to Arweave gateways and GraphQL endpoints
- Provides higher-level access patterns on top of the basic HTTP functionality
- Handles specific network communication concerns for Arweave interaction

### Integration with Core Infrastructure

- Uses `hb_converge` for message navigation and key operations
- Uses `hb_util` for utility functions
- Uses `hb_opts` for configuration access

### Integration with Arweave Subsystem

- Interfaces directly with Arweave's GraphQL API and gateway endpoints
- Works with Arweave-specific transaction formats
- Uses `ar_bundles` for transaction verification
- Implements specialized queries for Arweave-specific concepts like scheduler location

### Integration with Codec Subsystem

- Uses `dev_codec_ans104` and `dev_codec_structured` for format conversions
- Bridges between Arweave's data format and HyperBEAM's message format

## Recategorization Considerations

This module sits at an interesting boundary between the Network Communication Subsystem and the Arweave Integration Subsystem. While it is involved in network communication, its functionality is very specific to Arweave's protocols and data structures.

Given the current categorization approach, it makes sense to keep it in the Network Communication Subsystem as it extends the HTTP client capabilities for a specific use case. However, it's worth noting that this module could arguably be part of an Arweave Integration Subsystem as well, given its tight coupling with Arweave-specific concepts and protocols.

The module's transitional nature, as noted in its documentation, suggests that its current implementation addresses a temporary gap. As Arweave gateways evolve, this module's role may change or diminish, which could impact future categorization decisions.
