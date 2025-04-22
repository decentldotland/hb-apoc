# Module: hb_message

## Basic Information
- **Source File:** hb_message.erl
- **Module Type:** Core Message Protocol
- **Behavior:** None

## Purpose
Acts as an adapter between messages in the AO-Core protocol and their underlying binary representations and formats. Converts between different message formats using a common intermediate format called Type Annotated Binary Messages (TABM).

## Supported Message Formats
1. Richly typed AO-Core structured messages
2. Arweave transactions
3. ANS-104 data items
4. HTTP Signed Messages
5. Flat Maps

## Interface

### Public Functions

#### Message Conversion
- `convert/3(Msg, TargetFormat, Opts)` - Convert message to target format
- `convert/4(Msg, TargetFormat, SourceFormat, Opts)` - Convert from source to target format

#### Message Identification
- `id/1(Msg)` - Get message ID
- `id/2(Msg, Committers)` - Get ID with specific committers
- `id/3(Msg, Committers, Opts)` - Get ID with committers and options

#### Message Verification
- `verify/1(Msg)` - Verify message signatures
- `verify/2(Msg, Committers)` - Verify specific committers' signatures

#### Message Commitment
- `commit/2(Msg, WalletOrOpts)` - Sign message with wallet
- `commit/3(Msg, Wallet, Format)` - Sign message in specific format
- `committed/1,2,3` - Get committed keys from message

#### Message Manipulation
- `with_only_committed/1,2` - Filter to only committed keys
- `with_only_committers/2` - Filter to specific committers
- `uncommitted/1` - Get unsigned version
- `minimize/1` - Remove regeneratable keys

## Dependencies

### Direct Dependencies
- hb_ao: Core protocol operations
- hb_util: Utility functions
- hb_crypto: Cryptographic operations
- hb_path: Path handling
- dev_message: Device interface for message formats

### Inverse Dependencies
- Used by most modules that need to handle messages
- Core component for message processing

## Implementation Details

### Key Concepts

1. **Type Annotated Binary Messages (TABM)**
   - Common intermediate format
   - Deep Erlang maps
   - Keys contain only TABMs or binary values
   - O(1) access for efficiency

2. **Conversion Flow**
```
Arweave TX/ANS-104 ==> dev_codec_ans104:from/1 ==> TABM
HTTP Signed Message ==> dev_codec_httpsig_conv:from/1 ==> TABM
Flat Maps ==> dev_codec_flat:from/1 ==> TABM

TABM ==> dev_codec_structured:to/1 ==> AO-Core Message
AO-Core Message ==> dev_codec_structured:from/1 ==> TABM

TABM ==> dev_codec_ans104:to/1 ==> Arweave TX/ANS-104
TABM ==> dev_codec_httpsig_conv:to/1 ==> HTTP Signed Message
TABM ==> dev_codec_flat:to/1 ==> Flat Maps
```

3. **Message Signing**
   - Supports multiple signature formats
   - Handles nested signed messages
   - Preserves signature verification across conversions

### State Management
- Stateless module
- Uses TABM as intermediate state during conversions
- Preserves private data across conversions

### Error Handling
- Throws errors for invalid message formats
- Validates signatures during verification
- Preserves error information across conversions

## Integration Points

1. **Message Codecs**
   - Pluggable codec system
   - Each format has dedicated codec module
   - Codecs handle format-specific details

2. **Cache Integration**
   - Works with hb_cache for storage
   - Uses TABM as storage format
   - Handles cache control headers

3. **Protocol Integration**
   - Core part of AO-Core protocol
   - Handles message normalization
   - Manages protocol versioning

## Analysis Insights

### Performance Considerations
1. Uses O(1) access maps for efficiency
2. Minimizes data duplication
3. Lazy verification of signatures
4. Efficient handling of large messages

### Security Implications
1. Preserves signatures across conversions
2. Handles multiple signature formats
3. Supports nested signed messages
4. Maintains signature verification integrity

### Best Practices
1. Use TABM for internal processing
2. Verify signatures before processing
3. Handle private data carefully
4. Use appropriate codec for each format
