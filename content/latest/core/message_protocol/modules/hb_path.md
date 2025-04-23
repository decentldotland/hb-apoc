# Module: hb_path

## Basic Information
- **Source File:** hb_path.erl
- **Module Type:** Core Message Protocol
- **Behavior:** None

## Purpose
Provides utilities for manipulating two types of paths in messages:
1. Request Path (referred to as just 'Path')
2. HashPath - a rolling Merkle list of messages that represents message history

## Interface

### HashPath Functions
- `hashpath/2(Msg, Opts)` - Get message's HashPath
- `hashpath/3(Msg1, Msg2, Opts)` - Add Msg2's ID to Msg1's HashPath
- `hashpath/4(Msg1, Msg2, HashpathAlg, Opts)` - Add with specific algorithm
- `hashpath_alg/1(Msg)` - Get message's HashPath algorithm
- `verify_hashpath/2(MsgList, Opts)` - Verify HashPath against message history

### Path Manipulation
- `hd/2(Msg, Opts)` - Extract first key from Path
- `tl/2(Msg, Opts)` - Return message without first path element
- `push_request/2(Msg, Path)` - Add message to head of request path
- `queue_request/2(Msg, Path)` - Queue message at back of request path
- `pop_request/2(Msg, Opts)` - Pop next element from request path

### Private Path Functions
- `priv_remaining/2(Msg, Opts)` - Get remaining path from private AO-Core key
- `priv_store_remaining/2(Msg, Path)` - Store remaining path in private AO-Core key

### Path Utilities
- `term_to_path_parts/1,2` - Convert term to executable path
- `from_message/2` - Extract request path or hashpath from message
- `matches/2` - Check if two keys match
- `regex_matches/2` - Check if keys match using regex
- `normalize/1` - Normalize path to binary
- `to_binary/1` - Convert path to binary

## Dependencies

### Direct Dependencies
- hb_ao: Core protocol operations
- hb_util: Utility functions
- hb_crypto: Cryptographic operations
- hb_private: Private data handling
- dev_message: Message operations

### Inverse Dependencies
- Used by modules that need path manipulation
- Core component for message routing
- Essential for message history tracking

## Implementation Details

### Key Concepts

1. **HashPath Implementation**
   ```
   Msg1.HashPath = Msg1.ID
   Msg3.HashPath = Hash(Msg1.HashPath, Msg2.ID)
   Msg3.{...} = AO-Core.apply(Msg1, Msg2)
   ```
   - Rolling Merkle list of applied messages
   - Each message depends on all previous messages
   - Represents a history tree, not just linear history

2. **HashPath Algorithms**
   - Default: sha-256-chain
   - Alternative: accumulate-256
   - Customizable per message

3. **Path Manipulation**
   - Supports multiple path formats
   - Handles binary, list, and atom paths
   - Provides regex matching capabilities

### State Management
- Stateless module
- Paths stored in message maps
- Private paths in AO-Core key
- HashPaths maintained in message history

### Error Handling
- Validates path formats
- Handles missing paths
- Verifies HashPath integrity
- Supports custom error strategies

## Integration Points

1. **Message System**
   - Core part of message routing
   - Integrates with message history
   - Supports message verification

2. **Protocol Integration**
   - Essential for AO-Core protocol
   - Handles path normalization
   - Manages message history

3. **Security Integration**
   - HashPath verification
   - Message history integrity
   - Cryptographic algorithm support

## Analysis Insights

### Performance Considerations
1. Efficient path manipulation
   - O(1) head/tail operations
   - Optimized binary handling
   - Lazy path normalization

2. HashPath Optimization
   - Caches computed HashPaths
   - Minimizes recalculations
   - Efficient verification

### Security Implications
1. Message History Integrity
   - Merkle tree structure
   - Cryptographic verification
   - History tampering detection

2. Path Security
   - Path normalization
   - Regex safety checks
   - Private path protection

### Best Practices
1. HashPath Usage
   - Verify message history
   - Use appropriate algorithms
   - Handle verification failures

2. Path Manipulation
   - Normalize paths consistently
   - Use appropriate path formats
   - Handle private paths carefully
