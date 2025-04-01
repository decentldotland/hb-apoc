/**
 * HyperBEAM Conceptual Glossary Data
 * 
 * This file contains the parsed and structured data from the HyperBEAM conceptual glossary.
 * It provides definitions for key terms, concepts, and patterns used throughout the system.
 */

// Main glossary terms
export const glossaryTerms = [
  // Terms starting with A
  {
    term: 'AO',
    definition: 'Actor Oriented programming paradigm and runtime built on Arweave. AO enables serverless, permissionless distributed applications with guaranteed availability. In HyperBEAM, AO integration provides a computational framework for executing distributed processes across the network.',
    related: ['Arweave', 'Actor Model', 'Distributed Computing'],
    category: 'Protocol',
    letter: 'A'
  },
  {
    term: 'ANS-104',
    definition: 'A data bundling standard used in the Arweave network that allows multiple independent data items to be grouped together in a single transaction. HyperBEAM uses this format via `dev_codec_ans104` to bridge between internal message formats and Arweave blockchain storage.',
    related: ['Arweave Integration', 'Transaction Bundling', 'Codec'],
    category: 'Arweave',
    letter: 'A'
  },
  {
    term: 'Arweave',
    definition: 'A decentralized storage network that provides permanent, immutable data storage. Arweave uses its native AR token for storage incentives and a unique "Proof of Access" consensus mechanism. HyperBEAM leverages Arweave for permanent storage and integration with the permaweb ecosystem.',
    related: ['AO', 'ANS-104', 'Permaweb', 'Permanent Storage'],
    category: 'Storage',
    letter: 'A'
  },
  {
    term: 'Attestation',
    definition: 'Cryptographic proof of a message\'s origin and processing history. Attestations in HyperBEAM form chains that validate the provenance and integrity of messages throughout their lifecycle.',
    related: ['Attestation Chain', 'Cryptographic Verification', 'Message Integrity'],
    category: 'Security',
    letter: 'A'
  },
  {
    term: 'Attestation Chain',
    definition: 'A linked series of cryptographic attestations that allows verification of a message\'s processing history. Each step in message processing can add its signature to the chain, enabling end-to-end verification across subsystem boundaries.',
    related: ['Attestation', 'Message Processing', 'Trust Boundary'],
    category: 'Security',
    letter: 'A'
  },

  // Terms starting with B
  {
    term: 'Boundary',
    definition: 'A defined interface between subsystems or components where messages undergo validation, transformation, or security checks. HyperBEAM implements explicit boundaries with clear responsibilities for maintaining system integrity.',
    related: ['Trust Boundary', 'Protocol Adaptation', 'Message Transformation'],
    category: 'Architecture',
    letter: 'B'
  },

  // Terms starting with C
  {
    term: 'Cache',
    definition: 'In HyperBEAM, a content-addressed storage system for messages and computation results. The cache system provides deduplication, attestation linking, and efficient retrieval of previously processed content.',
    related: ['Cache Control', 'Content-Addressed Storage', 'Storage'],
    category: 'Storage',
    letter: 'C'
  },
  {
    term: 'Cache Control',
    definition: 'A system similar to HTTP cache control that manages caching policies through directives like `max-age`, `must-revalidate`, and `no-store`. HyperBEAM implements cache control through `hb_cache_control.erl` with precedence rules for multiple policy sources.',
    related: ['Cache', 'Storage', 'Cascading Configuration Resolution'],
    category: 'Storage',
    letter: 'C'
  },
  {
    term: 'Capability',
    definition: 'A defined and encapsulated piece of functionality that can be discovered, invoked, and composed with other capabilities. In HyperBEAM, capabilities are often implemented as devices that can be dynamically selected and chained together.',
    related: ['Device', 'Composition', 'Delegated Capability'],
    category: 'Architecture',
    letter: 'C'
  },
  {
    term: 'Codec',
    definition: 'A component that converts between different message formats. HyperBEAM includes codecs for various formats including ANS-104, HTTP signatures, JSON, and its internal typed message format (TABM).',
    related: ['TABM', 'Protocol Adaptation', 'Message Transformation'],
    category: 'Codec',
    letter: 'C'
  },
  {
    term: 'Composition',
    definition: 'The architectural pattern of combining multiple simpler components to create more complex functionality. HyperBEAM uses composition extensively through mechanisms like device stacks, message transformation chains, and delegation patterns.',
    related: ['Device Stack', 'Delegated Capability', 'Message Transformation Chain'],
    category: 'Architecture',
    letter: 'C'
  },
  {
    term: 'Content-Addressed Storage',
    definition: 'A storage system where content is retrieved based on its cryptographic hash rather than a location-based address. HyperBEAM\'s cache and store subsystems are content-addressed, enabling deduplication, verification, and distributed storage.',
    related: ['Cache', 'Store', 'Hash Path'],
    category: 'Storage',
    letter: 'C'
  },
  {
    term: 'Converge Protocol',
    definition: 'The core message resolution system in HyperBEAM, implemented in `hb_converge.erl`. It manages the process of resolving a message through a series of device handlers, implementing cryptographic chaining and dispatch mechanisms.',
    related: ['Message Resolution', 'Device', 'Attestation Chain'],
    category: 'Core Infrastructure',
    letter: 'C'
  },

  // Terms starting with D
  {
    term: 'Deep Hash',
    definition: 'A recursive hashing algorithm used in Arweave, implemented in `ar_deep_hash.erl`. It provides deterministic hashing for complex nested data structures, ensuring consistent identification across the distributed system.',
    related: ['Content-Addressed Storage', 'Arweave Integration', 'Hash Path'],
    category: 'Arweave',
    letter: 'D'
  },
  {
    term: 'Delegation',
    definition: 'The architectural pattern where one component passes responsibility for an operation to another specialized component. HyperBEAM implements delegation through various mechanisms including device swapping, message forwarding, and computation offloading.',
    related: ['Delegated Capability', 'Device Stack', 'Device'],
    category: 'Architecture',
    letter: 'D'
  },
  {
    term: 'Device',
    definition: 'The fundamental processing unit in HyperBEAM\'s architecture. Devices are specialized components that implement specific capabilities and can be composed into stacks or chains. Example devices include `dev_message`, `dev_wasm`, and `dev_router`.',
    related: ['Device Stack', 'Capability', 'Message Resolution'],
    category: 'Device Ecosystem',
    letter: 'D'
  },
  {
    term: 'Device Stack',
    definition: 'A composed sequence of devices that process messages in either fold or map mode. Device stacks allow complex operations to be built from simpler components, with `dev_stack` providing the infrastructure for this composition.',
    related: ['Device', 'Fold Mode', 'Map Mode', 'Composition'],
    category: 'Device Ecosystem',
    letter: 'D'
  },

  // Terms starting with F
  {
    term: 'Fold Mode',
    definition: 'A processing mode for device stacks where each device processes the output of the previous device sequentially, creating a transformation pipeline. Contrast with Map Mode.',
    related: ['Device Stack', 'Map Mode', 'Composition'],
    category: 'Device Ecosystem',
    letter: 'F'
  },

  // Terms starting with G
  {
    term: 'Green Zone',
    definition: 'A secure communication and identity management system between trusted nodes, implemented in `dev_green_zone.erl`. It uses hardware attestation, RSA key exchange, and AES encryption to establish secure channels.',
    related: ['Trust Boundary', 'SEV-SNP', 'Attestation'],
    category: 'Security',
    letter: 'G'
  },

  // Terms starting with H
  {
    term: 'Hash Path',
    definition: 'A data structure representing cryptographically linked paths within messages, implemented through `hb_path.erl`. Hash paths enable secure navigation, verification, and modification of nested message structures.',
    related: ['Message Path', 'Content-Addressed Storage', 'Path Patching'],
    category: 'Core Infrastructure',
    letter: 'H'
  },
  {
    term: 'HyperBEAM Message',
    definition: 'The primary data structure in the HyperBEAM system, represented as a nested map-like structure. Messages contain both content and metadata, including attestations, security properties, and processing directives.',
    related: ['Message Path', 'Attestation', 'Message Resolution'],
    category: 'Core Infrastructure',
    letter: 'H'
  },

  // Terms starting with I
  {
    term: 'Identity Device',
    definition: 'A device that provides message field access, manipulation, and attestation handling, implemented in `dev_message.erl`. It serves as the foundational device for working with message contents.',
    related: ['Device', 'Message Path', 'HyperBEAM Message'],
    category: 'Device Ecosystem',
    letter: 'I'
  },

  // Terms starting with L
  {
    term: 'Late Binding',
    definition: 'The architectural pattern where components are connected or selected at runtime rather than compile time. HyperBEAM uses late binding extensively for device selection, configuration resolution, and protocol adaptation.',
    related: ['Delegated Capability', 'Option Map', 'Capability'],
    category: 'Architecture',
    letter: 'L'
  },

  // Terms starting with M
  {
    term: 'Map Mode',
    definition: 'A processing mode for device stacks where messages are processed by multiple devices in parallel, and the results are combined. Contrast with Fold Mode.',
    related: ['Device Stack', 'Fold Mode', 'Composition'],
    category: 'Device Ecosystem',
    letter: 'M'
  },
  {
    term: 'Message Path',
    definition: 'A hierarchical identifier used to locate elements within a message, similar to JSONPath. HyperBEAM uses path expressions for accessing, modifying, and verifying portions of message content.',
    related: ['Hash Path', 'HyperBEAM Message', 'Path Patching'],
    category: 'Core Infrastructure',
    letter: 'M'
  },
  {
    term: 'Message Resolution',
    definition: 'The process of handling a message through the Converge protocol, which may involve multiple transformation steps, device processing, and attestation. Message resolution is the central concept in HyperBEAM\'s processing model.',
    related: ['Converge Protocol', 'Device', 'Message Transformation'],
    category: 'Core Infrastructure',
    letter: 'M'
  },
  {
    term: 'Message Transformation',
    definition: 'The process of converting messages between different formats or enriching them with additional metadata. Transformation occurs at various points in message processing, especially at subsystem boundaries.',
    related: ['Codec', 'Protocol Adaptation', 'Boundary'],
    category: 'Core Infrastructure',
    letter: 'M'
  },
  {
    term: 'Meta Device',
    definition: 'A device that serves as an entry point and gateway for all messages, implemented in `dev_meta.erl`. It establishes a processing pipeline with pre/post-processing and node configuration management.',
    related: ['Device', 'Message Resolution', 'Device Stack'],
    category: 'Device Ecosystem',
    letter: 'M'
  },

  // Terms starting with O
  {
    term: 'Option Map (Opts)',
    definition: 'A common pattern in HyperBEAM\'s APIs where a map of options is passed to functions to control their behavior. The `hb_opts.erl` module provides utilities for working with option maps, including default values and override mechanisms.',
    related: ['Cascading Configuration Resolution', 'Late Binding', 'Configuration'],
    category: 'Core Infrastructure',
    letter: 'O'
  },

  // Terms starting with P
  {
    term: 'Path Patching',
    definition: 'A mechanism for modifying parts of a message outside a component\'s primary area through declarative PATCH operations, implemented in `dev_patch.erl`. This allows components to make targeted changes without requiring full message access.',
    related: ['Message Path', 'Hash Path', 'HyperBEAM Message'],
    category: 'Core Infrastructure',
    letter: 'P'
  },
  {
    term: 'Pricing Policy',
    definition: 'A component of the payment system that determines the cost of processing messages based on various factors such as message size, complexity, or user identity. Pricing policies can be plugged into the `dev_p4` payment framework.',
    related: ['Process', 'Scheduler', 'Option Map'],
    category: 'Payment',
    letter: 'P'
  },
  {
    term: 'Process',
    definition: 'In HyperBEAM, a high-level abstraction for a computational unit that executes operations through devices. Processes are managed by the scheduler and may involve multiple computation steps across different devices.',
    related: ['Scheduler', 'Device', 'Slot'],
    category: 'Core Infrastructure',
    letter: 'P'
  },
  {
    term: 'Protocol Adaptation',
    definition: 'The mechanism for converting between different communication protocols, especially at system boundaries. HyperBEAM implements protocol adaptation through dedicated components that bridge between internal message formats and external protocols like HTTP.',
    related: ['Boundary', 'Codec', 'Message Transformation'],
    category: 'Network',
    letter: 'P'
  },

  // Terms starting with R
  {
    term: 'Router',
    definition: 'A service discovery mechanism implemented in `hb_router.erl` and `dev_router.erl` that directs outbound messages to appropriate network endpoints using configurable routes, pattern matching, and load balancing strategies.',
    related: ['Protocol Adaptation', 'Message Resolution', 'Network'],
    category: 'Network',
    letter: 'R'
  },

  // Terms starting with S
  {
    term: 'Scheduler',
    definition: 'A component that manages process execution order through slot-based scheduling, implemented in `dev_scheduler.erl` and related modules. The scheduler ensures consistent execution across distributed nodes.',
    related: ['Process', 'Slot', 'Device'],
    category: 'Core Infrastructure',
    letter: 'S'
  },
  {
    term: 'SEV-SNP (Secure Encrypted Virtualization-Secure Nested Paging)',
    definition: 'AMD\'s hardware-based security technology that HyperBEAM leverages through `dev_snp` to provide cryptographic validation of node integrity and establish trust between components.',
    related: ['Green Zone', 'Attestation', 'Trust Boundary'],
    category: 'Security',
    letter: 'S'
  },
  {
    term: 'Slot',
    definition: 'A position in the scheduler\'s sequence that represents a unique point in the processing timeline. Slots are used to order operations consistently across distributed nodes and provide a reference point for retrieving computation results.',
    related: ['Scheduler', 'Process', 'Message Resolution'],
    category: 'Core Infrastructure',
    letter: 'S'
  },
  {
    term: 'Store',
    definition: 'HyperBEAM\'s pluggable storage subsystem that provides a unified interface to multiple backends including file system, RocksDB, remote nodes, and gateways. The store system is content-addressed and hierarchical.',
    related: ['Cache', 'Content-Addressed Storage', 'Storage'],
    category: 'Storage',
    letter: 'S'
  },

  // Terms starting with T
  {
    term: 'TABM (Type-Aware Binary Message)',
    definition: 'HyperBEAM\'s internal richly typed message format, implemented in `dev_codec_structured.erl`. TABM preserves Erlang types across serialization boundaries, enabling type-safe message processing.',
    related: ['Codec', 'HyperBEAM Message', 'Protocol Adaptation'],
    category: 'Codec',
    letter: 'T'
  },
  {
    term: 'Trust Boundary',
    definition: 'A security perimeter between components or subsystems with different trust levels. HyperBEAM implements explicit trust boundaries with verification mechanisms to ensure security properties are maintained during boundary crossings.',
    related: ['Boundary', 'Attestation', 'Layered Security Verification'],
    category: 'Security',
    letter: 'T'
  },

  // Terms starting with W
  {
    term: 'WASM (WebAssembly)',
    definition: 'A binary instruction format used in HyperBEAM to provide a sandboxed execution environment through `dev_wasm` and `dev_wasi`. The WASM runtime enables secure execution of untrusted code within the trusted HyperBEAM environment.',
    related: ['Device', 'Capability', 'Sandbox'],
    category: 'Device Ecosystem',
    letter: 'W'
  },
];

// Architectural patterns
export const architecturalPatterns = [
  {
    term: 'Cascading Configuration Resolution',
    definition: 'A pattern where configuration is resolved through multiple sources with clear precedence: request-specific → context-specific → component-specific → subsystem default → global default → built-in default. This enables both system-wide consistency and local customization.',
    related: ['Option Map', 'Configuration', 'Late Binding'],
    category: 'Architectural Pattern',
    letter: 'C'
  },
  {
    term: 'Delegated Capability',
    definition: 'A pattern where specialized components are dynamically selected and invoked based on required capabilities. The pattern involves request → capability determination → component selection → capability invocation → result collection → response composition.',
    related: ['Capability', 'Device', 'Late Binding'],
    category: 'Architectural Pattern',
    letter: 'D'
  },
  {
    term: 'Layered Security Verification',
    definition: 'A pattern implementing security checks in progressive layers: external request → boundary authentication → message integrity verification → authorization check → attestation verification → capability check → operation execution.',
    related: ['Trust Boundary', 'Attestation', 'Security'],
    category: 'Architectural Pattern',
    letter: 'L'
  },
  {
    term: 'Message Transformation Chain',
    definition: 'A pattern for processing messages through sequential transformations: input message → validation → transformation → delegation → component processing → result transformation → output message.',
    related: ['Message Transformation', 'Codec', 'Device Stack'],
    category: 'Architectural Pattern',
    letter: 'M'
  },
  {
    term: 'Trusted Gateway',
    definition: 'A pattern mediating between trust domains: source domain → gateway → protocol conversion → security validation → authorization → transformation → target domain.',
    related: ['Trust Boundary', 'Protocol Adaptation', 'Boundary'],
    category: 'Architectural Pattern',
    letter: 'T'
  }
];

// Abbreviations
export const abbreviations = [
  {
    term: 'ANS',
    definition: 'Arweave Network Standard',
    related: ['ANS-104', 'Arweave Integration'],
    category: 'Abbreviation',
    letter: 'A'
  },
  {
    term: 'API',
    definition: 'Application Programming Interface',
    related: [],
    category: 'Abbreviation',
    letter: 'A'
  },
  {
    term: 'CU',
    definition: 'Computation Unit',
    related: ['Process', 'Scheduler'],
    category: 'Abbreviation',
    letter: 'C'
  },
  {
    term: 'JSON',
    definition: 'JavaScript Object Notation',
    related: ['Codec', 'Message Format'],
    category: 'Abbreviation',
    letter: 'J'
  },
  {
    term: 'OTP',
    definition: 'Open Telecom Platform (Erlang\'s standard library and runtime)',
    related: [],
    category: 'Abbreviation',
    letter: 'O'
  },
  {
    term: 'P4',
    definition: 'Payment system in HyperBEAM (dev_p4)',
    related: ['Pricing Policy', 'Payment'],
    category: 'Abbreviation',
    letter: 'P'
  },
  {
    term: 'PODA',
    definition: 'Proof of Data Availability',
    related: ['Arweave Integration', 'Storage'],
    category: 'Abbreviation',
    letter: 'P'
  },
  {
    term: 'SNP',
    definition: 'Secure Nested Paging',
    related: ['SEV-SNP', 'Security'],
    category: 'Abbreviation',
    letter: 'S'
  },
  {
    term: 'TABM',
    definition: 'Type-Aware Binary Message',
    related: ['Codec', 'Message Format'],
    category: 'Abbreviation',
    letter: 'T'
  },
  {
    term: 'WASI',
    definition: 'WebAssembly System Interface',
    related: ['WASM', 'Device Ecosystem'],
    category: 'Abbreviation',
    letter: 'W'
  },
  {
    term: 'WASM',
    definition: 'WebAssembly',
    related: ['Device', 'Sandbox'],
    category: 'Abbreviation',
    letter: 'W'
  }
];

// Combine all glossary items for export
export const allGlossaryItems = [
  ...glossaryTerms,
  ...architecturalPatterns,
  ...abbreviations
];

// Categorized glossary items
export const glossaryCategories = [
  'Architecture',
  'Arweave',
  'Architectural Pattern',
  'Abbreviation',
  'Codec',
  'Core Infrastructure',
  'Device Ecosystem',
  'Network',
  'Payment',
  'Security',
  'Storage'
];

// Export default for easy importing
export default allGlossaryItems;
