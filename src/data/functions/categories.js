// Module categories for function reference
export const moduleCategories = [
  {
    id: 'core',
    name: 'Core Infrastructure',
    count: 15,
    description: 'Fundamental system operations that form the backbone of the HyperBEAM infrastructure. These functions handle critical operations such as bootstrap, convergence, and system management.',
    modules: [
      {
        id: 'hb_system',
        name: 'System Management',
        count: 5
      },
      {
        id: 'hb_converge',
        name: 'Convergence',
        count: 3
      },
      {
        id: 'hb_bootstrap',
        name: 'Bootstrap',
        count: 4
      },
      {
        id: 'hb_monitor',
        name: 'Monitoring',
        count: 3
      }
    ]
  },
  {
    id: 'storage',
    name: 'Storage Subsystem',
    count: 8,
    description: 'Functions related to data storage, retrieval, and management. The storage subsystem handles both persistent and ephemeral data with transaction support.',
    modules: [
      {
        id: 'hb_storage',
        name: 'Storage API',
        count: 3
      },
      {
        id: 'hb_cache',
        name: 'Cache',
        count: 3
      },
      {
        id: 'hb_persistence',
        name: 'Persistence',
        count: 2
      }
    ]
  },
  {
    id: 'network',
    name: 'Network Subsystem',
    count: 6,
    description: 'Network communication functions for internal and external connectivity. These functions handle protocol negotiation, message routing, and data transmission.',
    modules: [
      {
        id: 'hb_net',
        name: 'Network Core',
        count: 3
      },
      {
        id: 'hb_protocol',
        name: 'Protocol',
        count: 3
      }
    ]
  },
  {
    id: 'arweave',
    name: 'Arweave Integration',
    count: 7,
    description: 'Functions for interacting with the Arweave permaweb blockchain. These functions manage transaction creation, submission, and data retrieval from the Arweave network.',
    modules: [
      {
        id: 'hb_arweave',
        name: 'Arweave Core',
        count: 4
      },
      {
        id: 'hb_arweave_tx',
        name: 'Transaction',
        count: 3
      }
    ]
  },
  {
    id: 'app-management',
    count: 5,
    name: 'App Management',
    description: 'Functions for managing applications within the HyperBEAM ecosystem. These functions handle application lifecycle, deployment, and resource allocation.',
    modules: [
      {
        id: 'hb_app',
        name: 'App Core',
        count: 2
      },
      {
        id: 'hb_app_runner',
        name: 'App Runner',
        count: 3
      }
    ]
  },
  {
    id: 'codec',
    name: 'Codec and Data Format',
    count: 3,
    description: 'Data serialization and deserialization functions for various formats. These functions handle encoding, decoding, and schema validation.',
    modules: [
      {
        id: 'hb_codec',
        name: 'Codec Core',
        count: 3
      }
    ]
  }
];

// Function categories for filtering
export const functionCategories = [
  {
    id: 'public_api',
    name: 'Public API',
    count: 28
  },
  {
    id: 'internal_api',
    name: 'Internal API',
    count: 16
  },
  {
    id: 'lifecycle',
    name: 'Lifecycle Management',
    count: 12
  },
  {
    id: 'data',
    name: 'Data Processing',
    count: 15
  },
  {
    id: 'network',
    name: 'Network Operations',
    count: 9
  }
];
