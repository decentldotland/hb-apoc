"use client"

import Link from 'next/link';
import Terminal from '../../../components/Terminal';

// Metadata should be defined in a separate metadata.js file since it can't be exported from 'use client' components

export default function ArweaveSubsystem() {
  // Sample terminal commands for arweave subsystem
  const initialCommands = [
    { 
      command: 'ls -la subsystems/arweave/', 
      output: `total 7
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 ./
drwxr-xr-x 5 hyperbeam system 4096 Mar 27 2025 ../
-rw-r--r-- 1 hyperbeam system 14269 Mar 27 2025 hb_arweave.erl
-rw-r--r-- 1 hyperbeam system 12583 Mar 27 2025 hb_arweave_tx.erl
-rw-r--r-- 1 hyperbeam system 11478 Mar 27 2025 hb_arweave_wallet.erl
-rw-r--r-- 1 hyperbeam system 9856 Mar 27 2025 hb_arweave_gateway.erl
-rw-r--r-- 1 hyperbeam system 13742 Mar 27 2025 hb_arweave_bundle.erl
-rw-r--r-- 1 hyperbeam system 10283 Mar 27 2025 hb_arweave_tag.erl
-rw-r--r-- 1 hyperbeam system 9157 Mar 27 2025 hb_arweave_query.erl`
    }
  ];

  // Arweave modules data
  const arweaveModules = [
    {
      id: 'ar_timestamp',
      name: 'ar_timestamp.erl',
      description: 'Core Arweave timestamp services for blockchain time information.',
      functions: 14,
      dependencies: 4,
      dependents: 6,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/ar_timestamp.erl'
    },
    {
      id: 'ar_tx',
      name: 'ar_tx.erl',
      description: 'Transaction creation, signing, and submission to the Arweave network.',
      functions: 12,
      dependencies: 3,
      dependents: 2,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/ar_tx.erl'
    },
    {
      id: 'ar_wallet',
      name: 'ar_wallet.erl',
      description: 'Wallet management for Arweave including key generation, storage, and signing operations.',
      functions: 10,
      dependencies: 2,
      dependents: 3,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/ar_wallet.erl'
    },
    {
      id: 'ar_rate_limiter',
      name: 'ar_rate_limiter.erl',
      description: 'Rate limiting services for Arweave network operations.',
      functions: 9,
      dependencies: 3,
      dependents: 1,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/ar_rate_limiter.erl'
    },
    {
      id: 'ar_bundles',
      name: 'ar_bundles.erl',
      description: 'Transaction bundling for efficient batch submission of multiple transactions.',
      functions: 11,
      dependencies: 2,
      dependents: 1,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/ar_bundles.erl'
    },
    {
      id: 'ar_deep_hash',
      name: 'ar_deep_hash.erl',
      description: 'Deep hash algorithm implementation for Arweave cryptographic operations.',
      functions: 8,
      dependencies: 1,
      dependents: 4,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/ar_deep_hash.erl'
    }
  ];

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <Link href="/subsystems" className="hover:text-terminal-green">Subsystems</Link>
          <span>/</span>
          <span className="text-terminal-green">Arweave</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Arweave Integration Subsystem
        </h1>
        
        <p className="text-lg mb-8">
          The Arweave Integration Subsystem provides a comprehensive interface for interacting with the Arweave 
          permaweb blockchain, enabling permanent data storage, transaction management, and decentralized application support.
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6 flex items-center justify-between">
          <div className="font-mono text-sm">
            <span className="text-terminal-green">$</span> 
            <span className="text-terminal-accent ml-2">cd</span> 
            <span className="text-terminal-output ml-2">subsystems/arweave/</span>
          </div>
          <div className="bg-[#333] px-3 py-1 rounded text-xs font-mono">
            7 modules | 73 functions | 18 dependencies
          </div>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-8 mb-12">
        <div className="lg:col-span-2">
          {/* Subsystem Architecture */}
          <section className="bg-[#252526] border border-[#333] rounded-md p-6 mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Subsystem Architecture</h2>
            
            <div className="space-y-6">
              <p>
                The Arweave Integration Subsystem provides a layered approach to interacting with the Arweave 
                blockchain, abstracting low-level details while providing powerful functionality.
              </p>
              
              <div className="bg-[#2d2d2d] p-5 rounded-md">
                {/* ASCII architecture diagram */}
                <pre className="text-xs sm:text-sm text-terminal-text font-mono whitespace-pre overflow-x-auto">
{`┌──────────────────────────────────────────────────────────────┐
│                    HYPERBEAM APPLICATION                      │
└────────────────────────────┬─────────────────────────────────┘
                             │
┌────────────────────────────▼─────────────────────────────────┐
│                        hb_arweave                            │
│                (Main Arweave Interface)                       │
└───┬─────────┬─────────┬────────────┬──────────────┬──────────┘
    │         │         │            │              │
┌───▼───┐ ┌───▼───┐ ┌───▼───┐  ┌─────▼────┐    ┌────▼─────┐
│  TX   │ │Wallet │ │Gateway│  │  Bundle  │    │  Query   │
│ Module│ │ Module│ │ Module│  │  Module  │    │  Module  │
└───┬───┘ └───────┘ └───────┘  └──────────┘    └──────────┘
    │
┌───▼───┐
│  Tag  │
│ Module│
└───────┘
    │
┌───▼─────────────────────────────────────────────────────────┐
│                     ARWEAVE NETWORK                          │
│  (Nodes, Gateways, GraphQL Endpoints, Bundlers, etc.)        │
└─────────────────────────────────────────────────────────────┘`}
                </pre>
              </div>
              
              <p className="text-sm">
                The subsystem consists of these main components:
              </p>
              
              <ul className="space-y-2 text-sm pl-5 list-disc">
                <li>
                  <strong>Main Interface (hb_arweave)</strong> - Provides a unified API for interacting with the Arweave network
                </li>
                <li>
                  <strong>Transaction Module (hb_arweave_tx)</strong> - Handles transaction creation, signing, and submission
                </li>
                <li>
                  <strong>Wallet Module (hb_arweave_wallet)</strong> - Manages Arweave wallets, keys, and signing operations
                </li>
                <li>
                  <strong>Gateway Module (hb_arweave_gateway)</strong> - Interfaces with Arweave gateways like arweave.net
                </li>
                <li>
                  <strong>Bundle Module (hb_arweave_bundle)</strong> - Implements transaction bundling for efficient data upload
                </li>
                <li>
                  <strong>Tag Module (hb_arweave_tag)</strong> - Manages transaction tags for data indexing and categorization
                </li>
                <li>
                  <strong>Query Module (hb_arweave_query)</strong> - Provides GraphQL query capabilities for data retrieval
                </li>
              </ul>
            </div>
          </section>
          
          {/* Key Features */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Key Features</h2>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-5">
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Permanent Storage</h3>
                <p className="text-sm opacity-80">
                  Store data permanently on the Arweave network with a single payment, ensuring indefinite availability.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Transaction Bundling</h3>
                <p className="text-sm opacity-80">
                  Bundle multiple transactions together for efficient submission and reduced costs through ANS-104 standard.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Tag-Based Indexing</h3>
                <p className="text-sm opacity-80">
                  Create and manage transaction tags for efficient data categorization, indexing, and retrieval.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">GraphQL Queries</h3>
                <p className="text-sm opacity-80">
                  Search and retrieve data from the Arweave network using GraphQL queries with flexible filtering.
                </p>
              </div>
            </div>
          </section>
          
          {/* Core Modules */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Core Modules</h2>
            
            <div className="grid grid-cols-1 gap-4">
              {arweaveModules.map((module) => (
                <div key={module.id} className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-all duration-200 card-hover-anim">
                  <div className="flex flex-col md:flex-row justify-between items-start">
                    <div>
                      <h3 className="font-mono text-lg font-semibold mb-2 text-terminal-primary">
                        <Link href={`/subsystems/arweave/${module.id}`} className="hover:text-terminal-accent">
                          {module.name}
                        </Link>
                        <a 
                          href={module.githubLink}
                          target="_blank" 
                          rel="noopener noreferrer"
                          className="ml-3 text-xs font-normal text-terminal-green inline-flex items-center hover:underline"
                        >
                          <svg xmlns="http://www.w3.org/2000/svg" className="h-3 w-3 mr-1" viewBox="0 0 20 20" fill="currentColor">
                            <path fillRule="evenodd" d="M12.316 3.051a1 1 0 01.633 1.265l-4 12a1 1 0 11-1.898-.632l4-12a1 1 0 011.265-.633zM5.707 6.293a1 1 0 010 1.414L3.414 10l2.293 2.293a1 1 0 11-1.414 1.414l-3-3a1 1 0 010-1.414l3-3a1 1 0 011.414 0zm8.586 0a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 11-1.414-1.414L16.586 10l-2.293-2.293a1 1 0 010-1.414z" clipRule="evenodd" />
                          </svg>
                          GitHub
                        </a>
                      </h3>
                      <Link href={`/subsystems/arweave/${module.id}`} className="block">
                        <p className="text-sm opacity-80 mb-4 hover:text-terminal-text">{module.description}</p>
                      </Link>
                    </div>
                    <div className="flex flex-wrap gap-3 text-xs mt-2 md:mt-0">
                      <span className="bg-[#333] px-2 py-1 rounded-full text-terminal-accent">
                        {module.functions} functions
                      </span>
                      <span className="bg-[#333] px-2 py-1 rounded-full text-terminal-green">
                        {module.dependents} dependents
                      </span>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          </section>
          
          {/* Usage Examples */}
          <section>
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Usage Examples</h2>
            
            <div className="bg-[#252526] border border-[#333] rounded-md p-6">
              <p className="mb-4">
                The following examples demonstrate common usage patterns for the Arweave Integration Subsystem:
              </p>
              
              <div className="mb-6">
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Storing Data on Arweave</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Initialize wallet from keyfile
{ok, Wallet} = hb_arweave_wallet:from_file("path/to/keyfile.json"),

%% Create transaction data
Data = <<"Hello, Permaweb!">>,
Tags = [
  hb_arweave_tag:new(<<"Content-Type">>, <<"text/plain">>),
  hb_arweave_tag:new(<<"App-Name">>, <<"HyperBEAM">>),
  hb_arweave_tag:new(<<"App-Version">>, <<"0.1.0">>)
],

%% Create and sign transaction
{ok, Tx} = hb_arweave_tx:create(Data, Tags, Wallet),
{ok, SignedTx} = hb_arweave_tx:sign(Tx, Wallet),

%% Submit transaction to the network
{ok, TxId} = hb_arweave:submit(SignedTx),

%% Get transaction status
{ok, Status} = hb_arweave:get_tx_status(TxId)`}</code>
                </pre>
              </div>
              
              <div className="mb-6">
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Transaction Bundling</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Create multiple data items
DataItems = [
  {<<"Item 1">>, [hb_arweave_tag:new(<<"Type">>, <<"text">>)]},
  {<<"Item 2">>, [hb_arweave_tag:new(<<"Type">>, <<"text">>)]},
  {<<"Item 3">>, [hb_arweave_tag:new(<<"Type">>, <<"text">>)]}
],

%% Create a bundle
{ok, Bundle} = hb_arweave_bundle:create(),

%% Add data items to the bundle
{ok, UpdatedBundle} = lists:foldl(
  fun({Data, Tags}, {ok, AccBundle}) ->
    hb_arweave_bundle:add_item(AccBundle, Data, Tags)
  end,
  {ok, Bundle},
  DataItems
),

%% Sign the bundle
{ok, SignedBundle} = hb_arweave_bundle:sign(UpdatedBundle, Wallet),

%% Submit the bundle
{ok, BundleTxId} = hb_arweave_bundle:submit(SignedBundle)`}</code>
                </pre>
              </div>
              
              <div>
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Querying the Arweave Network</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Define a GraphQL query to find transactions by tags
Query = #{
  query => "
    query {
      transactions(
        tags: [
          { name: \\"App-Name\\", values: [\\"HyperBEAM\\"] }
        ],
        first: 10
      ) {
        edges {
          node {
            id
            tags {
              name
              value
            }
          }
        }
      }
    }
  "
},

%% Execute the query
{ok, Results} = hb_arweave_query:execute(Query),

%% Process results
Transactions = maps:get(<<"data">>, Results),
Edges = maps:get(<<"transactions">>, Transactions),
TxNodes = [Node || #{<<"node">> := Node} <- Edges],

%% Retrieve data for a specific transaction
TxId = maps:get(<<"id">>, hd(TxNodes)),
{ok, TxData} = hb_arweave:get_tx_data(TxId)`}</code>
                </pre>
              </div>
            </div>
          </section>
        </div>
        
        <div className="lg:col-span-1">
          <div className="sticky top-[80px] space-y-6">
            {/* Quick Navigation */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">In This Section</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <Link href="#architecture" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Subsystem Architecture
                  </Link>
                </li>
                <li>
                  <Link href="#features" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Key Features
                  </Link>
                </li>
                <li>
                  <Link href="#modules" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Core Modules
                  </Link>
                </li>
                <li>
                  <Link href="#usage" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Usage Examples
                  </Link>
                </li>
              </ul>
            </div>
            
            {/* Related Subsystems */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Related Subsystems</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <Link href="/subsystems/storage" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z" clipRule="evenodd" />
                    </svg>
                    Storage Subsystem
                  </Link>
                </li>
                <li>
                  <Link href="/subsystems/network" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z" clipRule="evenodd" />
                    </svg>
                    Network Communication
                  </Link>
                </li>
                <li>
                  <Link href="/subsystems/codec" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z" clipRule="evenodd" />
                    </svg>
                    Codec and Data Format
                  </Link>
                </li>
              </ul>
            </div>
            
            {/* Arweave Standards */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Arweave Standards</h3>
              <ul className="space-y-3 text-sm">
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">ANS-104</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Arweave Network Standard for bundling multiple data items into a single transaction
                    </p>
                  </div>
                </li>
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">ANS-102</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Standard for representing token transfers on the Arweave network
                    </p>
                  </div>
                </li>
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">SmartWeave</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Protocol for creating and executing smart contracts on Arweave
                    </p>
                  </div>
                </li>
              </ul>
            </div>
            
            {/* Terminal Explorer */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Terminal Explorer</h3>
              <Terminal initialCommands={initialCommands} height={200} />
            </div>
          </div>
        </div>
      </div>
    </>
  );
}
