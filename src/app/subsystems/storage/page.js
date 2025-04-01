"use client"

import Link from 'next/link';
import Terminal from '../../../components/Terminal';

export default function StorageSubsystem() {
  // Sample terminal commands for storage subsystem
  const initialCommands = [
    { 
      command: 'ls -la subsystems/storage/', 
      output: `total 9
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 ./
drwxr-xr-x 5 hyperbeam system 4096 Mar 27 2025 ../
-rw-r--r-- 1 hyperbeam system 15891 Mar 27 2025 hb_store.erl
-rw-r--r-- 1 hyperbeam system 12583 Mar 27 2025 hb_store_fs.erl
-rw-r--r-- 1 hyperbeam system 14210 Mar 27 2025 hb_store_gateway.erl
-rw-r--r-- 1 hyperbeam system 11324 Mar 27 2025 hb_store_remote_node.erl
-rw-r--r-- 1 hyperbeam system 16752 Mar 27 2025 hb_store_rocksdb.erl
-rw-r--r-- 1 hyperbeam system 8901 Mar 27 2025 hb_cache.erl
-rw-r--r-- 1 hyperbeam system 7326 Mar 27 2025 hb_cache_control.erl
-rw-r--r-- 1 hyperbeam system 5691 Mar 27 2025 hb_cache_render.erl
-rw-r--r-- 1 hyperbeam system 10124 Mar 27 2025 hb_persistent.erl`
    }
  ];

  // Storage modules data
  const storageModules = [
    {
      id: 'hb_store',
      name: 'hb_store.erl',
      description: 'Primary storage interface module that abstracts various storage backends.',
      functions: 14,
      dependencies: 3,
      dependents: 12,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_store.erl'
    },
    {
      id: 'hb_store_fs',
      name: 'hb_store_fs.erl',
      description: 'Filesystem-based storage implementation for persisting data to disk.',
      functions: 10,
      dependencies: 2,
      dependents: 1,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_store_fs.erl'
    },
    {
      id: 'hb_store_gateway',
      name: 'hb_store_gateway.erl',
      description: 'Gateway implementation for routing storage operations to remote nodes.',
      functions: 12,
      dependencies: 5,
      dependents: 1,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_store_gateway.erl'
    },
    {
      id: 'hb_store_remote_node',
      name: 'hb_store_remote_node.erl',
      description: 'Remote node storage implementation for distributed data storage.',
      functions: 9,
      dependencies: 4,
      dependents: 1,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_store_remote_node.erl'
    },
    {
      id: 'hb_store_rocksdb',
      name: 'hb_store_rocksdb.erl',
      description: 'RocksDB-based storage implementation for high-performance persistence.',
      functions: 15,
      dependencies: 3,
      dependents: 1,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_store_rocksdb.erl'
    },
    {
      id: 'hb_cache',
      name: 'hb_cache.erl',
      description: 'Caching system for temporary storage of frequently accessed data.',
      functions: 8,
      dependencies: 2,
      dependents: 9,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_cache.erl'
    },
    {
      id: 'hb_cache_control',
      name: 'hb_cache_control.erl',
      description: 'Cache policy enforcement and management module.',
      functions: 7,
      dependencies: 2,
      dependents: 2,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_cache_control.erl'
    },
    {
      id: 'hb_cache_render',
      name: 'hb_cache_render.erl',
      description: 'Rendering and visualization module for cache statistics and state.',
      functions: 5,
      dependencies: 2,
      dependents: 1,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_cache_render.erl'
    },
    {
      id: 'hb_persistent',
      name: 'hb_persistent.erl',
      description: 'Management of persistent processes and state across system restarts.',
      functions: 9,
      dependencies: 3,
      dependents: 7,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_persistent.erl'
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
          <span className="text-terminal-green">Storage</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Storage Subsystem
        </h1>
        
        <p className="text-lg mb-8">
          The Storage Subsystem provides reliable and efficient data persistence mechanisms, with multiple backend options 
          and an integrated caching system to optimize performance.
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6 flex items-center justify-between">
          <div className="font-mono text-sm">
            <span className="text-terminal-green">$</span> 
            <span className="text-terminal-accent ml-2">cd</span> 
            <span className="text-terminal-output ml-2">subsystems/storage/</span>
          </div>
          <div className="bg-[#333] px-3 py-1 rounded text-xs font-mono">
            9 modules | 89 functions | 24 dependencies
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
                The Storage Subsystem is designed with a layered architecture that abstracts storage details away from 
                the rest of the system, providing a unified interface regardless of the underlying storage technology.
              </p>
              
              <div className="bg-[#2d2d2d] p-5 rounded-md">
                {/* ASCII architecture diagram */}
                <pre className="text-xs sm:text-sm text-terminal-text font-mono whitespace-pre overflow-x-auto">
{`┌─────────────────────────────────────────────────────────────┐
│                   APPLICATION LAYER                          │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│                  STORAGE INTERFACE (hb_store)                │
└───┬─────────────┬─────────┬────────────┬──────────────┬─────┘
    │             │         │            │              │
┌───▼───┐   ┌─────▼────┐   ┌▼────────┐  ┌▼──────────┐ ┌▼─────────┐
│  FS   │   │ Gateway  │   │ Remote  │  │  RocksDB  │ │   ETS    │
│Backend│   │ Backend  │   │  Node   │  │  Backend  │ │  Cache   │
└───────┘   └──────────┘   └─────────┘  └───────────┘ └───────────┘
                                                           │
                                                       ┌───▼────┐
                                                       │ Cache  │
                                                       │Control │
                                                       └────────┘`}
                </pre>
              </div>
              
              <p className="text-sm">
                The subsystem is composed of several interconnected components:
              </p>
              
              <ul className="space-y-2 text-sm pl-5 list-disc">
                <li>
                  <strong>Storage Interface (hb_store)</strong> - Provides a unified API for all storage operations, abstracting away backend details
                </li>
                <li>
                  <strong>Storage Backends</strong> - Multiple backend implementations for different use cases:
                  <ul className="pl-5 mt-1 space-y-1 list-disc">
                    <li>Filesystem Backend - Local file storage</li>
                    <li>Gateway Backend - Routes storage operations to appropriate backends</li>
                    <li>Remote Node Backend - Distributed storage across nodes</li>
                    <li>RocksDB Backend - High-performance key-value storage</li>
                  </ul>
                </li>
                <li>
                  <strong>Caching System</strong> - Optimizes performance by storing frequently accessed data:
                  <ul className="pl-5 mt-1 space-y-1 list-disc">
                    <li>ETS Cache - In-memory storage using Erlang Term Storage</li>
                    <li>Cache Control - Manages cache policies, eviction, and TTL</li>
                    <li>Cache Render - Visualization and statistics for cache performance</li>
                  </ul>
                </li>
                <li>
                  <strong>Persistence Management</strong> - Handles long-lived processes and state persistence across restarts
                </li>
              </ul>
            </div>
          </section>
          
          {/* Key Features */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Key Features</h2>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-5">
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Storage Abstraction</h3>
                <p className="text-sm opacity-80">
                  Provides a unified interface across different storage backends, allowing seamless switching between storage technologies.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Distributed Storage</h3>
                <p className="text-sm opacity-80">
                  Enables data distribution across multiple nodes with automatic routing and failover capabilities.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Tiered Caching</h3>
                <p className="text-sm opacity-80">
                  Implements a multi-level caching system with configurable policies for optimal performance tuning.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Persistence Guarantees</h3>
                <p className="text-sm opacity-80">
                  Provides configurable persistence guarantees from eventual consistency to strict durability requirements.
                </p>
              </div>
            </div>
          </section>
          
          {/* Core Modules */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Core Modules</h2>
            
            <div className="grid grid-cols-1 gap-4">
              {storageModules.map((module) => {
                // All modules now have documentation since JSON files exist
                const hasDocumentation = true;
                
                return (
                <Link 
                    href={`/subsystems/storage/${module.id}`}
                    key={module.id} 
                    className="block"
                  >
                    <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-all duration-200 card-hover-anim">
                      <div className="flex flex-col md:flex-row justify-between items-start">
                        <div>
                          <h3 className="font-mono text-lg font-semibold mb-2 text-terminal-primary">
                            {module.name}
                            {hasDocumentation ? (
                              <span className="ml-3 text-xs font-normal text-terminal-accent bg-[#1E1E1E] px-2 py-0.5 rounded">
                                Full Documentation
                              </span>
                            ) : (
                              <a 
                                href={module.githubLink}
                                target="_blank" 
                                rel="noopener noreferrer"
                                className="ml-3 text-xs font-normal text-terminal-green inline-flex items-center hover:underline"
                                onClick={(e) => e.stopPropagation()}
                              >
                                <svg xmlns="http://www.w3.org/2000/svg" className="h-3 w-3 mr-1" viewBox="0 0 20 20" fill="currentColor">
                                  <path fillRule="evenodd" d="M12.316 3.051a1 1 0 01.633 1.265l-4 12a1 1 0 11-1.898-.632l4-12a1 1 0 011.265-.633zM5.707 6.293a1 1 0 010 1.414L3.414 10l2.293 2.293a1 1 0 11-1.414 1.414l-3-3a1 1 0 010-1.414l3-3a1 1 0 011.414 0zm8.586 0a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 11-1.414-1.414L16.586 10l-2.293-2.293a1 1 0 010-1.414z" clipRule="evenodd" />
                                </svg>
                                GitHub
                              </a>
                            )}
                          </h3>
                          <p className="text-sm opacity-80 mb-4">{module.description}</p>
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
                  </Link>
                );
              })}
            </div>
          </section>
          
          {/* Usage Examples */}
          <section>
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Usage Examples</h2>
            
            <div className="bg-[#252526] border border-[#333] rounded-md p-6">
              <p className="mb-4">
                The following examples demonstrate common usage patterns for the Storage Subsystem:
              </p>
              
              <div className="mb-6">
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Basic Storage Operations</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Store a value
Key = <<"user:123">>,
Value = #{name => <<"John">>, age => 30},
{ok, _} = hb_store:put(Key, Value),

%% Retrieve a value
{ok, StoredValue} = hb_store:get(Key),

%% Delete a value
{ok, _} = hb_store:delete(Key),

%% Check if a key exists
{ok, Exists} = hb_store:exists(Key),

%% Atomic updates
{ok, _} = hb_store:update(Key, fun(OldValue) -> 
    OldValue#{visits => maps:get(visits, OldValue, 0) + 1}
end)`}</code>
                </pre>
              </div>
              
              <div className="mb-6">
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Working with Cache</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Cache a value with TTL
Key = <<"session:456">>,
Value = #{token => <<"abc123">>, user_id => 789},
TTL = 3600,  % seconds
{ok, _} = hb_cache:put(Key, Value, TTL),

%% Get cached value
{ok, CachedValue} = hb_cache:get(Key),

%% Invalidate cache entry
{ok, _} = hb_cache:invalidate(Key),

%% Clear entire cache
{ok, _} = hb_cache:clear(),

%% Get cache statistics
{ok, Stats} = hb_cache_render:get_stats(),
io:format("Cache hit rate: ~p%~n", [maps:get(hit_rate, Stats)])`}</code>
                </pre>
              </div>
              
              <div>
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Using Specific Backend</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Configure specific backend options
Options = #{
    backend => rocksdb,
    path => "/var/lib/hyperbeam/data",
    compression => true
},

%% Initialize storage with options
{ok, _} = hb_store:init(Options),

%% Use backend-specific features
{ok, _} = hb_store_rocksdb:create_snapshot(),

%% Run compaction
{ok, _} = hb_store_rocksdb:compact(),

%% Close backend connection
{ok, _} = hb_store:close()`}</code>
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
                  <Link href="/subsystems/network" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z" clipRule="evenodd" />
                    </svg>
                    Network Communication
                  </Link>
                </li>
                <li>
                  <Link href="/subsystems/arweave" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z" clipRule="evenodd" />
                    </svg>
                    Arweave Integration
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
