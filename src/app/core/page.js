"use client"

import { useState } from 'react';
import Link from 'next/link';
import Terminal from '../../components/Terminal';

export default function CoreInfrastructure() {
  // Sample terminal commands for core infrastructure
  const initialCommands = [
    { 
      command: 'ls -la core/', 
      output: `total 6
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 ./
drwxr-xr-x 8 hyperbeam system 4096 Mar 27 2025 ../
-rw-r--r-- 1 hyperbeam system 12583 Mar 27 2025 hb.erl
-rw-r--r-- 1 hyperbeam system 22105 Mar 27 2025 hb_converge.erl
-rw-r--r-- 1 hyperbeam system 8742 Mar 27 2025 hb_message.erl
-rw-r--r-- 1 hyperbeam system 6190 Mar 27 2025 hb_opts.erl
-rw-r--r-- 1 hyperbeam system 7326 Mar 27 2025 hb_path.erl
-rw-r--r-- 1 hyperbeam system 15891 Mar 27 2025 hb_util.erl`
    }
  ];

  // Core modules data
  const coreModules = [
    {
      id: 'hb_util',
      name: 'hb_util.erl',
      description: 'Utility functions and common operations for the HyperBEAM core system.',
      functions: 12,
      dependencies: 2,
      dependents: 15,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_util.erl'
    },
    {
      id: 'hb_converge',
      name: 'hb_converge.erl',
      description: 'Message resolution system with cryptographic chaining and device dispatch.',
      functions: 9,
      dependencies: 4,
      dependents: 11,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_converge.erl'
    },
    {
      id: 'hb_opts',
      name: 'hb_opts.erl',
      description: 'Configuration management and option processing for HyperBEAM.',
      functions: 7,
      dependencies: 1,
      dependents: 8,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_opts.erl'
    },
    {
      id: 'hb_message',
      name: 'hb_message.erl',
      description: 'Message creation, validation, and transformation utilities.',
      functions: 10,
      dependencies: 3,
      dependents: 18,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_message.erl'
    },
    {
      id: 'hb_path',
      name: 'hb_path.erl',
      description: 'Path-based access to nested message data structures.',
      functions: 6,
      dependencies: 1,
      dependents: 7,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_path.erl'
    },
    {
      id: 'hb',
      name: 'hb.erl',
      description: 'Main application entry point and API surface for HyperBEAM.',
      functions: 14,
      dependencies: 5,
      dependents: 4,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb.erl'
    }
  ];

  return (
    <>
      <div className="mb-6">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <span className="text-terminal-green">Core Modules</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Core Modules
        </h1>
        
        <p className="text-lg mb-8">
          HyperBEAM's core infrastructure consists of six essential modules that provide the foundation 
          for message processing, device coordination, and system configuration.
        </p>
      </div>
      
      {/* Simplified UI - Cleaner layout with fewer visual elements */}
      <div className="grid gap-8 mb-12">
        {/* Core Infrastructure Architecture */}
        <section className="bg-[#252526] border border-[#333] rounded-md p-6">
          <h2 className="text-2xl font-bold text-terminal-primary mb-4">Architecture Overview</h2>
          
          <div className="mb-6">
            <p className="mb-6">
              The core infrastructure modules form a layered architecture with clear responsibilities:
            </p>
            
            <div className="bg-[#2d2d2d] p-5 rounded overflow-hidden">
              {/* Simplified diagram with basic ASCII art */}
              <pre className="text-sm text-terminal-text font-mono overflow-x-auto">
{`┌───────────────────────────────────────────────────────────────┐
│                          hb.erl                                │
│                 (Application Entry Point)                      │
└───────────────────────────┬───────────────────────────────────┘
                            │
┌───────────────────────────▼───────────────────────────────────┐
│                      hb_converge.erl                           │
│                 (Message Resolution Core)                      │
└─┬─────────────────────────┬────────────────────────────────┬──┘
  │                         │                                │
  ▼                         ▼                                ▼
┌─────────────┐    ┌────────────────┐              ┌────────────────┐
│ hb_opts.erl │    │ hb_message.erl │              │  hb_path.erl   │
│(Config Mgmt)│    │(Message Ops)   │              │(Path Access)   │
└──────┬──────┘    └───────┬────────┘              └───────┬────────┘
       │                   │                               │
       └───────────────────▼───────────────────────────────┘
                           │
                 ┌─────────▼──────────┐
                 │    hb_util.erl     │
                 │   (Utilities)      │
                 └────────────────────┘`}
              </pre>
            </div>
          </div>
          
          <div>
            <p className="mb-4">
              The core modules follow these key design principles:
            </p>
            
            <ul className="space-y-2 pl-5 list-disc mb-4">
              <li>Clear separation of concerns with single-responsibility modules</li>
              <li>Consistent error handling patterns across all modules</li>
              <li>Immutable message transformations</li>
              <li>Cryptographic attestation and verification</li>
            </ul>
          </div>
        </section>
        
        {/* Core Modules - Simplified Cards */}
        <section>
          <h2 className="text-2xl font-bold text-terminal-primary mb-6">Core Modules</h2>
          
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {coreModules.map((module) => (
              <Link href={`/core/${module.id}`} key={module.id} className="block">
                <div className="bg-[#252526] border border-[#333] rounded-md p-5 h-full hover:border-terminal-green transition-all duration-200 card-hover-anim">
                  <h3 className="font-mono text-lg font-semibold mb-3 text-terminal-primary">
                    {module.name}
                  </h3>
                  <p className="text-sm opacity-80 mb-4">{module.description}</p>
                  <div className="flex flex-wrap gap-3 text-xs">
                    <span className="bg-[#333] px-2 py-1 rounded-full text-terminal-accent">
                      {module.functions} functions
                    </span>
                    <span className="bg-[#333] px-2 py-1 rounded-full text-terminal-green">
                      {module.dependents} dependents
                    </span>
                  </div>
                </div>
              </Link>
            ))}
          </div>
        </section>
        
        {/* Key Interactions - More focused content */}
        <section className="bg-[#252526] border border-[#333] rounded-md p-6">
          <h2 className="text-2xl font-bold text-terminal-primary mb-4">Key Interactions</h2>
          
          <p className="mb-6">
            The following examples demonstrate how the core modules interact to process messages:
          </p>
          
          <div className="bg-[#2d2d2d] p-4 rounded-md overflow-x-auto">
            <pre className="text-sm font-mono text-terminal-text">
              <code>{`%% Message resolution with converge protocol
{ok, ProcessedMsg} = hb:resolve(
    hb_message:create(#{
        operation => "example_operation",
        data => #{key => "value"}
    })
).

%% Path-based access to nested message data
{ok, Value} = hb_path:get(ProcessedMsg, [data, key]).

%% Configuration with options
Config = hb_opts:from_map(#{
    mode => strict,
    timeout => 5000,
    retry => 3
}).

%% Utility functions
ID = hb_util:generate_id(),
Timestamp = hb_util:timestamp().`}</code>
            </pre>
          </div>
        </section>
        
        {/* Terminal access - Simplified */}
        <section>
          <h2 className="text-2xl font-bold text-terminal-primary mb-4">Interactive Exploration</h2>
          <p className="mb-4">
            Explore the core modules through this interactive terminal:
          </p>
          <Terminal initialCommands={initialCommands} height={300} />
        </section>
      </div>
    </>
  );
}
