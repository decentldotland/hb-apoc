"use client"

import { useState } from 'react';
import Link from 'next/link';
import Terminal from '../../components/Terminal';

export default function ArchitectureOverview() {
  // Sample terminal commands for architecture exploration
  const initialCommands = [
    { 
      command: 'ls -l architecture/', 
      output: `total 5
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 message-processing/
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 device-composition/
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 subsystems/
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 cross-subsystem/
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 security/`
    }
  ];

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <span className="text-terminal-green">Architecture</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Architecture Overview
        </h1>
        
        <p className="text-lg mb-8">
          HyperBEAM implements a sophisticated architecture built around cryptographic attestation, 
          device-based extensibility, and cross-subsystem integration. This overview explains the key 
          architectural concepts and how they work together.
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6 flex items-center justify-between">
          <div className="font-mono text-sm">
            <span className="text-terminal-green">$</span> 
            <span className="text-terminal-accent ml-2">cd</span> 
            <span className="text-terminal-output ml-2">architecture/</span>
          </div>
          <div className="bg-[#333] px-3 py-1 rounded text-xs font-mono">
            Current version: v0.9.2
          </div>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-8 mb-12">
        <div className="lg:col-span-2">
          {/* Core Architectural Principles */}
          <section className="bg-[#252526] border border-[#333] rounded-md p-6 mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Core Architectural Principles</h2>
            
            <div className="space-y-4">
              <p>
                HyperBEAM is built on several key architectural principles that guide its design and implementation:
              </p>
              
              <div className="bg-[#252526] border border-[#333] rounded-md overflow-hidden">
                <div className="grid grid-cols-1 md:grid-cols-2 divide-y md:divide-y-0 md:divide-x divide-[#333]">
                  <div className="p-4">
                    <h3 className="text-lg font-semibold text-terminal-green mb-2">Device-Based Extensibility</h3>
                    <p className="text-sm opacity-80">
                      The system's functionality is organized into composable devices that can be added, removed, or reconfigured 
                      without modifying core code. This allows the system to be extended in ways not originally foreseen.
                    </p>
                  </div>
                  <div className="p-4">
                    <h3 className="text-lg font-semibold text-terminal-green mb-2">Cryptographic Attestation</h3>
                    <p className="text-sm opacity-80">
                      All messages and operations are cryptographically signed and verified, ensuring that only properly 
                      attested actions are processed. This enables secure delegation and verification.
                    </p>
                  </div>
                </div>
                <div className="grid grid-cols-1 md:grid-cols-2 divide-y md:divide-y-0 md:divide-x divide-[#333] border-t border-[#333]">
                  <div className="p-4">
                    <h3 className="text-lg font-semibold text-terminal-green mb-2">Cross-Subsystem Integration</h3>
                    <p className="text-sm opacity-80">
                      HyperBEAM bridges multiple subsystems through a unified message format and protocol, allowing disparate 
                      systems to communicate while maintaining their specialized functionality.
                    </p>
                  </div>
                  <div className="p-4">
                    <h3 className="text-lg font-semibold text-terminal-green mb-2">Message-Driven Architecture</h3>
                    <p className="text-sm opacity-80">
                      All operations are expressed as messages that flow through the system, being transformed, routed, 
                      scheduled, and processed by various devices before resulting in actions or responses.
                    </p>
                  </div>
                </div>
              </div>
            </div>
          </section>
          
          {/* High-Level System Architecture */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">High-Level System Architecture</h2>
            
            <div className="bg-[#252526] border border-[#333] rounded-md p-6">
              <div className="mb-6">
                <p className="mb-4">
                  At a high level, HyperBEAM can be understood as a series of layers that handle different aspects 
                  of message processing:
                </p>
                
                <div className="bg-[#2d2d2d] p-5 rounded-md">
                  {/* ASCII architecture diagram - would be replaced with a proper SVG in production */}
                  <pre className="text-xs sm:text-sm text-terminal-text font-mono whitespace-pre overflow-x-auto">
{`┌──────────────────────────────────────────────────────────────────┐
│                         CLIENT INTERFACES                         │
└─────────────────────────────┬────────────────────────────────────┘
                              │
┌─────────────────────────────▼────────────────────────────────────┐
│                      MESSAGE PROCESSING LAYER                     │
│  ┌───────────────┐    ┌───────────────┐    ┌───────────────┐     │
│  │   Validation  │◄──►│   Scheduling  │◄──►│   Routing     │     │
│  └───────────────┘    └───────────────┘    └───────────────┘     │
└─────────────────────────────┬────────────────────────────────────┘
                              │
┌─────────────────────────────▼────────────────────────────────────┐
│                        DEVICE ECOSYSTEM                          │
│  ┌───────────┐ ┌───────────┐ ┌───────────┐ ┌───────────┐        │
│  │Core       │ │Runtime    │ │Security   │ │Payment    │        │
│  │Devices    │ │Devices    │ │Devices    │ │Devices    │        │
│  └───────────┘ └───────────┘ └───────────┘ └───────────┘        │
│  ┌───────────┐ ┌───────────┐ ┌───────────┐                      │
│  │Routing    │ │Scheduler  │ │Utility    │                      │
│  │Devices    │ │Devices    │ │Devices    │                      │
│  └───────────┘ └───────────┘ └───────────┘                      │
└─────────────────────────────┬────────────────────────────────────┘
                              │
┌─────────────────────────────▼────────────────────────────────────┐
│                      EXECUTION & STORAGE LAYER                    │
│  ┌───────────────┐    ┌───────────────┐    ┌───────────────┐     │
│  │   WebAssembly │    │  Persistence  │    │ Cryptographic │     │
│  │   Runtime     │    │  & State      │    │ Verification  │     │
│  └───────────────┘    └───────────────┘    └───────────────┘     │
└─────────────────────────────┬────────────────────────────────────┘
                              │
┌─────────────────────────────▼────────────────────────────────────┐
│                         CROSS-SUBSYSTEM                           │
│                        INTEGRATION LAYER                          │
└──────────────────────────────────────────────────────────────────┘`}
                  </pre>
                </div>
              </div>
              
              <div className="space-y-4 text-sm">
                <p>
                  <span className="text-terminal-green font-semibold">Client Interfaces:</span>{' '}
                  Entry points for external systems to interact with HyperBEAM through a unified API and protocol.
                </p>
                
                <p>
                  <span className="text-terminal-green font-semibold">Message Processing Layer:</span>{' '}
                  Handles message validation, scheduling, and routing to appropriate devices based on message content and metadata.
                </p>
                
                <p>
                  <span className="text-terminal-green font-semibold">Device Ecosystem:</span>{' '}
                  The core of HyperBEAM's extensibility, comprising various device categories that implement specific functionality.
                </p>
                
                <p>
                  <span className="text-terminal-green font-semibold">Execution & Storage Layer:</span>{' '}
                  Provides the underlying execution environment, state management, and cryptographic primitives.
                </p>
                
                <p>
                  <span className="text-terminal-green font-semibold">Cross-Subsystem Integration Layer:</span>{' '}
                  Connects HyperBEAM to external subsystems and services through specialized adapters and protocols.
                </p>
              </div>
            </div>
          </section>
          
          {/* Message Flow */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Message Flow</h2>
            
            <div className="bg-[#252526] border border-[#333] rounded-md p-6">
              <p className="mb-6">
                Messages are the fundamental unit of work in HyperBEAM. Let's trace the journey of a typical message through the system:
              </p>
              
              <div className="relative rounded-md overflow-hidden bg-[#2d2d2d] border border-[#444]">
                <div className="absolute left-0 top-0 bottom-0 w-1 bg-terminal-accent"></div>
                
                <div className="py-4 px-6">
                  <h3 className="font-semibold mb-2 text-terminal-primary">1. Message Creation</h3>
                  <p className="text-sm opacity-80 mb-4">
                    A client creates a message containing an operation, data, and metadata. The message is signed 
                    using the client's private key for attestation.
                  </p>
                  <pre className="bg-[#2d2d2d] p-3 text-xs rounded overflow-x-auto">
                    <code>{`Msg = #{
  id => hb_util:generate_id(),
  operation => "transfer",
  data => #{from => "account1", to => "account2", amount => 100},
  timestamp => erlang:system_time(millisecond)
},
SignedMsg = dev_message:sign(Msg, PrivateKey)`}</code>
                  </pre>
                </div>
                
                <div className="py-4 px-6 border-t border-[#333]">
                  <h3 className="font-semibold mb-2 text-terminal-primary">2. Message Validation</h3>
                  <p className="text-sm opacity-80">
                    The message is validated for proper format, required fields, and signature verification 
                    before entering the processing pipeline.
                  </p>
                </div>
                
                <div className="py-4 px-6 border-t border-[#333]">
                  <h3 className="font-semibold mb-2 text-terminal-primary">3. Scheduling</h3>
                  <p className="text-sm opacity-80">
                    The <Link href="/devices/scheduler" className="text-terminal-green hover:underline">scheduler device</Link> assigns 
                    the message to a specific slot in the processing sequence, ensuring proper ordering and deterministic execution.
                  </p>
                </div>
                
                <div className="py-4 px-6 border-t border-[#333]">
                  <h3 className="font-semibold mb-2 text-terminal-primary">4. Device Processing</h3>
                  <p className="text-sm opacity-80">
                    The message is processed by a pipeline of devices, each performing specific operations like transformation, 
                    validation, computation, or storage updates.
                  </p>
                </div>
                
                <div className="py-4 px-6 border-t border-[#333]">
                  <h3 className="font-semibold mb-2 text-terminal-primary">5. State Updates</h3>
                  <p className="text-sm opacity-80">
                    Changes to system state are recorded based on the message processing results. This may include 
                    updating balances, recording events, or modifying configuration.
                  </p>
                </div>
                
                <div className="py-4 px-6 border-t border-[#333]">
                  <h3 className="font-semibold mb-2 text-terminal-primary">6. Response Generation</h3>
                  <p className="text-sm opacity-80 mb-4">
                    A response message is generated containing the results of the operation, potential errors, 
                    and relevant metadata. This response is signed for verification by the client.
                  </p>
                  <pre className="bg-[#2d2d2d] p-3 text-xs rounded overflow-x-auto">
                    <code>{`Response = #{
  id => hb_util:generate_id(),
  ref => maps:get(id, SignedMsg),  % Reference to original message
  status => "success",
  result => #{transaction_id => "tx123", timestamp => erlang:system_time(millisecond)},
  timestamp => erlang:system_time(millisecond)
},
SignedResponse = dev_message:sign(Response, SystemKey)`}</code>
                  </pre>
                </div>
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
                  <Link href="#principles" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Core Architectural Principles
                  </Link>
                </li>
                <li>
                  <Link href="#architecture" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    High-Level System Architecture
                  </Link>
                </li>
                <li>
                  <Link href="#message-flow" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Message Flow
                  </Link>
                </li>
                <li>
                  <Link href="#device-ecosystem" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Device Ecosystem
                  </Link>
                </li>
                <li>
                  <Link href="#subsystems" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Subsystem Integration
                  </Link>
                </li>
              </ul>
            </div>
            
            {/* Key Documentation */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Key Documentation</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <Link 
                    href="/architecture/message-processing" 
                    className="text-terminal-text hover:text-terminal-green flex items-center"
                  >
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M4 4a2 2 0 012-2h4.586A2 2 0 0112 2.586L15.414 6A2 2 0 0116 7.414V16a2 2 0 01-2 2H6a2 2 0 01-2-2V4z" clipRule="evenodd" />
                    </svg>
                    Message Processing
                  </Link>
                </li>
                <li>
                  <Link 
                    href="/architecture/device-composition" 
                    className="text-terminal-text hover:text-terminal-green flex items-center"
                  >
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M4 4a2 2 0 012-2h4.586A2 2 0 0112 2.586L15.414 6A2 2 0 0116 7.414V16a2 2 0 01-2 2H6a2 2 0 01-2-2V4z" clipRule="evenodd" />
                    </svg>
                    Device Composition
                  </Link>
                </li>
                <li>
                  <Link 
                    href="/architecture/subsystems" 
                    className="text-terminal-text hover:text-terminal-green flex items-center"
                  >
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M4 4a2 2 0 012-2h4.586A2 2 0 0112 2.586L15.414 6A2 2 0 0116 7.414V16a2 2 0 01-2 2H6a2 2 0 01-2-2V4z" clipRule="evenodd" />
                    </svg>
                    Subsystem Integration
                  </Link>
                </li>
                <li>
                  <Link 
                    href="/architecture/security" 
                    className="text-terminal-text hover:text-terminal-green flex items-center"
                  >
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M4 4a2 2 0 012-2h4.586A2 2 0 0112 2.586L15.414 6A2 2 0 0116 7.414V16a2 2 0 01-2 2H6a2 2 0 01-2-2V4z" clipRule="evenodd" />
                    </svg>
                    Security Architecture
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
      
      {/* Device Ecosystem */}
      <section className="mb-8">
        <h2 className="text-2xl font-bold text-terminal-primary mb-4">The Device Ecosystem</h2>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-6">
          <p className="mb-6">
            The device ecosystem is the core of HyperBEAM's extensibility model. Each device is a specialized module 
            that can be combined with others to create complex behaviors.
          </p>
          
          <div className="grid grid-cols-1 md:grid-cols-3 gap-5 mb-6">
            <div className="bg-[#2d2d2d] p-4 rounded-md hover:border-terminal-green transition-colors card-hover-anim">
              <h3 className="font-semibold mb-2 text-terminal-green">Modularity</h3>
              <p className="text-sm opacity-80">
                Devices are self-contained modules with clear interfaces, making them easy to understand, test, and compose.
              </p>
            </div>
            
            <div className="bg-[#2d2d2d] p-4 rounded-md hover:border-terminal-green transition-colors card-hover-anim">
              <h3 className="font-semibold mb-2 text-terminal-green">Composability</h3>
              <p className="text-sm opacity-80">
                Devices can be assembled into pipelines, stacks, and other patterns to create complex behaviors from simple parts.
              </p>
            </div>
            
            <div className="bg-[#2d2d2d] p-4 rounded-md hover:border-terminal-green transition-colors card-hover-anim">
              <h3 className="font-semibold mb-2 text-terminal-green">Extensibility</h3>
              <p className="text-sm opacity-80">
                New devices can be added without modifying existing code, allowing the system to evolve and adapt to new requirements.
              </p>
            </div>
          </div>
        
          <div className="bg-[#252526] border border-[#333] rounded-md p-5">
            <p className="mb-4">
              Explore the complete <Link href="/devices" className="text-terminal-green hover:underline">Device Ecosystem</Link> to learn more 
              about the available devices and how they can be used in your applications.
            </p>
            
            <pre className="bg-[#2d2d2d] p-3 text-sm rounded overflow-x-auto">
              <code>{`%% Example of device composition
Pipeline = [
  {dev_authentication, [#{mode => strict}]},
  {dev_validation, [#{schema => payment_schema}]},
  {dev_process, [#{cache => true}]},
  {dev_scheduler, [#{slot => next}]},
  {dev_payment, []}
],

%% Process a message through the pipeline
{ok, Result} = hb:process_pipeline(Msg, Pipeline)`}</code>
            </pre>
          </div>
        </div>
      </section>
      
      {/* Cross-Subsystem Integration */}
      <section className="mb-8">
        <h2 className="text-2xl font-bold text-terminal-primary mb-4">Cross-Subsystem Integration</h2>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-6">
          <p className="mb-6">
            HyperBEAM serves as an integration layer between multiple subsystems, allowing them to communicate 
            and share data while maintaining their specialized functionality.
          </p>
          <div className="bg-[#2d2d2d] p-4 rounded-md mb-6">
            {/* ASCII subsystem integration diagram - would be replaced with a proper SVG in production */}
            <pre className="text-xs sm:text-sm text-terminal-text font-mono whitespace-pre overflow-x-auto">
{`┌───────────────────────────────────────────────────────────────┐
│                          HyperBEAM                             │
│                                                               │
│    ┌────────────┐     ┌────────────┐     ┌────────────┐      │
│    │   Device   │     │  Message   │     │  Security  │      │
│    │ Ecosystem  │◄───►│ Processing │◄───►│  Layer     │      │
│    └────────────┘     └────────────┘     └────────────┘      │
│            ▲                 ▲                  ▲            │
└────────────┼─────────────────┼──────────────────┼────────────┘
             │                 │                  │
┌────────────▼─────┐ ┌─────────▼───────┐ ┌────────▼──────────┐
│                  │ │                 │ │                    │
│ Decentralized    │ │ Smart Contract  │ │ Machine Learning   │
│  Storage         │ │ Processing      │ │ Layer              │
│                  │ │                 │ │                    │
└──────────────────┘ └─────────────────┘ └────────────────────┘`}
            </pre>
          </div>
          
          <p className="mb-4 text-sm">
            HyperBEAM provides several mechanisms for cross-subsystem integration:
          </p>
          
          <ul className="space-y-3 text-sm mb-6">
            <li className="flex items-start">
              <span className="text-terminal-green mr-2">•</span>
              <div>
                <span className="font-semibold">Unified Message Format:</span>{' '}
                A common message structure that can represent operations across different subsystems.
              </div>
            </li>
            <li className="flex items-start">
              <span className="text-terminal-green mr-2">•</span>
              <div>
                <span className="font-semibold">Adapter Devices:</span>{' '}
                Specialized devices that translate between HyperBEAM's internal format and subsystem-specific formats.
              </div>
            </li>
            <li className="flex items-start">
              <span className="text-terminal-green mr-2">•</span>
              <div>
                <span className="font-semibold">Cross-Chain Verification:</span>{' '}
                Cryptographic mechanisms to verify operations across different consensus domains.
              </div>
            </li>
            <li className="flex items-start">
              <span className="text-terminal-green mr-2">•</span>
              <div>
                <span className="font-semibold">Gateway APIs:</span>{' '}
                Standardized interfaces for external systems to interact with HyperBEAM.
              </div>
            </li>
          </ul>
          
          <p className="text-sm opacity-80">
            Learn more about subsystem integration in the{' '}
            <Link href="/cross-subsystem" className="text-terminal-green hover:underline">
              Cross-Subsystem Integration
            </Link>{' '}
            section.
          </p>
        </div>
      </section>
    </>
  );
}
