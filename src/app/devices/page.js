"use client"

import { useState } from 'react';
import Link from 'next/link';
import DeviceCarousel from '../../components/DeviceCarousel';

export default function DevicesIndex() {
  const [searchTerm, setSearchTerm] = useState('');
  
  // Device category descriptions
  const categoryDescriptions = [
    {
      id: 'core',
      name: 'Core Devices',
      description: 'Fundamental building blocks of the HyperBEAM architecture that handle essential operations like message processing, scheduling, and state management.',
      count: 6,
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4" />
        </svg>
      )
    },
    {
      id: 'scheduler',
      name: 'Scheduler Devices',
      description: 'Devices responsible for managing execution order, handling assignments, and coordinating task sequencing across the system.',
      count: 4,
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z" />
        </svg>
      )
    },
    {
      id: 'runtime',
      name: 'Runtime Devices',
      description: 'Execution environment devices that provide WebAssembly support, system interfaces, and computation capabilities.',
      count: 5,
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 10V3L4 14h7v7l9-11h-7z" />
        </svg>
      )
    },
    {
      id: 'security',
      name: 'Security Devices',
      description: 'Devices that handle cryptographic attestation, hardware security integration, and secure communication between trusted nodes.',
      count: 3,
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z" />
        </svg>
      )
    },
    {
      id: 'payment',
      name: 'Payment Devices',
      description: 'Economic incentive devices that handle pricing, ledger interfaces, and computational unit tracking for system resources.',
      count: 4,
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2.08.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
        </svg>
      )
    },
    {
      id: 'routing',
      name: 'Routing Devices',
      description: 'Message handling devices that direct communications between system components and external endpoints.',
      count: 4,
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 20l-5.447-2.724A1 1 0 013 16.382V5.618a1 1 0 011.447-.894L9 7m0 13l6-3m-6 3V7m6 10l4.553 2.276A1 1 0 0021 18.382V7.618a1 1 0 00-.553-.894L15 4m0 13V4m0 0L9 7" />
        </svg>
      )
    },
    {
      id: 'utility',
      name: 'Utility Devices',
      description: 'Auxiliary devices that provide support functions like caching, deduplication, scheduling, and monitoring.',
      count: 7,
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M11 4a2 2 0 114 0v1a1 1 0 001 1h3a1 1 0 011 1v3a1 1 0 01-1 1h-1a2 2 0 100 4h1a1 1 0 011 1v3a1 1 0 01-1 1h-3a1 1 0 01-1-1v-1a2 2 0 10-4 0v1a1 1 0 01-1 1H7a1 1 0 01-1-1v-3a1 1 0 00-1-1H4a2 2 0 110-4h1a1 1 0 001-1V7a1 1 0 011-1h3a1 1 0 001-1V4z" />
        </svg>
      )
    }
  ];

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <span className="text-terminal-green">Devices</span>
        </div>
        
        <div className="flex flex-col md:flex-row justify-between items-start md:items-center mb-6">
          <h1 className="text-3xl font-bold text-terminal-primary mb-4 md:mb-0">
            Device Ecosystem
          </h1>
          
          <div className="w-full md:w-auto">
            <div className="flex items-center bg-[#333] rounded px-3 py-1.5 w-full md:w-[300px]">
              <span className="text-terminal-prompt mr-1.5">$</span>
              <input
                type="text"
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                placeholder="filter devices..."
                className="bg-transparent border-none text-terminal-command w-full outline-none font-mono text-sm"
                aria-label="Filter devices"
              />
            </div>
          </div>
        </div>
        
        <p className="text-lg mb-6">
          HyperBEAM's architecture is built on a composable device ecosystem that enables extensibility without modifying core code.
          Each device is a specialized module that performs a specific function and can be combined with others to create complex behaviors.
        </p>
        
        <div className="bg-[#252526] border-l-4 border-terminal-green p-4 rounded mb-6">
          <p>
            <span className="text-terminal-green font-mono">$</span> <span className="text-terminal-accent font-mono">wc -l</span> <span className="text-terminal-output font-mono">*.erl</span>
            <br />
            <span className="ml-4 text-terminal-text font-mono">33 devices across 7 categories in 5,827 lines of code</span>
          </p>
        </div>
      </div>

      {/* Device Carousel */}
      <section className="mb-12">
        <h2 className="text-xl font-bold text-terminal-primary mb-4">
          Browse All Devices
        </h2>
        <DeviceCarousel />
      </section>
      
      {/* Device Categories */}
      <section className="mb-12">
        <h2 className="text-xl font-bold text-terminal-primary mb-6">
          Device Categories
        </h2>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          {categoryDescriptions.map((category) => (
            <div 
              key={category.id}
              className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-all duration-200 card-hover-anim"
            >
              <div className="flex items-start">
                <div className="text-terminal-green mr-4 mt-1">
                  {category.icon}
                </div>
                <div>
                  <h3 className="text-lg font-semibold mb-2 text-terminal-primary">
                    {category.name} <span className="text-xs text-terminal-accent">({category.count})</span>
                  </h3>
                  <p className="text-sm opacity-80 mb-3">{category.description}</p>
                  <Link 
                    href={`#${category.id}`}
                    className="text-terminal-green text-sm hover:underline flex items-center"
                    onClick={() => document.querySelector(`[data-category="${category.id}"]`)?.scrollIntoView({ behavior: 'smooth' })}
                  >
                    <span className="mr-1">Browse {category.name}</span>
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M14 5l7 7m0 0l-7 7m7-7H3" />
                    </svg>
                  </Link>
                </div>
              </div>
            </div>
          ))}
        </div>
      </section>
      
      {/* Integration Architecture */}
      <section className="mb-12">
        <h2 className="text-xl font-bold text-terminal-primary mb-4">
          Device Integration Architecture
        </h2>
        <div className="bg-[#252526] border border-[#333] rounded-md p-5">
          <div className="mb-4">
            <p className="mb-4">
              HyperBEAM devices follow a consistent pattern for integration and communication. They can be composed in pipelines,
              stacks, and other patterns to create complex behaviors from simple building blocks.
            </p>
          </div>
          
          <h3 className="text-lg font-semibold mb-3 text-terminal-accent">Core Integration Patterns</h3>
          
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">
            <div className="bg-[#2d2d2d] p-4 rounded">
              <h4 className="font-semibold mb-2 text-sm text-terminal-green">Pipeline Pattern</h4>
              <p className="text-xs opacity-80">
                Devices chained in sequence where each device processes the output of the previous one, 
                enabling transformation pipelines and multi-stage processing.
              </p>
            </div>
            <div className="bg-[#2d2d2d] p-4 rounded">
              <h4 className="font-semibold mb-2 text-sm text-terminal-green">Stack Pattern</h4>
              <p className="text-xs opacity-80">
                Multiple devices arranged in a stack, with operations folded over the stack or mapped across the stack, 
                allowing for parallel or aggregated operations.
              </p>
            </div>
            <div className="bg-[#2d2d2d] p-4 rounded">
              <h4 className="font-semibold mb-2 text-sm text-terminal-green">Broadcast Pattern</h4>
              <p className="text-xs opacity-80">
                A message is distributed to multiple devices simultaneously, with results potentially merged or processed independently,
                enabling fan-out operations and parallel processing.
              </p>
            </div>
          </div>
          
          <div className="rounded bg-[#2d2d2d] p-4 overflow-x-auto">
            <pre className="text-sm font-mono text-terminal-text">
              <code>{`%% Example of device composition using the stack pattern
DeviceStack = [
    {dev_message, []},
    {dev_process, [#{cache => true}]},
    {dev_validator, [#{strict => true}]}
],

%% Using the stack in fold mode (each result passed to the next device)
{ok, Result} = dev_stack:process(Msg, DeviceStack, fold),

%% Using the stack in map mode (message processed by each device separately)
{ok, Results} = dev_stack:process(Msg, DeviceStack, map)`}</code>
            </pre>
          </div>
        </div>
      </section>
      
      {/* Getting Started */}
      <section className="mb-12">
        <h2 className="text-xl font-bold text-terminal-primary mb-4">
          Getting Started with Devices
        </h2>
        <div className="bg-[#252526] border border-[#333] rounded-md p-5">
          <p className="mb-4">
            To start working with HyperBEAM devices, begin with the Core and Utility devices which provide the basic building blocks
            for more complex behavior. Here's a simple guide to help you get started:
          </p>
          
          <ol className="space-y-3 list-decimal pl-5 mb-6">
            <li>
              Start with <Link href="/devices/message" className="text-terminal-green hover:underline">dev_message.erl</Link>{' '}
              to understand how message handling works
            </li>
            <li>
              Learn about process orchestration with{' '}
              <Link href="/devices/process" className="text-terminal-green hover:underline">dev_process.erl</Link>
            </li>
            <li>
              Understand device composition with{' '}
              <Link href="/devices/stack" className="text-terminal-green hover:underline">dev_stack.erl</Link>
            </li>
            <li>
              Explore scheduling and sequencing with{' '}
              <Link href="/devices/scheduler" className="text-terminal-green hover:underline">dev_scheduler.erl</Link>
            </li>
          </ol>
          
          <div className="rounded bg-[#2d2d2d] p-4 overflow-x-auto">
            <pre className="text-sm font-mono text-terminal-text">
              <code>{`%% Example of a minimal HyperBEAM application using devices
-module(my_app).

%% Start the required devices
start() ->
    {ok, _} = dev_message:start_link(),
    {ok, _} = dev_process:start_link(),
    {ok, _} = dev_scheduler:start_link(),
    ok.

%% Create and process a message
process_message(Content) ->
    % Create a new message
    Msg = #{
        id => hb_util:generate_id(),
        content => Content,
        timestamp => erlang:system_time(millisecond)
    },
    
    % Process the message with dev_process
    {ok, Result} = dev_process:handle(Msg),
    
    % Return the result
    {ok, Result}.`}</code>
            </pre>
          </div>
        </div>
      </section>
    </>
  );
}
