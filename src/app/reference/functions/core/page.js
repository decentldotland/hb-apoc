"use client"

import { useState } from 'react';
import Link from 'next/link';
import Terminal from '../../../../components/Terminal';
import { moduleCategories } from '../../../../data/functions/categories';

export const metadata = {
  title: 'Core Infrastructure Functions - HyperBEAM Documentation',
  description: 'Reference documentation for core infrastructure functions in the HyperBEAM system, including system management, convergence, bootstrap, and monitoring operations.'
};

export default function CoreFunctions() {
  const [selectedModule, setSelectedModule] = useState('all');
  
  // Get core subsystem data
  const coreSubsystem = moduleCategories.find(subsystem => subsystem.id === 'core');
  
  // Sample terminal commands for core functions
  const initialCommands = [
    { 
      command: 'grep -r "hb_system:" src/core/', 
      output: 'Multiple system management function references found across 7 files'
    },
    { 
      command: 'grep -r "hb_converge:" src/core/', 
      output: 'Multiple convergence function references found across 5 files'
    }
  ];

  // Handle module selection change
  const handleModuleChange = (moduleId) => {
    setSelectedModule(moduleId);
  };

  // Filter modules based on selection
  const modules = selectedModule === 'all'
    ? coreSubsystem.modules
    : coreSubsystem.modules.filter(module => module.id === selectedModule);

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <Link href="/reference" className="hover:text-terminal-green">Reference</Link>
          <span>/</span>
          <Link href="/reference/functions" className="hover:text-terminal-green">Functions</Link>
          <span>/</span>
          <span className="text-terminal-green">Core</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Core Infrastructure Functions
        </h1>
        
        <p className="text-lg mb-8">
          {coreSubsystem.description}
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6">
          <div className="flex items-center justify-between">
            <div className="font-mono text-sm">
              <span className="text-terminal-green">$</span> 
              <span className="text-terminal-accent ml-2">find</span> 
              <span className="text-terminal-output ml-2">src/core -name "*.erl" | wc -l</span>
            </div>
            <div className="bg-[#333] px-3 py-1 rounded text-xs font-mono">
              {coreSubsystem.count} core functions in {coreSubsystem.modules.length} modules
            </div>
          </div>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-4 gap-8 mb-12">
        <div className="lg:col-span-3">
          <div className="space-y-12">
            {modules.map((module) => (
              <div key={module.id} className="mb-10">
                <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6">
                  <div className="flex justify-between items-start">
                    <div>
                      <h2 className="text-2xl font-mono font-bold text-terminal-primary mb-2">
                        {module.name}
                        <span className="text-xs text-terminal-accent font-normal ml-3">{module.id}</span>
                      </h2>
                      <p className="text-sm opacity-80 mb-4">
                        {module.id === 'hb_system' && 'System management functions handle various aspects of system lifecycle, health checks, and environment configuration.'}
                        {module.id === 'hb_converge' && 'Convergence functions handle distributed state convergence, ensuring consistent global state across the network.'}
                        {module.id === 'hb_bootstrap' && 'Bootstrap functions handle the initialization and startup sequence for system components.'}
                        {module.id === 'hb_monitor' && 'Monitoring functions track system health, performance metrics, and operational status.'}
                      </p>
                    </div>
                    <Link 
                      href={`/reference/functions/core/${module.id}`}
                      className="text-xs bg-terminal-green text-black px-3 py-1 rounded-md hover:bg-terminal-green/80 transition-colors"
                    >
                      View All {module.count} Functions
                    </Link>
                  </div>
                </div>
                
                {/* Sample functions for each module */}
                <div className="grid grid-cols-1 gap-4">
                  {module.id === 'hb_system' && (
                    <>
                      <Link href={`/reference/functions/core/hb_system/start`} className="block">
                        <div className="bg-[#252526] border border-[#333] rounded-md p-4 hover:border-terminal-green transition-colors">
                          <div className="flex justify-between items-start">
                            <h3 className="font-mono text-lg text-terminal-green mb-1">start/2</h3>
                            <span className="text-xs bg-[#2d2d2d] px-2 py-1 rounded-full">Public API</span>
                          </div>
                          <p className="text-sm opacity-80 mb-3">
                            Starts the HyperBEAM system with the provided configuration and options.
                          </p>
                          <div className="font-mono text-xs bg-[#1e1e1e] p-3 rounded">
                            <span className="text-terminal-accent">start</span>(Config, Options) -&gt; {'{'}ok, Pid{'}'} | {'{'}error, Reason{'}'}
                          </div>
                        </div>
                      </Link>
                      
                      <Link href={`/reference/functions/core/hb_system/stop`} className="block">
                        <div className="bg-[#252526] border border-[#333] rounded-md p-4 hover:border-terminal-green transition-colors">
                          <div className="flex justify-between items-start">
                            <h3 className="font-mono text-lg text-terminal-green mb-1">stop/1</h3>
                            <span className="text-xs bg-[#2d2d2d] px-2 py-1 rounded-full">Public API</span>
                          </div>
                          <p className="text-sm opacity-80 mb-3">
                            Gracefully shuts down the HyperBEAM system or a specific component.
                          </p>
                          <div className="font-mono text-xs bg-[#1e1e1e] p-3 rounded">
                            <span className="text-terminal-accent">stop</span>(Pid) -&gt; ok | {'{'}error, Reason{'}'}
                          </div>
                        </div>
                      </Link>
                    </>
                  )}
                  
                  {module.id === 'hb_converge' && (
                    <>
                      <Link href={`/reference/functions/core/hb_converge/sync_state`} className="block">
                        <div className="bg-[#252526] border border-[#333] rounded-md p-4 hover:border-terminal-green transition-colors">
                          <div className="flex justify-between items-start">
                            <h3 className="font-mono text-lg text-terminal-green mb-1">sync_state/2</h3>
                            <span className="text-xs bg-[#2d2d2d] px-2 py-1 rounded-full">Public API</span>
                          </div>
                          <p className="text-sm opacity-80 mb-3">
                            Synchronizes state between nodes to achieve convergence.
                          </p>
                          <div className="font-mono text-xs bg-[#1e1e1e] p-3 rounded">
                            <span className="text-terminal-accent">sync_state</span>(State, Options) -&gt; {'{'}ok, NewState{'}'} | {'{'}error, Reason{'}'}
                          </div>
                        </div>
                      </Link>
                      
                      <Link href={`/reference/functions/core/hb_converge/resolve_conflict`} className="block">
                        <div className="bg-[#252526] border border-[#333] rounded-md p-4 hover:border-terminal-green transition-colors">
                          <div className="flex justify-between items-start">
                            <h3 className="font-mono text-lg text-terminal-green mb-1">resolve_conflict/3</h3>
                            <span className="text-xs bg-[#2d2d2d] px-2 py-1 rounded-full">Internal API</span>
                          </div>
                          <p className="text-sm opacity-80 mb-3">
                            Resolves conflicts between differing state versions according to resolution strategy.
                          </p>
                          <div className="font-mono text-xs bg-[#1e1e1e] p-3 rounded">
                            <span className="text-terminal-accent">resolve_conflict</span>(State1, State2, Strategy) -&gt; ResolvedState
                          </div>
                        </div>
                      </Link>
                    </>
                  )}
                  
                  {module.id === 'hb_bootstrap' && (
                    <>
                      <Link href={`/reference/functions/core/hb_bootstrap/initialize`} className="block">
                        <div className="bg-[#252526] border border-[#333] rounded-md p-4 hover:border-terminal-green transition-colors">
                          <div className="flex justify-between items-start">
                            <h3 className="font-mono text-lg text-terminal-green mb-1">initialize/1</h3>
                            <span className="text-xs bg-[#2d2d2d] px-2 py-1 rounded-full">Public API</span>
                          </div>
                          <p className="text-sm opacity-80 mb-3">
                            Initializes the system with default or provided configuration.
                          </p>
                          <div className="font-mono text-xs bg-[#1e1e1e] p-3 rounded">
                            <span className="text-terminal-accent">initialize</span>(Config) -&gt; {'{'}ok, SystemState{'}'} | {'{'}error, Reason{'}'}
                          </div>
                        </div>
                      </Link>
                      
                      <Link href={`/reference/functions/core/hb_bootstrap/load_components`} className="block">
                        <div className="bg-[#252526] border border-[#333] rounded-md p-4 hover:border-terminal-green transition-colors">
                          <div className="flex justify-between items-start">
                            <h3 className="font-mono text-lg text-terminal-green mb-1">load_components/2</h3>
                            <span className="text-xs bg-[#2d2d2d] px-2 py-1 rounded-full">Internal API</span>
                          </div>
                          <p className="text-sm opacity-80 mb-3">
                            Loads system components in the correct dependency order.
                          </p>
                          <div className="font-mono text-xs bg-[#1e1e1e] p-3 rounded">
                            <span className="text-terminal-accent">load_components</span>(Components, Options) -&gt; {'{'}ok, LoadedComponents{'}'} | {'{'}error, Reason{'}'}
                          </div>
                        </div>
                      </Link>
                    </>
                  )}
                  
                  {module.id === 'hb_monitor' && (
                    <>
                      <Link href={`/reference/functions/core/hb_monitor/get_metrics`} className="block">
                        <div className="bg-[#252526] border border-[#333] rounded-md p-4 hover:border-terminal-green transition-colors">
                          <div className="flex justify-between items-start">
                            <h3 className="font-mono text-lg text-terminal-green mb-1">get_metrics/1</h3>
                            <span className="text-xs bg-[#2d2d2d] px-2 py-1 rounded-full">Public API</span>
                          </div>
                          <p className="text-sm opacity-80 mb-3">
                            Retrieves system metrics based on specified options.
                          </p>
                          <div className="font-mono text-xs bg-[#1e1e1e] p-3 rounded">
                            <span className="text-terminal-accent">get_metrics</span>(Options) -&gt; {'{'}ok, Metrics{'}'} | {'{'}error, Reason{'}'}
                          </div>
                        </div>
                      </Link>
                      
                      <Link href={`/reference/functions/core/hb_monitor/watch`} className="block">
                        <div className="bg-[#252526] border border-[#333] rounded-md p-4 hover:border-terminal-green transition-colors">
                          <div className="flex justify-between items-start">
                            <h3 className="font-mono text-lg text-terminal-green mb-1">watch/2</h3>
                            <span className="text-xs bg-[#2d2d2d] px-2 py-1 rounded-full">Public API</span>
                          </div>
                          <p className="text-sm opacity-80 mb-3">
                            Sets up a monitoring process to track specific metrics or system status.
                          </p>
                          <div className="font-mono text-xs bg-[#1e1e1e] p-3 rounded">
                            <span className="text-terminal-accent">watch</span>(Target, Options) -&gt; {'{'}ok, WatcherId{'}'} | {'{'}error, Reason{'}'}
                          </div>
                        </div>
                      </Link>
                    </>
                  )}
                </div>
              </div>
            ))}
          </div>
        </div>
        
        <div className="lg:col-span-1">
          <div className="sticky top-[80px] space-y-6">
            {/* Module Navigation */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Core Modules</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <button 
                    className={`text-left block w-full ${selectedModule === 'all' ? 'text-terminal-green' : 'text-terminal-text hover:text-terminal-green'} transition-colors`}
                    onClick={() => handleModuleChange('all')}
                  >
                    All Core Modules <span className="opacity-60">({coreSubsystem.count})</span>
                  </button>
                </li>
                {coreSubsystem.modules.map(module => (
                  <li key={module.id}>
                    <button 
                      className={`text-left block w-full ${selectedModule === module.id ? 'text-terminal-green' : 'text-terminal-text hover:text-terminal-green'} transition-colors`}
                      onClick={() => handleModuleChange(module.id)}
                    >
                      {module.name} <span className="opacity-60">({module.count})</span>
                    </button>
                  </li>
                ))}
              </ul>
            </div>
            
            {/* Related Sections */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Related Sections</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <Link href="/core" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM4.332 8.027a6.012 6.012 0 011.912-2.706C6.512 5.73 6.974 6 7.5 6A1.5 1.5 0 019 7.5V8a2 2 0 004 0 2 2 0 011.523-1.943A5.977 5.977 0 0116 10c0 .34-.028.675-.083 1H15a2 2 0 00-2 2v2.197A5.973 5.973 0 0110 16v-2a2 2 0 00-2-2 2 2 0 01-2-2 2 2 0 00-1.668-1.973z" clipRule="evenodd" />
                    </svg>
                    Core Infrastructure Overview
                  </Link>
                </li>
                <li>
                  <Link href="/cross-subsystem" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path d="M5 3a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 00-2-2H5zM5 11a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2v-2a2 2 0 00-2-2H5zM11 5a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V5zM14 11a1 1 0 011 1v1h1a1 1 0 110 2h-1v1a1 1 0 11-2 0v-1h-1a1 1 0 110-2h1v-1a1 1 0 011-1z" />
                    </svg>
                    Cross-Subsystem Integration
                  </Link>
                </li>
                <li>
                  <Link href="/reference/glossary" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path d="M9 4.804A7.968 7.968 0 005.5 4c-1.255 0-2.443.29-3.5.804v10A7.969 7.969 0 015.5 14c1.669 0 3.218.51 4.5 1.385A7.962 7.962 0 0114.5 14c1.255 0 2.443.29 3.5.804v-10A7.968 7.968 0 0014.5 4c-1.255 0-2.443.29-3.5.804V12a1 1 0 11-2 0V4.804z" />
                    </svg>
                    Glossary of Core Terms
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
