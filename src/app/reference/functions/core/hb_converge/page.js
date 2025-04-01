"use client"

import { useState } from 'react';
import Link from 'next/link';
import Terminal from '../../../../../components/Terminal';
import { moduleCategories } from '../../../../../data/functions/categories';

export const metadata = {
  title: 'Convergence Functions (hb_converge) - HyperBEAM Documentation',
  description: 'Reference documentation for HyperBEAM convergence functions that handle distributed state convergence, ensuring consistent global state across the network.'
};

export default function ConvergenceFunctions() {
  // Get core subsystem data and hb_converge module
  const coreSubsystem = moduleCategories.find(subsystem => subsystem.id === 'core');
  const convergeModule = coreSubsystem.modules.find(module => module.id === 'hb_converge');
  
  // Sample terminal commands for convergence functions
  const initialCommands = [
    { 
      command: 'grep -A 5 "sync_state" src/core/hb_converge.erl', 
      output: '% @doc Synchronizes state between nodes to achieve convergence\n%\n% @param State Current state to synchronize\n% @param Options Synchronization options\n% @returns {ok, NewState} | {error, Reason}\nsync_state(State, Options) ->'
    },
    { 
      command: 'grep -A 5 "resolve_conflict" src/core/hb_converge.erl', 
      output: '% @doc Resolves conflicts between differing state versions\n%\n% @param State1 First state version\n% @param State2 Second state version\n% @param Strategy Conflict resolution strategy\n% @returns ResolvedState\nresolve_conflict(State1, State2, Strategy) ->'
    }
  ];

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
          <Link href="/reference/functions/core" className="hover:text-terminal-green">Core</Link>
          <span>/</span>
          <span className="text-terminal-green">hb_converge</span>
        </div>
        
        <div className="flex justify-between items-start mb-4">
          <h1 className="text-3xl font-bold text-terminal-primary">
            Convergence Functions
            <span className="text-sm text-terminal-accent font-normal ml-3">hb_converge</span>
          </h1>
          
          <Link 
            href="/core#convergence"
            className="text-xs bg-[#2d2d2d] hover:bg-[#3d3d3d] px-3 py-1 rounded-md text-terminal-green transition-colors"
          >
            Core Infrastructure Overview
          </Link>
        </div>
        
        <p className="text-lg mb-8">
          Convergence functions handle distributed state convergence, ensuring consistent global state across the network.
          These functions are critical for maintaining data consistency in distributed operations.
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6">
          <div className="flex items-center justify-between">
            <div className="font-mono text-sm">
              <span className="text-terminal-green">$</span> 
              <span className="text-terminal-accent ml-2">wc</span> 
              <span className="text-terminal-output ml-2">-l src/core/hb_converge.erl</span>
            </div>
            <div className="bg-[#333] px-3 py-1 rounded text-xs font-mono">
              {convergeModule.count} functions | 214 lines
            </div>
          </div>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-4 gap-8 mb-12">
        <div className="lg:col-span-3">
          <div className="space-y-12">
            <section>
              <h2 className="text-2xl font-bold text-terminal-primary mb-6 border-b border-[#444] pb-2">
                Module Overview
              </h2>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-6 mb-8">
                <p className="mb-4">
                  The <code className="text-terminal-accent">hb_converge</code> module is responsible for managing state convergence in a distributed environment. 
                  It ensures that all nodes in the system maintain a consistent view of shared state, even in the presence of 
                  network partitions, temporary node failures, or concurrent updates.
                </p>
                
                <div className="bg-[#2d2d2d] p-5 rounded-md mb-4">
                  <div className="flex justify-between mb-2">
                    <h3 className="font-mono font-semibold text-terminal-green">Module Declaration</h3>
                    <span className="text-xs bg-[#333] px-2 py-1 rounded">src/core/hb_converge.erl</span>
                  </div>
                  <pre className="text-xs font-mono overflow-x-auto">
{`-module(hb_converge).

%% Convergence API
-export([sync_state/2, detect_conflicts/1, resolve_conflict/3]).

%% Extended API
-export([state_version/1, merge_versions/2, divergence_point/2]).

%% Internal functions
-export([state_diff/2, apply_diff/2]).

%% Include convergence utilities
-include("hb_converge_utils.hrl").`}
                  </pre>
                </div>
                
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm">
                  <div>
                    <h3 className="font-semibold text-terminal-accent mb-2">Key Features:</h3>
                    <ul className="space-y-1 list-disc pl-5">
                      <li>Vector clock-based conflict detection</li>
                      <li>Customizable conflict resolution strategies</li>
                      <li>Efficient state synchronization</li>
                      <li>Support for partial state updates</li>
                      <li>Event-based convergence notifications</li>
                    </ul>
                  </div>
                  
                  <div>
                    <h3 className="font-semibold text-terminal-accent mb-2">Dependencies:</h3>
                    <ul className="space-y-1 list-disc pl-5">
                      <li>hb_system - Base system APIs</li>
                      <li>hb_net - Network communication</li>
                      <li>hb_clock - Vector clock implementation</li>
                    </ul>
                  </div>
                </div>
              </div>
            </section>
            
            <section>
              <h2 className="text-2xl font-bold text-terminal-primary mb-6 border-b border-[#444] pb-2">
                Core Functions
              </h2>
              
              <div className="space-y-8">
                {/* sync_state function */}
                <div className="bg-[#252526] border border-[#333] rounded-md p-6" id="sync_state">
                  <div className="flex justify-between items-start mb-4">
                    <h3 className="font-mono text-xl text-terminal-green">sync_state/2</h3>
                    <span className="text-xs bg-[#2d2d2d] px-2 py-1 rounded-full">Public API</span>
                  </div>
                  
                  <div className="mb-4">
                    <p className="mb-3">
                      Synchronizes state between nodes to achieve convergence. This function attempts to 
                      reconcile differences between local and remote state versions.
                    </p>
                    
                    <div className="font-mono text-sm bg-[#1e1e1e] p-4 rounded mb-4">
                      <span className="text-terminal-accent">sync_state</span>(State, Options) -&gt; {'{'}ok, NewState{'}'} | {'{'}error, Reason{'}'}
                    </div>
                    
                    <div className="grid grid-cols-1 md:grid-cols-3 gap-4 text-sm mb-4">
                      <div>
                        <h4 className="font-semibold text-terminal-accent mb-2">Parameters:</h4>
                        <ul className="space-y-2">
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">State</span>
                            <p className="mt-1 opacity-80">Current state to synchronize</p>
                          </li>
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">Options</span>
                            <p className="mt-1 opacity-80">Map of synchronization options</p>
                          </li>
                        </ul>
                      </div>
                      
                      <div>
                        <h4 className="font-semibold text-terminal-accent mb-2">Returns:</h4>
                        <ul className="space-y-2">
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">{'{'}ok, NewState{'}'}</span>
                            <p className="mt-1 opacity-80">Updated converged state</p>
                          </li>
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">{'{'}error, Reason{'}'}</span>
                            <p className="mt-1 opacity-80">Error with reason</p>
                          </li>
                        </ul>
                      </div>
                      
                      <div>
                        <h4 className="font-semibold text-terminal-accent mb-2">Options:</h4>
                        <ul className="space-y-2">
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">nodes</span>
                            <p className="mt-1 opacity-80">List of nodes to sync with</p>
                          </li>
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">strategy</span>
                            <p className="mt-1 opacity-80">Conflict resolution strategy</p>
                          </li>
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">timeout</span>
                            <p className="mt-1 opacity-80">Synchronization timeout</p>
                          </li>
                        </ul>
                      </div>
                    </div>
                    
                    <div className="bg-[#2d2d2d] p-4 rounded-md mb-4">
                      <h4 className="font-semibold text-terminal-accent mb-2">Example:</h4>
                      <pre className="text-xs font-mono overflow-x-auto">
{`%% Synchronize state with all cluster nodes
Nodes = hb_cluster:get_nodes(),
Options = #{
  nodes => Nodes,
  strategy => last_write_wins,
  timeout => 5000
},
{ok, SyncedState} = hb_converge:sync_state(CurrentState, Options),

%% Use the synchronized state
ProcessResult = process_data(SyncedState),

%% Log the result
hb_log:info("State synchronized across ~p nodes", [length(Nodes)]),`}
                      </pre>
                    </div>
                    
                    <div className="text-sm">
                      <h4 className="font-semibold text-terminal-accent mb-2">Notes:</h4>
                      <ul className="list-disc pl-5 space-y-1">
                        <li>Uses vector clocks to track state versions and detect conflicts</li>
                        <li>Applies the specified conflict resolution strategy when conflicts are detected</li>
                        <li>Returns an error if synchronization fails or times out</li>
                        <li>May trigger convergence events that can be handled with <code className="bg-[#2d2d2d] px-1 rounded">hb_events:subscribe/2</code></li>
                      </ul>
                    </div>
                  </div>
                </div>
                
                {/* resolve_conflict function */}
                <div className="bg-[#252526] border border-[#333] rounded-md p-6" id="resolve_conflict">
                  <div className="flex justify-between items-start mb-4">
                    <h3 className="font-mono text-xl text-terminal-green">resolve_conflict/3</h3>
                    <span className="text-xs bg-[#2d2d2d] px-2 py-1 rounded-full">Internal API</span>
                  </div>
                  
                  <div className="mb-4">
                    <p className="mb-3">
                      Resolves conflicts between differing state versions according to the specified resolution strategy.
                      This function is typically called internally by <code className="bg-[#2d2d2d] px-1 rounded">sync_state/2</code> when conflicts are detected.
                    </p>
                    
                    <div className="font-mono text-sm bg-[#1e1e1e] p-4 rounded mb-4">
                      <span className="text-terminal-accent">resolve_conflict</span>(State1, State2, Strategy) -&gt; ResolvedState
                    </div>
                    
                    <div className="grid grid-cols-1 md:grid-cols-3 gap-4 text-sm mb-4">
                      <div>
                        <h4 className="font-semibold text-terminal-accent mb-2">Parameters:</h4>
                        <ul className="space-y-2">
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">State1</span>
                            <p className="mt-1 opacity-80">First state version</p>
                          </li>
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">State2</span>
                            <p className="mt-1 opacity-80">Second state version</p>
                          </li>
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">Strategy</span>
                            <p className="mt-1 opacity-80">Conflict resolution strategy</p>
                          </li>
                        </ul>
                      </div>
                      
                      <div>
                        <h4 className="font-semibold text-terminal-accent mb-2">Returns:</h4>
                        <ul className="space-y-2">
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">ResolvedState</span>
                            <p className="mt-1 opacity-80">The merged state after conflict resolution</p>
                          </li>
                        </ul>
                      </div>
                      
                      <div>
                        <h4 className="font-semibold text-terminal-accent mb-2">Strategies:</h4>
                        <ul className="space-y-2">
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">last_write_wins</span>
                            <p className="mt-1 opacity-80">Use most recent version</p>
                          </li>
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">merge</span>
                            <p className="mt-1 opacity-80">Attempt to merge differences</p>
                          </li>
                          <li>
                            <span className="font-mono bg-[#2d2d2d] px-1 rounded">custom</span>
                            <p className="mt-1 opacity-80">Use custom function</p>
                          </li>
                        </ul>
                      </div>
                    </div>
                    
                    <div className="bg-[#2d2d2d] p-4 rounded-md mb-4">
                      <h4 className="font-semibold text-terminal-accent mb-2">Example:</h4>
                      <pre className="text-xs font-mono overflow-x-auto">
{`%% Custom conflict resolution for user preferences
CustomStrategy = fun(S1, S2) ->
  %% Keep preferences from both states with S1 taking precedence for conflicts
  BasePrefs = maps:get(preferences, S2, #{}),
  S1Prefs = maps:get(preferences, S1, #{}),
  
  %% Merge preferences map with S1 taking precedence
  MergedPrefs = maps:merge(BasePrefs, S1Prefs),
  
  %% Create new state with merged preferences
  maps:put(preferences, MergedPrefs, S1)
end,

%% Resolve conflict using custom strategy
MergedState = hb_converge:resolve_conflict(
  LocalState, 
  RemoteState,
  {custom, CustomStrategy}
),

%% Update local state
ets:insert(state_table, {current_state, MergedState}),`}
                      </pre>
                    </div>
                    
                    <div className="text-sm">
                      <h4 className="font-semibold text-terminal-accent mb-2">Notes:</h4>
                      <ul className="list-disc pl-5 space-y-1">
                        <li>The specific algorithm used depends on the resolution strategy</li>
                        <li>Custom strategies provide maximum flexibility but require careful implementation</li>
                        <li>Many conflicts can be automatically resolved by using CRDTs (Conflict-free Replicated Data Types) instead of manual resolution</li>
                        <li>Consider using the <code className="bg-[#2d2d2d] px-1 rounded">hb_crdt</code> module for automatic conflict resolution when possible</li>
                      </ul>
                    </div>
                  </div>
                </div>
              </div>
            </section>
            
            <section>
              <h2 className="text-2xl font-bold text-terminal-primary mb-6 border-b border-[#444] pb-2">
                Related Documentation
              </h2>
              
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <Link href="/core/concepts/convergence" className="block">
                  <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-all">
                    <h3 className="text-lg font-semibold text-terminal-green mb-2">Convergence Concepts</h3>
                    <p className="text-sm opacity-80">
                      Learn about the theoretical foundations of state convergence in distributed systems.
                    </p>
                  </div>
                </Link>
                
                <Link href="/reference/functions/core/hb_clock" className="block">
                  <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-all">
                    <h3 className="text-lg font-semibold text-terminal-green mb-2">Vector Clock API</h3>
                    <p className="text-sm opacity-80">
                      Explore the vector clock implementation used by the convergence module.
                    </p>
                  </div>
                </Link>
                
                <Link href="/subsystems/storage/consistency" className="block">
                  <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-all">
                    <h3 className="text-lg font-semibold text-terminal-green mb-2">Storage Consistency</h3>
                    <p className="text-sm opacity-80">
                      Understand how convergence relates to storage subsystem consistency.
                    </p>
                  </div>
                </Link>
                
                <Link href="/patterns/crdt" className="block">
                  <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-all">
                    <h3 className="text-lg font-semibold text-terminal-green mb-2">CRDT Design Pattern</h3>
                    <p className="text-sm opacity-80">
                      Learn about Conflict-free Replicated Data Types for automatic conflict resolution.
                    </p>
                  </div>
                </Link>
              </div>
            </section>
          </div>
        </div>
        
        <div className="lg:col-span-1">
          <div className="sticky top-[80px] space-y-6">
            {/* Function Navigation */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">hb_converge Functions</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <a href="#sync_state" className="text-terminal-text hover:text-terminal-green block">
                    sync_state/2
                  </a>
                </li>
                <li>
                  <a href="#resolve_conflict" className="text-terminal-text hover:text-terminal-green block">
                    resolve_conflict/3
                  </a>
                </li>
                <li>
                  <span className="opacity-60 block">detect_conflicts/1</span>
                </li>
                <li>
                  <span className="opacity-60 block">state_version/1</span>
                </li>
                <li>
                  <span className="opacity-60 block">merge_versions/2</span>
                </li>
                <li>
                  <span className="opacity-60 block">divergence_point/2</span>
                </li>
              </ul>
            </div>
            
            {/* Related Modules */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Related Modules</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <Link href="/reference/functions/core/hb_clock" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm1-12a1 1 0 10-2 0v4a1 1 0 00.293.707l2.828 2.829a1 1 0 101.415-1.415L11 9.586V6z" clipRule="evenodd" />
                    </svg>
                    hb_clock
                  </Link>
                </li>
                <li>
                  <Link href="/reference/functions/core/hb_system" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M3 5a2 2 0 012-2h10a2 2 0 012 2v8a2 2 0 01-2 2h-2.22l.123.489.804.804A1 1 0 0113 18H7a1 1 0 01-.707-1.707l.804-.804L7.22 15H5a2 2 0 01-2-2V5zm5.771 7H5V5h10v7H8.771z" clipRule="evenodd" />
                    </svg>
                    hb_system
                  </Link>
                </li>
                <li>
                  <Link href="/reference/functions/core/hb_crdt" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path d="M5 3a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 00-2-2H5zM5 11a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2v-2a2 2 0 00-2-2H5zM11 5a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V5zM11 13a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2v-2z" />
                    </svg>
                    hb_crdt
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
