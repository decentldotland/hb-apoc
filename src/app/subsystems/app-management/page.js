"use client"

import Link from 'next/link';
import Terminal from '../../../components/Terminal';

// Metadata moved to separate metadata.js file since it can't be exported from 'use client' components

export default function AppManagementSubsystem() {
  // Sample terminal commands for app management subsystem
  const initialCommands = [
    { 
      command: 'ls -la subsystems/app-management/', 
      output: `total 7
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 ./
drwxr-xr-x 5 hyperbeam system 4096 Mar 27 2025 ../
-rw-r--r-- 1 hyperbeam system 15283 Mar 27 2025 hb_app.erl
-rw-r--r-- 1 hyperbeam system 12457 Mar 27 2025 hb_app_mounter.erl
-rw-r--r-- 1 hyperbeam system 10986 Mar 27 2025 hb_app_runner.erl
-rw-r--r-- 1 hyperbeam system 9752 Mar 27 2025 hb_app_state.erl
-rw-r--r-- 1 hyperbeam system 14367 Mar 27 2025 hb_app_registry.erl
-rw-r--r-- 1 hyperbeam system 13425 Mar 27 2025 hb_app_inspector.erl
-rw-r--r-- 1 hyperbeam system 11084 Mar 27 2025 hb_app_package.erl`
    }
  ];

  // App Management modules data
  const appManagementModules = [
    {
      id: 'hb_app',
      name: 'hb_app.erl',
      description: 'Core application management interface providing a unified API for application lifecycle operations.',
      functions: 15,
      dependencies: 4,
      dependents: 7,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_app.erl'
    },
    {
      id: 'hb_logger',
      name: 'hb_logger.erl',
      description: 'Logging infrastructure for application diagnostics and operational monitoring.',
      functions: 12,
      dependencies: 3,
      dependents: 2,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_logger.erl'
    },
    {
      id: 'hb_metrics_collector',
      name: 'hb_metrics_collector.erl',
      description: 'Performance metrics collection and analysis for application monitoring.',
      functions: 10,
      dependencies: 4,
      dependents: 2,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_metrics_collector.erl'
    },
    {
      id: 'hb_name',
      name: 'hb_name.erl',
      description: 'Naming service for registering and looking up named resources across the system.',
      functions: 9,
      dependencies: 3,
      dependents: 4,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_name.erl'
    },
    {
      id: 'hb_process_monitor',
      name: 'hb_process_monitor.erl',
      description: 'Process monitoring and management for tracking application health and resource usage.',
      functions: 13,
      dependencies: 2,
      dependents: 3,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_process_monitor.erl'
    },
    {
      id: 'hb_sup',
      name: 'hb_sup.erl',
      description: 'Top-level supervisor for the HyperBEAM application management hierarchy.',
      functions: 11,
      dependencies: 3,
      dependents: 2,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_sup.erl'
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
          <span className="text-terminal-green">App Management</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Application Management Subsystem
        </h1>
        
        <p className="text-lg mb-8">
          The Application Management Subsystem provides a comprehensive framework for deploying, running, and managing
          applications within the HyperBEAM environment, with support for lifecycle management, state persistence, and secure execution.
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6 flex items-center justify-between">
          <div className="font-mono text-sm">
            <span className="text-terminal-green">$</span> 
            <span className="text-terminal-accent ml-2">cd</span> 
            <span className="text-terminal-output ml-2">subsystems/app-management/</span>
          </div>
          <div className="bg-[#333] px-3 py-1 rounded text-xs font-mono">
            7 modules | 80 functions | 22 dependencies
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
                The Application Management Subsystem provides a robust framework for deploying and running applications
                within HyperBEAM, with a focus on security, isolation, and lifecycle management.
              </p>
              
              <div className="bg-[#2d2d2d] p-5 rounded-md">
                {/* ASCII architecture diagram */}
                <pre className="text-xs sm:text-sm text-terminal-text font-mono whitespace-pre overflow-x-auto">
{`┌──────────────────────────────────────────────────────────────┐
│                    APPLICATION REQUEST                        │
└────────────────────────────┬─────────────────────────────────┘
                             │
┌────────────────────────────▼─────────────────────────────────┐
│                         hb_app                               │
│                (Main Application Interface)                   │
└───┬─────────┬─────────┬────────────┬───────────┬─────────────┘
    │         │         │            │           │
┌───▼───┐ ┌───▼───┐ ┌───▼───┐  ┌─────▼────┐ ┌────▼─────┐
│Registry│ │Package│ │Mounter│  │  Runner  │ │  State   │
│ Module │ │ Module│ │ Module│  │  Module  │ │  Module  │
└───────┘ └───────┘ └───────┘  └──────────┘ └──────────┘
                                    │
                              ┌─────▼────┐
                              │Inspector │
                              │  Module  │
                              └──────────┘
                                    │
┌────────────────────────────┬─────▼────┬───────────────────────┐
│    STORAGE SUBSYSTEM       │  DEVICE  │    RUNTIME ENVIRON.   │
│                           │ECOSYSTEM │                       │
└────────────────────────────┴──────────┴───────────────────────┘`}
                </pre>
              </div>
              
              <p className="text-sm">
                The subsystem consists of these main components:
              </p>
              
              <ul className="space-y-2 text-sm pl-5 list-disc">
                <li>
                  <strong>Main Interface (hb_app)</strong> - Provides a unified API for application lifecycle operations
                </li>
                <li>
                  <strong>Registry Module (hb_app_registry)</strong> - Manages application registration and discovery
                </li>
                <li>
                  <strong>Package Module (hb_app_package)</strong> - Handles application packaging and distribution
                </li>
                <li>
                  <strong>Mounter Module (hb_app_mounter)</strong> - Prepares applications for execution in the HyperBEAM environment
                </li>
                <li>
                  <strong>Runner Module (hb_app_runner)</strong> - Manages the application execution environment with isolation
                </li>
                <li>
                  <strong>State Module (hb_app_state)</strong> - Handles application state persistence and management
                </li>
                <li>
                  <strong>Inspector Module (hb_app_inspector)</strong> - Validates applications for security and compatibility
                </li>
              </ul>
            </div>
          </section>
          
          {/* Key Features */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Key Features</h2>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-5">
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Application Lifecycle</h3>
                <p className="text-sm opacity-80">
                  Complete lifecycle management from installation to execution, update, and removal with proper resource cleanup.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Secure Execution</h3>
                <p className="text-sm opacity-80">
                  Sandbox isolation with configurable resource limits and permissions for secure application execution.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">State Management</h3>
                <p className="text-sm opacity-80">
                  Persistent state storage with transaction support for maintaining application data across restarts.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Package System</h3>
                <p className="text-sm opacity-80">
                  Standardized packaging format for distribution with dependency resolution and version management.
                </p>
              </div>
            </div>
          </section>
          
          {/* Core Modules */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Core Modules</h2>
            
            <div className="grid grid-cols-1 gap-4">
              {appManagementModules.map((module) => (
                <div key={module.id} className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-all duration-200 card-hover-anim">
                  <div className="flex flex-col md:flex-row justify-between items-start">
                    <div>
                      <h3 className="font-mono text-lg font-semibold mb-2 text-terminal-primary">
                        <Link href={`/subsystems/app-management/${module.id}`} className="hover:text-terminal-accent">
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
                      <Link href={`/subsystems/app-management/${module.id}`} className="block">
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
                The following examples demonstrate common usage patterns for the Application Management Subsystem:
              </p>
              
              <div className="mb-6">
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Installing and Running an Application</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Install an application from a package file
PackagePath = "/path/to/myapp-1.0.0.hbpkg",
{ok, AppId} = hb_app:install(PackagePath),

%% Configure the application
Config = #{
  memory_limit => 104857600, % 100 MB
  storage_limit => 1073741824, % 1 GB
  network_access => true,
  permissions => [
    {read, [<<"data/">>]},
    {write, [<<"data/myapp/">>]}
  ]
},
{ok, _} = hb_app:configure(AppId, Config),

%% Start the application
{ok, AppPid} = hb_app:start(AppId),

%% Check application status
{ok, Status} = hb_app:status(AppId),
io:format("App Status: ~p~n", [Status]),

%% Stop the application
{ok, _} = hb_app:stop(AppId)`}</code>
                </pre>
              </div>
              
              <div className="mb-6">
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Creating and Packaging an Application</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Define package metadata
Metadata = #{
  name => "myapp",
  version => "1.0.0",
  description => "Example HyperBEAM application",
  author => "HyperBEAM Developer",
  entry_point => "main.wasm",
  dependencies => [
    {runtime, ">=0.5.0"},
    {storage, ">=0.3.0"}
  ]
},

%% Create a new package
{ok, Package} = hb_app_package:create(Metadata),

%% Add files to the package
Files = [
  {<<"main.wasm">>, "/path/to/main.wasm"},
  {<<"static/index.html">>, "/path/to/static/index.html"},
  {<<"static/app.js">>, "/path/to/static/app.js"},
  {<<"static/style.css">>, "/path/to/static/style.css"}
],

{ok, UpdatedPackage} = lists:foldl(
  fun({FileName, FilePath}, {ok, Pkg}) ->
    hb_app_package:add_file(Pkg, FileName, FilePath)
  end,
  {ok, Package},
  Files
),

%% Build and save the package
OutputPath = "/path/to/myapp-1.0.0.hbpkg",
{ok, _} = hb_app_package:build(UpdatedPackage, OutputPath)`}</code>
                </pre>
              </div>
              
              <div>
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Managing Application State</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Get application state manager
{ok, StateManager} = hb_app:get_state_manager(AppId),

%% Read current state
{ok, State} = hb_app_state:get(StateManager),

%% Start a transaction for state updates
{ok, Tx} = hb_app_state:begin_transaction(StateManager),

%% Update state within transaction
{ok, UpdatedTx} = hb_app_state:update(Tx, 
  fun(CurrentState) ->
    CurrentState#{
      user_count => maps:get(user_count, CurrentState, 0) + 1,
      last_update => erlang:system_time(second)
    }
  end
),

%% Commit the transaction
{ok, NewState} = hb_app_state:commit(UpdatedTx),

%% Snapshot state for backup
{ok, SnapshotId} = hb_app_state:create_snapshot(StateManager),

%% Inspect application data usage
{ok, Stats} = hb_app_inspector:storage_stats(AppId),
io:format("Data usage: ~p bytes~n", [maps:get(total_size, Stats)])`}</code>
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
                  <Link href="/devices" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z" clipRule="evenodd" />
                    </svg>
                    Device Ecosystem
                  </Link>
                </li>
                <li>
                  <Link href="/cross-subsystem" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z" clipRule="evenodd" />
                    </svg>
                    Cross-Subsystem Integration
                  </Link>
                </li>
              </ul>
            </div>
            
            {/* Application Types */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Application Types</h3>
              <ul className="space-y-3 text-sm">
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">WebAssembly Apps</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Portable applications compiled to WebAssembly with WASI system interface access
                    </p>
                  </div>
                </li>
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">Erlang Native Apps</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Native Erlang applications with enhanced performance but stricter security isolation
                    </p>
                  </div>
                </li>
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">JavaScript Apps</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Applications written in JavaScript using the HyperBEAM JS runtime environment
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
