"use client"

import Link from 'next/link';
import Terminal from '../../../components/Terminal';

// Metadata moved to separate metadata.js file since it can't be exported from 'use client' components

export default function NetworkSubsystem() {
  // Sample terminal commands for network subsystem
  const initialCommands = [
    { 
      command: 'ls -la subsystems/network/', 
      output: `total 9
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 ./
drwxr-xr-x 5 hyperbeam system 4096 Mar 27 2025 ../
-rw-r--r-- 1 hyperbeam system 14623 Mar 27 2025 hb_http.erl
-rw-r--r-- 1 hyperbeam system 16789 Mar 27 2025 hb_http_server.erl
-rw-r--r-- 1 hyperbeam system 12358 Mar 27 2025 hb_http_client.erl
-rw-r--r-- 1 hyperbeam system 10491 Mar 27 2025 hb_http_client_sup.erl
-rw-r--r-- 1 hyperbeam system 13762 Mar 27 2025 hb_client.erl
-rw-r--r-- 1 hyperbeam system 11842 Mar 27 2025 hb_gateway_client.erl
-rw-r--r-- 1 hyperbeam system 15273 Mar 27 2025 hb_router.erl
-rw-r--r-- 1 hyperbeam system 6982 Mar 27 2025 hb_singleton.erl`
    }
  ];

  // Network modules data
  const networkModules = [
    {
      id: 'hb_http',
      name: 'hb_http.erl',
      description: 'Core HTTP functionality and common utilities for HTTP communications.',
      functions: 12,
      dependencies: 2,
      dependents: 7,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_http.erl'
    },
    {
      id: 'hb_http_server',
      name: 'hb_http_server.erl',
      description: 'HTTP server implementation for handling incoming API requests.',
      functions: 15,
      dependencies: 4,
      dependents: 3,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_http_server.erl'
    },
    {
      id: 'hb_http_client',
      name: 'hb_http_client.erl',
      description: 'HTTP client implementation for making outgoing API requests.',
      functions: 11,
      dependencies: 3,
      dependents: 2,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_http_client.erl'
    },
    {
      id: 'hb_http_client_sup',
      name: 'hb_http_client_sup.erl',
      description: 'Supervisor for HTTP client processes, manages client connections.',
      functions: 8,
      dependencies: 2,
      dependents: 1,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_http_client_sup.erl'
    },
    {
      id: 'hb_client',
      name: 'hb_client.erl',
      description: 'Generic client interface for interacting with remote services.',
      functions: 12,
      dependencies: 3,
      dependents: 5,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_client.erl'
    },
    {
      id: 'hb_gateway_client',
      name: 'hb_gateway_client.erl',
      description: 'Client implementation for communicating with HyperBEAM gateway services.',
      functions: 10,
      dependencies: 4,
      dependents: 2,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_gateway_client.erl'
    },
    {
      id: 'hb_router',
      name: 'hb_router.erl',
      description: 'Message routing between nodes and external services.',
      functions: 14,
      dependencies: 5,
      dependents: 6,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_router.erl'
    },
    {
      id: 'hb_singleton',
      name: 'hb_singleton.erl',
      description: 'Manages singleton resource access across the distributed network.',
      functions: 7,
      dependencies: 2,
      dependents: 3,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_singleton.erl'
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
          <span className="text-terminal-green">Network</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Network Communication Subsystem
        </h1>
        
        <p className="text-lg mb-8">
          The Network Communication Subsystem enables reliable data exchange between HyperBEAM instances and external services through
          a flexible, extensible set of protocols and clients.
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6 flex items-center justify-between">
          <div className="font-mono text-sm">
            <span className="text-terminal-green">$</span> 
            <span className="text-terminal-accent ml-2">cd</span> 
            <span className="text-terminal-output ml-2">subsystems/network/</span>
          </div>
          <div className="bg-[#333] px-3 py-1 rounded text-xs font-mono">
            8 modules | 89 functions | 25 dependencies
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
                The Network Communication Subsystem is organized into client and server components, 
                with support for various protocols and routing strategies.
              </p>
              
              <div className="bg-[#2d2d2d] p-5 rounded-md">
                {/* ASCII architecture diagram */}
                <pre className="text-xs sm:text-sm text-terminal-text font-mono whitespace-pre overflow-x-auto">
{`┌─────────────────────────────────────────────────────────────┐
│                  EXTERNAL SYSTEMS & CLIENTS                  │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│                       HTTP LAYER                            │
│  ┌────────────────┐                  ┌────────────────┐     │
│  │  HTTP Server   │◄────────────────►│  HTTP Client   │     │
│  └────────────────┘                  └────────────────┘     │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│                     ROUTING LAYER                           │
│  ┌────────────┐    ┌────────────┐    ┌──────────────────┐   │
│  │ Gateway    │    │ Message    │    │ Singleton        │   │
│  │ Clients    │◄──►│ Router     │◄──►│ Resource Manager │   │
│  └────────────┘    └────────────┘    └──────────────────┘   │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│                  HYPERBEAM CORE SYSTEM                      │
└─────────────────────────────────────────────────────────────┘`}
                </pre>
              </div>
              
              <p className="text-sm">
                The subsystem consists of these main components:
              </p>
              
              <ul className="space-y-2 text-sm pl-5 list-disc">
                <li>
                  <strong>HTTP Layer</strong> - Handles HTTP protocol operations:
                  <ul className="pl-5 mt-1 space-y-1 list-disc">
                    <li>HTTP Server - Receives and processes incoming requests</li>
                    <li>HTTP Client - Sends outgoing requests and handles responses</li>
                    <li>HTTP Client Supervisor - Manages client connection pools</li>
                  </ul>
                </li>
                <li>
                  <strong>Routing Layer</strong> - Manages message and request routing:
                  <ul className="pl-5 mt-1 space-y-1 list-disc">
                    <li>Router - Directs messages based on content and destination</li>
                    <li>Gateway Clients - Specialized clients for HyperBEAM gateways</li>
                    <li>Client Interface - Abstract client interface for all protocols</li>
                  </ul>
                </li>
                <li>
                  <strong>Singleton Manager</strong> - Coordinates access to distributed resources across nodes</li>
              </ul>
            </div>
          </section>
          
          {/* Key Features */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Key Features</h2>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-5">
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Protocol Abstraction</h3>
                <p className="text-sm opacity-80">
                  A unified client interface that works with multiple protocols (HTTP, WebSockets) through a consistent API.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Content-Based Routing</h3>
                <p className="text-sm opacity-80">
                  Smart routing based on message content, destination, and network topology for optimal performance.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Connection Pooling</h3>
                <p className="text-sm opacity-80">
                  Efficient management of connection resources with automatic scaling, throttling, and backpressure.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Fallback Mechanisms</h3>
                <p className="text-sm opacity-80">
                  Built-in resilience with automatic retries, circuit breaking, and alternative path routing on failures.
                </p>
              </div>
            </div>
          </section>
          
          {/* Core Modules */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Core Modules</h2>
            
            <div className="grid grid-cols-1 gap-4">
              {networkModules.map((module) => (
                <Link href={`/subsystems/network/${module.id}`} key={module.id} className="block">
                  <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-all duration-200 card-hover-anim">
                    <div className="flex flex-col md:flex-row justify-between items-start">
                      <div>
                        <h3 className="font-mono text-lg font-semibold mb-2 text-terminal-primary">
                          {module.name}
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
              ))}
            </div>
          </section>
          
          {/* Usage Examples */}
          <section>
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Usage Examples</h2>
            
            <div className="bg-[#252526] border border-[#333] rounded-md p-6">
              <p className="mb-4">
                The following examples demonstrate common usage patterns for the Network Communication Subsystem:
              </p>
              
              <div className="mb-6">
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Making HTTP Requests</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Simple GET request
URL = "https://api.example.com/v1/resource",
Headers = #{<<"accept">> => <<"application/json">>},
{ok, Response} = hb_http_client:get(URL, Headers),

%% POST request with JSON body
URL = "https://api.example.com/v1/users",
Headers = #{
  <<"content-type">> => <<"application/json">>,
  <<"accept">> => <<"application/json">>
},
Body = #{name => <<"Alice">>, email => <<"alice@example.com">>},
Options = #{timeout => 5000, retries => 3},
{ok, Response} = hb_http_client:post(URL, Headers, Body, Options),

%% Handle response
#{
  status_code := 201,
  headers := ResponseHeaders,
  body := ResponseBody
} = Response,

%% Parse JSON response
User = hb_http:decode_json(ResponseBody)`}</code>
                </pre>
              </div>
              
              <div className="mb-6">
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Setting Up an HTTP Server</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Define routes
Routes = [
  {"/api/v1/users", {user_handler, handle_users}, []},
  {"/api/v1/posts", {post_handler, handle_posts}, []},
  {"/api/v1/comments", {comment_handler, handle_comments}, []}
],

%% Server configuration
Config = #{
  port => 8080,
  max_connections => 1000,
  idle_timeout => 30000,
  routes => Routes
},

%% Start the server
{ok, _} = hb_http_server:start(Config),

%% Handler function example (in user_handler.erl)
handle_users(<<"GET">>, _Path, _Headers, _Body, _State) ->
  Users = fetch_users_from_db(),
  Response = #{
    status_code => 200,
    headers => #{<<"content-type">> => <<"application/json">>},
    body => hb_http:encode_json(Users)
  },
  {ok, Response, _State}`}</code>
                </pre>
              </div>
              
              <div>
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Message Routing</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Define a message
Msg = #{
  id => hb_util:generate_id(),
  operation => "store_data",
  data => #{key => "sensor-123", value => 42},
  destination => <<"node-456">>
},

%% Route the message to the appropriate handler
{ok, Result} = hb_router:route(Msg),

%% Using the gateway client to interact with a remote HyperBEAM instance
RemoteNode = "https://gateway.hyperbeam.example.com",
{ok, Client} = hb_gateway_client:connect(RemoteNode, #{
  auth_token => "secure-token-123",
  timeout => 10000
}),

%% Execute operation on remote node
{ok, Response} = hb_gateway_client:execute(Client, Msg),

%% Disconnect client when done
ok = hb_gateway_client:disconnect(Client)`}</code>
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
                  <Link href="/subsystems/arweave" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z" clipRule="evenodd" />
                    </svg>
                    Arweave Integration
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
            
            {/* Protocol Details */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Supported Protocols</h3>
              <ul className="space-y-3 text-sm">
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">HTTP/HTTPS</span>
                    <p className="mt-1 opacity-80 text-xs">
                      REST API support with JSON, MessagePack, and custom format serialization
                    </p>
                  </div>
                </li>
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">WebSockets</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Bidirectional communication for real-time data exchange and events
                    </p>
                  </div>
                </li>
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">Internal Protocol</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Optimized binary protocol for HyperBEAM node-to-node communication
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
