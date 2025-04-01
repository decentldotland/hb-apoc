"use client"

import Link from 'next/link';
import Terminal from '../../../components/Terminal';

// Metadata moved to separate metadata.js file since it can't be exported from 'use client' components

export default function CodecSubsystem() {
  // Sample terminal commands for codec subsystem
  const initialCommands = [
    { 
      command: 'ls -la subsystems/codec/', 
      output: `total 7
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 ./
drwxr-xr-x 5 hyperbeam system 4096 Mar 27 2025 ../
-rw-r--r-- 1 hyperbeam system 12473 Mar 27 2025 hb_codec.erl
-rw-r--r-- 1 hyperbeam system 9584 Mar 27 2025 hb_codec_json.erl
-rw-r--r-- 1 hyperbeam system 8721 Mar 27 2025 hb_codec_msgpack.erl
-rw-r--r-- 1 hyperbeam system 11652 Mar 27 2025 hb_codec_binary.erl
-rw-r--r-- 1 hyperbeam system 10387 Mar 27 2025 hb_codec_erlang.erl
-rw-r--r-- 1 hyperbeam system 14586 Mar 27 2025 hb_schema.erl
-rw-r--r-- 1 hyperbeam system 13478 Mar 27 2025 hb_transform.erl`
    }
  ];

  // Codec modules data
  const codecModules = [
    {
      id: 'dev_codec_json',
      name: 'dev_codec_json.erl',
      description: 'JSON format codec implementation for human-readable data exchange.',
      functions: 8,
      dependencies: 2,
      dependents: 1,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_json.erl'
    },
    {
      id: 'dev_codec_structured',
      name: 'dev_codec_structured.erl',
      description: 'Schema definition and validation for ensuring data format consistency.',
      functions: 12,
      dependencies: 2,
      dependents: 7,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_structured.erl'
    },
    {
      id: 'hb_structured_fields',
      name: 'hb_structured_fields.erl',
      description: 'Data transformation utilities for converting between different formats and structures.',
      functions: 11,
      dependencies: 3,
      dependents: 5,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/hb_structured_fields.erl'
    },
    {
      id: 'dev_codec_ans104',
      name: 'dev_codec_ans104.erl',
      description: 'ANS-104 codec implementation for Arweave Network Standard data bundling.',
      functions: 9,
      dependencies: 3,
      dependents: 2,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_ans104.erl'
    },
    {
      id: 'dev_codec_flat',
      name: 'dev_codec_flat.erl',
      description: 'Flat data structure codec for simplified data serialization and deserialization.',
      functions: 7,
      dependencies: 1,
      dependents: 3,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_flat.erl'
    },
    {
      id: 'dev_codec_httpsig',
      name: 'dev_codec_httpsig.erl',
      description: 'HTTP Signature codec for secure request authentication and validation.',
      functions: 10,
      dependencies: 2,
      dependents: 2,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_httpsig.erl'
    },
    {
      id: 'dev_codec_httpsig_conv',
      name: 'dev_codec_httpsig_conv.erl',
      description: 'HTTP Signature conversion utilities for different signature formats.',
      functions: 8,
      dependencies: 2,
      dependents: 1,
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_httpsig_conv.erl'
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
          <span className="text-terminal-green">Codec</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Codec and Data Format Subsystem
        </h1>
        
        <p className="text-lg mb-8">
          The Codec Subsystem provides a flexible framework for serializing and deserializing data in various formats,
          with schema validation and transformation capabilities for interoperability.
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6 flex items-center justify-between">
          <div className="font-mono text-sm">
            <span className="text-terminal-green">$</span> 
            <span className="text-terminal-accent ml-2">cd</span> 
            <span className="text-terminal-output ml-2">subsystems/codec/</span>
          </div>
          <div className="bg-[#333] px-3 py-1 rounded text-xs font-mono">
            7 modules | 65 functions | 13 dependencies
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
                The Codec Subsystem provides a unified interface for data serialization and deserialization, with multiple
                codec implementations supporting different formats and use cases.
              </p>
              
              <div className="bg-[#2d2d2d] p-5 rounded-md">
                {/* ASCII architecture diagram */}
                <pre className="text-xs sm:text-sm text-terminal-text font-mono whitespace-pre overflow-x-auto">
{`┌──────────────────────────────────────────────────────────────┐
│              APPLICATION DATA (Erlang Terms)                  │
└────────────────────────────┬─────────────────────────────────┘
                             │
┌────────────────────────────▼─────────────────────────────────┐
│                        hb_schema                             │
│               (Schema Definition & Validation)                │
└────────────────────────────┬─────────────────────────────────┘
                             │
┌────────────────────────────▼─────────────────────────────────┐
│                        hb_transform                          │
│                (Data Structure Transformation)                │
└────────────────────────────┬─────────────────────────────────┘
                             │
┌────────────────────────────▼─────────────────────────────────┐
│                         hb_codec                             │
│                    (Codec Interface)                          │
└───┬─────────┬─────────┬────────────┬───────────────────────┬─┘
    │         │         │            │                       │
┌───▼───┐ ┌───▼───┐ ┌───▼───┐  ┌─────▼────┐            ┌─────▼────┐
│ JSON  │ │MsgPack│ │Binary │  │ Erlang   │            │ Custom   │
│ Codec │ │ Codec │ │ Codec │  │ Codec    │            │ Codecs   │
└───────┘ └───────┘ └───────┘  └──────────┘            └──────────┘
    │         │         │            │                       │
┌───▼─────────▼─────────▼────────────▼───────────────────────▼────┐
│                      SERIALIZED DATA                             │
│  (JSON, MessagePack, Binary, Erlang External Term Format, etc.)  │
└──────────────────────────────────────────────────────────────────┘`}
                </pre>
              </div>
              
              <p className="text-sm">
                The subsystem consists of these main components:
              </p>
              
              <ul className="space-y-2 text-sm pl-5 list-disc">
                <li>
                  <strong>Codec Interface (hb_codec)</strong> - Provides a unified API for encoding and decoding data
                </li>
                <li>
                  <strong>Codec Implementations</strong> - Format-specific codec modules:
                  <ul className="pl-5 mt-1 space-y-1 list-disc">
                    <li>JSON Codec - Human-readable text format for web integration</li>
                    <li>MessagePack Codec - Efficient binary serialization format</li>
                    <li>Binary Codec - Custom binary format optimized for performance</li>
                    <li>Erlang Codec - Native Erlang term serialization</li>
                  </ul>
                </li>
                <li>
                  <strong>Schema System (hb_schema)</strong> - Defines and validates data structures
                </li>
                <li>
                  <strong>Transformation Engine (hb_transform)</strong> - Converts data between different structures
                </li>
              </ul>
            </div>
          </section>
          
          {/* Key Features */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Key Features</h2>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-5">
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Format Abstraction</h3>
                <p className="text-sm opacity-80">
                  A unified API that abstracts away format-specific details, allowing seamless switching between serialization formats.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Schema Validation</h3>
                <p className="text-sm opacity-80">
                  Ensures data integrity through schema-based validation during encoding and decoding operations.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Format Negotiation</h3>
                <p className="text-sm opacity-80">
                  Automatic format selection based on content negotiation, client capabilities, or performance considerations.
                </p>
              </div>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim">
                <h3 className="text-lg font-semibold text-terminal-green mb-2">Transformation Pipeline</h3>
                <p className="text-sm opacity-80">
                  Data structure transformation capabilities for converting between different data representations and formats.
                </p>
              </div>
            </div>
          </section>
          
          {/* Core Modules */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Core Modules</h2>
            
            <div className="grid grid-cols-1 gap-4">
              {codecModules.map((module) => (
                <div key={module.id} className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-all duration-200 card-hover-anim">
                  <div className="flex flex-col md:flex-row justify-between items-start">
                    <div>
                      <h3 className="font-mono text-lg font-semibold mb-2 text-terminal-primary">
                        <Link href={`/subsystems/codec/${module.id}`} className="hover:text-terminal-accent">
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
                      <Link href={`/subsystems/codec/${module.id}`} className="block">
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
                The following examples demonstrate common usage patterns for the Codec Subsystem:
              </p>
              
              <div className="mb-6">
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Basic Encoding and Decoding</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Define a data structure
Data = #{
  id => <<"msg-123">>,
  timestamp => 1679854321,
  content => #{
    text => <<"Hello, world!">>,
    language => <<"en">>,
    tags => [<<"greeting">>, <<"example">>]
  },
  metadata => #{
    source => <<"user-456">>,
    priority => 1
  }
},

%% Encode to JSON
Options = #{pretty => true},
{ok, JsonBinary} = hb_codec:encode(Data, json, Options),

%% Decode from JSON
{ok, DecodedData} = hb_codec:decode(JsonBinary, json),

%% Encode to MessagePack (more compact binary format)
{ok, MsgPackBinary} = hb_codec:encode(Data, msgpack),

%% Encode to custom binary format
{ok, BinaryData} = hb_codec:encode(Data, binary),

%% Choose codec automatically based on content type
ContentType = <<"application/json">>,
{ok, EncodedData} = hb_codec:encode_for_content_type(Data, ContentType)`}</code>
                </pre>
              </div>
              
              <div className="mb-6">
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Schema Validation</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Define a schema
MessageSchema = #{
  type => map,
  required => [id, content],
  properties => #{
    id => #{type => binary},
    timestamp => #{type => integer},
    content => #{
      type => map,
      required => [text],
      properties => #{
        text => #{type => binary},
        language => #{type => binary},
        tags => #{
          type => array,
          items => #{type => binary}
        }
      }
    },
    metadata => #{type => map}
  }
},

%% Register the schema
ok = hb_schema:register(message, MessageSchema),

%% Validate data against schema
{ok, ValidatedData} = hb_schema:validate(Data, message),

%% Encode with schema validation
Options = #{schema => message},
{ok, JsonBinary} = hb_codec:encode(Data, json, Options),

%% This will fail validation due to missing required field
InvalidData = #{timestamp => 1679854321},
{error, {validation_error, missing_required_field, id}} = 
  hb_schema:validate(InvalidData, message)`}</code>
                </pre>
              </div>
              
              <div>
                <h3 className="text-lg font-semibold text-terminal-accent mb-3">Data Transformation</h3>
                <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto">
                  <code>{`%% Define a source data structure (internal format)
InternalData = #{
  message_id => <<"msg-123">>,
  user => <<"user-456">>,
  msg => <<"Hello, world!">>,
  ts => 1679854321,
  tags => [<<"greeting">>, <<"example">>]
},

%% Define a transformation spec
TransformSpec = #{
  id => {value, message_id},
  timestamp => {value, ts},
  content => #{
    text => {value, msg},
    language => {default, <<"en">>},
    tags => {value, tags}
  },
  metadata => #{
    source => {value, user},
    priority => {default, 1}
  }
},

%% Apply the transformation
{ok, TransformedData} = hb_transform:apply(InternalData, TransformSpec),

%% Transform and encode in one operation
{ok, JsonBinary} = hb_transform:transform_and_encode(
  InternalData, TransformSpec, json
),

%% Decode and transform in one operation
{ok, InternalData} = hb_transform:decode_and_transform(
  JsonBinary, 
  json,
  #{
    message_id => {value, id},
    user => {path, [metadata, source]},
    msg => {path, [content, text]},
    ts => {value, timestamp},
    tags => {path, [content, tags]}
  }
)`}</code>
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
                  <Link href="/subsystems/storage" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z" clipRule="evenodd" />
                    </svg>
                    Storage Subsystem
                  </Link>
                </li>
              </ul>
            </div>
            
            {/* Format Comparison */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Format Comparison</h3>
              <div className="text-xs mb-3">
                <table className="w-full">
                  <thead>
                    <tr className="text-left border-b border-[#444]">
                      <th className="pb-2">Format</th>
                      <th className="pb-2">Size</th>
                      <th className="pb-2">Speed</th>
                      <th className="pb-2">Use Case</th>
                    </tr>
                  </thead>
                  <tbody className="opacity-90">
                    <tr className="border-b border-[#333]">
                      <td className="py-2">JSON</td>
                      <td className="py-2">Large</td>
                      <td className="py-2">Medium</td>
                      <td className="py-2">API, Web</td>
                    </tr>
                    <tr className="border-b border-[#333]">
                      <td className="py-2">MessagePack</td>
                      <td className="py-2">Small</td>
                      <td className="py-2">Fast</td>
                      <td className="py-2">Network</td>
                    </tr>
                    <tr className="border-b border-[#333]">
                      <td className="py-2">Binary</td>
                      <td className="py-2">Smallest</td>
                      <td className="py-2">Fastest</td>
                      <td className="py-2">Internal</td>
                    </tr>
                    <tr>
                      <td className="py-2">Erlang</td>
                      <td className="py-2">Medium</td>
                      <td className="py-2">Fast</td>
                      <td className="py-2">Erlang</td>
                    </tr>
                  </tbody>
                </table>
              </div>
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
