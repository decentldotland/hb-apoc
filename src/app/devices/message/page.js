"use client"

import { useState } from 'react';
import Link from 'next/link';
import Terminal from '../../../components/Terminal';

export default function MessageDeviceDoc() {
  // Sample terminal commands for this device
  const initialCommands = [
    { 
      command: 'cat dev_message.erl | head -n 20', 
      output: `%% @doc Message manipulation and attestation device.
%% Core identity device that provides field access, manipulation, and attestation handling.
-module(dev_message).
-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_field/2, set_field/3, merge_fields/2, sign/2, verify/2]).

-include_lib("kernel/include/logger.hrl").

%% Public API

%% @doc Retrieves a field from the message.
-spec get_field(Msg :: map(), Field :: atom() | binary()) -> {ok, term()} | {error, atom()}.
get_field(Msg, Field) when is_map(Msg) ->
    gen_server:call(?MODULE, {get_field, Msg, Field}).

%% @doc Sets a field in the message.
-spec set_field(Msg :: map(), Field :: atom() | binary(), Value :: term()) -> {ok, map()} | {error, atom()}`
    }
  ];

  return (
    <>
      <div className="mb-6">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <Link href="/devices" className="hover:text-terminal-green">Devices</Link>
          <span>/</span>
          <span className="text-terminal-green">Message</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-2">
          dev_message.erl
          <a 
            href="https://github.com/permaweb/HyperBEAM/blob/main/src/dev_message.erl" 
            target="_blank" 
            rel="noopener noreferrer"
            className="ml-3 text-sm font-normal text-terminal-green inline-flex items-center hover:underline"
          >
            <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-1" viewBox="0 0 20 20" fill="currentColor">
              <path fillRule="evenodd" d="M12.316 3.051a1 1 0 01.633 1.265l-4 12a1 1 0 11-1.898-.632l4-12a1 1 0 011.265-.633zM5.707 6.293a1 1 0 010 1.414L3.414 10l2.293 2.293a1 1 0 11-1.414 1.414l-3-3a1 1 0 010-1.414l3-3a1 1 0 011.414 0zm8.586 0a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 11-1.414-1.414L16.586 10l-2.293-2.293a1 1 0 010-1.414z" clipRule="evenodd" />
            </svg>
            View Source
          </a>
        </h1>
        
        <div className="flex items-center mb-6">
          <span className="px-2 py-1 rounded-full bg-[#333] text-xs font-semibold mr-2">Core</span>
          <span className="text-terminal-accent text-sm">14 downstream dependents</span>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-4 gap-8 mb-12">
        <div className="lg:col-span-3">
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Overview</h2>
            <p className="mb-4 leading-relaxed">
              The Message Device (dev_message.erl) is a core identity device in the HyperBEAM architecture. It provides essential 
              functionality for message field access, manipulation, and cryptographic attestation handling. As one of the most 
              fundamental devices, it's widely used throughout the system with 14 downstream dependents.
            </p>
            <p className="mb-4 leading-relaxed">
              This device implements operations for retrieving, setting, and merging message fields, as well as signing and verifying 
              messages using cryptographic techniques to ensure data integrity and authenticity. It's a critical component of HyperBEAM's 
              security model, enabling secure message passing between subsystems.
            </p>
          </section>
          
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Key Functions</h2>
            
            <div className="bg-[#252526] border border-[#333] rounded-md mb-4 overflow-hidden">
              <div className="bg-[#333] px-4 py-2 font-mono text-sm">get_field/2</div>
              <div className="p-4">
                <p className="mb-2 leading-relaxed">
                  Retrieves a value from a message by field name. The field can be specified as an atom or a binary string.
                </p>
                <pre className="bg-[#2d2d2d] p-3 rounded text-sm overflow-x-auto">
                  <code>{`-spec get_field(Msg :: map(), Field :: atom() | binary()) -> {ok, term()} | {error, atom()}.`}</code>
                </pre>
                <div className="mt-4">
                  <h4 className="text-sm font-semibold mb-2 text-terminal-accent">Examples:</h4>
                  <pre className="bg-[#2d2d2d] p-3 rounded text-sm overflow-x-auto">
                    <code>{`{ok, Value} = dev_message:get_field(Msg, author).
{ok, Value} = dev_message:get_field(Msg, <<"timestamp">>).`}</code>
                  </pre>
                </div>
              </div>
            </div>
            
            <div className="bg-[#252526] border border-[#333] rounded-md mb-4 overflow-hidden">
              <div className="bg-[#333] px-4 py-2 font-mono text-sm">set_field/3</div>
              <div className="p-4">
                <p className="mb-2 leading-relaxed">
                  Sets a field in a message to a specified value, returning the updated message.
                </p>
                <pre className="bg-[#2d2d2d] p-3 rounded text-sm overflow-x-auto">
                  <code>{`-spec set_field(Msg :: map(), Field :: atom() | binary(), Value :: term()) -> {ok, map()} | {error, atom()}.`}</code>
                </pre>
                <div className="mt-4">
                  <h4 className="text-sm font-semibold mb-2 text-terminal-accent">Examples:</h4>
                  <pre className="bg-[#2d2d2d] p-3 rounded text-sm overflow-x-auto">
                    <code>{`{ok, UpdatedMsg} = dev_message:set_field(Msg, priority, high).
{ok, UpdatedMsg} = dev_message:set_field(Msg, <<"status">>, <<"processed">>).`}</code>
                  </pre>
                </div>
              </div>
            </div>
            
            <div className="bg-[#252526] border border-[#333] rounded-md mb-4 overflow-hidden">
              <div className="bg-[#333] px-4 py-2 font-mono text-sm">merge_fields/2</div>
              <div className="p-4">
                <p className="mb-2 leading-relaxed">
                  Merges a map of fields into an existing message, overwriting any duplicate keys.
                </p>
                <pre className="bg-[#2d2d2d] p-3 rounded text-sm overflow-x-auto">
                  <code>{`-spec merge_fields(Msg :: map(), Fields :: map()) -> {ok, map()} | {error, atom()}.`}</code>
                </pre>
                <div className="mt-4">
                  <h4 className="text-sm font-semibold mb-2 text-terminal-accent">Examples:</h4>
                  <pre className="bg-[#2d2d2d] p-3 rounded text-sm overflow-x-auto">
                    <code>{`FieldsToMerge = #{priority => high, status => pending}.
{ok, UpdatedMsg} = dev_message:merge_fields(Msg, FieldsToMerge).`}</code>
                  </pre>
                </div>
              </div>
            </div>
            
            <div className="bg-[#252526] border border-[#333] rounded-md mb-4 overflow-hidden">
              <div className="bg-[#333] px-4 py-2 font-mono text-sm">sign/2</div>
              <div className="p-4">
                <p className="mb-2 leading-relaxed">
                  Cryptographically signs a message using the provided key, adding a signature field.
                </p>
                <pre className="bg-[#2d2d2d] p-3 rounded text-sm overflow-x-auto">
                  <code>{`-spec sign(Msg :: map(), Key :: binary()) -> {ok, map()} | {error, atom()}.`}</code>
                </pre>
                <div className="mt-4">
                  <h4 className="text-sm font-semibold mb-2 text-terminal-accent">Examples:</h4>
                  <pre className="bg-[#2d2d2d] p-3 rounded text-sm overflow-x-auto">
                    <code>{`{ok, SignedMsg} = dev_message:sign(Msg, PrivateKey).`}</code>
                  </pre>
                </div>
              </div>
            </div>
            
            <div className="bg-[#252526] border border-[#333] rounded-md overflow-hidden">
              <div className="bg-[#333] px-4 py-2 font-mono text-sm">verify/2</div>
              <div className="p-4">
                <p className="mb-2 leading-relaxed">
                  Verifies the signature of a message using the provided public key.
                </p>
                <pre className="bg-[#2d2d2d] p-3 rounded text-sm overflow-x-auto">
                  <code>{`-spec verify(Msg :: map(), Key :: binary()) -> {ok, boolean()} | {error, atom()}.`}</code>
                </pre>
                <div className="mt-4">
                  <h4 className="text-sm font-semibold mb-2 text-terminal-accent">Examples:</h4>
                  <pre className="bg-[#2d2d2d] p-3 rounded text-sm overflow-x-auto">
                    <code>{`{ok, IsValid} = dev_message:verify(SignedMsg, PublicKey).`}</code>
                  </pre>
                </div>
              </div>
            </div>
          </section>
          
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Implementation Details</h2>
            <p className="mb-4 leading-relaxed">
              The dev_message device is implemented as a gen_server, providing a clean API while encapsulating implementation 
              details. It uses Erlang's efficient pattern matching for field access and ETS tables for caching frequently used 
              messages to improve performance.
            </p>
            <p className="mb-4 leading-relaxed">
              For cryptographic operations, it leverages the Erlang crypto module and includes support for both symmetric 
              and asymmetric cryptography, depending on the use case. Message signing uses ECDSA with the secp256k1 curve, 
              providing robust security with low computational overhead.
            </p>
            <p className="leading-relaxed">
              The device maintains message immutability by always returning a new message copy after modifications, which is 
              critical for HyperBEAM's append-only message architecture.
            </p>
          </section>
        </div>
        
        <div className="lg:col-span-1">
          <div className="sticky top-[80px]">
            <div className="bg-[#252526] border border-[#333] rounded-md p-4 mb-6">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Device Information</h3>
              <ul className="space-y-2 text-sm">
                <li className="flex justify-between">
                  <span className="text-gray-400">Type:</span>
                  <span>Core</span>
                </li>
                <li className="flex justify-between">
                  <span className="text-gray-400">Dependencies:</span>
                  <span>3</span>
                </li>
                <li className="flex justify-between">
                  <span className="text-gray-400">Dependents:</span>
                  <span>14</span>
                </li>
                <li className="flex justify-between">
                  <span className="text-gray-400">Added:</span>
                  <span>v0.1.0</span>
                </li>
                <li className="flex justify-between">
                  <span className="text-gray-400">Last Updated:</span>
                  <span>v0.9.2</span>
                </li>
              </ul>
            </div>
            
            <div className="bg-[#252526] border border-[#333] rounded-md p-4 mb-6">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Device Dependencies</h3>
              <ul className="space-y-1 text-sm">
                <li>
                  <Link href="/core/hb-util" className="text-terminal-text hover:text-terminal-green">
                    hb_util.erl
                  </Link>
                </li>
                <li>
                  <Link href="/core/hb-converge" className="text-terminal-text hover:text-terminal-green">
                    hb_converge.erl
                  </Link>
                </li>
                <li>
                  <Link href="/core/hb-message" className="text-terminal-text hover:text-terminal-green">
                    hb_message.erl
                  </Link>
                </li>
              </ul>
            </div>
            
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">See Also</h3>
              <ul className="space-y-1 text-sm">
                <li>
                  <Link href="/devices/stack" className="text-terminal-text hover:text-terminal-green">
                    dev_stack.erl
                  </Link>
                </li>
                <li>
                  <Link href="/devices/process" className="text-terminal-text hover:text-terminal-green">
                    dev_process.erl
                  </Link>
                </li>
                <li>
                  <Link href="/devices/scheduler" className="text-terminal-text hover:text-terminal-green">
                    dev_scheduler.erl
                  </Link>
                </li>
                <li>
                  <Link href="/devices/meta" className="text-terminal-text hover:text-terminal-green">
                    dev_meta.erl
                  </Link>
                </li>
              </ul>
            </div>
          </div>
        </div>
      </div>
      
      <section className="mb-8">
        <h2 className="text-2xl font-bold text-terminal-primary mb-4">Interactive Terminal</h2>
        <p className="mb-4">
          Try out some commands to explore the dev_message.erl device:
        </p>
        <Terminal initialCommands={initialCommands} height={250} />
      </section>
      
      <section className="mb-8">
        <h2 className="text-2xl font-bold text-terminal-primary mb-4">Usage Examples</h2>
        
        <div className="bg-[#252526] border border-[#333] rounded-md mb-6 overflow-hidden">
          <div className="bg-[#333] px-4 py-2 font-semibold">Basic Field Manipulation</div>
          <div className="p-4">
            <pre className="bg-[#2d2d2d] p-3 rounded text-sm overflow-x-auto">
              <code>{`%% Create a basic message
Msg = #{
  author => <<"user1">>,
  content => <<"Hello, HyperBEAM!">>,
  timestamp => erlang:system_time(millisecond)
}.

%% Get a field value
{ok, Author} = dev_message:get_field(Msg, author),
io:format("Author: ~p~n", [Author]),

%% Set a field value
{ok, UpdatedMsg} = dev_message:set_field(Msg, priority, high),

%% Merge multiple fields
FieldsToMerge = #{
  tags => [<<"important">>, <<"announcement">>],
  status => <<"pending">>
},
{ok, FinalMsg} = dev_message:merge_fields(UpdatedMsg, FieldsToMerge),
io:format("Final message: ~p~n", [FinalMsg]).`}</code>
            </pre>
          </div>
        </div>
        
        <div className="bg-[#252526] border border-[#333] rounded-md overflow-hidden">
          <div className="bg-[#333] px-4 py-2 font-semibold">Message Signing and Verification</div>
          <div className="p-4">
            <pre className="bg-[#2d2d2d] p-3 rounded text-sm overflow-x-auto">
              <code>{`%% Generate a key pair (in a real application, you would have these securely stored)
{PublicKey, PrivateKey} = crypto:generate_key(ecdsa, secp256k1),

%% Create a message
Msg = #{
  id => crypto:strong_rand_bytes(16),
  author => <<"system">>,
  content => <<"Critical system update required">>,
  timestamp => erlang:system_time(second)
},

%% Sign the message
{ok, SignedMsg} = dev_message:sign(Msg, PrivateKey),

%% Verify the signature (should return {ok, true})
{ok, IsValid} = dev_message:verify(SignedMsg, PublicKey),
io:format("Signature valid: ~p~n", [IsValid]),

%% Try to verify with tampered message
{ok, TamperedMsg} = dev_message:set_field(SignedMsg, content, <<"Fake message">>),
{ok, IsTamperedValid} = dev_message:verify(TamperedMsg, PublicKey),
io:format("Tampered signature valid: ~p~n", [IsTamperedValid]).`}</code>
            </pre>
          </div>
        </div>
      </section>
    </>
  );
}
