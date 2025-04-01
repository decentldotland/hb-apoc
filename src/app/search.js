"use client"

import { useState, useEffect } from 'react';
import Link from 'next/link';
import { useSearchParams } from 'next/navigation';

export const metadata = {
  title: 'Search - HyperBEAM Documentation',
  description: 'Search the HyperBEAM documentation for devices, functions, and concepts.'
};

export default function Search() {
  const searchParams = useSearchParams();
  const q = searchParams.get('q');
  const [searchTerm, setSearchTerm] = useState(q || '');
  const [isSearching, setIsSearching] = useState(false);
  const [searchResults, setSearchResults] = useState([]);
  
  // Mock search function - in a real implementation, this would query a backend or search index
  useEffect(() => {
    if (!q) return;
    
    setIsSearching(true);
    
    // Simulate search delay
    const timer = setTimeout(() => {
      const results = performSearch(q);
      setSearchResults(results);
      setIsSearching(false);
    }, 500);
    
    return () => clearTimeout(timer);
  }, [q]);
  
  const handleSearch = (e) => {
    e.preventDefault();
    if (searchTerm.trim()) {
      window.location.href = `/search?q=${encodeURIComponent(searchTerm)}`;
    }
  };
  
  // Mock search function
  const performSearch = (query) => {
    // In production, this would be a real search mechanism
    const mockSearchData = [
      // Devices
      {
        type: 'device',
        title: 'dev_message.erl',
        path: '/devices/message',
        category: 'Core',
        description: 'Core identity device with 14 downstream dependents, provides message field access, manipulation, and attestation handling.',
        relevance: 95
      },
      {
        type: 'device',
        title: 'dev_process.erl',
        path: '/devices/process',
        category: 'Core',
        description: 'Core orchestration module with 4 downstream dependents, 19 outgoing dependencies, routes operations to specialized devices.',
        relevance: 87
      },
      {
        type: 'device',
        title: 'dev_stack.erl',
        path: '/devices/stack',
        category: 'Core',
        description: 'Meta-device with 3 downstream dependents, manages execution of device stacks in fold or map mode, enabling complex device pipelines.',
        relevance: 82
      },
      
      // Documentation sections
      {
        type: 'section',
        title: 'Message Processing',
        path: '/architecture/message-processing',
        category: 'Architecture',
        description: 'Overview of the HyperBEAM message processing system and how messages flow through the architecture.',
        relevance: 90
      },
      {
        type: 'section',
        title: 'Device Composition',
        path: '/architecture/device-composition',
        category: 'Architecture',
        description: 'How devices can be composed to create complex behaviors from simple building blocks.',
        relevance: 85
      },
      
      // Functions
      {
        type: 'function',
        title: 'get_field/2',
        path: '/devices/message#get_field',
        device: 'dev_message.erl',
        description: 'Retrieves a value from a message by field name. The field can be specified as an atom or a binary string.',
        relevance: 93
      },
      {
        type: 'function',
        title: 'set_field/3',
        path: '/devices/message#set_field',
        device: 'dev_message.erl',
        description: 'Sets a field in a message to a specified value, returning the updated message.',
        relevance: 91
      },
      {
        type: 'function',
        title: 'merge_fields/2',
        path: '/devices/message#merge_fields',
        device: 'dev_message.erl',
        description: 'Merges a map of fields into an existing message, overwriting any duplicate keys.',
        relevance: 88
      },
      
      // Concepts
      {
        type: 'concept',
        title: 'Cryptographic Attestation',
        path: '/concepts/attestation',
        category: 'Security',
        description: 'How HyperBEAM uses cryptographic attestation to ensure message integrity and authenticity across subsystems.',
        relevance: 80
      },
      {
        type: 'concept',
        title: 'WebAssembly Integration',
        path: '/concepts/wasm-integration',
        category: 'Runtime',
        description: 'Overview of HyperBEAM\'s WebAssembly integration for secure execution of custom code.',
        relevance: 75
      }
    ];
    
    // Filter results based on query
    const normalizedQuery = query.toLowerCase();
    const filteredResults = mockSearchData.filter(result => {
      const titleMatch = result.title.toLowerCase().includes(normalizedQuery);
      const descMatch = result.description.toLowerCase().includes(normalizedQuery);
      const categoryMatch = result.category?.toLowerCase().includes(normalizedQuery);
      const deviceMatch = result.device?.toLowerCase().includes(normalizedQuery);
      
      return titleMatch || descMatch || categoryMatch || deviceMatch;
    });
    
    // Sort by relevance
    return filteredResults.sort((a, b) => b.relevance - a.relevance);
  };

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <span className="text-terminal-green">Search</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-6">Search Results</h1>
        
        <form 
          onSubmit={handleSearch} 
          className="mb-8"
        >
          <div className="flex items-center bg-[#333] rounded px-4 py-3 w-full max-w-3xl">
            <span className="text-terminal-prompt mr-2 text-lg">$</span>
            <input
              type="text"
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              placeholder="Search the documentation..."
              className="bg-transparent border-none text-terminal-command w-full outline-none font-mono text-lg"
              aria-label="Search documentation"
            />
            <button 
              type="submit" 
              className="bg-terminal-green text-terminal-bg px-4 py-1 rounded hover:bg-opacity-90 transition-colors"
            >
              Search
            </button>
          </div>
        </form>
        
        {q && (
          <div className="mb-4 text-gray-400">
            {isSearching ? (
              <div className="flex items-center">
                <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-terminal-green mr-2"></div>
                Searching for "{q}"...
              </div>
            ) : (
              <div>
                {searchResults.length === 0 ? (
                  <div className="bg-[#252526] border-l-4 border-red-500 p-4 rounded">
                    <p className="text-lg mb-2">No results found for "{q}"</p>
                    <p>Try a different search term or browse the documentation.</p>
                  </div>
                ) : (
                  <div>Found {searchResults.length} results for "{q}"</div>
                )}
              </div>
            )}
          </div>
        )}
        
        {!isSearching && searchResults.length > 0 && (
          <div className="flex flex-col gap-6">
            {/* Group results by type */}
            {['device', 'function', 'section', 'concept'].map(type => {
              const typeResults = searchResults.filter(result => result.type === type);
              if (typeResults.length === 0) return null;
              
              return (
                <div key={type} className="mb-6">
                  <h2 className="text-xl font-bold text-terminal-primary mb-4 capitalize">
                    {type === 'device' ? 'Devices' : 
                     type === 'function' ? 'Functions' : 
                     type === 'section' ? 'Documentation Sections' : 
                     'Concepts'}
                  </h2>
                  
                  <div className="grid grid-cols-1 gap-4">
                    {typeResults.map((result, i) => (
                      <div 
                        key={i} 
                        className="bg-[#252526] border border-[#333] rounded-md p-4 hover:border-terminal-green transition-all duration-200"
                      >
                        <div className="flex items-start justify-between">
                          <div>
                            <Link 
                              href={result.path} 
                              className="text-lg font-semibold text-terminal-primary hover:text-terminal-green transition-colors"
                            >
                              {result.title}
                            </Link>
                            
                            <div className="flex items-center gap-2 mt-1 mb-2">
                              {result.category && (
                                <span className="text-xs font-semibold px-2 py-0.5 rounded-full bg-[#333] text-terminal-text">
                                  {result.category}
                                </span>
                              )}
                              
                              {result.device && (
                                <span className="text-xs text-gray-400">
                                  in <Link href={`/devices/${result.device.split('.')[0].replace('dev_', '')}`} className="text-terminal-green hover:underline">
                                    {result.device}
                                  </Link>
                                </span>
                              )}
                            </div>
                            
                            <p className="text-sm opacity-80">{result.description}</p>
                          </div>
                          
                          <div className="flex flex-col items-end">
                            <div className="text-xs font-mono bg-[#333] px-2 py-1 rounded text-terminal-accent">
                              {result.relevance}% match
                            </div>
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              );
            })}
          </div>
        )}
        
        {!q && (
          <div className="bg-[#252526] border border-[#333] rounded-md p-6 max-w-3xl">
            <h2 className="text-xl font-bold text-terminal-primary mb-4">Search Tips</h2>
            
            <div className="space-y-4">
              <p>
                Use the search bar above to find information across the HyperBEAM documentation.
                Here are some tips to help you find what you're looking for:
              </p>
              
              <div className="bg-[#2d2d2d] p-4 rounded space-y-2">
                <p className="flex items-center">
                  <span className="text-terminal-green font-mono mr-2">$</span>
                  <span className="text-terminal-command font-mono">device:message</span>
                  <span className="ml-3 text-sm text-gray-400">Search for specific devices</span>
                </p>
                
                <p className="flex items-center">
                  <span className="text-terminal-green font-mono mr-2">$</span>
                  <span className="text-terminal-command font-mono">function:get_field</span>
                  <span className="ml-3 text-sm text-gray-400">Search for specific functions</span>
                </p>
                
                <p className="flex items-center">
                  <span className="text-terminal-green font-mono mr-2">$</span>
                  <span className="text-terminal-command font-mono">category:security</span>
                  <span className="ml-3 text-sm text-gray-400">Filter by category</span>
                </p>
                
                <p className="flex items-center">
                  <span className="text-terminal-green font-mono mr-2">$</span>
                  <span className="text-terminal-command font-mono">"attestation handling"</span>
                  <span className="ml-3 text-sm text-gray-400">Search for exact phrases</span>
                </p>
              </div>
              
              <p className="text-sm opacity-80">
                Search results include devices, functions, documentation sections, and concepts
                related to your query, sorted by relevance.
              </p>
            </div>
          </div>
        )}
      </div>
    </>
  );
}
