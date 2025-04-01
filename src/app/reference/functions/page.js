"use client"

import { useState } from 'react';
import Link from 'next/link';
import Terminal from '../../../components/Terminal';
import { moduleCategories, functionCategories } from '../../../data/functions/categories';
import SubsystemList from './components/SubsystemList';

export default function FunctionReference() {
  const [selectedModule, setSelectedModule] = useState('all');
  const [selectedCategory, setSelectedCategory] = useState('all');
  
  // Sample terminal commands for function reference
  const initialCommands = [
    { 
      command: 'cat reference/functions.md | wc -l', 
      output: '2874 lines'
    },
    { 
      command: 'grep -c "^#### " reference/functions.md', 
      output: '44 key function definitions'
    },
    {
      command: 'grep -c "^### " reference/functions.md',
      output: '23 module definitions'
    }
  ];

  // Handle module selection change
  const handleModuleChange = (moduleId) => {
    setSelectedModule(moduleId);
  };

  // Handle category selection change
  const handleCategoryChange = (e) => {
    setSelectedCategory(e.target.value);
  };

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <Link href="/reference" className="hover:text-terminal-green">Reference</Link>
          <span>/</span>
          <span className="text-terminal-green">Functions</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Function Reference
        </h1>
        
        <p className="text-lg mb-8">
          This reference documents selected key functions in the HyperBEAM system,
          organized by module and subsystem. Each function includes parameter descriptions, 
          return values, and usage examples. These functions represent a curated subset
          of the complete API, chosen for their importance to the system's operation,
          usage frequency, and representation of key architectural patterns.
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6">
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div className="col-span-1 md:col-span-3">
              <div className="font-mono text-sm mb-4">
                <span className="text-terminal-green">$</span> 
                <span className="text-terminal-accent ml-2">find</span> 
                <span className="text-terminal-output ml-2">-name "*.erl" -type f -exec grep -l "export" {} \;</span>
              </div>
            </div>
            
            <div>
              <label className="block text-sm mb-2 text-terminal-accent">Filter by API Type</label>
              <select
                value={selectedCategory}
                onChange={handleCategoryChange}
                className="bg-[#2d2d2d] border border-[#444] rounded px-3 py-2 text-sm w-full"
              >
                <option value="all">All API Types</option>
                {functionCategories.map(cat => (
                  <option key={cat.id} value={cat.id}>
                    {cat.name} ({cat.count})
                  </option>
                ))}
              </select>
            </div>
            
            <div>
              <label className="block text-sm mb-2 text-terminal-accent">Selected Key Functions</label>
              <div className="bg-[#2d2d2d] rounded px-3 py-2 text-sm">
                <span className="text-terminal-green">44</span> key functions across <span className="text-terminal-accent">7</span> subsystems
              </div>
            </div>
          </div>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-4 gap-8 mb-12">
        <div className="lg:col-span-3">
          {/* Subsystem List */}
          <SubsystemList 
            selectedModule={selectedModule} 
            selectedCategory={selectedCategory}
          />
        </div>
        
        <div className="lg:col-span-1">
          <div className="sticky top-[80px] space-y-6">
            {/* Quick Navigation */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Subsystem Functions</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <button 
                    className={`text-left block w-full ${selectedModule === 'all' ? 'text-terminal-green' : 'text-terminal-text hover:text-terminal-green'} transition-colors`}
                    onClick={() => handleModuleChange('all')}
                  >
                    All Subsystems <span className="opacity-60">(438)</span>
                  </button>
                </li>
                {moduleCategories.map(cat => (
                  <li key={cat.id}>
                    <button 
                      className={`text-left block w-full ${selectedModule === cat.id ? 'text-terminal-green' : 'text-terminal-text hover:text-terminal-green'} transition-colors`}
                      onClick={() => handleModuleChange(cat.id)}
                    >
                      {cat.name} <span className="opacity-60">({cat.count})</span>
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
                  <Link href="/reference/glossary" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path d="M9 4.804A7.968 7.968 0 005.5 4c-1.255 0-2.443.29-3.5.804v10A7.969 7.969 0 015.5 14c1.669 0 3.218.51 4.5 1.385A7.962 7.962 0 0114.5 14c1.255 0 2.443.29 3.5.804v-10A7.968 7.968 0 0014.5 4c-1.255 0-2.443.29-3.5.804V12a1 1 0 11-2 0V4.804z" />
                    </svg>
                    Conceptual Glossary
                  </Link>
                </li>
                <li>
                  <Link href="/reference/dependency-visualization" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path d="M2 11a1 1 0 011-1h2a1 1 0 011 1v5a1 1 0 01-1 1H3a1 1 0 01-1-1v-5zM8 7a1 1 0 011-1h2a1 1 0 011 1v9a1 1 0 01-1 1H9a1 1 0 01-1-1V7zM14 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1h-2a1 1 0 01-1-1V4z" />
                    </svg>
                    Dependency Visualization
                  </Link>
                </li>
                <li>
                  <Link href="/core" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM4.332 8.027a6.012 6.012 0 011.912-2.706C6.512 5.73 6.974 6 7.5 6A1.5 1.5 0 019 7.5V8a2 2 0 004 0 2 2 0 011.523-1.943A5.977 5.977 0 0116 10c0 .34-.028.675-.083 1H15a2 2 0 00-2 2v2.197A5.973 5.973 0 0110 16v-2a2 2 0 00-2-2 2 2 0 01-2-2 2 2 0 00-1.668-1.973z" clipRule="evenodd" />
                    </svg>
                    Core Infrastructure
                  </Link>
                </li>
              </ul>
            </div>
            
            {/* Function Statistics */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Selected Function Statistics</h3>
              <ul className="space-y-3 text-sm">
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">44 Key Functions</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Selected across 7 subsystems
                    </p>
                  </div>
                </li>
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">15 Core Infrastructure Functions</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Fundamental system operations
                    </p>
                  </div>
                </li>
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">29 Subsystem Functions</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Specialized domain operations
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
