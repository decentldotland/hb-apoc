"use client"

import { useState, useEffect } from 'react';
import Link from 'next/link';
import Terminal from '../../../components/Terminal';
import allGlossaryItems, { glossaryCategories } from '../../../data/glossary/index';

export default function ConceptualGlossary() {
  // State for filtered glossary items
  const [filter, setFilter] = useState('');
  const [activeLetter, setActiveLetter] = useState('A');
  
  // Terminal commands for glossary
  const initialCommands = [
    { 
      command: 'cat reference/glossary.md | wc -l', 
      output: '2874 lines'
    },
    { 
      command: 'grep -c "^## " reference/glossary.md', 
      output: '3 sections'
    },
    { 
      command: 'grep -c "^### " reference/glossary.md', 
      output: '7 categories'
    },
    { 
      command: 'grep -c "^#### " reference/glossary.md', 
      output: '59 term definitions'
    }
  ];

  // Glossary items imported from structured data
  const glossaryItems = allGlossaryItems;

  // Get all available first letters from the glossary items
  const alphabet = [...new Set(glossaryItems.map(item => item.letter))].sort();

  // Filter glossary items based on search and active letter
  const filteredItems = glossaryItems.filter(item => {
    const matchesFilter = filter === '' || 
      item.term.toLowerCase().includes(filter.toLowerCase()) || 
      item.definition.toLowerCase().includes(filter.toLowerCase()) ||
      item.category.toLowerCase().includes(filter.toLowerCase());
    
    const matchesLetter = activeLetter === '' || item.letter === activeLetter;
    
    return matchesFilter && matchesLetter;
  });

  // Group items by letter for the filtered items
  const groupedItems = filteredItems.reduce((acc, item) => {
    if (!acc[item.letter]) {
      acc[item.letter] = [];
    }
    acc[item.letter].push(item);
    return acc;
  }, {});

  // Handle letter selection
  const handleLetterClick = (letter) => {
    setActiveLetter(letter);
    setFilter('');
  };

  // Handle search filter
  const handleFilterChange = (e) => {
    setFilter(e.target.value);
    if (e.target.value !== '') {
      setActiveLetter('');
    } else {
      setActiveLetter('A');
    }
  };

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <Link href="/reference" className="hover:text-terminal-green">Reference</Link>
          <span>/</span>
          <span className="text-terminal-green">Glossary</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Conceptual Glossary
        </h1>
        
        <p className="text-lg mb-8">
          This glossary defines key terms, concepts, and design patterns used throughout the HyperBEAM system.
          Understanding these concepts is essential for working with the system effectively.
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6 flex flex-col sm:flex-row items-start sm:items-center justify-between gap-4">
          <div className="font-mono text-sm">
            <span className="text-terminal-green">$</span> 
            <span className="text-terminal-accent ml-2">grep</span> 
            <span className="text-terminal-output ml-2">-i "concept" reference/glossary.md</span>
          </div>
          <div className="w-full sm:w-auto">
            <input
              type="text"
              placeholder="Search glossary..."
              value={filter}
              onChange={handleFilterChange}
              className="bg-[#2d2d2d] border border-[#444] rounded px-3 py-1 text-sm w-full"
            />
          </div>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-4 gap-8 mb-12">
        <div className="lg:col-span-3">
          {/* Alphabet Navigation */}
          <div className="flex flex-wrap gap-2 mb-6">
            {alphabet.map(letter => (
              <button
                key={letter}
                className={`w-8 h-8 flex items-center justify-center rounded-md text-sm
                  ${activeLetter === letter ? 
                    'bg-terminal-green text-black font-semibold' : 
                    'bg-[#2d2d2d] text-terminal-text hover:bg-[#3a3a3a]'
                  }`}
                onClick={() => handleLetterClick(letter)}
              >
                {letter}
              </button>
            ))}
            {filter && (
              <div className="ml-2 py-1 px-3 bg-[#2d2d2d] rounded-md text-sm flex items-center">
                <span className="text-terminal-accent mr-2">Search:</span>
                <span className="text-terminal-green">{filter}</span>
                <button 
                  className="ml-2 text-terminal-text hover:text-terminal-green"
                  onClick={() => setFilter('')}
                >
                  ✕
                </button>
              </div>
            )}
          </div>
          
          {/* Glossary Content */}
          <div className="space-y-8">
            {Object.keys(groupedItems).sort().map(letter => (
              <div key={letter} id={`letter-${letter}`}>
                <h2 className="text-2xl font-bold text-terminal-primary mb-4 border-b border-[#444] pb-2">
                  {letter}
                </h2>
                <div className="space-y-6">
                  {groupedItems[letter].map((item, idx) => (
                    <div key={idx} className="bg-[#252526] border border-[#333] rounded-md p-5">
                      <h3 className="text-xl font-mono font-semibold text-terminal-green mb-2">
                        {item.term}
                      </h3>
                      <div className="mb-4">
                        <span className="inline-block bg-[#3a3a3a] text-xs rounded-full px-2 py-1 text-terminal-accent">
                          {item.category}
                        </span>
                      </div>
                      <p className="text-sm mb-4">
                        {item.definition}
                      </p>
                      
                      {item.related && item.related.length > 0 && (
                        <div>
                          <h4 className="text-sm font-semibold text-terminal-accent mb-2">Related Concepts:</h4>
                          <div className="flex flex-wrap gap-2">
                            {item.related.map((related, i) => (
                              <span 
                                key={i} 
                                className="text-xs bg-[#2d2d2d] px-2 py-1 rounded-md hover:bg-[#3a3a3a] cursor-pointer"
                                onClick={() => setFilter(related)}
                              >
                                {related}
                              </span>
                            ))}
                          </div>
                        </div>
                      )}
                    </div>
                  ))}
                </div>
              </div>
            ))}
            
            {Object.keys(groupedItems).length === 0 && (
              <div className="bg-[#252526] border border-[#333] rounded-md p-6 text-center">
                <p className="text-terminal-accent">No matching glossary entries found.</p>
                <button 
                  className="mt-3 text-terminal-green hover:underline"
                  onClick={() => {setFilter(''); setActiveLetter('A');}}
                >
                  Reset filters
                </button>
              </div>
            )}
          </div>
        </div>
        
        <div className="lg:col-span-1">
          <div className="sticky top-[80px] space-y-6">
            {/* Quick Navigation */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Glossary Categories</h3>
              <ul className="space-y-2 text-sm">
                {glossaryCategories.map((category, idx) => (
                  <li key={idx}>
                    <button 
                      className="text-terminal-text hover:text-terminal-green transition-colors block w-full text-left"
                      onClick={() => setFilter(category)}
                    >
                      {category}
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
                  <Link href="/reference/functions" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M12.316 3.051a1 1 0 01.633 1.265l-4 12a1 1 0 11-1.898-.632l4-12a1 1 0 011.265-.633zM5.707 6.293a1 1 0 010 1.414L3.414 10l2.293 2.293a1 1 0 11-1.414 1.414l-3-3a1 1 0 010-1.414l3-3a1 1 0 011.414 0zm8.586 0a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 11-1.414-1.414L16.586 10l-2.293-2.293a1 1 0 010-1.414z" clipRule="evenodd" />
                    </svg>
                    Function Reference
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
                  <Link href="/architecture" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M3 5a2 2 0 012-2h10a2 2 0 012 2v10a2 2 0 01-2 2H5a2 2 0 01-2-2V5zm11 1H6v8l4-2 4 2V6z" clipRule="evenodd" />
                    </svg>
                    Architecture Overview
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
            
            {/* Glossary Notes */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">About the Glossary</h3>
              <p className="text-sm mb-3 opacity-80">
                This glossary contains {glossaryItems.length} terms and concepts that form the foundation of HyperBEAM's architecture and implementation.
              </p>
              <p className="text-sm opacity-80">
                Concepts are cross-referenced to help understand relationships between different parts of the system.
                Click on related concepts to explore connections.
              </p>
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
