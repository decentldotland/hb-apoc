"use client"

import Link from 'next/link';
import { useState } from 'react';
import SearchBar from './SearchBar';

export default function Header({ toggleSidebar }) {
  const [searchTerm, setSearchTerm] = useState('');

  return (
    <header className="bg-[#252526] border-b border-[#333] fixed top-0 left-0 right-0 z-50 w-full">
      <div className="flex items-center justify-between px-4 md:px-6 h-[60px]">
        <div className="flex items-center gap-3">
          <Link href="/" className="font-bold text-xl text-terminal-green">
            hyperBEAM Apocryphal_
          </Link>
          <div className="text-xs opacity-80 hidden md:block">
            A public good initiative by <a href="https://x.com/EntityC_HQ?t=Dwf8Orz4VCnzBZ5O1TOghg&s=08" target="_blank" rel="noopener noreferrer" className="underline hover:text-terminal-green">EntityC</a> and <a href="https://x.com/decentlandlabs?t=sV-MLAX0S8MwQ4LzOB9zGA&s=08" target="_blank" rel="noopener noreferrer" className="underline hover:text-terminal-green">Decent Land Labs</a>
          </div>
        </div>

        <SearchBar 
          searchTerm={searchTerm} 
          setSearchTerm={setSearchTerm} 
        />

        <button 
          className="md:hidden text-2xl"
          onClick={toggleSidebar}
          aria-label="Toggle navigation menu"
        >
          ☰
        </button>
      </div>

      <div className="absolute bottom-0 left-0 w-full bg-[#333] text-[10px] py-0.5 px-2 text-center">
        Documentation current with hyperBEAM main branch as of 27.03.2025 | Generated with AI assistance, curated by humans, errors to be expected | Not vetted/reviewed in any way or form by the core team
      </div>
    </header>
  );
}
