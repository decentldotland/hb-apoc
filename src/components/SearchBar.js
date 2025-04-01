"use client"

import { useEffect, useRef } from 'react';
import { useRouter } from 'next/navigation';

export default function SearchBar({ searchTerm, setSearchTerm }) {
  const router = useRouter();
  const searchInputRef = useRef(null);

  const handleSearch = (e) => {
    e.preventDefault();
    if (searchTerm.trim()) {
      // App Router navigation
      router.push(`/search?q=${encodeURIComponent(searchTerm)}`);
    }
  };

  // Terminal-style keyboard shortcut for search
  useEffect(() => {
    const handleKeyDown = (e) => {
      // Cmd+K or Ctrl+K to focus search
      if ((e.metaKey || e.ctrlKey) && e.key === 'k') {
        e.preventDefault();
        searchInputRef.current?.focus();
      }
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, []);

  return (
    <form 
      onSubmit={handleSearch} 
      className="flex items-center bg-[#333] rounded px-3 py-1.5 w-1/2 max-w-[400px]"
    >
      <span className="text-terminal-prompt mr-1.5">$</span>
      <input
        ref={searchInputRef}
        type="text"
        value={searchTerm}
        onChange={(e) => setSearchTerm(e.target.value)}
        placeholder="search docs..."
        className="bg-transparent border-none text-terminal-command w-full outline-none font-mono"
        aria-label="Search documentation"
      />
    </form>
  );
}
