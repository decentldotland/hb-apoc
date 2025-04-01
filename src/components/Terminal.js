"use client"

import { useEffect, useRef } from 'react';
import { useRouter } from 'next/navigation';

// Simplified Terminal component that doesn't rely on xterm
export default function Terminal({ height = 400, initialCommands = [] }) {
  const terminalRef = useRef(null);
  const router = useRouter();

  // Mock terminal with static content for SSR compatibility
  useEffect(() => {
    if (typeof window !== 'undefined' && terminalRef.current) {
      // Clear any previous content
      terminalRef.current.innerHTML = '';
      
      // Create terminal container
      const terminalOutput = document.createElement('div');
      terminalOutput.className = 'terminal-output p-3';
      
      // Welcome message
      appendLine(terminalOutput, '# Welcome to HyperBEAM Documentation Terminal', 'text-terminal-green');
      appendLine(terminalOutput, '');
      
      // Handle initial commands
      if (initialCommands && initialCommands.length > 0) {
        initialCommands.forEach(cmd => {
          appendLine(terminalOutput, `$ ${cmd.command}`, 'text-terminal-accent');
          appendLine(terminalOutput, cmd.output);
          appendLine(terminalOutput, '');
        });
      } else {
        // Default help content
        appendLine(terminalOutput, '$ help', 'text-terminal-accent');
        appendLine(terminalOutput, 'Available commands:');
        appendLine(terminalOutput, '  - ls: List available documentation sections', 'text-yellow-500');
        appendLine(terminalOutput, '  - cd <section>: Navigate to a section', 'text-yellow-500');
        appendLine(terminalOutput, '  - cat <file>: View documentation file', 'text-yellow-500');
        appendLine(terminalOutput, '  - find <keyword>: Search documentation', 'text-yellow-500');
        appendLine(terminalOutput, '  - clear: Clear terminal screen', 'text-yellow-500');
        appendLine(terminalOutput, '  - help: Show this help message', 'text-yellow-500');
      }
      
      // Add prompt
      appendLine(terminalOutput, '');
      appendPrompt(terminalOutput);
      
      // Add to DOM
      terminalRef.current.appendChild(terminalOutput);
    }
  }, [initialCommands]);

  // Helper function to add a line to the mock terminal
  function appendLine(container, text, className = '') {
    const line = document.createElement('div');
    line.className = `whitespace-pre-wrap ${className}`;
    line.textContent = text;
    container.appendChild(line);
  }
  
  // Helper function to add a prompt
  function appendPrompt(container) {
    const prompt = document.createElement('div');
    prompt.className = 'flex items-center';
    prompt.innerHTML = '<span class="text-terminal-accent mr-2">hyperbeam-docs $</span><span class="border-r border-white animate-pulse">&nbsp;</span>';
    container.appendChild(prompt);
  }

  return (
    <div 
      ref={terminalRef} 
      className="terminal bg-[#1e1e1e] text-gray-200 font-mono text-sm rounded-md overflow-auto" 
      style={{ height: `${height}px` }}
    />
  );
}
