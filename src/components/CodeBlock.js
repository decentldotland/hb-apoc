import React from 'react';
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import { oneDark } from 'react-syntax-highlighter/dist/cjs/styles/prism';

/**
 * CodeBlock component for syntax-highlighted code display
 * 
 * @param {string} language - The programming language
 * @param {string} value - The code to display
 */
const CodeBlock = ({ language, value }) => {
  return (
    <div className="rounded-md overflow-hidden mb-4 text-sm">
      <div className="bg-[#282c34] px-4 py-1 text-xs flex justify-between items-center border-b border-[#444]">
        <span className="text-terminal-accent font-mono">{language}</span>
        <span className="text-[#888] text-xs">HyperBEAM</span>
      </div>
      <SyntaxHighlighter
        language={language}
        style={oneDark}
        customStyle={{ 
          margin: 0,
          padding: '1rem',
          borderRadius: '0 0 4px 4px',
          fontSize: '0.85rem',
          backgroundColor: '#282c34'
        }}
      >
        {value}
      </SyntaxHighlighter>
    </div>
  );
};

export default CodeBlock;
