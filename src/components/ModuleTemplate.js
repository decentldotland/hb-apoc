'use client';

import React, { useState } from 'react';
import Link from 'next/link';
import CodeBlock from './CodeBlock';
import { FaChevronRight, FaChevronDown } from 'react-icons/fa';
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';
import rehypeRaw from 'rehype-raw';
import Mermaid from './Mermaid';

/**
 * A template component for displaying subsystem module documentation
 * Uses the structured data parsed from module analysis files with full markdown content
 */
const ModuleTemplate = ({ module, subsystem }) => {
  const [expandedContent, setExpandedContent] = useState(true);

  // Toggle content expansion
  const toggleContent = () => {
    setExpandedContent(!expandedContent);
  };

  // Custom components for ReactMarkdown
  const components = {
    code: ({ node, inline, className, children, ...props }) => {
      const match = /language-(\w+)/.exec(className || '');
      
      // Handle mermaid diagrams
      if (!inline && match && match[1] === 'mermaid') {
        return <Mermaid chart={String(children).replace(/\n$/, '')} />;
      }
      
      // Handle regular code blocks
      return !inline && match ? (
        <CodeBlock
          language={match[1]}
          value={String(children).replace(/\n$/, '')}
          {...props}
        />
      ) : (
        <code className={className ? className : "bg-[#333] px-1 py-0.5 rounded font-mono text-sm"} {...props}>
          {children}
        </code>
      );
    },
    h1: ({ node, children, ...props }) => (
      <h1 className="text-2xl font-bold text-terminal-primary mt-6 mb-3" {...props}>{children}</h1>
    ),
    h2: ({ node, children, ...props }) => (
      <h2 className="text-xl font-semibold text-terminal-accent mt-5 mb-3" {...props}>{children}</h2>
    ),
    h3: ({ node, children, ...props }) => (
      <h3 className="text-lg font-semibold text-terminal-green mt-4 mb-2" {...props}>{children}</h3>
    ),
    p: ({ node, children, ...props }) => (
      <p className="mb-3 text-sm leading-relaxed" {...props}>{children}</p>
    ),
    ul: ({ node, children, ...props }) => (
      <ul className="list-disc pl-5 mb-4 text-sm space-y-1" {...props}>{children}</ul>
    ),
    ol: ({ node, children, ...props }) => (
      <ol className="list-decimal pl-5 mb-4 text-sm space-y-1" {...props}>{children}</ol>
    ),
    li: ({ node, children, ...props }) => (
      <li className="mb-1" {...props}>{children}</li>
    ),
    a: ({ node, children, href, ...props }) => (
      <a 
        href={href} 
        className="text-terminal-green hover:underline"
        target={href.startsWith('http') ? "_blank" : "_self"}
        rel={href.startsWith('http') ? "noopener noreferrer" : ""}
        {...props}
      >
        {children}
      </a>
    ),
    table: ({ node, children, ...props }) => (
      <div className="overflow-x-auto mb-4">
        <table className="min-w-full bg-[#252526] border border-[#333] text-sm" {...props}>
          {children}
        </table>
      </div>
    ),
    thead: ({ node, children, ...props }) => (
      <thead className="bg-[#333]" {...props}>
        {children}
      </thead>
    ),
    tbody: ({ node, children, ...props }) => (
      <tbody className="divide-y divide-[#333]" {...props}>
        {children}
      </tbody>
    ),
    tr: ({ node, children, ...props }) => (
      <tr className="hover:bg-[#2d2d2d]" {...props}>
        {children}
      </tr>
    ),
    th: ({ node, children, ...props }) => (
      <th className="px-4 py-2 text-left text-terminal-accent font-medium" {...props}>
        {children}
      </th>
    ),
    td: ({ node, children, ...props }) => (
      <td className="px-4 py-2 border-t border-[#333]" {...props}>
        {children}
      </td>
    ),
    blockquote: ({ node, children, ...props }) => (
      <blockquote className="border-l-4 border-terminal-accent pl-4 py-2 mb-4 bg-[#2d2d2d] italic" {...props}>
        {children}
      </blockquote>
    ),
    hr: ({ node, ...props }) => (
      <hr className="border-[#333] my-4" {...props} />
    )
  };

  // Render markdown content
  const renderMarkdown = (content) => {
    if (!content) return null;
    return (
      <ReactMarkdown 
        components={components}
        remarkPlugins={[remarkGfm]}
        rehypePlugins={[rehypeRaw]}
      >
        {content}
      </ReactMarkdown>
    );
  };

  return (
    <div className="max-w-4xl mx-auto my-8">
      {/* Module Header */}
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <Link href="/subsystems" className="hover:text-terminal-green">Subsystems</Link>
          <span>/</span>
          <Link 
            href={`/subsystems/${subsystem}`} 
            className="hover:text-terminal-green"
          >
            {subsystem.charAt(0).toUpperCase() + subsystem.slice(1)}
          </Link>
          <span>/</span>
          <span className="text-terminal-green">{module.name.replace(/`/g, '')}</span>
        </div>
        
        <div className="flex items-center mb-4">
          <div>
            <h1 className="text-3xl font-bold text-terminal-primary">
              {module.name}
            </h1>
            <div className="text-sm font-mono text-terminal-accent mt-1">
              {module.filename}
            </div>
          </div>
        </div>
        
        <div className="text-sm mb-6">
          {module.description}
        </div>
      </div>
      
      {/* Content Toggle */}
      <div 
        className="flex items-center cursor-pointer bg-[#252526] border-b border-[#333] p-3 mb-3"
        onClick={toggleContent}
      >
        {expandedContent ? (
          <FaChevronDown className="text-terminal-accent mr-2" />
        ) : (
          <FaChevronRight className="text-terminal-accent mr-2" />
        )}
        <h2 className="text-xl font-mono text-terminal-green">Documentation</h2>
      </div>
      
      {/* Full Markdown Content */}
      {expandedContent && (
        <div className="bg-[#252526] border border-[#333] rounded-md p-6">
          {renderMarkdown(module.content)}
        </div>
      )}
    </div>
  );
};

export default ModuleTemplate;
