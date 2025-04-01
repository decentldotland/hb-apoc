'use client';

import React, { useState } from 'react';
import Link from 'next/link';
import CodeBlock from '../components/CodeBlock';
import { FaChevronRight, FaChevronDown } from 'react-icons/fa';
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';
import rehypeRaw from 'rehype-raw';
import Mermaid from '../components/Mermaid';

/**
 * A template component for displaying device documentation
 * Uses the structured data parsed from device analysis files
 */
const DeviceTemplate = ({ device }) => {
  const [expandedSections, setExpandedSections] = useState({
    overview: true,
    fullContent: false
  });

  // Helper to toggle section expansion
  const toggleSection = (section) => {
    setExpandedSections({
      ...expandedSections,
      [section]: !expandedSections[section]
    });
  };

  // Get category display name and icon
  const getCategoryDetails = (categoryId) => {
    const categories = {
      core: {
        name: 'Core Device',
        icon: (
          <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4" />
          </svg>
        )
      },
      scheduler: {
        name: 'Scheduler Device',
        icon: (
          <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z" />
          </svg>
        )
      },
      runtime: {
        name: 'Runtime Device',
        icon: (
          <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 10V3L4 14h7v7l9-11h-7z" />
          </svg>
        )
      },
      security: {
        name: 'Security Device',
        icon: (
          <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z" />
          </svg>
        )
      },
      payment: {
        name: 'Payment Device',
        icon: (
          <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2.08.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
        )
      },
      routing: {
        name: 'Routing Device',
        icon: (
          <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 20l-5.447-2.724A1 1 0 013 16.382V5.618a1 1 0 011.447-.894L9 7m0 13l6-3m-6 3V7m6 10l4.553 2.276A1 1 0 0021 18.382V7.618a1 1 0 00-.553-.894L15 4m0 13V4m0 0L9 7" />
          </svg>
        )
      },
      utility: {
        name: 'Utility Device',
        icon: (
          <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M11 4a2 2 0 114 0v1a1 1 0 001 1h3a1 1 0 011 1v3a1 1 0 01-1 1h-1a2 2 0 100 4h1a1 1 0 011 1v3a1 1 0 01-1 1h-3a1 1 0 01-1-1v-1a2 2 0 10-4 0v1a1 1 0 01-1 1H7a1 1 0 01-1-1v-3a1 1 0 00-1-1H4a2 2 0 110-4h1a1 1 0 001-1V7a1 1 0 011-1h3a1 1 0 001-1V4z" />
          </svg>
        )
      }
    };

    return categories[categoryId] || categories.utility;
  };

  const categoryDetails = getCategoryDetails(device.category);

  // Section component for collapsible sections
  const Section = ({ title, id, children }) => (
    <div className="mb-6">
      <div 
        className="flex items-center cursor-pointer bg-[#252526] border-b border-[#333] p-3 mb-3"
        onClick={() => toggleSection(id)}
      >
        {expandedSections[id] ? (
          <FaChevronDown className="text-terminal-accent mr-2" />
        ) : (
          <FaChevronRight className="text-terminal-accent mr-2" />
        )}
        <h2 className="text-xl font-mono text-terminal-green">{title}</h2>
      </div>
      
      {expandedSections[id] && (
        <div className="px-2">
          {children}
        </div>
      )}
    </div>
  );

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
    h3: ({ node, children, ...props }) => (
      <h3 className="text-lg font-semibold text-terminal-accent mt-4 mb-2" {...props}>{children}</h3>
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
    <div className="max-w-full">
      {/* Device Header */}
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <Link href="/devices" className="hover:text-terminal-green">Devices</Link>
          <span>/</span>
          <Link 
            href={`/devices/categories/${device.category}`} 
            className="hover:text-terminal-green"
          >
            {categoryDetails.name}s
          </Link>
          <span>/</span>
          <span className="text-terminal-green">{device.name}</span>
        </div>
        
        <div className="flex items-center mb-4">
          <div className="text-terminal-green mr-4">
            {categoryDetails.icon}
          </div>
          <div>
            <h1 className="text-3xl font-bold text-terminal-primary">
              {device.name}
            </h1>
            <div className="text-sm font-mono text-terminal-accent mt-1">
              {device.filename}
            </div>
          </div>
        </div>
      </div>
      
      {/* Category Badge */}
      <div className="mb-6">
        <span className="bg-[#252526] border border-[#333] px-3 py-1 rounded-full text-xs text-terminal-accent">
          {categoryDetails.name}
        </span>
      </div>

      {/* Overview Section - Always expanded */}
      <Section title="Overview" id="overview">
        {renderMarkdown(device.sections.overview)}
      </Section>
      
      {/* Full Documentation Section */}
      {device.originalContent && (
        <Section title="Full Documentation" id="fullContent">
          <div className="text-xs opacity-70 italic mb-3">
            This is the complete original documentation with all content preserved in context.
          </div>
          {renderMarkdown(device.originalContent)}
        </Section>
      )}
      
      {/* Related Devices */}
      <div className="mt-8 bg-[#252526] border border-[#333] rounded-md p-4">
        <h2 className="text-lg font-mono text-terminal-green mb-3">
          Related Devices
        </h2>
        <p className="text-sm mb-3">
          Explore other {categoryDetails.name.toLowerCase()}s in HyperBEAM:
        </p>
        
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-2">
          {device.relatedDevices ? (
            // Use the dynamically provided related devices
            device.relatedDevices.map((relatedDevice) => (
              <Link 
                href={`/devices/${relatedDevice.id || relatedDevice.name.replace('.erl', '')}`}
                key={relatedDevice.id || relatedDevice.name}
                className="text-terminal-green hover:underline text-sm"
              >
                {relatedDevice.name}
              </Link>
            ))
          ) : (
            // Fallback to hard-coded related devices if none provided
            <>
              <Link href="/devices/dev_message" className="text-terminal-green hover:underline text-sm">
                dev_message.erl
              </Link>
              <Link href="/devices/dev_process" className="text-terminal-green hover:underline text-sm">
                dev_process.erl
              </Link>
              <Link href="/devices/dev_scheduler" className="text-terminal-green hover:underline text-sm">
                dev_scheduler.erl
              </Link>
            </>
          )}
        </div>
      </div>
    </div>
  );
};

export default DeviceTemplate;
