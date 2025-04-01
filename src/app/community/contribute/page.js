"use client"

import { useState } from 'react';
import Link from 'next/link';
import Terminal from '../../../components/Terminal';

export default function Contribute() {
  const [copied, setCopied] = useState(false);
  
  // Email template text for contributions
  const emailTemplate = `Subject: [Contribution] - Brief description of your contribution

## Contribution Type
<!-- e.g., Code, Documentation, Bug Fix, Feature, etc. -->

## Description
<!-- Detailed description of your contribution -->

## Motivation
<!-- Why are you making this contribution? What problem does it solve? -->

## Implementation Details
<!-- For code contributions, provide technical details on your implementation -->

## Testing
<!-- How has this been tested? What tests should be run? -->

## Additional Context
<!-- Any other information that might be helpful -->

## About You
<!-- Your name, contact information, and any relevant background (optional) -->`;

  // Handle copy to clipboard
  const copyTemplate = () => {
    navigator.clipboard.writeText(emailTemplate);
    setCopied(true);
    setTimeout(() => setCopied(false), 2000);
  };

  // Terminal commands for contribution guidelines
  const initialCommands = [
    { 
      command: 'cat CONTRIBUTING.md | head -10', 
      output: '# Contributing to HyperBEAM\n\nThank you for your interest in contributing to HyperBEAM! This document provides guidelines and instructions for contributing to the project.\n\n## Code of Conduct\n\nWe expect all contributors to follow our [Code of Conduct](CODE_OF_CONDUCT.md). Please read it before participating.'
    }
  ];

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <Link href="/community" className="hover:text-terminal-green">Community</Link>
          <span>/</span>
          <span className="text-terminal-green">Contribution Guidelines</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Contribution Guidelines
        </h1>
        
        <p className="text-lg mb-8">
          Learn how to contribute to HyperBEAM's development, documentation, and community resources.
          We welcome contributions from developers of all experience levels.
        </p>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-4 gap-8 mb-12">
        <div className="lg:col-span-3">
          <div className="space-y-8">
            {/* Email Contribution Method */}
            <section>
              <h2 className="text-2xl font-bold text-terminal-primary mb-4 border-b border-[#444] pb-2">
                How to Submit Contributions
              </h2>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-6 mt-4">
                <div className="bg-[#2d2d2d] rounded-md p-6 text-center mb-6">
                  <svg xmlns="http://www.w3.org/2000/svg" className="h-12 w-12 mx-auto mb-4 text-terminal-accent" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z" />
                  </svg>
                  <h3 className="text-lg font-mono font-semibold text-terminal-green mb-3">
                    Email Your Contributions to:
                  </h3>
                  <a 
                    href="mailto:contribute@entityC.com" 
                    className="text-xl font-mono font-bold text-terminal-accent hover:underline block mb-4"
                  >
                    contribute@entityC.com
                  </a>
                  <p className="text-sm mb-4 max-w-lg mx-auto">
                    Please include as much detail as possible about your contribution. Use the template below to structure your email.
                  </p>
                  <a 
                    href="mailto:contribute@entityC.com?subject=[Contribution] - &body=## Contribution Type%0A<!-- e.g., Code, Documentation, Bug Fix, Feature, etc. -->%0A%0A## Description%0A<!-- Detailed description of your contribution -->%0A%0A## Motivation%0A<!-- Why are you making this contribution? What problem does it solve? -->%0A%0A## Implementation Details%0A<!-- For code contributions, provide technical details on your implementation -->%0A%0A## Testing%0A<!-- How has this been tested? What tests should be run? -->%0A%0A## Additional Context%0A<!-- Any other information that might be helpful -->%0A%0A## About You%0A<!-- Your name, contact information, and any relevant background (optional) -->"
                    className="bg-terminal-accent text-black font-semibold py-2 px-6 rounded-md hover:bg-terminal-accent/90 transition-colors inline-block"
                  >
                    Open Email Client
                  </a>
                </div>
                
                <div className="mb-6">
                  <div className="flex justify-between items-center mb-2">
                    <h3 className="text-lg font-semibold text-terminal-green">Template for Your Email</h3>
                    <button
                      onClick={copyTemplate}
                      className="bg-[#333] hover:bg-[#444] text-xs py-1 px-3 rounded flex items-center transition-colors"
                    >
                      {copied ? (
                        <>
                          <svg xmlns="http://www.w3.org/2000/svg" className="h-3.5 w-3.5 mr-1 text-terminal-green" viewBox="0 0 20 20" fill="currentColor">
                            <path fillRule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clipRule="evenodd" />
                          </svg>
                          Copied!
                        </>
                      ) : (
                        <>
                          <svg xmlns="http://www.w3.org/2000/svg" className="h-3.5 w-3.5 mr-1" viewBox="0 0 20 20" fill="currentColor">
                            <path d="M8 2a1 1 0 000 2h2a1 1 0 100-2H8z" />
                            <path d="M3 5a2 2 0 012-2 3 3 0 003 3h2a3 3 0 003-3 2 2 0 012 2v6h-4.586l1.293-1.293a1 1 0 00-1.414-1.414l-3 3a1 1 0 000 1.414l3 3a1 1 0 001.414-1.414L10.414 13H15v3a2 2 0 01-2 2H5a2 2 0 01-2-2V5zM15 11h2a1 1 0 110 2h-2v-2z" />
                          </svg>
                          Copy Template
                        </>
                      )}
                    </button>
                  </div>
                  <pre className="bg-[#1e1e1e] p-4 rounded-md text-xs font-mono whitespace-pre-wrap overflow-x-auto">
                    {emailTemplate}
                  </pre>
                </div>
              </div>
            </section>
            
            {/* Ways to contribute section */}
            <section>
              <h2 className="text-2xl font-bold text-terminal-primary mb-4 border-b border-[#444] pb-2">
                Ways to Contribute
              </h2>
              
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mt-6">
                <div className="bg-[#252526] border border-[#333] rounded-md p-6">
                  <h3 className="text-lg font-mono font-semibold text-terminal-green mb-2">
                    Code Contributions
                  </h3>
                  <p className="text-sm opacity-80 mb-4">
                    Contribute to HyperBEAM's core implementation, subsystems, or device ecosystem by submitting patches, enhancements, or new features.
                  </p>
                  <ul className="text-xs space-y-1 opacity-80">
                    <li>• Fix bugs and issues</li>
                    <li>• Implement new features</li>
                    <li>• Optimize performance</li>
                    <li>• Add tests and examples</li>
                  </ul>
                </div>
                
                <div className="bg-[#252526] border border-[#333] rounded-md p-6">
                  <h3 className="text-lg font-mono font-semibold text-terminal-green mb-2">
                    Documentation
                  </h3>
                  <p className="text-sm opacity-80 mb-4">
                    Improve HyperBEAM's documentation with corrections, clarifications, examples, or tutorials to help users understand the system better.
                  </p>
                  <ul className="text-xs space-y-1 opacity-80">
                    <li>• Fix typos and errors</li>
                    <li>• Improve explanations</li>
                    <li>• Add examples and diagrams</li>
                    <li>• Write tutorials and guides</li>
                  </ul>
                </div>
                
                <div className="bg-[#252526] border border-[#333] rounded-md p-6">
                  <h3 className="text-lg font-mono font-semibold text-terminal-green mb-2">
                    Issue Reporting
                  </h3>
                  <p className="text-sm opacity-80 mb-4">
                    Help identify bugs, inconsistencies, or areas for improvement by submitting detailed issue reports with steps to reproduce.
                  </p>
                  <ul className="text-xs space-y-1 opacity-80">
                    <li>• Report bugs</li>
                    <li>• Suggest enhancements</li>
                    <li>• Identify documentation gaps</li>
                    <li>• Verify fixed issues</li>
                  </ul>
                </div>
                
                <div className="bg-[#252526] border border-[#333] rounded-md p-6">
                  <h3 className="text-lg font-mono font-semibold text-terminal-green mb-2">
                    Community Support
                  </h3>
                  <p className="text-sm opacity-80 mb-4">
                    Engage with the community by answering questions, sharing knowledge, and helping others understand HyperBEAM.
                  </p>
                  <ul className="text-xs space-y-1 opacity-80">
                    <li>• Answer questions</li>
                    <li>• Share your experience</li>
                    <li>• Help new contributors</li>
                    <li>• Participate in discussions</li>
                  </ul>
                </div>
              </div>
            </section>
            
            {/* Getting Started section */}
            <section>
              <h2 className="text-2xl font-bold text-terminal-primary mb-4 border-b border-[#444] pb-2">
                Getting Started
              </h2>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-6 mt-4">
                <ol className="space-y-6">
                  <li className="flex">
                    <div className="bg-terminal-green text-black rounded-full w-6 h-6 flex items-center justify-center font-semibold text-sm mr-3 mt-0.5 flex-shrink-0">1</div>
                    <div>
                      <h3 className="text-md font-semibold text-terminal-green mb-2">
                        Set Up Your Development Environment
                      </h3>
                      <p className="text-sm mb-3">
                        Follow our development environment setup guide to get your local environment ready for HyperBEAM development.
                      </p>
                    </div>
                  </li>
                  
                  <li className="flex">
                    <div className="bg-terminal-green text-black rounded-full w-6 h-6 flex items-center justify-center font-semibold text-sm mr-3 mt-0.5 flex-shrink-0">2</div>
                    <div>
                      <h3 className="text-md font-semibold text-terminal-green mb-2">
                        Find an Issue to Work On
                      </h3>
                      <p className="text-sm mb-3">
                        Look for issues labeled "good first issue" or "help wanted" in our issue tracker. 
                        These are specifically chosen to be accessible to new contributors.
                      </p>
                    </div>
                  </li>
                  
                  <li className="flex">
                    <div className="bg-terminal-green text-black rounded-full w-6 h-6 flex items-center justify-center font-semibold text-sm mr-3 mt-0.5 flex-shrink-0">3</div>
                    <div>
                      <h3 className="text-md font-semibold text-terminal-green mb-2">
                        Make Your Changes and Test
                      </h3>
                      <p className="text-sm mb-3">
                        Implement your changes following our coding standards and thoroughly test them to ensure they work as expected.
                      </p>
                    </div>
                  </li>
                  
                  <li className="flex">
                    <div className="bg-terminal-green text-black rounded-full w-6 h-6 flex items-center justify-center font-semibold text-sm mr-3 mt-0.5 flex-shrink-0">4</div>
                    <div>
                      <h3 className="text-md font-semibold text-terminal-green mb-2">
                        Submit Your Contribution
                      </h3>
                      <p className="text-sm mb-3">
                        Email your contribution to contribute@entityC.com using the template above, including all relevant details about your changes.
                      </p>
                    </div>
                  </li>
                </ol>
              </div>
            </section>
            
            {/* Coding Standards section */}
            <section>
              <h2 className="text-2xl font-bold text-terminal-primary mb-4 border-b border-[#444] pb-2">
                Coding Standards
              </h2>
              
              <div className="bg-[#252526] border border-[#333] rounded-md p-6 mt-4">
                <p className="text-sm mb-4">
                  When contributing code to HyperBEAM, please follow these general guidelines:
                </p>
                
                <ul className="text-sm space-y-2 list-disc pl-5">
                  <li>Follow the existing code style in the project</li>
                  <li>Write comprehensive tests for new functionality</li>
                  <li>Document public APIs with clear comments</li>
                  <li>Keep commit messages clear and descriptive</li>
                  <li>Make sure all tests pass before submitting</li>
                </ul>
              </div>
            </section>
          </div>
        </div>
        
        <div className="lg:col-span-1">
          <div className="sticky top-[80px] space-y-6">
            {/* Terminal Explorer */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Contribution Guide</h3>
              <Terminal initialCommands={initialCommands} height={200} />
            </div>
            
            {/* Quick Links */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Quick Links</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <a href="#" className="text-terminal-text hover:text-terminal-green">GitHub Repository</a>
                </li>
                <li>
                  <a href="#" className="text-terminal-text hover:text-terminal-green">Issue Tracker</a>
                </li>
                <li>
                  <a href="#" className="text-terminal-text hover:text-terminal-green">Style Guide</a>
                </li>
                <li>
                  <a href="#" className="text-terminal-text hover:text-terminal-green">Developer Documentation</a>
                </li>
              </ul>
            </div>
            
            {/* Contact Methods */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Contact Methods</h3>
              <ul className="space-y-2 text-sm">
                <li className="flex items-center">
                  <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                    <path d="M2.003 5.884L10 9.882l7.997-3.998A2 2 0 0016 4H4a2 2 0 00-1.997 1.884z" />
                    <path d="M18 8.118l-8 4-8-4V14a2 2 0 002 2h12a2 2 0 002-2V8.118z" />
                  </svg>
                  <a href="mailto:contribute@entityC.com" className="hover:text-terminal-green">contribute@entityC.com</a>
                </li>
                <li className="flex items-center">
                  <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                    <path fillRule="evenodd" d="M10 2a1 1 0 00-1 1v1a1 1 0 002 0V3a1 1 0 00-1-1zM4 4h3a3 3 0 006 0h3a2 2 0 012 2v9a2 2 0 01-2 2H4a2 2 0 01-2-2V6a2 2 0 012-2zm2.5 7a1.5 1.5 0 100-3 1.5 1.5 0 000 3zm2.45 4a2.5 2.5 0 10-4.9 0h4.9zM12 9a1 1 0 100 2h3a1 1 0 100-2h-3zm-1 4a1 1 0 011-1h2a1 1 0 110 2h-2a1 1 0 01-1-1z" clipRule="evenodd" />
                  </svg>
                  <a href="#" className="hover:text-terminal-green">GitHub Issues</a>
                </li>
                <li className="flex items-center">
                  <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                    <path d="M2 5a2 2 0 012-2h7a2 2 0 012 2v4a2 2 0 01-2 2H9l-3 3v-3H4a2 2 0 01-2-2V5z" />
                    <path d="M15 7v2a4 4 0 01-4 4H9.828l-1.766 1.767c.28.149.599.233.938.233h2l3 3v-3h2a2 2 0 002-2V9a2 2 0 00-2-2h-1z" />
                  </svg>
                  <a href="#" className="hover:text-terminal-green">Discord Channel</a>
                </li>
              </ul>
            </div>
            
            {/* Tools & Resources */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Tools & Resources</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <a href="#" className="text-terminal-text hover:text-terminal-green">Development Environment Setup</a>
                </li>
                <li>
                  <a href="#" className="text-terminal-text hover:text-terminal-green">Testing Guide</a>
                </li>
                <li>
                  <a href="#" className="text-terminal-text hover:text-terminal-green">Documentation Guidelines</a>
                </li>
                <li>
                  <a href="#" className="text-terminal-text hover:text-terminal-green">API Reference</a>
                </li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </>
  );
}
