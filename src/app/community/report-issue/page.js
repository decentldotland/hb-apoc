"use client"

import { useState } from 'react';
import Link from 'next/link';
import Terminal from '../../../components/Terminal';

export default function ReportIssue() {
  // Terminal commands showing email template
  const initialCommands = [
    { 
      command: 'cat email_template.md', 
      output: 'Subject: [Issue Type] - Brief issue title\n\n## Issue Description\n\n<!-- A clear, concise description of the issue -->\n\n## Steps to Reproduce\n\n1. \n2. \n3. \n\n## Expected Behavior\n\n<!-- What should happen -->\n\n## Actual Behavior\n\n<!-- What actually happens -->\n\n## Additional Context\n\n<!-- Any other information that might be helpful -->'
    }
  ];

  // State for copy to clipboard functionality
  const [copied, setCopied] = useState(false);
  
  // Email template text
  const emailTemplate = `Subject: [Issue Type] - Brief issue title

## Issue Description

<!-- A clear, concise description of the issue -->

## Steps to Reproduce

1. 
2. 
3. 

## Expected Behavior

<!-- What should happen -->

## Actual Behavior

<!-- What actually happens -->

## Additional Context

<!-- Any other information that might be helpful -->`;

  // Handle copy to clipboard
  const copyTemplate = () => {
    navigator.clipboard.writeText(emailTemplate);
    setCopied(true);
    setTimeout(() => setCopied(false), 2000);
  };

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <Link href="/community" className="hover:text-terminal-green">Community</Link>
          <span>/</span>
          <span className="text-terminal-green">Report an Issue</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Report an Issue
        </h1>
        
        <p className="text-lg mb-8">
          Found a bug, inaccuracy, or problem with the documentation? Send us an email with your report 
          to help us improve HyperBEAM.
        </p>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-4 gap-8 mb-12">
        <div className="lg:col-span-3">
          <div className="bg-[#252526] border border-[#333] rounded-md p-6">
            <h2 className="text-xl font-mono font-semibold text-terminal-green mb-4 border-b border-[#444] pb-2">
              Email-Based Issue Reporting
            </h2>
            
            <div className="space-y-6">
              <div className="bg-[#2d2d2d] rounded-md p-6 text-center">
                <svg xmlns="http://www.w3.org/2000/svg" className="h-12 w-12 mx-auto mb-4 text-terminal-accent" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z" />
                </svg>
                <h3 className="text-lg font-mono font-semibold text-terminal-green mb-3">
                  Send Your Issue Report to:
                </h3>
                <a 
                  href="mailto:contact@entityC.com" 
                  className="text-xl font-mono font-bold text-terminal-accent hover:underline block mb-4"
                >
                  contact@entityC.com
                </a>
                <p className="text-sm mb-4 max-w-lg mx-auto">
                  Please include as much detail as possible to help us understand and resolve the issue quickly.
                  Use the template below for your email.
                </p>
                <a 
                  href="mailto:contact@entityC.com?subject=[Issue Report] - &body=## Issue Description%0A%0A<!-- A clear, concise description of the issue -->%0A%0A## Steps to Reproduce%0A%0A1. %0A2. %0A3. %0A%0A## Expected Behavior%0A%0A<!-- What should happen -->%0A%0A## Actual Behavior%0A%0A<!-- What actually happens -->%0A%0A## Additional Context%0A%0A<!-- Any other information that might be helpful -->"
                  className="bg-terminal-accent text-black font-semibold py-2 px-6 rounded-md hover:bg-terminal-accent/90 transition-colors inline-block"
                >
                  Open Email Client
                </a>
              </div>
              
              <div>
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
              
              <div className="bg-[#2d2d2d] rounded-md p-5 border-l-4 border-terminal-accent">
                <h3 className="text-terminal-accent font-semibold mb-2">Tips for Effective Issue Reports</h3>
                <ul className="text-sm space-y-1.5 list-disc pl-4">
                  <li>Be specific about the issue and where you encountered it</li>
                  <li>Include the exact steps to reproduce the problem</li>
                  <li>Mention which browser/device you're using</li>
                  <li>Attach screenshots if possible</li>
                  <li>Suggest a fix if you have one in mind</li>
                </ul>
              </div>
            </div>
          </div>
        </div>
        
        <div className="lg:col-span-1">
          <div className="sticky top-[80px] space-y-6">
            {/* Report template in terminal */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Email Template</h3>
              <Terminal initialCommands={initialCommands} height={200} />
            </div>
            
            {/* Issue Types */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Common Issue Types</h3>
              <ul className="space-y-2 text-sm">
                <li className="flex items-center">
                  <span className="w-2 h-2 rounded-full bg-red-500 mr-2"></span>
                  <span>Documentation errors or inaccuracies</span>
                </li>
                <li className="flex items-center">
                  <span className="w-2 h-2 rounded-full bg-yellow-500 mr-2"></span>
                  <span>Unclear explanations or examples</span>
                </li>
                <li className="flex items-center">
                  <span className="w-2 h-2 rounded-full bg-blue-500 mr-2"></span>
                  <span>Missing information</span>
                </li>
                <li className="flex items-center">
                  <span className="w-2 h-2 rounded-full bg-purple-500 mr-2"></span>
                  <span>Website functionality problems</span>
                </li>
                <li className="flex items-center">
                  <span className="w-2 h-2 rounded-full bg-green-500 mr-2"></span>
                  <span>Improvement suggestions</span>
                </li>
              </ul>
            </div>
            
            {/* Alternative contact methods */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Other Contact Methods</h3>
              <ul className="space-y-2 text-sm">
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
                  <a href="#" className="hover:text-terminal-green">Discord Server</a>
                </li>
                <li className="flex items-center">
                  <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                    <path d="M17.414 2.586a2 2 0 00-2.828 0L7 10.172V13h2.828l7.586-7.586a2 2 0 000-2.828z" />
                    <path fillRule="evenodd" d="M2 6a2 2 0 012-2h4a1 1 0 010 2H4v10h10v-4a1 1 0 112 0v4a2 2 0 01-2 2H4a2 2 0 01-2-2V6z" clipRule="evenodd" />
                  </svg>
                  <Link href="/community/feedback" className="hover:text-terminal-green">Feedback Form</Link>
                </li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </>
  );
}
