"use client"

import { useState } from 'react';
import Link from 'next/link';
import Terminal from '../../../components/Terminal';

export default function FAQ() {
  const [question, setQuestion] = useState('');
  const [email, setEmail] = useState('');
  const [submitted, setSubmitted] = useState(false);
  
  // Terminal commands for empty faq
  const initialCommands = [
    { 
      command: 'cat community/faq_placeholder.md', 
      output: 'FAQ section is being built with community input.\nPlease submit your questions to help us create the most useful resource.'
    }
  ];

  // Handle question submission
  const handleSubmit = (e) => {
    e.preventDefault();
    // In a real implementation, this would send the question to a backend service
    console.log('Question submitted:', question, 'from:', email);
    setSubmitted(true);
    
    // Reset form after a delay
    setTimeout(() => {
      setQuestion('');
      setEmail('');
      setSubmitted(false);
    }, 3000);
  };

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <Link href="/community" className="hover:text-terminal-green">Community</Link>
          <span>/</span>
          <span className="text-terminal-green">FAQ</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Frequently Asked Questions
        </h1>
        
        <p className="text-lg mb-8">
          We're building our FAQ section based on community questions. Help us create the most useful 
          resource by submitting your questions about HyperBEAM.
        </p>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-4 gap-8 mb-12">
        <div className="lg:col-span-3">
          {/* Placeholder content */}
          <div className="bg-[#252526] border border-[#333] rounded-md p-8 text-center mb-8">
            <svg xmlns="http://www.w3.org/2000/svg" className="h-16 w-16 mx-auto mb-4 text-terminal-accent opacity-50" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
            </svg>
            <h2 className="text-xl font-mono font-semibold text-terminal-green mb-3">
              FAQ Section Coming Soon
            </h2>
            <p className="text-sm mb-6 max-w-lg mx-auto">
              We're collecting questions from the community to build a comprehensive FAQ section.
              Please submit your questions using the form below.
            </p>
            <div className="flex justify-center">
              <Terminal initialCommands={initialCommands} height={100} />
            </div>
          </div>
          
          {/* Question submission form */}
          <div className="bg-[#252526] border border-[#333] rounded-md p-6">
            <h2 className="text-xl font-mono font-semibold text-terminal-green mb-4">
              Submit Your Question
            </h2>
            
            {submitted ? (
              <div className="bg-[#2d2d2d] rounded-md p-4 text-center">
                <svg xmlns="http://www.w3.org/2000/svg" className="h-10 w-10 mx-auto mb-2 text-terminal-green" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
                </svg>
                <p className="text-sm">
                  Thanks for your question! We'll review it and add it to our FAQ section.
                </p>
              </div>
            ) : (
              <div className="space-y-4">
                <div className="border-2 border-dashed border-terminal-accent bg-[#2d2d2d1a] rounded-md p-5 text-center">
                  <svg xmlns="http://www.w3.org/2000/svg" className="h-10 w-10 mx-auto mb-3 text-terminal-accent" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M19.428 15.428a2 2 0 00-1.022-.547l-2.387-.477a6 6 0 00-3.86.517l-.318.158a6 6 0 01-3.86.517L6.05 15.21a2 2 0 00-1.806.547M8 4h8l-1 1v5.172a2 2 0 00.586 1.414l5 5c1.26 1.26.367 3.414-1.415 3.414H4.828c-1.782 0-2.674-2.154-1.414-3.414l5-5A2 2 0 009 10.172V5L8 4z" />
                  </svg>
                  <h3 className="text-lg font-mono font-semibold text-terminal-accent mb-2">
                    Under Construction
                  </h3>
                  <p className="text-sm mb-3">
                    The question submission system is currently being built. 
                    Soon you'll be able to submit questions and get answers from the HyperBEAM team.
                  </p>
                  <p className="text-xs opacity-70">
                    Backend storage system coming soon.
                  </p>
                </div>
                
                <div>
                  <label className="block text-sm font-medium mb-1 opacity-50">
                    Your Question
                  </label>
                  <textarea
                    disabled
                    className="w-full bg-[#2d2d2d] border border-[#444] rounded px-3 py-2 text-sm min-h-[100px] opacity-50 cursor-not-allowed"
                    placeholder="What would you like to know about HyperBEAM?"
                  ></textarea>
                </div>
                
                <div>
                  <label className="block text-sm font-medium mb-1 opacity-50">
                    Email (optional)
                  </label>
                  <input
                    type="email"
                    disabled
                    className="w-full bg-[#2d2d2d] border border-[#444] rounded px-3 py-2 text-sm opacity-50 cursor-not-allowed"
                    placeholder="Your email for follow-up"
                  />
                </div>
                
                <div className="pt-2">
                  <button
                    disabled
                    className="bg-gray-500 text-black font-semibold py-2 px-4 rounded-md opacity-50 cursor-not-allowed"
                  >
                    Submit Question
                  </button>
                </div>
              </div>
            )}
          </div>
        </div>
        
        <div className="lg:col-span-1">
          <div className="sticky top-[80px] space-y-6">
            {/* Coming soon info */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">FAQ Status</h3>
              <div className="flex items-center mb-2">
                <div className="w-3 h-3 rounded-full bg-yellow-400 mr-2"></div>
                <span className="text-sm">In Progress</span>
              </div>
              <p className="text-xs opacity-80">
                We're actively collecting questions and preparing answers. Check back soon for updates.
              </p>
            </div>
            
            {/* Popular topics */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Suggested Topics</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <button className="text-terminal-text hover:text-terminal-green">
                    HyperBEAM Architecture
                  </button>
                </li>
                <li>
                  <button className="text-terminal-text hover:text-terminal-green">
                    Device Ecosystem
                  </button>
                </li>
                <li>
                  <button className="text-terminal-text hover:text-terminal-green">
                    Arweave Integration
                  </button>
                </li>
                <li>
                  <button className="text-terminal-text hover:text-terminal-green">
                    Development Setup
                  </button>
                </li>
                <li>
                  <button className="text-terminal-text hover:text-terminal-green">
                    Deployment Options
                  </button>
                </li>
              </ul>
            </div>
            
            {/* Related Sections */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Also See</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <Link href="/community/feedback" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M18 5v8a2 2 0 01-2 2h-5l-5 4v-4H4a2 2 0 01-2-2V5a2 2 0 012-2h12a2 2 0 012 2zM7 8H5v2h2V8zm2 0h2v2H9V8zm6 0h-2v2h2V8z" clipRule="evenodd" />
                    </svg>
                    Provide Feedback
                  </Link>
                </li>
                <li>
                  <Link href="/reference/glossary" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path d="M5 4a1 1 0 00-2 0v7.268a2 2 0 000 3.464V16a1 1 0 102 0v-1.268a2 2 0 000-3.464V4zM11 4a1 1 0 10-2 0v1.268a2 2 0 000 3.464V16a1 1 0 102 0V8.732a2 2 0 000-3.464V4zM16 3a1 1 0 011 1v7.268a2 2 0 010 3.464V16a1 1 0 11-2 0v-1.268a2 2 0 010-3.464V4a1 1 0 011-1z" />
                    </svg>
                    Glossary
                  </Link>
                </li>
                <li>
                  <Link href="/community" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path d="M13 6a3 3 0 11-6 0 3 3 0 016 0zM18 8a2 2 0 11-4 0 2 2 0 014 0zM14 15a4 4 0 00-8 0v3h8v-3zM6 8a2 2 0 11-4 0 2 2 0 014 0zM16 18v-3a5.972 5.972 0 00-.75-2.906A3.005 3.005 0 0119 15v3h-3zM4.75 12.094A5.973 5.973 0 004 15v3H1v-3a3 3 0 013.75-2.906z" />
                    </svg>
                    Community Resources
                  </Link>
                </li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </>
  );
}
