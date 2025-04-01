"use client"

import { useState } from 'react';
import Link from 'next/link';
import Terminal from '../../../components/Terminal';

export default function Feedback() {
  const [feedbackData, setFeedbackData] = useState({
    type: 'suggestion',
    subject: '',
    message: '',
    rating: 3,
    email: ''
  });
  
  const [submitted, setSubmitted] = useState(false);
  
  // Terminal commands for feedback examples
  const initialCommands = [
    { 
      command: 'cat feedback_stats.json | jq .summary', 
      output: '{\n  "total_submissions": 57,\n  "average_rating": 4.2,\n  "suggestion_count": 31,\n  "improvement_count": 15,\n  "compliment_count": 11\n}'
    }
  ];

  // Handle form field changes
  const handleChange = (e) => {
    const { name, value } = e.target;
    setFeedbackData({
      ...feedbackData,
      [name]: value
    });
  };

  // Handle form submission
  const handleSubmit = (e) => {
    e.preventDefault();
    // In a real implementation, this would send the feedback to a backend service
    console.log('Feedback submitted:', feedbackData);
    setSubmitted(true);
    
    // Reset form after a delay
    setTimeout(() => {
      setFeedbackData({
        type: 'suggestion',
        subject: '',
        message: '',
        rating: 3,
        email: ''
      });
      setSubmitted(false);
    }, 5000);
  };

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <Link href="/community" className="hover:text-terminal-green">Community</Link>
          <span>/</span>
          <span className="text-terminal-green">Provide Feedback</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Provide Feedback
        </h1>
        
        <p className="text-lg mb-8">
          Your feedback helps us improve the HyperBEAM documentation and make it more useful for everyone.
          Share your thoughts, suggestions, or report any issues you've encountered.
        </p>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-4 gap-8 mb-12">
        <div className="lg:col-span-3">
          {submitted ? (
            <div className="bg-[#252526] border border-[#333] rounded-md p-8 text-center">
              <svg xmlns="http://www.w3.org/2000/svg" className="h-16 w-16 mx-auto mb-4 text-terminal-green" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
              </svg>
              <h2 className="text-xl font-mono font-semibold text-terminal-green mb-3">
                Feedback Submitted Successfully
              </h2>
              <p className="text-sm mb-6 max-w-lg mx-auto">
                Thank you for your feedback! Your input is valuable and helps us improve 
                the HyperBEAM documentation for everyone.
              </p>
              <button
                onClick={() => setSubmitted(false)}
                className="bg-[#3a3a3a] text-terminal-green text-sm py-2 px-3 rounded-md hover:bg-[#444] transition-colors"
              >
                Submit More Feedback
              </button>
            </div>
          ) : (
            <div className="bg-[#252526] border border-[#333] rounded-md p-6">
              <h2 className="text-xl font-mono font-semibold text-terminal-green mb-4 border-b border-[#444] pb-2">
                Feedback Form
              </h2>
              
              {/* Under Construction Notice */}
              <div className="border-2 border-dashed border-terminal-accent bg-[#2d2d2d1a] rounded-md p-5 text-center mb-6">
                <svg xmlns="http://www.w3.org/2000/svg" className="h-10 w-10 mx-auto mb-3 text-terminal-accent" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M19.428 15.428a2 2 0 00-1.022-.547l-2.387-.477a6 6 0 00-3.86.517l-.318.158a6 6 0 01-3.86.517L6.05 15.21a2 2 0 00-1.806.547M8 4h8l-1 1v5.172a2 2 0 00.586 1.414l5 5c1.26 1.26.367 3.414-1.415 3.414H4.828c-1.782 0-2.674-2.154-1.414-3.414l5-5A2 2 0 009 10.172V5L8 4z" />
                </svg>
                <h3 className="text-lg font-mono font-semibold text-terminal-accent mb-2">
                  Feedback System Under Construction
                </h3>
                <p className="text-sm mb-3">
                  The feedback submission system is currently being implemented. 
                  Soon you'll be able to share your thoughts and suggestions directly.
                </p>
                <p className="text-xs opacity-70">
                  Backend integration coming soon.
                </p>
              </div>
              
              <div className="space-y-6">
                {/* Basic Feedback Information - Disabled */}
                <div className="space-y-4">
                  <div>
                    <label className="block text-sm font-medium mb-1 opacity-50">
                      Feedback Type <span className="text-red-500">*</span>
                    </label>
                    <select
                      disabled
                      className="w-full bg-[#2d2d2d] border border-[#444] rounded px-3 py-2 text-sm opacity-50 cursor-not-allowed"
                    >
                      <option value="suggestion">Suggestion</option>
                      <option value="improvement">Improvement Idea</option>
                      <option value="content">Content Issue</option>
                      <option value="usability">Usability Issue</option>
                      <option value="compliment">Compliment</option>
                      <option value="other">Other</option>
                    </select>
                  </div>
                  
                  <div>
                    <label className="block text-sm font-medium mb-1 opacity-50">
                      Subject <span className="text-red-500">*</span>
                    </label>
                    <input
                      type="text"
                      disabled
                      className="w-full bg-[#2d2d2d] border border-[#444] rounded px-3 py-2 text-sm opacity-50 cursor-not-allowed"
                      placeholder="Brief summary of your feedback"
                    />
                  </div>
                  
                  <div>
                    <label className="block text-sm font-medium mb-1 opacity-50">
                      Your Feedback <span className="text-red-500">*</span>
                    </label>
                    <textarea
                      disabled
                      className="w-full bg-[#2d2d2d] border border-[#444] rounded px-3 py-2 text-sm min-h-[150px] opacity-50 cursor-not-allowed"
                      placeholder="Please share your detailed feedback, suggestions, or concerns..."
                    ></textarea>
                  </div>
                  
                  <div>
                    <label className="block text-sm font-medium mb-3 opacity-50">
                      Overall Rating
                    </label>
                    <div className="flex items-center space-x-2 opacity-50">
                      <span className="text-xs">Poor</span>
                      <div className="flex space-x-1">
                        {[1, 2, 3, 4, 5].map((rating) => (
                          <div 
                            key={rating}
                            className="w-8 h-8 rounded-full bg-[#2d2d2d] border border-[#444] flex items-center justify-center cursor-not-allowed"
                          >
                            {rating}
                          </div>
                        ))}
                      </div>
                      <span className="text-xs">Excellent</span>
                    </div>
                  </div>
                  
                  <div>
                    <label className="block text-sm font-medium mb-1 opacity-50">
                      Email (optional)
                    </label>
                    <input
                      type="email"
                      disabled
                      className="w-full bg-[#2d2d2d] border border-[#444] rounded px-3 py-2 text-sm opacity-50 cursor-not-allowed"
                      placeholder="Your email for follow-up if needed"
                    />
                    <p className="text-xs opacity-50 mt-1">
                      We'll only use this to follow up on your feedback if needed.
                    </p>
                  </div>
                </div>
                
                {/* Submit Button */}
                <div className="pt-4 flex justify-end">
                  <button
                    disabled
                    className="bg-gray-500 text-black font-semibold py-2 px-4 rounded-md opacity-50 cursor-not-allowed"
                  >
                    Submit Feedback
                  </button>
                </div>
              </div>
            </div>
          )}
          
          {/* What we do with feedback section */}
          <div className="bg-[#252526] border border-[#333] rounded-md p-6 mt-6">
            <h2 className="text-xl font-mono font-semibold text-terminal-green mb-4 border-b border-[#444] pb-2">
              How We Use Your Feedback
            </h2>
            <div className="space-y-4">
              <p className="text-sm">
                Your feedback is invaluable to us. Here's how we use it to improve the documentation:
              </p>
              
              <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mt-4">
                <div className="bg-[#2d2d2d] rounded-md p-4">
                  <h3 className="font-semibold text-terminal-accent mb-2">Review</h3>
                  <p className="text-xs opacity-80">
                    Each piece of feedback is reviewed by our documentation team to identify areas for improvement.
                  </p>
                </div>
                
                <div className="bg-[#2d2d2d] rounded-md p-4">
                  <h3 className="font-semibold text-terminal-accent mb-2">Prioritize</h3>
                  <p className="text-xs opacity-80">
                    We prioritize changes based on impact, frequency of issues reported, and available resources.
                  </p>
                </div>
                
                <div className="bg-[#2d2d2d] rounded-md p-4">
                  <h3 className="font-semibold text-terminal-accent mb-2">Implement</h3>
                  <p className="text-xs opacity-80">
                    Regular updates incorporate your feedback to continuously improve the documentation.
                  </p>
                </div>
              </div>
            </div>
          </div>
        </div>
        
        <div className="lg:col-span-1">
          <div className="sticky top-[80px] space-y-6">
            {/* Feedback Stats */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Feedback Stats</h3>
              <Terminal initialCommands={initialCommands} height={200} />
            </div>
            
            {/* Alternative Feedback Methods */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Other Ways to Help</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <Link href="/community/report-issue" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7 4a1 1 0 11-2 0 1 1 0 012 0zm-1-9a1 1 0 00-1 1v4a1 1 0 102 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
                    </svg>
                    Report a Specific Issue
                  </Link>
                </li>
                <li>
                  <Link href="/community/contribute" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path d="M13.586 3.586a2 2 0 112.828 2.828l-.793.793-2.828-2.828.793-.793zM11.379 5.793L3 14.172V17h2.828l8.38-8.379-2.83-2.828z" />
                    </svg>
                    Contribute Improvements
                  </Link>
                </li>
                <li>
                  <Link href="/community/faq" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-8-3a1 1 0 00-.867.5 1 1 0 11-1.731-1A3 3 0 0113 8a3.001 3.001 0 01-2 2.83V11a1 1 0 11-2 0v-1a1 1 0 011-1 1 1 0 100-2zm0 8a1 1 0 100-2 1 1 0 000 2z" clipRule="evenodd" />
                    </svg>
                    Submit a Question
                  </Link>
                </li>
              </ul>
            </div>
            
            {/* Related Documentation */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Related Resources</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <a href="#" className="text-terminal-text hover:text-terminal-green">Documentation Guidelines</a>
                </li>
                <li>
                  <a href="#" className="text-terminal-text hover:text-terminal-green">Community Standards</a>
                </li>
                <li>
                  <a href="#" className="text-terminal-text hover:text-terminal-green">Style Guide</a>
                </li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </>
  );
}
