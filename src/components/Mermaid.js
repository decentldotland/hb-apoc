'use client';

import React, { useEffect, useRef, useState } from 'react';

/**
 * Mermaid component for rendering mermaid diagrams
 * 
 * @param {string} chart - The mermaid chart definition
 */
const Mermaid = ({ chart }) => {
  const containerRef = useRef(null);
  const [mermaidLoaded, setMermaidLoaded] = useState(false);
  const [renderError, setRenderError] = useState(null);
  const diagramId = `mermaid-${Math.floor(Math.random() * 100000)}`;

  // Load and initialize mermaid on client-side only
  useEffect(() => {
    // Import mermaid dynamically
    const loadMermaid = async () => {
      try {
        // Try to load mermaid from CDN if local import fails
        let mermaid;
        try {
          // First attempt to use locally installed module
          const mermaidModule = await import('mermaid');
          mermaid = mermaidModule.default;
          console.log('Loaded mermaid from local installation');
        } catch (err) {
          console.warn('Failed to load local mermaid module:', err);
          
          // If that fails, dynamically insert script from CDN
          if (typeof window !== 'undefined' && !window.mermaid) {
            console.log('Attempting to load mermaid from CDN');
            
            // Create a new script element
            const script = document.createElement('script');
            script.src = 'https://cdn.jsdelivr.net/npm/mermaid@10.6.1/dist/mermaid.min.js';
            script.async = true;
            
            // Add the script to the document
            document.body.appendChild(script);
            
            // Wait for the script to load
            await new Promise((resolve, reject) => {
              script.onload = resolve;
              script.onerror = reject;
            });
            
            mermaid = window.mermaid;
          } else if (typeof window !== 'undefined') {
            mermaid = window.mermaid;
          } else {
            throw new Error('Unable to load mermaid from any source');
          }
        }
        
        // Initialize mermaid
        mermaid.initialize({
          startOnLoad: false,
          theme: 'dark',
          securityLevel: 'loose',
          fontFamily: 'monospace',
          themeVariables: {
            // Theme customization
            primaryColor: '#4CAF50',
            primaryTextColor: '#FFFFFF',
            primaryBorderColor: '#4CAF50',
            secondaryColor: '#6C757D',
            tertiaryColor: '#252526',
            background: '#252526',
            mainBkg: '#1E1E1E',
            nodeBorder: '#444444',
            fontSize: '14px',
            clusterBkg: '#2D2D2D',
            clusterBorder: '#444444',
            edgeLabelBackground: '#2D2D2D',
            lineColor: '#6C757D',
          },
          logLevel: 'error',
        });
        
        setMermaidLoaded(true);
        
        // Render the diagram if we have a container
        if (containerRef.current) {
          try {
            containerRef.current.innerHTML = '';
            mermaid.render(diagramId, chart).then(result => {
              containerRef.current.innerHTML = result.svg;
            });
          } catch (error) {
            console.error('Mermaid rendering error:', error);
            setRenderError(error);
            
            // Fall back to displaying the raw chart
            displayRawChart();
          }
        }
      } catch (error) {
        console.error('Error loading mermaid:', error);
        setRenderError(error);
        
        // Fall back to displaying the raw chart
        displayRawChart();
      }
    };
    
    // Display raw chart as fallback
    const displayRawChart = () => {
      if (containerRef.current) {
        containerRef.current.innerHTML = `
          <pre style="padding: 1rem; color: #cccccc; overflow-x: auto; white-space: pre-wrap;">
            ${chart.replace(/</g, '&lt;').replace(/>/g, '&gt;')}
          </pre>
        `;
      }
    };
    
    loadMermaid();
  }, [chart, diagramId]);

  return (
    <div className="my-4 bg-[#1e1e1e] border border-[#333] rounded-md overflow-hidden">
      <div className="bg-[#252526] px-4 py-1 text-xs flex justify-between items-center border-b border-[#444]">
        <span className="text-terminal-accent font-mono">mermaid</span>
        <span className="text-[#888] text-xs">HyperBEAM</span>
      </div>
      <div 
        ref={containerRef}
        className="py-3 px-2 overflow-auto max-w-full"
      >
        {/* Mermaid diagram will be rendered here */}
        <div className="animate-pulse flex space-x-2 justify-center items-center p-4">
          <div className="w-3 h-3 bg-terminal-green rounded-full"></div>
          <div className="w-3 h-3 bg-terminal-accent rounded-full"></div>
          <div className="w-3 h-3 bg-terminal-green rounded-full"></div>
          <div className="text-xs text-gray-400">Rendering diagram...</div>
        </div>
      </div>
    </div>
  );
};

export default Mermaid;
