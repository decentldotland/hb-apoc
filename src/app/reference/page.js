import Link from 'next/link';
import Terminal from '../../components/Terminal';

export default function ReferenceIndex() {
  // Sample terminal commands for reference section
  const initialCommands = [
    { 
      command: 'ls -la reference/', 
      output: `total 4
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 ./
drwxr-xr-x 5 hyperbeam system 4096 Mar 27 2025 ../
-rw-r--r-- 1 hyperbeam system 324859 Mar 27 2025 functions.md
-rw-r--r-- 1 hyperbeam system 158742 Mar 27 2025 glossary.md
-rw-r--r-- 1 hyperbeam system 135680 Mar 27 2025 dependency-visualization.md
-rw-r--r-- 1 hyperbeam system 78431 Mar 27 2025 resolved-questions.md`
    }
  ];

  // Reference materials data
  const referenceItems = [
    {
      id: 'functions',
      title: 'Function Reference',
      description: 'Comprehensive reference of all HyperBEAM functions, their parameters, return values, and usage examples.',
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-8 w-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4" />
        </svg>
      ),
      content: '438 documented functions across core modules and subsystems',
      link: '/reference/functions'
    },
    {
      id: 'glossary',
      title: 'Conceptual Glossary',
      description: 'Definitions of key terms, concepts, and design patterns used throughout the HyperBEAM system.',
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-8 w-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M12 6.253v13m0-13C10.832 5.477 9.246 5 7.5 5S4.168 5.477 3 6.253v13C4.168 18.477 5.754 18 7.5 18s3.332.477 4.5 1.253m0-13C13.168 5.477 14.754 5 16.5 5c1.747 0 3.332.477 4.5 1.253v13C19.832 18.477 18.247 18 16.5 18c-1.746 0-3.332.477-4.5 1.253" />
        </svg>
      ),
      content: '187 terms and concepts with cross-references',
      link: '/reference/glossary'
    },
    {
      id: 'dependency-visualization',
      title: 'Dependency Visualization',
      description: 'Interactive visualizations of dependencies between modules, subsystems, and components.',
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-8 w-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
        </svg>
      ),
      content: '12 visualizations with module-level dependency graphs',
      link: '/reference/dependency-visualization'
    },
    {
      id: 'resolved-questions',
      title: 'Resolved Questions',
      description: 'Documentation of questions that arose during development and their resolutions.',
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-8 w-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
        </svg>
      ),
      content: '43 resolved questions with detailed explanations',
      link: '/reference/resolved-questions'
    }
  ];

  // Function categories
  const functionCategories = [
    { 
      name: 'Core Functions', 
      count: 124,
      examples: ['hb_converge:process/2', 'hb_message:decode/1', 'hb_path:resolve/2']
    },
    { 
      name: 'Storage Functions', 
      count: 87,
      examples: ['hb_store:put/3', 'hb_cache:get/2', 'hb_persistent:register/3']
    },
    { 
      name: 'Network Functions', 
      count: 76,
      examples: ['hb_http_server:start/2', 'hb_http_client:request/4', 'hb_router:dispatch/3']
    },
    { 
      name: 'Device Functions', 
      count: 95,
      examples: ['dev_message:handle/2', 'dev_storage:store/3', 'dev_security:verify/2']
    },
    { 
      name: 'Utility Functions', 
      count: 56,
      examples: ['hb_util:merge/2', 'hb_util:to_binary/1', 'hb_util:timestamp/0']
    }
  ];

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <span className="text-terminal-green">Reference</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          Reference Materials
        </h1>
        
        <p className="text-lg mb-8">
          Comprehensive reference documentation for HyperBEAM including functions, concepts, dependencies,
          and more. These materials provide detailed technical information for developers and researchers.
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6 flex items-center justify-between">
          <div className="font-mono text-sm">
            <span className="text-terminal-green">$</span> 
            <span className="text-terminal-accent ml-2">cd</span> 
            <span className="text-terminal-output ml-2">reference/</span>
          </div>
          <div className="bg-[#333] px-3 py-1 rounded text-xs font-mono">
            4 reference documents | 438 functions | 187 concepts
          </div>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-8 mb-12">
        <div className="lg:col-span-2">
          {/* Reference Materials Grid */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-6">Reference Materials</h2>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-5">
              {referenceItems.map((item) => (
                <Link href={item.link} key={item.id}>
                  <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-all duration-200 card-hover-anim h-full">
                    <div className="flex items-start">
                      <div className="text-terminal-green mr-4 mt-1">
                        {item.icon}
                      </div>
                      <div>
                        <h3 className="text-lg font-semibold mb-2 text-terminal-primary">
                          {item.title}
                        </h3>
                        <p className="text-sm opacity-80 mb-3">{item.description}</p>
                        <div className="bg-[#2d2d2d] px-3 py-1 rounded-full text-xs inline-block text-terminal-accent">
                          {item.content}
                        </div>
                      </div>
                    </div>
                  </div>
                </Link>
              ))}
            </div>
          </section>
          
          {/* Function Reference Preview */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Function Reference Overview</h2>
            
            <div className="bg-[#252526] border border-[#333] rounded-md p-6">
              <p className="mb-4">
                The function reference provides detailed documentation for all public functions in the HyperBEAM system. 
                Functions are organized by category and include parameter descriptions, return values, and usage examples.
              </p>
              
              <div className="grid grid-cols-1 sm:grid-cols-2 gap-4 mb-6">
                {functionCategories.map((category, idx) => (
                  <div key={idx} className="bg-[#2d2d2d] p-4 rounded-md">
                    <h3 className="text-terminal-green font-semibold mb-2">{category.name}</h3>
                    <p className="text-xs opacity-80 mb-2">{category.count} documented functions</p>
                    <div className="text-xs font-mono space-y-1 opacity-70">
                      {category.examples.map((example, i) => (
                        <div key={i}>{example}</div>
                      ))}
                    </div>
                  </div>
                ))}
              </div>
              
              <div className="flex justify-end">
                <Link href="/reference/functions" className="text-terminal-green text-sm hover:underline flex items-center">
                  <span className="mr-1">View Full Function Reference</span>
                  <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M14 5l7 7m0 0l-7 7m7-7H3" />
                  </svg>
                </Link>
              </div>
            </div>
          </section>
          
          {/* Glossary Preview */}
          <section className="mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Conceptual Glossary</h2>
            
            <div className="bg-[#252526] border border-[#333] rounded-md p-6">
              <p className="mb-4">
                The conceptual glossary defines key terms and concepts used throughout the HyperBEAM system.
                Understanding these concepts is essential for working with the system effectively.
              </p>
              
              <div className="grid grid-cols-1 sm:grid-cols-2 gap-4 mb-6">
                <div className="bg-[#2d2d2d] p-4 rounded-md">
                  <h3 className="font-mono text-terminal-accent mb-2">Convergence Protocol</h3>
                  <p className="text-xs">
                    A protocol that enables distributed devices to reach consensus on message processing order
                    and state through a defined sequence of steps. Central to HyperBEAM's distributed architecture.
                  </p>
                </div>
                <div className="bg-[#2d2d2d] p-4 rounded-md">
                  <h3 className="font-mono text-terminal-accent mb-2">Device Ecosystem</h3>
                  <p className="text-xs">
                    The collection of specialized components that process messages and provide system functionality.
                    Devices are organized into categories based on their purpose and behavior.
                  </p>
                </div>
                <div className="bg-[#2d2d2d] p-4 rounded-md">
                  <h3 className="font-mono text-terminal-accent mb-2">Storage Backend</h3>
                  <p className="text-xs">
                    An implementation of the storage interface that provides persistent data storage.
                    Backends include filesystem, RocksDB, remote nodes, and gateway implementations.
                  </p>
                </div>
                <div className="bg-[#2d2d2d] p-4 rounded-md">
                  <h3 className="font-mono text-terminal-accent mb-2">Message Path</h3>
                  <p className="text-xs">
                    A hierarchical identifier that routes messages to their intended destinations within
                    the system. Paths enable targeted message delivery to specific devices or components.
                  </p>
                </div>
              </div>
              
              <div className="flex justify-end">
                <Link href="/reference/glossary" className="text-terminal-green text-sm hover:underline flex items-center">
                  <span className="mr-1">View Complete Glossary</span>
                  <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M14 5l7 7m0 0l-7 7m7-7H3" />
                  </svg>
                </Link>
              </div>
            </div>
          </section>
          
          {/* Dependency Visualization Preview */}
          <section>
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Dependency Visualizations</h2>
            
            <div className="bg-[#252526] border border-[#333] rounded-md p-6">
              <p className="mb-4">
                The dependency visualizations provide interactive diagrams showing the relationships
                between modules, subsystems, and components in the HyperBEAM system.
              </p>
              
              <div className="bg-[#2d2d2d] p-5 rounded-md mb-4">
                {/* ASCII dependency diagram preview */}
                <pre className="text-xs sm:text-sm text-terminal-text font-mono whitespace-pre overflow-x-auto">
{`┌───────────┐   ┌───────────┐   ┌───────────┐   ┌───────────┐
│ hb_message │◄──┤ hb_converge│◄──┤  hb_core  │◄──┤  dev_*    │
└─────┬─────┘   └─────┬─────┘   └─────┬─────┘   └───────────┘
      │               │               │                       
      │               │               │                       
      ▼               ▼               ▼                       
┌───────────┐   ┌───────────┐   ┌───────────┐   ┌───────────┐
│  hb_path  │◄──┤   hb_util │◄──┤  hb_opts  │◄──┤ Subsystems │
└───────────┘   └───────────┘   └───────────┘   └───────────┘`}
                </pre>
              </div>
              
              <div className="flex justify-end">
                <Link href="/reference/dependency-visualization" className="text-terminal-green text-sm hover:underline flex items-center">
                  <span className="mr-1">Explore All Visualizations</span>
                  <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M14 5l7 7m0 0l-7 7m7-7H3" />
                  </svg>
                </Link>
              </div>
            </div>
          </section>
        </div>
        
        <div className="lg:col-span-1">
          <div className="sticky top-[80px] space-y-6">
            {/* Quick Navigation */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">In This Section</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <Link href="#materials" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Reference Materials
                  </Link>
                </li>
                <li>
                  <Link href="#functions" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Function Reference
                  </Link>
                </li>
                <li>
                  <Link href="#glossary" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Conceptual Glossary
                  </Link>
                </li>
                <li>
                  <Link href="#dependency" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Dependency Visualizations
                  </Link>
                </li>
              </ul>
            </div>
            
            {/* Related Sections */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Related Sections</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <Link href="/architecture" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M3 5a2 2 0 012-2h10a2 2 0 012 2v10a2 2 0 01-2 2H5a2 2 0 01-2-2V5zm11 1H6v8l4-2 4 2V6z" clipRule="evenodd" />
                    </svg>
                    Architecture Overview
                  </Link>
                </li>
                <li>
                  <Link href="/core" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM4.332 8.027a6.012 6.012 0 011.912-2.706C6.512 5.73 6.974 6 7.5 6A1.5 1.5 0 019 7.5V8a2 2 0 004 0 2 2 0 011.523-1.943A5.977 5.977 0 0116 10c0 .34-.028.675-.083 1H15a2 2 0 00-2 2v2.197A5.973 5.973 0 0110 16v-2a2 2 0 00-2-2 2 2 0 01-2-2 2 2 0 00-1.668-1.973z" clipRule="evenodd" />
                    </svg>
                    Core Infrastructure
                  </Link>
                </li>
                <li>
                  <Link href="/subsystems" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path d="M3 4a1 1 0 011-1h12a1 1 0 011 1v2a1 1 0 01-1 1H4a1 1 0 01-1-1V4zM3 10a1 1 0 011-1h6a1 1 0 011 1v6a1 1 0 01-1 1H4a1 1 0 01-1-1v-6zM14 9a1 1 0 00-1 1v6a1 1 0 001 1h2a1 1 0 001-1v-6a1 1 0 00-1-1h-2z" />
                    </svg>
                    Subsystems
                  </Link>
                </li>
                <li>
                  <Link href="/guides" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path d="M9 4.804A7.968 7.968 0 005.5 4c-1.255 0-2.443.29-3.5.804v10A7.969 7.969 0 015.5 14c1.669 0 3.218.51 4.5 1.385A7.962 7.962 0 0114.5 14c1.255 0 2.443.29 3.5.804v-10A7.968 7.968 0 0014.5 4c-1.255 0-2.443.29-3.5.804V12a1 1 0 11-2 0V4.804z" />
                    </svg>
                    Guides and Tutorials
                  </Link>
                </li>
              </ul>
            </div>
            
            {/* Documentation Stats */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Documentation Stats</h3>
              <ul className="space-y-3 text-sm">
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">438 Functions</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Fully documented with parameters, returns, and examples
                    </p>
                  </div>
                </li>
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">187 Concepts</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Detailed explanations with cross-references
                    </p>
                  </div>
                </li>
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">12 Dependency Graphs</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Interactive visualizations of system relationships
                    </p>
                  </div>
                </li>
                <li className="flex items-start">
                  <span className="text-terminal-green mr-2">•</span>
                  <div>
                    <span className="font-semibold">43 Resolved Questions</span>
                    <p className="mt-1 opacity-80 text-xs">
                      Documented solutions to common development questions
                    </p>
                  </div>
                </li>
              </ul>
            </div>
            
            {/* Terminal Explorer */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Terminal Explorer</h3>
              <Terminal initialCommands={initialCommands} height={200} />
            </div>
          </div>
        </div>
      </div>
    </>
  );
}
