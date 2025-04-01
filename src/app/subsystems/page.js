import Link from 'next/link';
import Terminal from '../../components/Terminal';

export const metadata = {
  title: 'Subsystems - HyperBEAM Documentation',
  description: 'Explore HyperBEAM\'s specialized subsystems that work together to create a powerful and flexible platform for distributed applications.'
};

export default function SubsystemsIndex() {
  // Sample terminal commands for subsystems overview
  const initialCommands = [
    { 
      command: 'ls -la subsystems/', 
      output: `total 5
drwxr-xr-x 7 hyperbeam system 4096 Mar 27 2025 ./
drwxr-xr-x 5 hyperbeam system 4096 Mar 27 2025 ../
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 storage/
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 network/
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 codec/
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 arweave/
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 app-management/`
    }
  ];

  // Subsystems data
  const subsystems = [
    {
      id: 'storage',
      name: 'Storage Subsystem',
      description: 'Reliable and efficient data persistence with multiple backend options and integrated caching system.',
      modules: 9,
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-8 w-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M5 8h14M5 8a2 2 0 110-4h14a2 2 0 110 4M5 8v10a2 2 0 002 2h10a2 2 0 002-2V8m-9 4h4" />
        </svg>
      ),
      color: 'from-blue-500 to-cyan-500'
    },
    {
      id: 'network',
      name: 'Network Communication',
      description: 'Reliable data exchange between HyperBEAM instances and external services through protocols and clients.',
      modules: 8,
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-8 w-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M8 12h.01M12 12h.01M16 12h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
        </svg>
      ),
      color: 'from-purple-500 to-indigo-500'
    },
    {
      id: 'codec',
      name: 'Codec and Data Format',
      description: 'Flexible framework for serializing and deserializing data with schema validation and transformation.',
      modules: 7,
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-8 w-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M7 8h10M7 12h4m1 8l-4-4H5a2 2 0 01-2-2V6a2 2 0 012-2h14a2 2 0 012 2v8a2 2 0 01-2 2h-3l-4 4z" />
        </svg>
      ),
      color: 'from-green-500 to-teal-500'
    },
    {
      id: 'arweave',
      name: 'Arweave Integration',
      description: 'Comprehensive interface for the Arweave permaweb blockchain for permanent data storage and applications.',
      modules: 7,
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-8 w-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4" />
        </svg>
      ),
      color: 'from-yellow-500 to-amber-500'
    },
    {
      id: 'app-management',
      name: 'Application Management',
      description: 'Framework for deploying, running, and managing applications with lifecycle and state persistence support.',
      modules: 7,
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-8 w-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 3v2m6-2v2M9 19v2m6-2v2M5 9H3m2 6H3m18-6h-2m2 6h-2M7 19h10a2 2 0 002-2V7a2 2 0 00-2-2H7a2 2 0 00-2 2v10a2 2 0 002 2zM9 9h6v6H9V9z" />
        </svg>
      ),
      color: 'from-rose-500 to-pink-500'
    }
  ];

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <span className="text-terminal-green">Subsystems</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          HyperBEAM Subsystems
        </h1>
        
        <p className="text-lg mb-8">
          HyperBEAM is organized into specialized subsystems that work together to create a powerful and
          flexible platform. Each subsystem focuses on a specific area of functionality and can be used 
          independently or in combination with others.
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6 flex items-center justify-between">
          <div className="font-mono text-sm">
            <span className="text-terminal-green">$</span> 
            <span className="text-terminal-accent ml-2">ls</span> 
            <span className="text-terminal-output ml-2">--sort=type subsystems/</span>
          </div>
          <div className="bg-[#333] px-3 py-1 rounded text-xs font-mono">
            5 subsystems | 38 modules | 307 functions
          </div>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-8 mb-12">
        <div className="lg:col-span-2">
          {/* Subsystem Architecture Overview */}
          <section className="bg-[#252526] border border-[#333] rounded-md p-6 mb-8">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Subsystem Architecture</h2>
            
            <div className="space-y-6">
              <p>
                HyperBEAM's architecture is organized into interconnected subsystems that work together to provide a
                complete platform for distributed applications. This modular design allows for flexibility,
                extensibility, and clean separation of concerns.
              </p>
              
              <div className="bg-[#2d2d2d] p-5 rounded-md">
                {/* ASCII architecture diagram */}
                <pre className="text-xs sm:text-sm text-terminal-text font-mono whitespace-pre overflow-x-auto">
{`┌───────────────────────────────────────────────────────────────┐
│                     APPLICATION LAYER                          │
└─────────────────────────────┬─────────────────────────────────┘
                              │
┌─────────────────────────────▼─────────────────────────────────┐
│                  APPLICATION MANAGEMENT                        │
└───┬─────────────────────────┬─────────────────────────────────┘
    │                         │
┌───▼─────────────┐     ┌─────▼─────────────────────────────────┐
│    STORAGE      │     │             DEVICE ECOSYSTEM           │
└───┬─────────────┘     └─────────────────────┬─────────────────┘
    │                                         │
┌───▼─────────────┐                     ┌─────▼─────────────────┐
│    CODEC        │◄────────────────────►     NETWORK           │
└───┬─────────────┘                     └─────┬─────────────────┘
    │                                         │
    │             ┌───────────────────────┐   │
    └─────────────►       ARWEAVE         ◄───┘
                  └───────────────────────┘`}
                </pre>
              </div>
              
              <p className="text-sm">
                The subsystem architecture provides:
              </p>
              
              <ul className="space-y-2 text-sm pl-5 list-disc">
                <li>
                  <strong>Clear Responsibility Boundaries</strong> - Each subsystem has a well-defined area of responsibility
                </li>
                <li>
                  <strong>Simplified Dependencies</strong> - Dependencies between subsystems are explicit and controlled
                </li>
                <li>
                  <strong>Flexible Deployment</strong> - Subsystems can be deployed independently in different configurations
                </li>
                <li>
                  <strong>Independent Evolution</strong> - Each subsystem can evolve at its own pace
                </li>
                <li>
                  <strong>Focused Testing</strong> - Subsystems can be tested in isolation, simplifying quality assurance
                </li>
              </ul>
            </div>
          </section>
          
          {/* Subsystems Grid */}
          <section>
            <h2 className="text-2xl font-bold text-terminal-primary mb-6">Subsystem Overview</h2>
            
            <div className="grid grid-cols-1 gap-6">
              {subsystems.map((subsystem) => (
                <Link href={`/subsystems/${subsystem.id}`} key={subsystem.id} className="block">
                  <div className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-all duration-200 card-hover-anim">
                    <div className="flex">
                      <div className={`text-white rounded-md p-4 mr-5 bg-gradient-to-br ${subsystem.color}`}>
                        {subsystem.icon}
                      </div>
                      <div className="flex-1">
                        <h3 className="text-xl font-semibold mb-2 text-terminal-primary flex items-center">
                          {subsystem.name}
                          <span className="ml-3 px-2 py-1 text-xs bg-[#333] rounded-full text-terminal-accent">
                            {subsystem.modules} modules
                          </span>
                        </h3>
                        <p className="text-sm mb-4 opacity-80">{subsystem.description}</p>
                        <div className="flex justify-end">
                          <span className="text-terminal-green text-sm hover:underline flex items-center">
                            <span className="mr-1">Explore Subsystem</span>
                            <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M14 5l7 7m0 0l-7 7m7-7H3" />
                            </svg>
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </Link>
              ))}
            </div>
          </section>
        </div>
        
        <div className="lg:col-span-1">
          <div className="sticky top-[80px] space-y-6">
            {/* Quick Navigation */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Quick Navigation</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <Link href="/architecture" className="text-terminal-text hover:text-terminal-green transition-colors flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M3 5a2 2 0 012-2h10a2 2 0 012 2v10a2 2 0 01-2 2H5a2 2 0 01-2-2V5zm11 1H6v8l4-2 4 2V6z" clipRule="evenodd" />
                    </svg>
                    Architecture Overview
                  </Link>
                </li>
                <li>
                  <Link href="/devices" className="text-terminal-text hover:text-terminal-green transition-colors flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M11 3a1 1 0 10-2 0v1a1 1 0 102 0V3zM6.293 6.707a1 1 0 010-1.414l.7-.7a1 1 0 111.414 1.414l-.7.7a1 1 0 01-1.414 0zM15.657 8l.9-.9a1 1 0 10-1.414-1.414l-.9.9A5 5 0 118 15.657l-.9.9a1 1 0 001.414 1.414l.9-.9A7 7 0 0015.657 8z" clipRule="evenodd" />
                    </svg>
                    Device Ecosystem
                  </Link>
                </li>
                <li>
                  <Link href="/core" className="text-terminal-text hover:text-terminal-green transition-colors flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM4.332 8.027a6.012 6.012 0 011.912-2.706C6.512 5.73 6.974 6 7.5 6A1.5 1.5 0 019 7.5V8a2 2 0 004 0 2 2 0 011.523-1.943A5.977 5.977 0 0116 10c0 .34-.028.675-.083 1H15a2 2 0 00-2 2v2.197A5.973 5.973 0 0110 16v-2a2 2 0 00-2-2 2 2 0 01-2-2 2 2 0 00-1.668-1.973z" clipRule="evenodd" />
                    </svg>
                    Core System
                  </Link>
                </li>
                <li>
                  <Link href="/cross-subsystem" className="text-terminal-text hover:text-terminal-green transition-colors flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z" clipRule="evenodd" />
                    </svg>
                    Cross-Subsystem Integration
                  </Link>
                </li>
              </ul>
            </div>
            
            {/* Subsystems */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">All Subsystems</h3>
              <ul className="space-y-3 text-sm">
                {subsystems.map((subsystem) => (
                  <li key={subsystem.id}>
                    <Link href={`/subsystems/${subsystem.id}`} className="text-terminal-text hover:text-terminal-green flex items-center">
                      <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                        <path d="M3 4a1 1 0 011-1h12a1 1 0 011 1v2a1 1 0 01-1 1H4a1 1 0 01-1-1V4zM3 10a1 1 0 011-1h6a1 1 0 011 1v6a1 1 0 01-1 1H4a1 1 0 01-1-1v-6zM14 9a1 1 0 00-1 1v6a1 1 0 001 1h2a1 1 0 001-1v-6a1 1 0 00-1-1h-2z" />
                      </svg>
                      {subsystem.name}
                    </Link>
                  </li>
                ))}
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
