import Link from 'next/link';
import Terminal from '../components/Terminal';

export const metadata = {
  title: 'HyperBEAM Apocryphal',
  description: 'Documentation for the HyperBEAM system, a powerful device-based architecture.'
};

export default function Home() {
  // Sample welcome terminal sequence
  const welcomeCommands = [
    { 
      command: 'welcome', 
      output: `
██╗  ██╗██╗   ██╗██████╗ ███████╗██████╗ ██████╗ ███████╗ █████╗ ███╗   ███╗
██║  ██║╚██╗ ██╔╝██╔══██╗██╔════╝██╔══██╗██╔══██╗██╔════╝██╔══██╗████╗ ████║
███████║ ╚████╔╝ ██████╔╝█████╗  ██████╔╝██████╔╝█████╗  ███████║██╔████╔██║
██╔══██║  ╚██╔╝  ██╔═══╝ ██╔══╝  ██╔══██╗██╔══██╗██╔══╝  ██╔══██║██║╚██╔╝██║
██║  ██║   ██║   ██║     ███████╗██║  ██║██████╔╝███████╗██║  ██║██║ ╚═╝ ██║
╚═╝  ╚═╝   ╚═╝   ╚═╝     ╚══════╝╚═╝  ╚═╝╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝
                                                                             
Apocrypahl knowledge - v0.9.2
Documentation current with HyperBEAM main branch as of 27.03.2025
A public good initiative by EntityC and Decent Land Labs

Type 'help' to see available commands or explore the sections below.`
    },
    {
      command: 'help',
      output: `
Available commands:
  architecture   - View system architecture overview
  core           - Explore core infrastructure
  devices        - Browse device ecosystem
  subsystems     - Explore system subsystems
  cross-subsystem - View cross-subsystem integration
  search <query> - Search documentation
`
    }
  ];

  // Main sections data
  const mainSections = [
    {
      id: 'architecture',
      title: 'Architecture Overview',
      description: 'High-level architecture explanation, system diagrams, and core principles.',
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M19.5 14.25v-2.625a3.375 3.375 0 00-3.375-3.375h-1.5A1.125 1.125 0 0113.5 7.125v-1.5a3.375 3.375 0 00-3.375-3.375H8.25m0 12.75h7.5m-7.5 3H12M10.5 2.25H5.625c-.621 0-1.125.504-1.125 1.125v17.25c0 .621.504 1.125 1.125 1.125h12.75c.621 0 1.125-.504 1.125-1.125V11.25a9 9 0 00-9-9z" />
        </svg>
      ),
      color: 'text-blue-400',
      link: '/architecture'
    },
    {
      id: 'core',
      title: 'Core Infrastructure',
      description: 'Detailed documentation of core modules, their dependencies, and implementation patterns.',
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4" />
        </svg>
      ),
      color: 'text-indigo-400',
      link: '/core'
    },
    {
      id: 'devices',
      title: 'Device Ecosystem',
      description: 'Comprehensive overview of all devices, their relationships, and integration patterns.',
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 3v2m6-2v2M9 19v2m6-2v2M5 9H3m2 6H3m18-6h-2m2 6h-2M7 19h10a2 2 0 002-2V7a2 2 0 00-2-2H7a2 2 0 00-2 2v10a2 2 0 002 2zM9 9h6v6H9V9z" />
        </svg>
      ),
      color: 'text-green-400',
      link: '/devices'
    },
    {
      id: 'subsystems',
      title: 'Subsystems',
      description: 'Specialized subsystems for storage, network, codec, Arweave integration, and app management.',
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M4 7v10c0 2.21 3.582 4 8 4s8-1.79 8-4V7M4 7c0 2.21 3.582 4 8 4s8-1.79 8-4M4 7c0-2.21 3.582-4 8-4s8 1.79 8 4m0 5c0 2.21-3.582 4-8 4s-8-1.79-8-4" />
        </svg>
      ),
      color: 'text-amber-400',
      link: '/subsystems'
    },
    {
      id: 'cross-subsystem',
      title: 'Cross-Subsystem Integration',
      description: 'Integration patterns for subsystem communication, delegation, and composition.',
      icon: (
        <svg xmlns="http://www.w3.org/2000/svg" className="h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M3 15a4 4 0 004 4h9a5 5 0 10-.1-9.999 5.002 5.002 0 10-9.78 2.096A4.001 4.001 0 003 15z" />
        </svg>
      ),
      color: 'text-rose-400',
      link: '/cross-subsystem'
    }
  ];

  // Featured subsystems
  const featuredSubsystems = [
    {
      name: 'Storage',
      description: 'Reliable data persistence with multiple backends and caching',
      link: '/subsystems/storage'
    },
    {
      name: 'Network',
      description: 'Communication between instances and external services',
      link: '/subsystems/network'
    },
    {
      name: 'Codec',
      description: 'Data serialization with schema validation and transformation',
      link: '/subsystems/codec'
    },
    {
      name: 'Arweave',
      description: 'Permanent storage on the Arweave permaweb blockchain',
      link: '/subsystems/arweave'
    },
    {
      name: 'App Management',
      description: 'Application lifecycle and state management framework',
      link: '/subsystems/app-management'
    }
  ];

  return (
    <>

      {/* Welcome Terminal */}
      <div className="mb-10">
        <div className="bg-[#252526] border border-[#333] rounded-md p-4 h-72">
          <Terminal initialCommands={welcomeCommands} />
        </div>
      </div>

      {/* Main Sections */}
      <section className="mb-10">
        <h2 className="text-2xl font-bold text-terminal-primary mb-6">Documentation Sections</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {mainSections.map((section) => (
            <Link href={section.link} key={section.id} className="block">
              <div className="bg-[#252526] border border-[#333] rounded-md p-6 h-full hover:border-terminal-green transition-all duration-200 card-hover-anim">
                <div className={`${section.color} mb-4`}>
                  {section.icon}
                </div>
                <h3 className="text-xl font-semibold text-terminal-primary mb-2">
                  {section.title}
                </h3>
                <p className="text-sm opacity-80 mb-4">
                  {section.description}
                </p>
                <div className="flex justify-end">
                  <span className="text-terminal-green text-sm hover:underline flex items-center">
                    <span className="mr-1 font-mono">$ cd {section.id}</span>
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M14 5l7 7m0 0l-7 7m7-7H3" />
                    </svg>
                  </span>
                </div>
              </div>
            </Link>
          ))}
        </div>
      </section>

      {/* Architecture Diagram */}
      <section className="mb-10">
        <h2 className="text-2xl font-bold text-terminal-primary mb-4">Architecture Overview</h2>
        <div className="bg-[#252526] border border-[#333] rounded-md p-6">
          <p className="mb-6">
            HyperBEAM implements a layered architecture with device-based extensibility.
            The system is organized into specialized subsystems that work together through well-defined integration points.
          </p>
          <div className="bg-[#2d2d2d] p-5 rounded-md mb-4">
            {/* ASCII architecture diagram */}
            <pre className="text-xs sm:text-sm text-terminal-text font-mono whitespace-pre overflow-x-auto">
{`┌────────────────────────────────────────────────────────────┐
│                    CLIENT INTERFACES                        │
└─────────────────────────┬────────────────────────────────────┘
                          │
┌─────────────────────────▼────────────────────────────────────┐
│                    MESSAGE PROCESSING                        │
└─────────────────────────┬────────────────────────────────────┘
                          │
┌─────────────────────────▼────────────────────────────────────┐
│                    DEVICE ECOSYSTEM                          │
├────────────┬──────────┬──────────┬────────────┬──────────────┤
│    Core    │ Runtime  │ Security │  Payment   │    Utility   │
│   Devices  │ Devices  │ Devices  │  Devices   │    Devices   │
└────────────┴──────────┴────┬─────┴────────────┴──────────────┘
                             │
┌────────────────────────────▼────────────────────────────────┐
│                      SUBSYSTEMS                             │
├───────────┬────────────┬────────────┬────────────┬──────────┤
│  Storage  │  Network   │   Codec    │  Arweave   │   App    │
│           │            │            │            │ Management│
└───────────┴────────────┴────────────┴────────────┴──────────┘`}
            </pre>
          </div>
          <div className="flex justify-end">
            <Link href="/architecture" className="text-terminal-green text-sm hover:underline flex items-center">
              <span className="mr-1">Explore Full Architecture</span>
              <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M14 5l7 7m0 0l-7 7m7-7H3" />
              </svg>
            </Link>
          </div>
        </div>
      </section>

      {/* Featured Subsystems */}
      <section className="mb-10">
        <h2 className="text-2xl font-bold text-terminal-primary mb-4">Featured Subsystems</h2>
        <div className="bg-[#252526] border border-[#333] rounded-md p-6">
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {featuredSubsystems.map((subsystem, index) => (
              <Link href={subsystem.link} key={index} className="block">
                <div className="p-4 border border-[#333] rounded-md hover:border-terminal-green transition-colors card-hover-anim">
                  <h3 className="text-lg font-semibold text-terminal-primary mb-2 font-mono">
                    <span className="text-terminal-green">$</span> cd subsystems/{subsystem.name.toLowerCase()}
                  </h3>
                  <p className="text-sm opacity-80">{subsystem.description}</p>
                </div>
              </Link>
            ))}
          </div>
          <div className="flex justify-end mt-4">
            <Link href="/subsystems" className="text-terminal-green text-sm hover:underline flex items-center">
              <span className="mr-1">View All Subsystems</span>
              <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M14 5l7 7m0 0l-7 7m7-7H3" />
              </svg>
            </Link>
          </div>
        </div>
      </section>

      {/* Getting Started */}
      <section className="mb-10">
        <h2 className="text-2xl font-bold text-terminal-primary mb-4">Getting Started</h2>
        <div className="bg-[#252526] border border-[#333] rounded-md p-6">
          <p className="mb-6">
            New to HyperBEAM? Start with these resources to get familiar with the system:
          </p>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div className="p-4 border border-[#333] rounded-md">
              <h3 className="text-lg font-semibold text-terminal-green mb-2">Core Concepts</h3>
              <p className="text-sm mb-4">Understand the fundamental concepts that power HyperBEAM:</p>
              <ul className="space-y-1 text-sm">
                <li className="flex items-center">
                  <span className="text-terminal-green mr-2">•</span>
                  <Link href="/core/hb_converge" className="hover:text-terminal-green">Convergence Protocol</Link>
                </li>
                <li className="flex items-center">
                  <span className="text-terminal-green mr-2">•</span>
                  <Link href="/core/hb_message" className="hover:text-terminal-green">Message Processing</Link>
                </li>
                <li className="flex items-center">
                  <span className="text-terminal-green mr-2">•</span>
                  <Link href="/devices" className="hover:text-terminal-green">Device Ecosystem</Link>
                </li>
              </ul>
            </div>
            <div className="p-4 border border-[#333] rounded-md">
              <h3 className="text-lg font-semibold text-terminal-green mb-2">Practical Examples</h3>
              <p className="text-sm mb-4">See HyperBEAM in action with these examples:</p>
              <ul className="space-y-1 text-sm">
                <li className="flex items-center">
                  <span className="text-terminal-green mr-2">•</span>
                  <Link href="/subsystems/storage#usage" className="hover:text-terminal-green">Storage Operations</Link>
                </li>
                <li className="flex items-center">
                  <span className="text-terminal-green mr-2">•</span>
                  <Link href="/subsystems/network#usage" className="hover:text-terminal-green">Network Communication</Link>
                </li>
                <li className="flex items-center">
                  <span className="text-terminal-green mr-2">•</span>
                  <Link href="/cross-subsystem#examples" className="hover:text-terminal-green">Cross-Subsystem Integration</Link>
                </li>
              </ul>
            </div>
          </div>
        </div>
      </section>

      {/* Version Info */}
      <section>
        <div className="bg-[#252526] border border-[#333] rounded-md p-4 flex flex-col sm:flex-row justify-between items-center text-sm">
          <div>
            <span className="text-terminal-accent">Documentation current with HyperBEAM main branch as of 27.03.2025</span>
          </div>
          <div className="flex mt-2 sm:mt-0">
            <a 
              href="https://github.com/permaweb/HyperBEAM/tree/main" 
              target="_blank" 
              rel="noopener noreferrer"
              className="text-terminal-green hover:underline flex items-center"
            >
              <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-1" viewBox="0 0 24 24" fill="currentColor">
                <path fillRule="evenodd" clipRule="evenodd" d="M12 2C6.477 2 2 6.477 2 12c0 4.42 2.865 8.167 6.839 9.489.5.092.682-.217.682-.482 0-.237-.008-.866-.013-1.7-2.782.603-3.369-1.342-3.369-1.342-.454-1.155-1.11-1.462-1.11-1.462-.908-.62.069-.608.069-.608 1.003.07 1.531 1.03 1.531 1.03.892 1.529 2.341 1.088 2.91.832.092-.647.35-1.088.636-1.338-2.22-.253-4.555-1.11-4.555-4.943 0-1.091.39-1.984 1.029-2.683-.103-.253-.446-1.27.098-2.647 0 0 .84-.269 2.75 1.025A9.578 9.578 0 0112 6.836c.85.004 1.705.114 2.504.336 1.909-1.294 2.747-1.025 2.747-1.025.546 1.377.202 2.394.1 2.647.64.699 1.028 1.592 1.028 2.683 0 3.842-2.339 4.687-4.566 4.935.359.309.678.919.678 1.852 0 1.336-.012 2.415-.012 2.741 0 .267.18.578.688.479C19.137 20.164 22 16.418 22 12c0-5.523-4.477-10-10-10z" />
              </svg>
              GitHub Repository
            </a>
          </div>
        </div>
      </section>
    </>
  );
}
