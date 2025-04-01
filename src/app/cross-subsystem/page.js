import Link from 'next/link';
import Terminal from '../../components/Terminal';
import integrationData from '../../data/cross-subsystem/integration-data.json';

export default function CrossSubsystem() {
  // Terminal commands
  const initialCommands = [
    { 
      command: 'ls -la cross-subsystem/', 
      output: `total 4
drwxr-xr-x 2 hyperbeam system 4096 Mar 27 2025 ./
drwxr-xr-x 5 hyperbeam system 4096 Mar 27 2025 ../
-rw-r--r-- 1 hyperbeam system 14672 Mar 27 2025 hb_integration.erl
-rw-r--r-- 1 hyperbeam system 12986 Mar 27 2025 hb_adapter.erl
-rw-r--r-- 1 hyperbeam system 10458 Mar 27 2025 hb_bridge.erl
-rw-r--r-- 1 hyperbeam system 11235 Mar 27 2025 hb_event_bus.erl`
    }
  ];

  return (
    <>
      <div className="mb-8">
        <div className="flex items-center gap-2 text-sm mb-4 text-gray-400">
          <Link href="/" className="hover:text-terminal-green">Home</Link>
          <span>/</span>
          <span className="text-terminal-green">Cross-Subsystem Integration</span>
        </div>
        
        <h1 className="text-3xl font-bold text-terminal-primary mb-4">
          {integrationData.overview.title}
        </h1>
        
        <p className="text-lg mb-8">
          {integrationData.overview.description}
        </p>
        
        <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6 flex items-center justify-between">
          <div className="font-mono text-sm">
            <span className="text-terminal-green">$</span> 
            <span className="text-terminal-accent ml-2">cd</span> 
            <span className="text-terminal-output ml-2">cross-subsystem/</span>
          </div>
          <div className="bg-[#333] px-3 py-1 rounded text-xs font-mono">
            {integrationData.modules.length} integration modules | {integrationData.patterns.length} integration patterns
          </div>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-8 mb-12">
        <div className="lg:col-span-2">
          {/* Overview Section */}
          <section className="bg-[#252526] border border-[#333] rounded-md p-6 mb-8" id="overview">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Overview</h2>
            
            <div className="space-y-4">
              {integrationData.overview.details.map((detail, index) => (
                <p key={index}>{detail}</p>
              ))}

              <div className="bg-[#2d2d2d] p-5 rounded-md mt-6">
                <div className="text-sm font-mono text-terminal-accent mb-2">// {integrationData.overview.diagram.title}</div>
                <pre className="text-xs sm:text-sm text-terminal-text font-mono whitespace-pre overflow-x-auto">
                  {integrationData.overview.diagram.ascii}
                </pre>
              </div>
            </div>
          </section>
          
          {/* Key Integration Insights */}
          <section className="mb-8" id="insights">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Key Cross-Subsystem Integration Insights</h2>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-5">
              {integrationData.keyInsights.map((insight) => (
                <div 
                  key={insight.id}
                  className="bg-[#252526] border border-[#333] rounded-md p-5 hover:border-terminal-green transition-colors card-hover-anim"
                >
                  <div>
                    <h3 className="text-lg font-semibold mb-2 text-terminal-primary">
                      {insight.name}
                    </h3>
                    <p className="text-sm opacity-80 mb-3">{insight.description}</p>
                    <ul className="text-xs space-y-1 pl-4 list-disc opacity-80">
                      {insight.points.map((point, index) => (
                        <li key={index}>{point}</li>
                      ))}
                    </ul>
                  </div>
                </div>
              ))}
            </div>
          </section>
          
          {/* Common Architectural Patterns */}
          <section className="mb-8" id="patterns">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Common Cross-Subsystem Architectural Patterns</h2>
            
            <div className="space-y-6">
              {integrationData.patterns.map((pattern) => (
                <div key={pattern.id} className="bg-[#252526] border border-[#333] rounded-md p-6">
                  <h3 className="text-lg font-semibold text-terminal-accent mb-3">{pattern.name}</h3>
                  <p className="text-sm mb-4">
                    {pattern.description}
                  </p>
                  <pre className="bg-[#2d2d2d] p-4 text-sm rounded overflow-x-auto font-mono text-terminal-text mb-4">
                    {pattern.flow}
                  </pre>
                  <div>
                    <h4 className="text-sm font-semibold mb-2">Key Aspects:</h4>
                    <ul className="text-xs space-y-1 pl-4 list-disc opacity-80">
                      {pattern.aspects.map((aspect, index) => (
                        <li key={index}>{aspect}</li>
                      ))}
                    </ul>
                  </div>
                </div>
              ))}
            </div>
          </section>
          
          {/* Architectural Trade-offs */}
          <section className="mb-8" id="tradeoffs">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Architectural Trade-offs</h2>
            
            <div className="bg-[#252526] border border-[#333] rounded-md p-6">
              {integrationData.tradeoffs.map((tradeoff, tIndex) => (
                <div key={tIndex} className={tIndex < integrationData.tradeoffs.length - 1 ? "mb-6" : ""}>
                  <h3 className="text-lg font-semibold text-terminal-accent mb-3">{tradeoff.title}</h3>
                  <ul className="space-y-2 text-sm pl-5 list-disc">
                    {tradeoff.points.map((point, pIndex) => (
                      <li key={pIndex}>{point}</li>
                    ))}
                  </ul>
                </div>
              ))}
            </div>
          </section>
          
          {/* Best Practices */}
          <section className="mb-8" id="practices">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Cross-Subsystem Integration Best Practices</h2>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              {integrationData.bestPractices.map((practice, index) => (
                <div key={index} className="bg-[#252526] border border-[#333] rounded-md p-5">
                  <h3 className="text-md font-semibold text-terminal-accent mb-3">{practice.title}</h3>
                  <ul className="text-sm space-y-1 pl-4 list-disc">
                    {practice.practices.map((item, i) => (
                      <li key={i}>{item}</li>
                    ))}
                  </ul>
                </div>
              ))}
            </div>
          </section>
          
          {/* Future Directions */}
          <section id="future">
            <h2 className="text-2xl font-bold text-terminal-primary mb-4">Future Integration Directions</h2>
            
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-5">
              {integrationData.futureDirections.map((direction, index) => (
                <div key={index} className="bg-[#252526] border border-[#333] rounded-md p-5">
                  <h3 className="text-md font-semibold text-terminal-accent mb-3">{direction.title}</h3>
                  <ul className="text-sm space-y-2 pl-4 list-disc opacity-80">
                    {direction.items.map((item, i) => (
                      <li key={i}>{item}</li>
                    ))}
                  </ul>
                </div>
              ))}
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
                  <Link href="#overview" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Overview
                  </Link>
                </li>
                <li>
                  <Link href="#insights" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Key Integration Insights
                  </Link>
                </li>
                <li>
                  <Link href="#patterns" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Common Architectural Patterns
                  </Link>
                </li>
                <li>
                  <Link href="#tradeoffs" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Architectural Trade-offs
                  </Link>
                </li>
                <li>
                  <Link href="#practices" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Best Practices
                  </Link>
                </li>
                <li>
                  <Link href="#future" className="text-terminal-text hover:text-terminal-green transition-colors block">
                    Future Directions
                  </Link>
                </li>
              </ul>
            </div>
            
            {/* Related Sections */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Related Sections</h3>
              <ul className="space-y-2 text-sm">
                <li>
                  <Link href="/subsystems" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path d="M3 4a1 1 0 011-1h12a1 1 0 011 1v2a1 1 0 01-1 1H4a1 1 0 01-1-1V4zM3 10a1 1 0 011-1h6a1 1 0 011 1v6a1 1 0 01-1 1H4a1 1 0 01-1-1v-6zM14 9a1 1 0 00-1 1v6a1 1 0 001 1h2a1 1 0 001-1v-6a1 1 0 00-1-1h-2z" />
                    </svg>
                    Subsystems Overview
                  </Link>
                </li>
                <li>
                  <Link href="/architecture" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M3 5a2 2 0 012-2h10a2 2 0 012 2v10a2 2 0 01-2 2H5a2 2 0 01-2-2V5zm11 1H6v8l4-2 4 2V6z" clipRule="evenodd" />
                    </svg>
                    Architecture Overview
                  </Link>
                </li>
                <li>
                  <Link href="/devices" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M11 3a1 1 0 10-2 0v1a1 1 0 102 0V3zM6.293 6.707a1 1 0 010-1.414l.7-.7a1 1 0 111.414 1.414l-.7.7a1 1 0 01-1.414 0zM15.657 8l.9-.9a1 1 0 10-1.414-1.414l-.9.9A5 5 0 118 15.657l-.9.9a1 1 0 001.414 1.414l.9-.9A7 7 0 0015.657 8z" clipRule="evenodd" />
                    </svg>
                    Device Ecosystem
                  </Link>
                </li>
                <li>
                  <Link href="/core" className="text-terminal-text hover:text-terminal-green flex items-center">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4 mr-2 text-terminal-accent" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM4.332 8.027a6.012 6.012 0 011.912-2.706C6.512 5.73 6.974 6 7.5 6A1.5 1.5 0 019 7.5V8a2 2 0 004 0 2 2 0 011.523-1.943A5.977 5.977 0 0116 10c0 .34-.028.675-.083 1H15a2 2 0 00-2 2v2.197A5.973 5.973 0 0110 16v-2a2 2 0 00-2-2 2 2 0 01-2-2 2 2 0 00-1.668-1.973z" clipRule="evenodd" />
                    </svg>
                    Core Modules
                  </Link>
                </li>
              </ul>
            </div>
            
            {/* Integration Modules */}
            <div className="bg-[#252526] border border-[#333] rounded-md p-4">
              <h3 className="text-lg font-semibold mb-3 text-terminal-green">Integration Modules</h3>
              <ul className="space-y-3 text-sm">
                {integrationData.modules.map((module, index) => (
                  <li key={index} className="flex items-start">
                    <span className="text-terminal-green mr-2">•</span>
                    <div>
                      <span className="font-semibold">{module.name}</span>
                      <p className="mt-1 opacity-80 text-xs">
                        {module.description}
                      </p>
                    </div>
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
