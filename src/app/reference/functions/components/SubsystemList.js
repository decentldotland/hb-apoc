import React from 'react';
import Link from 'next/link';
import { moduleCategories } from '../../../../data/functions/categories';

const SubsystemList = ({ selectedModule, selectedCategory }) => {
  // Filter subsystems based on selected module
  const filteredSubsystems = selectedModule === 'all' 
    ? moduleCategories 
    : moduleCategories.filter(subsystem => subsystem.id === selectedModule);
  
  return (
    <div className="space-y-12">
      {filteredSubsystems.map((subsystem) => (
        <div key={subsystem.id} className="mb-10">
          <div className="bg-[#252526] border border-[#333] rounded-md p-5 mb-6">
            <h2 className="text-2xl font-bold text-terminal-primary mb-2">
              {subsystem.name}
            </h2>
            <p className="text-sm opacity-80 mb-4">
              {subsystem.description}
            </p>
            <div className="flex items-center justify-between">
              <div className="flex gap-2">
                <Link 
                  href={`/reference/functions/${subsystem.id}`}
                  className="text-xs bg-terminal-green text-black px-3 py-1 rounded-md hover:bg-terminal-green/80 transition-colors"
                >
                  View All {subsystem.count} Functions
                </Link>
              </div>
              <div className="text-xs opacity-70">
                {subsystem.modules.length} modules
              </div>
            </div>
          </div>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {subsystem.modules.map((module) => (
              <Link 
                key={module.id} 
                href={`/reference/functions/${subsystem.id}/${module.id}`}
                className="block"
              >
                <div className="bg-[#252526] border border-[#333] rounded-md p-4 hover:border-terminal-green transition-colors">
                  <div className="flex items-center justify-between mb-2">
                    <h3 className="font-mono text-terminal-green">
                      {module.name}
                    </h3>
                    <span className="text-xs bg-[#2d2d2d] px-2 py-1 rounded-full">
                      {module.count} functions
                    </span>
                  </div>
                  <div className="text-xs opacity-70 font-mono">
                    {module.id}
                  </div>
                </div>
              </Link>
            ))}
          </div>
        </div>
      ))}
      
      {filteredSubsystems.length === 0 && (
        <div className="bg-[#252526] border border-[#333] rounded-md p-6 text-center">
          <p className="text-terminal-accent">No matching subsystems found.</p>
        </div>
      )}
    </div>
  );
};

export default SubsystemList;
