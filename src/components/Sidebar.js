"use client"

import { useState, useEffect } from 'react';
import Link from 'next/link';
import { usePathname } from 'next/navigation';

export default function Sidebar({ isOpen }) {
  const pathname = usePathname();
  const [collapsed, setCollapsed] = useState(false);
  
  // Restore collapsed state from localStorage on component mount
  useEffect(() => {
    const savedState = localStorage.getItem('sidebarCollapsed');
    if (savedState !== null) {
      setCollapsed(savedState === 'true');
    }
  }, []);
  
  // Save collapsed state to localStorage when it changes
  useEffect(() => {
    localStorage.setItem('sidebarCollapsed', collapsed.toString());
  }, [collapsed]);
  
  const toggleCollapse = () => {
    setCollapsed(!collapsed);
  };
  
  // Initial navigation items - these would be populated from content folders in production
  const navItems = [
    {
      title: 'Quick Navigation',
      links: [
        { href: '/', label: 'Home' },
        { href: '/architecture', label: 'Architecture Overview' },
        { href: '/core', label: 'Core Modules' },
        { href: '/devices', label: 'Device Ecosystem' },
        { href: '/subsystems', label: 'Subsystems' },
        { href: '/cross-subsystem', label: 'Cross-Subsystem Integration' },
        { href: '/reference', label: 'Reference Materials' },
        { href: '/guides', label: 'Guides & Tutorials' },
        { href: '/community', label: 'Community' },
      ]
    },
    {
      title: 'Actions',
      links: [
        { href: '/report', label: 'Report inaccuracy' },
        { href: '/ask', label: 'Ask a question' },
        { href: 'https://github.com/permaweb/HyperBEAM/tree/main', label: 'View on GitHub', external: true },
      ]
    }
  ];

  // Display different sections based on current path
  // This would be expanded in production with dynamic section loading
  const contextSections = () => {
    if (pathname && pathname.startsWith('/devices')) {
      return [
        {
          title: 'Device Categories',
          links: [
            { href: '/devices/scheduler', label: 'Scheduler Devices' },
            { href: '/devices/process', label: 'Process Devices' },
            { href: '/devices/message', label: 'Message Devices' },
            { href: '/devices/wasm', label: 'WebAssembly Devices' },
            { href: '/devices/security', label: 'Security Devices' },
            { href: '/devices/payment', label: 'Payment Devices' },
            { href: '/devices/utility', label: 'Utility Devices' },
          ]
        }
      ];
    }
    return [];
  };

  // Placeholder for icon compatibility - returns nothing
  const getIcon = () => null;

  return (
    <aside 
      className={`bg-[#252526] border-r border-[#333] overflow-y-auto
                 h-[calc(100vh-75px)] z-10 
                 transform transition-all duration-200 ease-in-out
                 flex flex-col
                 ${collapsed ? 'w-[60px] p-2' : 'w-[300px] p-4'}`}
    >
      {/* Collapse Toggle Button */}
      <button 
        onClick={toggleCollapse}
        className="self-end mb-4 text-terminal-green hover:text-terminal-accent transition-colors"
        aria-label={collapsed ? "Expand sidebar" : "Collapse sidebar"}
      >
        {collapsed ? (
          <svg xmlns="http://www.w3.org/2000/svg" className="h-5 w-5" viewBox="0 0 20 20" fill="currentColor">
            <path fillRule="evenodd" d="M10.293 15.707a1 1 0 010-1.414L14.586 10l-4.293-4.293a1 1 0 111.414-1.414l5 5a1 1 0 010 1.414l-5 5a1 1 0 01-1.414 0z" clipRule="evenodd" />
            <path fillRule="evenodd" d="M4.293 15.707a1 1 0 010-1.414L8.586 10 4.293 5.707a1 1 0 011.414-1.414l5 5a1 1 0 010 1.414l-5 5a1 1 0 01-1.414 0z" clipRule="evenodd" />
          </svg>
        ) : (
          <svg xmlns="http://www.w3.org/2000/svg" className="h-5 w-5" viewBox="0 0 20 20" fill="currentColor">
            <path fillRule="evenodd" d="M15.707 15.707a1 1 0 01-1.414 0l-5-5a1 1 0 010-1.414l5-5a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 010 1.414zm-6 0a1 1 0 01-1.414 0l-5-5a1 1 0 010-1.414l5-5a1 1 0 011.414 1.414L5.414 10l4.293 4.293a1 1 0 010 1.414z" clipRule="evenodd" />
          </svg>
        )}
      </button>

      {/* Show navigation items only when not collapsed */}
      {!collapsed && (
        <>
          {navItems.map((section, i) => (
            <div key={i} className="mb-6">
              <h3 className="text-terminal-prompt mb-2 text-sm font-semibold">{section.title}</h3>
              <ul className="space-y-1">
                {section.links.map((link, j) => (
                  <li key={j}>
                    {link.external ? (
                      <a 
                        href={link.href} 
                        className="flex items-center py-1 px-2 rounded hover:bg-[#333] terminal-link"
                        target="_blank"
                        rel="noopener noreferrer"
                        title={link.label}
                      >
                        <span className="ml-3">{link.label}</span>
                      </a>
                    ) : (
                      <Link 
                        href={link.href}
                        className={`flex items-center py-1 px-2 rounded hover:bg-[#333] terminal-link
                                  ${pathname === link.href ? 'bg-[#333]' : ''}`}
                        title={link.label}
                      >
                        <span className="ml-3">{link.label}</span>
                      </Link>
                    )}
                  </li>
                ))}
              </ul>
            </div>
          ))}

          {contextSections().map((section, i) => (
            <div key={`context-${i}`} className="mb-6">
              <h3 className="text-terminal-prompt mb-2 text-sm font-semibold">{section.title}</h3>
              <ul className="space-y-1">
                {section.links.map((link, j) => (
                  <li key={j}>
                    <Link 
                      href={link.href}
                      className={`flex items-center py-1 px-2 rounded hover:bg-[#333] terminal-link
                                ${pathname === link.href ? 'bg-[#333]' : ''}`}
                      title={link.label}
                    >
                      <span className="ml-3">{link.label}</span>
                    </Link>
                  </li>
                ))}
              </ul>
            </div>
          ))}
        </>
      )}
    </aside>
  );
}
