"use client"

import { useState } from 'react';
import Header from './Header';
import Sidebar from './Sidebar';

export default function Layout({ children }) {
  const [sidebarOpen, setSidebarOpen] = useState(false);

  const toggleSidebar = () => {
    setSidebarOpen(!sidebarOpen);
  };

  return (
    <div className="min-h-screen flex flex-col">
      <Header toggleSidebar={toggleSidebar} />
      
      <div className="mt-[75px] flex">
        <div className="fixed w-[300px]">
          <Sidebar isOpen={sidebarOpen} />
        </div>
        
        <main className="flex-1 overflow-y-auto p-4 md:p-6 bg-terminal-bg ml-[300px]">
          {children}
        </main>
      </div>
    </div>
  );
}
