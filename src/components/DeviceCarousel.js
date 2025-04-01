import { useState, useRef, useEffect } from 'react';
import Link from 'next/link';

export default function DeviceCarousel() {
  const carouselRef = useRef(null);
  const [scrollPosition, setScrollPosition] = useState(0);
  const [maxScroll, setMaxScroll] = useState(0);
  
  // Comprehensive list of devices
  const allDevices = [
    // Core Devices
    {
      name: 'dev_message.erl',
      description: 'Core identity device with 14 downstream dependents, provides message field access, manipulation, and attestation handling.',
      category: 'Core',
      docLink: '/devices/dev_message',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_message.erl'
    },
    {
      name: 'dev_process.erl',
      description: 'Core orchestration module with 4 downstream dependents, 19 outgoing dependencies, routes operations to specialized devices.',
      category: 'Core',
      docLink: '/devices/dev_process',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_process.erl'
    },
    {
      name: 'dev_scheduler.erl',
      description: 'Core scheduler device with 1 downstream dependent, 29 outgoing dependencies, manages process execution order.',
      category: 'Core',
      docLink: '/devices/dev_scheduler',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_scheduler.erl'
    },
    {
      name: 'dev_stack.erl',
      description: 'Meta-device with 3 downstream dependents, manages execution of device stacks in fold or map mode, enabling complex device pipelines.',
      category: 'Core',
      docLink: '/devices/dev_stack',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_stack.erl'
    },
    {
      name: 'dev_process_cache.erl',
      description: 'Specialized caching module with dual-indexing for process computation results by slot and message ID.',
      category: 'Core',
      docLink: '/devices/dev_process_cache',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_process_cache.erl'
    },
    {
      name: 'dev_process_worker.erl',
      description: 'Long-lived worker module that maintains in-memory state between computation steps for efficiency.',
      category: 'Core',
      docLink: '/devices/dev_process_worker',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_process_worker.erl'
    },
    // Scheduler Devices
    {
      name: 'dev_scheduler_cache.erl',
      description: 'Specialized caching system for scheduler assignments with storage and retrieval of slot-based assignments.',
      category: 'Scheduler',
      docLink: '/devices/dev_scheduler_cache',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_scheduler_cache.erl'
    },
    {
      name: 'dev_scheduler_formats.erl',
      description: 'Format conversion module providing compatibility between internal assignment representation and client formats.',
      category: 'Scheduler',
      docLink: '/devices/dev_scheduler_formats',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_scheduler_formats.erl'
    },
    {
      name: 'dev_scheduler_registry.erl',
      description: 'Process registry and factory managing scheduler process lifecycle and discovery.',
      category: 'Scheduler',
      docLink: '/devices/dev_scheduler_registry',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_scheduler_registry.erl'
    },
    {
      name: 'dev_scheduler_server.erl',
      description: 'Long-lived server managing sequenced message assignments with hash chain linkage and multiple scheduling modes.',
      category: 'Scheduler',
      docLink: '/devices/dev_scheduler_server',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_scheduler_server.erl'
    },
    // Runtime Devices
    {
      name: 'dev_wasm.erl',
      description: 'WebAssembly runtime with 3 downstream dependents, provides sandboxed execution environment with WASI support.',
      category: 'Runtime',
      docLink: '/devices/dev_wasm',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_wasm.erl'
    },
    {
      name: 'dev_wasi.erl',
      description: 'WebAssembly System Interface implementation enabling safe system calls in sandboxed environment.',
      category: 'Runtime',
      docLink: '/devices/dev_wasi',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_wasi.erl'
    },
    {
      name: 'dev_json_iface.erl',
      description: 'JSON interface with 3 downstream dependents, bridges WebAssembly execution with HyperBEAM messaging system.',
      category: 'Runtime',
      docLink: '/devices/dev_json_iface',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_json_iface.erl'
    },
    {
      name: 'dev_genesis_wasm.erl',
      description: 'Compatibility layer for legacy AO processes to execute within HyperBEAM through delegated computation.',
      category: 'Runtime',
      docLink: '/devices/dev_genesis_wasm',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_genesis_wasm.erl'
    },
    {
      name: 'dev_delegated_compute.erl',
      description: 'Computation offloading to remote machines through the JSON interface while maintaining local messaging.',
      category: 'Runtime',
      docLink: '/devices/dev_delegated_compute',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_delegated_compute.erl'
    },
    // Security Devices
    {
      name: 'dev_green_zone.erl',
      description: 'Secure communication and identity management between trusted nodes using hardware attestation.',
      category: 'Security',
      docLink: '/devices/dev_green_zone',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_green_zone.erl'
    },
    {
      name: 'dev_snp.erl',
      description: 'Hardware-based security attestation using AMD SEV-SNP technology, enabling cryptographic validation of node integrity.',
      category: 'Security',
      docLink: '/devices/dev_snp',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_snp.erl'
    },
    {
      name: 'dev_poda.erl',
      description: 'Proof of Data Availability implementing decentralized proof of authority consensus with quorum-based validation.',
      category: 'Security',
      docLink: '/devices/dev_poda',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_poda.erl'
    },
    // Payment Devices
    {
      name: 'dev_p4.erl',
      description: 'Configurable payment framework with pluggable pricing and ledger devices, enabling economic incentives.',
      category: 'Payment',
      docLink: '/devices/dev_p4',
      githubLink: 'https://github.com/permaweb/HyperBEAM/tree/main/src'
    },
    {
      name: 'dev_simple_pay.erl',
      description: 'Simplified payment device implementing both pricing and ledger interfaces with per-message pricing model.',
      category: 'Payment',
      docLink: '/devices/dev_simple_pay',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_simple_pay.erl'
    },
    {
      name: 'dev_faff.erl',
      description: 'Friends and family pricing policy with allowlist-based access control and zero-cost model for trusted users.',
      category: 'Payment',
      docLink: '/devices/dev_faff',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_faff.erl'
    },
    {
      name: 'dev_cu.erl',
      description: 'Computation unit tracking device orchestrating distributed computation execution with cryptographic attestation.',
      category: 'Payment',
      docLink: '/devices/dev_cu',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_cu.erl'
    },
    // Routing Devices
    {
      name: 'dev_meta.erl',
      description: 'Default entry point and gateway for all messages, implementing a processing pipeline with pre/post-processing.',
      category: 'Routing',
      docLink: '/devices/dev_meta',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_meta.erl'
    },
    {
      name: 'dev_router.erl',
      description: 'Directs outbound messages to appropriate network endpoints using configurable routes and pattern matching.',
      category: 'Routing',
      docLink: '/devices/dev_router',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_router.erl'
    },
    {
      name: 'dev_push.erl',
      description: 'Message push device implementing recursive message propagation for cross-process communication across networks.',
      category: 'Routing',
      docLink: '/devices/dev_push',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_push.erl'
    },
    {
      name: 'dev_relay.erl',
      description: 'Message relay device bridging HyperBEAM\'s message system and external HTTP(S) endpoints with synchronous and asynchronous patterns.',
      category: 'Routing',
      docLink: '/devices/dev_relay',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_relay.erl'
    },
    // Utility Devices
    {
      name: 'dev_dedup.erl',
      description: 'Prevents duplicate processing of identical messages by maintaining an in-memory record of seen message IDs.',
      category: 'Utility',
      docLink: '/devices/dev_dedup',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_dedup.erl'
    },
    {
      name: 'dev_patch.erl',
      description: 'Enables processes to modify parts of a message outside their primary area through declarative PATCH operations.',
      category: 'Utility',
      docLink: '/devices/dev_patch',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_patch.erl'
    },
    {
      name: 'dev_lookup.erl',
      description: 'ID-based content retrieval from cache with format conversion capabilities based on requested media type.',
      category: 'Utility',
      docLink: '/devices/dev_lookup',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_lookup.erl'
    },
    {
      name: 'dev_cron.erl',
      description: 'Scheduled execution device enabling time-based periodic task execution with configurable intervals.',
      category: 'Utility',
      docLink: '/devices/dev_cron',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_cron.erl'
    },
    {
      name: 'dev_monitor.erl',
      description: 'Lightweight observer pattern for non-invasive process execution monitoring with dynamic registration.',
      category: 'Utility',
      docLink: '/devices/dev_monitor',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_monitor.erl'
    },
    {
      name: 'dev_multipass.erl',
      description: 'Multi-stage processing device implementing flow control for sequential operations across multiple passes.',
      category: 'Utility',
      docLink: '/devices/dev_multipass',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_multipass.erl'
    },
    {
      name: 'dev_test.erl',
      description: 'Testing utility device providing reference implementations and demonstrating proper device behavior patterns.',
      category: 'Utility',
      docLink: '/devices/dev_test',
      githubLink: 'https://github.com/permaweb/HyperBEAM/blob/main/src/dev_test.erl'
    }
  ];
  
  // Category filter state
  const [activeCategory, setActiveCategory] = useState('All');
  
  // Filter devices based on active category
  const filteredDevices = activeCategory === 'All' 
    ? allDevices 
    : allDevices.filter(device => device.category === activeCategory);
  
  // Calculate max scroll position
  useEffect(() => {
    if (carouselRef.current) {
      const maxScrollValue = carouselRef.current.scrollWidth - carouselRef.current.clientWidth;
      setMaxScroll(maxScrollValue);
    }
  }, [filteredDevices, activeCategory]);
  
  // Update scroll position for arrow buttons
  const updateScroll = (direction) => {
    if (carouselRef.current) {
      const scrollAmount = 320; // Roughly the width of one card + gap
      if (direction === 'left') {
        carouselRef.current.scrollBy({ left: -scrollAmount, behavior: 'smooth' });
      } else {
        carouselRef.current.scrollBy({ left: scrollAmount, behavior: 'smooth' });
      }
    }
  };
  
  // Track scroll position for showing/hiding navigation arrows
  const handleScroll = () => {
    if (carouselRef.current) {
      setScrollPosition(carouselRef.current.scrollLeft);
    }
  };
  
  // Categories derived from device list
  const categories = ['All', ...new Set(allDevices.map(device => device.category))];
  
  return (
    <div className="relative">
      {/* Category navigation */}
      <div className="mb-4 flex flex-wrap gap-2">
        {categories.map(category => (
          <button
            key={category}
            className={`px-3 py-1 text-sm rounded-md transition-colors
                      ${activeCategory === category 
                        ? 'bg-terminal-green text-terminal-bg font-semibold' 
                        : 'bg-[#333] text-terminal-text hover:bg-[#444]'}`}
            onClick={() => setActiveCategory(category)}
          >
            {category}
          </button>
        ))}
      </div>
      
      {/* Scroll navigation */}
      <div className="relative">
        {scrollPosition > 10 && (
          <button 
            className="absolute left-0 top-1/2 transform -translate-y-1/2 z-10 bg-[#252526] rounded-full p-2 shadow-lg hover:bg-[#333] transition-colors"
            onClick={() => updateScroll('left')}
            aria-label="Scroll left"
          >
            <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
              <polyline points="15 18 9 12 15 6"></polyline>
            </svg>
          </button>
        )}
        
        {scrollPosition < maxScroll - 10 && (
          <button 
            className="absolute right-0 top-1/2 transform -translate-y-1/2 z-10 bg-[#252526] rounded-full p-2 shadow-lg hover:bg-[#333] transition-colors"
            onClick={() => updateScroll('right')}
            aria-label="Scroll right"
          >
            <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
              <polyline points="9 18 15 12 9 6"></polyline>
            </svg>
          </button>
        )}

        {/* Carousel container */}
        <div 
          className="flex overflow-x-auto pb-4 hide-scrollbar snap-x snap-mandatory"
          ref={carouselRef}
          onScroll={handleScroll}
        >
          {filteredDevices.map((device, i) => (
            <Link 
              key={i}
              href={device.docLink}
              className="block flex-none w-72 mx-2 snap-start hover:border-terminal-green cursor-pointer"
            >
              <div className="h-full bg-[#252526] border border-[#333] rounded-md p-4 card-hover-anim transition-all">
                <div className="text-xs font-semibold inline-block px-2 py-1 rounded-full bg-[#333] text-terminal-text mb-2">
                  {device.category}
                </div>
                <h3 className="font-mono text-base font-semibold mb-3 text-terminal-primary">{device.name}</h3>
                <p className="text-sm opacity-80 mb-4 h-24 overflow-hidden">{device.description}</p>
                <div className="flex justify-between text-sm mt-auto">
                  <span className="text-terminal-green">
                    View Documentation
                  </span>
                  <a 
                    href={device.githubLink} 
                    target="_blank" 
                    rel="noopener noreferrer"
                    className="text-terminal-green hover:underline"
                    onClick={(e) => e.stopPropagation()}
                  >
                    GitHub
                  </a>
                </div>
              </div>
            </Link>
          ))}
        </div>
      </div>
      
      {/* Terminal-style card count */}
      <div className="mt-4 text-sm text-terminal-text opacity-80 font-mono">
        <span className="text-terminal-prompt mr-1">$</span>
        <span className="text-terminal-green">ls -l</span> | 
        <span className="text-terminal-accent"> grep</span> {activeCategory} | 
        <span className="text-terminal-secondary"> wc -l</span>
        <span className="bg-[#333] px-2 py-1 ml-2 rounded">{filteredDevices.length}</span>
      </div>
    </div>
  );
}
