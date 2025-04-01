/**
 * Function Reference Parser
 * 
 * This script parses the HB function reference.md file and generates 
 * structured JSON data files for each module that can be imported in the
 * respective module pages.
 * 
 * Usage:
 * node parse_function_reference.js
 */

const fs = require('fs');
const path = require('path');

// Path to the function reference MD file
const functionReferencePath = path.join(__dirname, '..', '..', 'HB function reference.md');

// Output directory for the JSON files
const outputDir = path.join(__dirname, '..', 'src', 'data', 'functions');

// Create output directory if it doesn't exist
if (!fs.existsSync(outputDir)) {
  fs.mkdirSync(outputDir, { recursive: true });
}

/**
 * Known module patterns by subsystem
 */
const MODULE_PATTERNS = {
  core: [
    /^hb_core$/,
    /^hb_converge$/,
    /^hb_message$/,
    /^hb_path$/,
    /^hb_opts$/,
    /^hb_util$/,
    /^hb_config$/,
    /^hb_registry$/
  ],
  storage: [
    /^hb_store$/,
    /^hb_cache$/,
    /^hb_persistent$/,
    /^hb_rocksdb$/,
    /^hb_fs$/,
    /^hb_db$/,
    /^hb_kv$/
  ],
  network: [
    /^hb_http$/,
    /^hb_http_server$/,
    /^hb_http_client$/,
    /^hb_websocket$/,
    /^hb_p2p$/,
    /^hb_router$/,
    /^hb_net$/,
    /^hb_socket$/,
    /^hb_transport$/,
    /^hb_client$/
  ],
  codec: [
    /^hb_codec$/,
    /^hb_json$/,
    /^hb_msgpack$/,
    /^hb_protobuf$/,
    /^hb_binary$/,
    /^hb_serialize$/,
    /^hb_format$/,
    /^dev_codec_/
  ],
  arweave: [
    /^hb_arweave$/,
    /^hb_arweave_tx$/,
    /^hb_arweave_wallet$/,
    /^hb_arweave_bundle$/,
    /^hb_arweave_block$/,
    /^hb_ar_/,
    /^ar_/
  ],
  app: [
    /^hb_app$/,
    /^hb_sandbox$/,
    /^hb_deploy$/,
    /^hb_resource$/,
    /^hb_container$/,
    /^hb_vm$/
  ],
  devices: [
    /^dev_/,
    /^device_/,
    /^hb_device$/
  ]
};

/**
 * Parse the function reference MD file
 */
function parseFunctionReference() {
  console.log('Parsing function reference file...');

  // Read the function reference file
  const content = fs.readFileSync(functionReferencePath, 'utf8');

  // Extract all unique module names
  const moduleNameRegex = /^### ([a-zA-Z0-9_]+)(?:\s|$)/gm;
  const allModuleMatches = [...content.matchAll(moduleNameRegex)];
  const allModules = new Set();
  
  allModuleMatches.forEach(match => {
    if (match && match[1]) {
      allModules.add(match[1]);
    }
  });
  
  console.log(`\nDetected ${allModules.size} unique modules in the function reference file:`);
  console.log([...allModules].join(', '));
  
  // Check if all detected modules are properly categorized
  const unknownModules = [];
  allModules.forEach(module => {
    const subsystem = getSubsystemForModule(module);
    if (!subsystem) {
      unknownModules.push(module);
    }
  });
  
  if (unknownModules.length > 0) {
    console.log(`\nWARNING: ${unknownModules.length} modules could not be categorized into a subsystem:`);
    console.log(unknownModules.join(', '));
    console.log('These modules will be assigned to "other" subsystem\n');
  }

  // Split by sections (each module is expected to start with ### for its heading)
  const sections = content.split(/(?=^### )/m);
  
  console.log(`Found ${sections.length} sections in the function reference file`);

  // Group functions by module
  const moduleGroups = {};
  
  // Keep track of parsing statistics
  const stats = {
    totalSections: sections.length,
    validFunctions: 0,
    failedSections: 0,
    moduleCount: 0,
    functionsByModule: {}
  };

  // Skip the first section if it doesn't start with ### (likely the intro)
  for (let i = 0; i < sections.length; i++) {
    const section = sections[i];
    if (!section.trim().startsWith('### ')) {
      console.log(`Skipping section ${i + 1} (not a module definition)`);
      continue;
    }

    // Extract module name from the section
    const moduleMatch = section.match(/^### ([a-zA-Z0-9_]+)(?:\s|$)/);
    if (!moduleMatch) {
      console.log(`Skipping section ${i + 1} (could not extract module name)`);
      stats.failedSections++;
      continue;
    }

    const moduleName = moduleMatch[1].trim();
    
    try {
      // Parse all functions within this module
      const moduleFunctions = parseModuleFunctions(section, moduleName);
      
      if (moduleFunctions && moduleFunctions.length > 0) {
        // Store all functions for this module
        moduleGroups[moduleName] = moduleFunctions;
        stats.moduleCount++;
        stats.validFunctions += moduleFunctions.length;
        stats.functionsByModule[moduleName] = moduleFunctions.length;
      } else {
        console.warn(`No functions found in module: ${moduleName}`);
        stats.failedSections++;
      }
    } catch (error) {
      console.error(`Error parsing module ${moduleName}: ${error.message}`);
      stats.failedSections++;
    }
  }
  
  // Sort functions within each module by name for consistent ordering
  Object.keys(moduleGroups).forEach(moduleName => {
    moduleGroups[moduleName].sort((a, b) => a.name.localeCompare(b.name));
  });

  // Group modules by subsystem
  const subsystemGroups = {
    core: {},
    storage: {},
    network: {},
    codec: {},
    arweave: {},
    app: {},
    devices: {},
    other: {}  // Add "other" subsystem for uncategorized modules
  };

  // Map modules to subsystems
  Object.keys(moduleGroups).forEach(moduleName => {
    const subsystem = getSubsystemForModule(moduleName);
    if (subsystem) {
      subsystemGroups[subsystem][moduleName] = moduleGroups[moduleName];
    } else {
      console.warn(`Unknown subsystem for module: ${moduleName}`);
      // Put in "other" subsystem
      subsystemGroups.other[moduleName] = moduleGroups[moduleName];
    }
  });

  // Write individual module files
  Object.keys(subsystemGroups).forEach(subsystem => {
    const subsystemPath = path.join(outputDir, subsystem);
    if (!fs.existsSync(subsystemPath)) {
      fs.mkdirSync(subsystemPath, { recursive: true });
    }

    Object.keys(subsystemGroups[subsystem]).forEach(moduleName => {
      const moduleData = subsystemGroups[subsystem][moduleName];
      const outputPath = path.join(subsystemPath, `${moduleName}.json`);
      
      fs.writeFileSync(outputPath, JSON.stringify(moduleData, null, 2));
      console.log(`Wrote ${moduleData.length} functions to ${outputPath}`);
    });
  });

  // Write index files for each subsystem
  Object.keys(subsystemGroups).forEach(subsystem => {
    const subsystemData = {
      name: getSubsystemName(subsystem),
      description: getSubsystemDescription(subsystem),
      modules: Object.keys(subsystemGroups[subsystem]).map(moduleName => ({
        id: moduleName,
        name: getModuleName(moduleName),
        count: subsystemGroups[subsystem][moduleName].length
      }))
    };

    const outputPath = path.join(outputDir, `${subsystem}.json`);
    fs.writeFileSync(outputPath, JSON.stringify(subsystemData, null, 2));
    console.log(`Wrote subsystem index to ${outputPath}`);
  });

  // Write global index file
  const globalIndex = {
    subsystems: Object.keys(subsystemGroups).map(subsystem => ({
      id: subsystem,
      name: getSubsystemName(subsystem),
      count: Object.values(subsystemGroups[subsystem])
        .reduce((total, module) => total + module.length, 0),
      modules: Object.keys(subsystemGroups[subsystem]).map(moduleName => ({
        id: moduleName,
        name: getModuleName(moduleName),
        count: subsystemGroups[subsystem][moduleName].length
      }))
    }))
  };

  const globalIndexPath = path.join(outputDir, 'index.json');
  fs.writeFileSync(globalIndexPath, JSON.stringify(globalIndex, null, 2));
  console.log(`Wrote global index to ${globalIndexPath}`);
  
  // Write parsing statistics to a log file
  const statsData = {
    summary: {
      totalSections: stats.totalSections,
      validFunctions: stats.validFunctions,
      failedSections: stats.failedSections,
      moduleCount: stats.moduleCount
    },
    subsystems: {},
    functionsByModule: stats.functionsByModule,
    failedSections: stats.failedSections
  };
  
  // Add subsystem statistics
  Object.keys(subsystemGroups).forEach(subsystem => {
    const modules = Object.keys(subsystemGroups[subsystem]);
    const functionCount = modules.reduce((sum, module) => 
      sum + subsystemGroups[subsystem][module].length, 0);
      
    statsData.subsystems[subsystem] = {
      name: getSubsystemName(subsystem),
      moduleCount: modules.length,
      functionCount,
      modules: modules.map(module => ({
        name: module,
        count: subsystemGroups[subsystem][module].length
      }))
    };
  });
  
  const statsPath = path.join(outputDir, 'parsing_stats.json');
  fs.writeFileSync(statsPath, JSON.stringify(statsData, null, 2));
  console.log(`Wrote parsing statistics to ${statsPath}`);
  
  // Print a summary to the console
  console.log('\n=========== PARSING SUMMARY ===========');
  console.log(`Total sections found: ${stats.totalSections}`);
  console.log(`Successfully parsed functions: ${stats.validFunctions}`);
  console.log(`Failed sections: ${stats.failedSections}`);
  console.log(`Total modules: ${stats.moduleCount}`);
  console.log('\nFunctions by subsystem:');
  Object.keys(subsystemGroups).forEach(subsystem => {
    const modules = Object.keys(subsystemGroups[subsystem]);
    const functionCount = modules.reduce((sum, module) => 
      sum + subsystemGroups[subsystem][module].length, 0);
    console.log(`  ${getSubsystemName(subsystem)}: ${functionCount} functions in ${modules.length} modules`);
  });
  console.log('======================================\n');

  console.log('Done!');
}

/**
 * Map a module name to its subsystem
 */
function getSubsystemForModule(moduleName) {
  // Check each subsystem's patterns
  for (const [subsystem, patterns] of Object.entries(MODULE_PATTERNS)) {
    for (const pattern of patterns) {
      if (pattern.test(moduleName)) {
        return subsystem;
      }
    }
  }
  
  // If no pattern matched but it's an hb_ module, put it in "other"
  if (moduleName.startsWith('hb_') || moduleName.startsWith('dev_')) {
    return 'other';
  }
  
  return null;
}

/**
 * Get a human-readable name for a subsystem
 */
function getSubsystemName(subsystem) {
  const names = {
    core: 'Core Infrastructure',
    storage: 'Storage Subsystem',
    network: 'Network Subsystem',
    codec: 'Codec Subsystem',
    arweave: 'Arweave Integration',
    app: 'Application Management',
    devices: 'Device Ecosystem',
    other: 'Additional Modules'
  };
  
  return names[subsystem] || subsystem;
}

/**
 * Get a description for a subsystem
 */
function getSubsystemDescription(subsystem) {
  const descriptions = {
    core: 'Core functions that provide the foundation of the HyperBEAM system, including message processing, path routing, convergence protocol, and state management.',
    storage: 'Functions for persistent data storage, caching, and retrieval. Includes filesystem, RocksDB, and distributed storage backends.',
    network: 'Functions for network communication, HTTP servers and clients, WebSocket connections, and peer-to-peer interactions.',
    codec: 'Functions for data encoding, decoding, serialization, and validation. Supports JSON, MessagePack, Protocol Buffers, and custom binary formats.',
    arweave: 'Functions for interacting with the Arweave blockchain, creating and submitting transactions, and managing data on the permaweb.',
    app: 'Functions for application lifecycle management, deployment, sandboxing, and resource allocation.',
    devices: 'Functions for device registration, message handling, and device-specific operations. Includes message processing devices, storage devices, security devices, and others.',
    other: 'Additional modules that extend the HyperBEAM functionality with specialized features or are still being categorized.'
  };
  
  return descriptions[subsystem] || '';
}

/**
 * Get a human-readable name for a module
 */
function getModuleName(moduleName) {
  const names = {
    // Core modules
    hb_converge: 'Convergence Protocol',
    hb_message: 'Message Handling',
    hb_path: 'Path Routing',
    hb_core: 'Core Application',
    hb_opts: 'Configuration',
    hb_util: 'Utility Functions',
    
    // Storage modules
    hb_store: 'Storage Interface',
    hb_cache: 'Caching Layer',
    hb_persistent: 'Persistent Storage',
    hb_rocksdb: 'RocksDB Backend',
    hb_fs: 'Filesystem Backend',
    
    // Network modules
    hb_http: 'HTTP Protocol',
    hb_client: 'Client Interface',
    hb_http_server: 'HTTP Server',
    hb_http_client: 'HTTP Client',
    hb_websocket: 'WebSocket',
    hb_p2p: 'Peer-to-Peer',
    hb_router: 'Request Routing',
    
    // Codec modules
    hb_codec: 'Codec Interface',
    hb_json: 'JSON Codec',
    hb_msgpack: 'MessagePack Codec',
    hb_protobuf: 'Protocol Buffers',
    hb_binary: 'Binary Codec',
    
    // Arweave modules
    hb_arweave: 'Arweave Interface',
    hb_arweave_tx: 'Transaction Creation',
    hb_arweave_wallet: 'Wallet Management',
    hb_arweave_bundle: 'Transaction Bundling',
    ar_wallet: 'Arweave Wallet',
    ar_bundles: 'Arweave Bundles',
    
    // App management modules
    hb_app: 'Application Interface',
    hb_sandbox: 'Sandboxing',
    hb_deploy: 'Deployment',
    hb_resource: 'Resource Management',
    
    // Device modules
    dev_scheduler: 'Scheduler Device',
    dev_process: 'Process Device',
    dev_message: 'Message Device',
    dev_stack: 'Stack Device',
    dev_codec_structured: 'Structured Codec Device',
    dev_codec_httpsig: 'HTTP Signature Codec Device',
    dev_wasm: 'WebAssembly Device',
    dev_green_zone: 'Green Zone Device',
    dev_p4: 'P4 Payment Device'
  };
  
  return names[moduleName] || moduleName;
}

/**
 * Parse all functions within a module section
 */
function parseModuleFunctions(moduleSection, moduleName) {
  const functions = [];
  
  // Split the section by function (each function starts with #### )
  const functionSections = moduleSection.split(/(?=^#### )/m);
  
  // Skip the first part (module info)
  if (functionSections.length <= 1) {
    console.warn(`No function sections found in module ${moduleName}`);
    return [];
  }
  
  // Log information for debugging
  console.log(`Found ${functionSections.length-1} function sections in module ${moduleName}`);
  
  for (let i = 1; i < functionSections.length; i++) {
    const funcSection = functionSections[i];
    
    // Extract function name and arity from the heading
    const funcMatch = funcSection.match(/^#### [`']?([a-zA-Z0-9_]+)\/(\d+)[`']?/);
    if (!funcMatch) {
      console.warn(`Could not parse function in ${moduleName} (section ${i}): ${funcSection.substring(0, 50)}...`);
      continue;
    }
    
    const [, functionName, arity] = funcMatch;
    console.log(`Processing function: ${moduleName}:${functionName}/${arity} (section ${i})`);
    
    // Extract description (text immediately after the heading)
    let description = '';
    const descriptionMatch = funcSection.match(/^#### .*?\n\n(.*?)(?:\n\n\*\*Parameters\*\*|\n\n\*\*Returns\*\*|$)/ms);
    if (descriptionMatch) {
      description = descriptionMatch[1].trim();
    }
    
    // Extract parameters
    const params = [];
    const paramsMatch = funcSection.match(/\*\*Parameters\*\*:\s*\n([\s\S]*?)(?:\n\n\*\*Returns\*\*|\n\n\*\*Related Functions\*\*|$)/m);
    if (paramsMatch) {
      const paramsContent = paramsMatch[1];
      
      // Match each parameter item (formatted as bullet points)
      const paramItems = paramsContent.match(/- `([^`]+)` \((.*?)\): (.*?)(?:\n(?!-)|\n?$)/gms);
      
      if (paramItems) {
        paramItems.forEach(item => {
          const itemMatch = item.match(/- `([^`]+)` \((.*?)\): (.*?)(?:\n(?!-)|\n?$)/s);
          if (itemMatch) {
            const [, name, type, paramDesc] = itemMatch;
            params.push({
              name,
              type,
              description: paramDesc.trim()
            });
          }
        });
      }
    }
    
    // Extract return values
    const returns = [];
    const returnsMatch = funcSection.match(/\*\*Returns\*\*:\s*\n([\s\S]*?)(?:\n\n\*\*Related Functions\*\*|\n\n\*\*Usage Notes\*\*|$)/m);
    if (returnsMatch) {
      const returnsContent = returnsMatch[1];
      
      // Match each return item (formatted as bullet points)
      const returnItems = returnsContent.match(/- `([^`]+)`: (.*?)(?:\n(?!-)|\n?$)/gms);
      
      if (returnItems) {
        returnItems.forEach(item => {
          const itemMatch = item.match(/- `([^`]+)`: (.*?)(?:\n(?!-)|\n?$)/s);
          if (itemMatch) {
            const [, type, returnDesc] = itemMatch;
            returns.push({
              type,
              description: returnDesc.trim()
            });
          }
        });
      }
    }
    
    // Extract related functions
    let relatedFunctions = [];
    const relatedMatch = funcSection.match(/\*\*Related Functions\*\*:\s*(.*?)(?:\n\n\*\*Usage Notes\*\*|$)/ms);
    if (relatedMatch && relatedMatch[1]) {
      // Split by commas but handle the possibility of function names containing commas
      // For example: "function1/1, function2/2, function3/3"
      const relatedString = relatedMatch[1].trim();
      relatedFunctions = relatedString.split(/,\s*/)
        .map(fn => fn.trim())
        .filter(fn => fn.length > 0);
    }
    
    // Extract usage notes
    let usageNotes = '';
    const usageMatch = funcSection.match(/\*\*Usage Notes\*\*:\s*\n([\s\S]*?)(?:\n\n|$)/ms);
    if (usageMatch) {
      usageNotes = usageMatch[1].trim();
    }
    
    // Extract examples - look for code blocks
    const examples = [];
    const codeBlockRegex = /```(?:erlang)?\n([\s\S]*?)```/g;
    let exampleMatch;
    while ((exampleMatch = codeBlockRegex.exec(funcSection)) !== null) {
      examples.push(exampleMatch[1].trim());
    }
    
    // Determine API type
    let apiType = 'public'; // Default to public
    if (funcSection.includes('**Internal Function**')) {
      apiType = 'internal';
    } else if (funcSection.includes('**Callback Function**')) {
      apiType = 'callback';
    } else if (funcSection.includes('**Private Function**')) {
      apiType = 'private';
    }
    
    // Extract source file and line if available
    let sourceFile = `src/${moduleName}.erl`; // Default
    let lineNumber = 1; // Default
    
    const sourceMatch = funcSection.match(/Source: \[([^\]]+)\]\(([^)]+)\)/);
    if (sourceMatch) {
      sourceFile = sourceMatch[1].trim();
      
      // Try to get line number if available
      const lineMatch = sourceMatch[2].match(/L(\d+)/);
      if (lineMatch) {
        lineNumber = parseInt(lineMatch[1], 10);
      }
    }
    
    // Create the function object with all extracted information
    const functionData = {
      name: `${moduleName}:${functionName}/${arity}`,
      module: moduleName,
      moduleId: moduleName,
      functionName,
      arity: parseInt(arity, 10),
      description,
      params,
      returns,
      relatedFunctions,
      usageNotes,
      examples,
      apiType,
      sourceFile,
      lineNumber
    };
    
    // Add to the functions array
    functions.push(functionData);
  }
  
  console.log(`Successfully parsed ${functions.length} functions for module ${moduleName}`);
  return functions;
}

// Run the parser
parseFunctionReference();
