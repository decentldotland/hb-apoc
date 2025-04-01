/**
 * Enhanced HyperBEAM Unified Subsystem Parser
 * 
 * This script parses markdown analysis files from all subsystems in the HyperBEAM analysis
 * and generates structured JSON data files in the website's data directory while preserving
 * the hierarchical classification.
 * 
 * Features:
 * - Processes all subsystem directories
 * - Extracts rich content from overview files
 * - Properly categorizes components
 * - Maintains hierarchical structure
 * - Handles special cases for device categories
 * 
 * Usage: node parse_all_subsystems.js
 */

const fs = require('fs');
const path = require('path');

// Base directories
const ANALYSIS_DIR = path.resolve(__dirname, '../../HB analysis stage 2');
const TARGET_BASE_DIR = path.resolve(__dirname, '../src/data/subsystems');

// Define subsystem structure (matching the directory structure)
const SUBSYSTEMS = [
  {
    id: 'core',
    name: 'Core Infrastructure',
    sourcePath: 'core_analysis',
    targetPath: 'core',
    description: 'Core functions that provide the foundation of the HyperBEAM system, including message processing, path routing, convergence protocol, and state management.',
    overviewPattern: /\d+_core_infrastructure_summary/
  },
  {
    id: 'storage',
    name: 'Storage Subsystem',
    sourcePath: 'storage_analysis',
    targetPath: 'storage',
    description: 'Functions for persistent data storage, caching, and retrieval. Includes filesystem, RocksDB, and distributed storage backends.',
    overviewPattern: /\d+_storage_subsystem_overview/
  },
  {
    id: 'network',
    name: 'Network Communication Subsystem',
    sourcePath: 'network_analysis',
    targetPath: 'network',
    description: 'Functions for network communication, HTTP servers and clients, WebSocket connections, and peer-to-peer interactions.',
    overviewPattern: /\d+_network_subsystem_overview/
  },
  {
    id: 'codec',
    name: 'Codec and Data Format Subsystem',
    sourcePath: 'codec_analysis',
    targetPath: 'codec',
    description: 'Functions for data encoding, decoding, serialization, and validation. Supports JSON, MessagePack, Protocol Buffers, and custom binary formats.',
    overviewPattern: /\d+_codec_subsystem_overview/
  },
  {
    id: 'arweave',
    name: 'Arweave Integration Subsystem',
    sourcePath: 'arweave_analysis',
    targetPath: 'arweave',
    description: 'Functions for interacting with the Arweave blockchain, creating and submitting transactions, and managing data on the permaweb.',
    overviewPattern: /\d+_arweave_subsystem_overview/
  },
  {
    id: 'app-management',
    name: 'Application Management Subsystem',
    sourcePath: 'app_management_analysis',
    targetPath: 'app-management',
    description: 'Functions for application lifecycle management, deployment, sandboxing, and resource allocation.',
    overviewPattern: /\d+_app_management_subsystem_overview/
  },
  {
    id: 'devices',
    name: 'Device Ecosystem',
    sourcePath: 'device_analysis',
    targetPath: 'devices',
    description: 'Functions for device registration, message handling, and device-specific operations. Includes message processing devices, storage devices, security devices, and others.',
    overviewPattern: /\d+_(?:final_devices_subsystem_overview|comprehensive_device_ecosystem_overview)/,
    // Special device category overviews
    categoryOverviews: {
      'scheduler': /06_scheduler_subsystem_overview\.md/,
      'other': /37_final_devices_subsystem_overview\.md/
    }
  }
];

// Define device categories
const DEVICE_CATEGORIES = [
  {
    id: 'scheduler',
    name: 'Scheduler Subsystem',
    description: 'Handles task scheduling and execution order',
    pattern: /scheduler/i
  },
  {
    id: 'process',
    name: 'Process Management',
    description: 'Manages process lifecycle and execution',
    pattern: /process|worker/i
  },
  {
    id: 'codec',
    name: 'Codec Devices',
    description: 'Handles data serialization and format conversion',
    pattern: /codec|format|structured/i
  },
  {
    id: 'security',
    name: 'Security Devices',
    description: 'Implements security features and protocols',
    pattern: /green_zone|snp|security/i
  },
  {
    id: 'payment',
    name: 'Payment and Pricing',
    description: 'Handles payment processing and pricing models',
    pattern: /pay|faff|p4/i
  },
  {
    id: 'wasm',
    name: 'WebAssembly Runtime',
    description: 'Provides WebAssembly execution environment',
    pattern: /wasm|wasi/i
  },
  {
    id: 'storage',
    name: 'Storage Devices',
    description: 'Handles data storage and retrieval',
    pattern: /lookup|dedup|cache/i
  },
  {
    id: 'core',
    name: 'Core Infrastructure Devices',
    description: 'Core functionality devices',
    pattern: /message|stack|meta/i
  },
  {
    id: 'other',
    name: 'Additional Devices',
    description: 'Specialized device modules with unique functionality',
    pattern: /.*/  // Catch-all for remaining devices
  }
];

// Component patterns for categorizing (used for cross-referencing)
const COMPONENT_PATTERNS = {
  core: [
    /^hb_util/i, /^hb_converge/i, /^hb_message/i, 
    /^hb_path/i, /^hb_core/i, /^hb_opts/i
  ],
  storage: [
    /^hb_store/i, /^hb_cache/i, /^hb_persistent/i
  ],
  network: [
    /^hb_http/i, /^hb_client/i, /^hb_router/i, 
    /^hb_gateway/i, /^hb_singleton/i
  ],
  codec: [
    /^dev_codec/i, /^hb_structured/i, /^hb_format/i
  ],
  arweave: [
    /^ar_/i, /^hb_ar/i
  ],
  'app-management': [
    /^hb_app/i, /^hb_sup/i, /^hb_name/i, 
    /^hb_logger/i, /^hb_metrics/i, /^hb_process_monitor/i
  ],
  devices: [
    /^dev_/i
  ]
};

// Special components that belong to multiple subsystems
const OVERLAP_COMPONENTS = {
  'dev_codec_httpsig': ['codec', 'devices'],
  'dev_codec_structured': ['codec', 'devices'],
  'dev_codec_ans104': ['codec', 'arweave', 'devices'],
  'dev_message': ['core', 'devices'],
  'dev_router': ['network', 'devices'],
  'dev_meta': ['core', 'devices'],
  'dev_stack': ['core', 'devices']
};

// Map device IDs to their categories
const DEVICE_CATEGORY_MAP = {
  // Scheduler-related devices
  'dev_scheduler': 'scheduler',
  'dev_scheduler_cache': 'scheduler',
  'dev_scheduler_formats': 'scheduler',
  'dev_scheduler_registry': 'scheduler',
  'dev_scheduler_server': 'scheduler',
  
  // Process-related devices
  'dev_process': 'process',
  'dev_process_cache': 'process',
  'dev_process_worker': 'process',
  
  // Core-related devices
  'dev_message': 'core',
  'dev_stack': 'core',
  'dev_meta': 'core',
  
  // Codec-related devices
  'dev_codec_ans104': 'codec',
  'dev_codec_httpsig': 'codec',
  'dev_codec_httpsig_conv': 'codec',
  'dev_codec_json': 'codec',
  'dev_codec_structured': 'codec',
  'dev_codec_flat': 'codec',
  
  // Custom mappings for specific devices
  'dev_green_zone': 'security',
  'dev_p4': 'payment',
  'dev_faff': 'payment',
  'dev_simple_pay': 'payment',
  'dev_wasm': 'wasm',
  'dev_wasi': 'wasm',
  'dev_genesis_wasm': 'wasm',
  'dev_lookup': 'storage',
  'dev_dedup': 'storage',
  'dev_cu': 'compute'
};

/**
 * Extract component ID from a filename
 * @param {string} filename The markdown filename
 * @returns {string} The component ID
 */
function extractComponentId(filename) {
  // Remove number prefix and extension, e.g., "01_hb_util_analysis.md" -> "hb_util"
  const match = filename.match(/\d+_([a-zA-Z0-9_]+)(?:_analysis)?\.md$/);
  if (match && match[1]) {
    return match[1];
  }
  
  // Fallback: remove only extension
  return path.basename(filename, '.md').replace(/_analysis$/, '');
}

/**
 * Get the subsystem for a component based on patterns
 * @param {string} componentId Component identifier
 * @returns {string|null} Subsystem ID or null if not found
 */
function getSubsystemForComponent(componentId) {
  // First check the overlap components
  if (OVERLAP_COMPONENTS[componentId]) {
    return OVERLAP_COMPONENTS[componentId][0]; // Return primary subsystem
  }

  // Then check each subsystem's patterns
  for (const [subsystemId, patterns] of Object.entries(COMPONENT_PATTERNS)) {
    for (const pattern of patterns) {
      if (pattern.test(componentId)) {
        return subsystemId;
      }
    }
  }
  
  // If it's a dev_ module, put it in devices
  if (componentId.startsWith('dev_')) {
    return 'devices';
  }
  
  return null;
}

/**
 * Get the device category for a component
 * @param {string} componentId Component identifier
 * @param {string} content The component's markdown content
 * @returns {string} Category ID
 */
function getDeviceCategory(componentId, content) {
  // First check the explicit mapping
  if (DEVICE_CATEGORY_MAP[componentId]) {
    return DEVICE_CATEGORY_MAP[componentId];
  }
  
  // Then try to determine from content
  for (const category of DEVICE_CATEGORIES) {
    if (category.pattern.test(componentId) || category.pattern.test(content)) {
      return category.id;
    }
  }
  
  // Default to "other"
  return "other";
}

/**
 * Parse an overview markdown file and extract structured data
 * @param {string} filePath Path to the markdown file
 * @returns {Object} Parsed overview data
 */
function parseOverviewFile(filePath) {
  console.log(`Parsing overview: ${filePath}...`);
  
  // Read file content
  const content = fs.readFileSync(filePath, 'utf8');
  
  // Extract the filename
  const filename = path.basename(filePath);
  
  // Default structure for overview data
  const overviewData = {
    title: '',
    summary: '',
    filename: filename
  };
  
  // Extract title (first heading)
  const titleMatch = content.match(/^# ([^\n]+)/m);
  if (titleMatch) {
    overviewData.title = titleMatch[1].trim();
  }
  
  // Extract summary (text immediately after title until the next heading)
  const summaryMatch = content.match(/^# [^\n]+\n\n([\s\S]+?)(?=\n## |$)/m);
  if (summaryMatch) {
    overviewData.summary = summaryMatch[1].trim();
  }
  
  // Process the content, excluding any sections that should be removed
  const processedContent = processOverviewContent(content);
  overviewData.content = processedContent;
  
  // Check for presence of mermaid diagrams and code blocks
  if (processedContent.includes("```mermaid") || processedContent.includes("```Mermaid")) {
    overviewData.hasMermaid = true;
  }
  
  if (processedContent.includes("```")) {
    overviewData.hasCode = true;
  }
  
  // We'll still extract structured data for components and patterns
  // to support the website's navigation and overview structure
  
  // Extract architectural patterns
  overviewData.architecturalPatterns = [];
  const patternsSection = extractSectionContent(content, 
    /## (?:Architectural Patterns|Design Patterns|Common Patterns)/);
  
  if (patternsSection) {
    const patternItems = patternsSection.match(/- ([^:]+): (.*?)(?=\n- |\n\n|$)/g);
    if (patternItems) {
      patternItems.forEach(item => {
        const patternMatch = item.match(/- ([^:]+): (.*)/);
        if (patternMatch) {
          overviewData.architecturalPatterns.push({
            name: patternMatch[1].trim(),
            description: patternMatch[2].trim()
          });
        }
      });
    }
  }
  
  // Extract key components
  overviewData.keyComponents = [];
  const componentsSection = extractSectionContent(content, 
    /## (?:Key Components|Main Components|Core Components)/);
  
  if (componentsSection) {
    const componentItems = componentsSection.match(/- ([^:]+): (.*?)(?=\n- |\n\n|$)/g);
    if (componentItems) {
      componentItems.forEach(item => {
        const componentMatch = item.match(/- ([^:]+): (.*)/);
        if (componentMatch) {
          overviewData.keyComponents.push({
            name: componentMatch[1].trim(),
            description: componentMatch[2].trim()
          });
        }
      });
    }
  }
  
  return overviewData;
}

/**
 * Process overview markdown content to exclude specific sections
 * @param {string} content The full markdown content
 * @returns {string} The processed content with excluded sections removed
 */
function processOverviewContent(content) {
  // Split content by section headers
  const sections = content.split(/^## /m);
  
  // First item is the title and any content before the first section
  let processedContent = sections[0];
  
  // Process each section
  for (let i = 1; i < sections.length; i++) {
    const sectionContent = sections[i];
    const sectionTitle = sectionContent.split('\n')[0].trim();
    
    // Skip sections we want to exclude (future reference - if needed)
    // Currently keeping all sections for overviews
    
    // Add this section to the processed content
    processedContent += `\n\n## ${sectionContent}`;
  }
  
  return processedContent.trim();
}

/**
 * Extract content from a specific section by its heading pattern
 * @param {string} content The full markdown content
 * @param {RegExp} headingPattern Regular expression to match the section heading
 * @returns {string|null} The section content or null if not found
 */
function extractSectionContent(content, headingPattern) {
  const sectionMatch = content.match(new RegExp(`${headingPattern.source}[^\\n]*\\n([\\s\\S]+?)(?=\\n## |$)`, 'm'));
  return sectionMatch ? sectionMatch[1].trim() : null;
}

/**
 * Parse a component markdown analysis file and extract structured data
 * @param {string} filePath Path to the markdown file
 * @param {string} subsystemId The subsystem ID
 * @returns {Object} Parsed component data
 */
function parseComponentFile(filePath, subsystemId) {
  console.log(`Parsing component: ${filePath}...`);
  
  // Read file content
  const content = fs.readFileSync(filePath, 'utf8');
  
  // Extract component ID from filename
  const filename = path.basename(filePath);
  const componentId = extractComponentId(filename);
  
  // Extract title (first heading)
  const titleMatch = content.match(/^# ([^(]+?)(?:\(|$)/m);
  const title = titleMatch ? titleMatch[1].trim() : componentId;
  
  // Default structure for component data
  const componentData = {
    id: componentId,
    name: title,
    subsystem: subsystemId,
    filename: filename
  };
  
  // If it's a device, determine the category
  if (subsystemId === 'devices') {
    componentData.category = getDeviceCategory(componentId, content);
  }
  
  // Check if this component overlaps with other subsystems
  if (OVERLAP_COMPONENTS[componentId]) {
    componentData.overlappingSubsystems = OVERLAP_COMPONENTS[componentId];
  }
  
  // Extract the main content, excluding specified sections
  // We'll keep everything except "Questions and Insights", "Conclusion", and "Recategorization Considerations"
  const processedContent = processMarkdownContent(content);
  
  // Add the processed content to the component data
  componentData.content = processedContent;
  
  // Extract the first paragraph from the Overview section for description
  const overviewMatch = content.match(/## Overview\s+([^\n]+)/);
  if (overviewMatch) {
    componentData.description = overviewMatch[1].trim();
  }
  
  // Check for presence of mermaid diagrams and code blocks
  if (processedContent.includes("```mermaid") || processedContent.includes("```Mermaid")) {
    componentData.hasMermaid = true;
  }
  
  if (processedContent.includes("```")) {
    componentData.hasCode = true;
  }
  
  return componentData;
}

/**
 * Process markdown content to exclude specific sections
 * @param {string} content The full markdown content
 * @returns {string} The processed content with excluded sections removed
 */
function processMarkdownContent(content) {
  // Split content by section headers
  const sections = content.split(/^## /m);
  
  // First item is the title and any content before the first section
  let processedContent = sections[0];
  
  // Process each section
  for (let i = 1; i < sections.length; i++) {
    const sectionContent = sections[i];
    const sectionTitle = sectionContent.split('\n')[0].trim();
    
    // Skip sections we want to exclude
    if (
      sectionTitle === 'Questions and Insights' || 
      sectionTitle === 'Questions' || 
      sectionTitle === 'Conclusion' || 
      sectionTitle === 'Recategorization Considerations'
    ) {
      continue;
    }
    
    // Add this section to the processed content
    processedContent += `\n\n## ${sectionContent}`;
  }
  
  return processedContent.trim();
}

/**
 * Main function to parse all subsystems
 */
function parseAllSubsystems() {
  console.log('🚀 Enhanced HyperBEAM Subsystem Parser');
  console.log('--------------------------------------');
  
  // Create the main target directory if it doesn't exist
  if (!fs.existsSync(TARGET_BASE_DIR)) {
    fs.mkdirSync(TARGET_BASE_DIR, { recursive: true });
  }
  
  // Store all data by subsystem
  const allData = {
    overviews: {},
    components: {},
    categoryOverviews: {}
  };
  
  // Initialize component storage for each subsystem
  SUBSYSTEMS.forEach(subsystem => {
    allData.components[subsystem.id] = [];
  });
  
  // Statistics
  const stats = {
    totalOverviews: 0,
    totalComponents: 0,
    subsystemStats: {}
  };
  
  // Process each subsystem
  for (const subsystem of SUBSYSTEMS) {
    console.log(`\nProcessing ${subsystem.name} subsystem...`);
    stats.subsystemStats[subsystem.id] = { components: 0, overviews: 0 };
    
    // Source and target paths
    const sourcePath = path.join(ANALYSIS_DIR, subsystem.sourcePath);
    const targetPath = path.join(TARGET_BASE_DIR, subsystem.targetPath);
    
    // Check if source directory exists
    if (!fs.existsSync(sourcePath)) {
      console.warn(`⚠️ Source directory not found: ${sourcePath}`);
      continue;
    }
    
    // Create target directory
    if (!fs.existsSync(targetPath)) {
      fs.mkdirSync(targetPath, { recursive: true });
    }
    
    // Get all markdown files
    const files = fs.readdirSync(sourcePath).filter(file => file.endsWith('.md'));
    
    // STEP 1: Find and process overview files first
    const overviewFiles = files.filter(file => {
      // Check main subsystem overview
      if (subsystem.overviewPattern && subsystem.overviewPattern.test(file)) {
        return true;
      }
      
      // Check category overviews for devices
      if (subsystem.id === 'devices' && subsystem.categoryOverviews) {
        for (const [category, pattern] of Object.entries(subsystem.categoryOverviews)) {
          if (pattern.test(file)) {
            return true;
          }
        }
      }
      
      return false;
    });
    
    // Process each overview file
    for (const file of overviewFiles) {
      const filePath = path.join(sourcePath, file);
      
      try {
        // Parse the overview file
        const overviewData = parseOverviewFile(filePath);
        
        // Determine if this is a main subsystem overview or a category overview
        let isMainOverview = true;
        let categoryId = null;
        
        if (subsystem.id === 'devices' && subsystem.categoryOverviews) {
          for (const [category, pattern] of Object.entries(subsystem.categoryOverviews)) {
            if (pattern.test(file)) {
              isMainOverview = false;
              categoryId = category;
              break;
            }
          }
        }
        
        if (isMainOverview) {
          // Store main subsystem overview
          allData.overviews[subsystem.id] = overviewData;
          
          // Save to file
          const overviewJsonPath = path.join(targetPath, 'overview.json');
          fs.writeFileSync(overviewJsonPath, JSON.stringify(overviewData, null, 2));
          console.log(`✅ Saved subsystem overview to ${overviewJsonPath}`);
        } else if (categoryId) {
          // Store category overview
          if (!allData.categoryOverviews[subsystem.id]) {
            allData.categoryOverviews[subsystem.id] = {};
          }
          allData.categoryOverviews[subsystem.id][categoryId] = overviewData;
          
          // Create category folder
          const categoryPath = path.join(targetPath, categoryId);
          if (!fs.existsSync(categoryPath)) {
            fs.mkdirSync(categoryPath, { recursive: true });
          }
          
          // Save to file
          const categoryOverviewJsonPath = path.join(categoryPath, 'overview.json');
          fs.writeFileSync(categoryOverviewJsonPath, JSON.stringify(overviewData, null, 2));
          console.log(`✅ Saved category overview for ${categoryId} to ${categoryOverviewJsonPath}`);
        }
        
        stats.totalOverviews++;
        stats.subsystemStats[subsystem.id].overviews++;
      } catch (error) {
        console.error(`❌ Error parsing overview file ${file}: ${error.message}`);
      }
    }
    
    // STEP 2: Process component files
    const componentFiles = files.filter(file => 
      file.endsWith('.md') && 
      !overviewFiles.includes(file)
    );
    
    // Process each component file
    for (const file of componentFiles) {
      const filePath = path.join(sourcePath, file);
      
      try {
        // Parse the component file
        const componentData = parseComponentFile(filePath, subsystem.id);
        
        // Add to subsystem list
        allData.components[subsystem.id].push(componentData);
        
        // Determine output path based on category for devices
        let componentJsonPath;
        if (subsystem.id === 'devices' && componentData.category) {
          // Create category folder if it doesn't exist
          const categoryPath = path.join(targetPath, componentData.category);
          if (!fs.existsSync(categoryPath)) {
            fs.mkdirSync(categoryPath, { recursive: true });
          }
          componentJsonPath = path.join(categoryPath, `${componentData.id}.json`);
        } else {
          componentJsonPath = path.join(targetPath, `${componentData.id}.json`);
        }
        
        // Save individual component JSON
        fs.writeFileSync(componentJsonPath, JSON.stringify(componentData, null, 2));
        
        stats.totalComponents++;
        stats.subsystemStats[subsystem.id].components++;
      } catch (error) {
        console.error(`❌ Error parsing component file ${file}: ${error.message}`);
      }
    }
    
    console.log(`✅ Processed ${stats.subsystemStats[subsystem.id].components} component files in ${subsystem.name}`);
  }
  
  // STEP 3: Create subsystem index files
  console.log('\nGenerating subsystem index files...');
  
  for (const subsystem of SUBSYSTEMS) {
    const components = allData.components[subsystem.id] || [];
    const targetPath = path.join(TARGET_BASE_DIR, subsystem.targetPath);
    
    // Skip if no components were found for this subsystem
    if (components.length === 0) {
      console.warn(`⚠️ No components found for ${subsystem.name}`);
      continue;
    }
    
    // Get overview data if available
    const overview = allData.overviews[subsystem.id] || {};
    
    // Prepare subsystem index data
    const subsystemIndex = {
      id: subsystem.id,
      name: subsystem.name,
      description: subsystem.description,
      title: overview.title || subsystem.name,
      summary: overview.summary || subsystem.description,
      components: components.map(comp => ({
        id: comp.id,
        name: comp.name,
        description: comp.description || '',
        category: comp.category || null
      }))
    };
    
    // Add architectural patterns if available
    if (overview.architecturalPatterns && overview.architecturalPatterns.length > 0) {
      subsystemIndex.architecturalPatterns = overview.architecturalPatterns;
    }
    
    // Add key components if available
    if (overview.keyComponents && overview.keyComponents.length > 0) {
      subsystemIndex.keyComponents = overview.keyComponents;
    }
    
    // For devices, create category groupings
    if (subsystem.id === 'devices') {
      subsystemIndex.categories = {};
      
      // Get all device categories present in the components
      const categories = [...new Set(components.map(comp => comp.category || 'other'))];
      
      // Group components by category
      for (const category of categories) {
        const categoryComponents = components.filter(comp => (comp.category || 'other') === category);
        
        // Get the category name and description
        const categoryInfo = DEVICE_CATEGORIES.find(c => c.id === category) || {
          id: category,
          name: category.charAt(0).toUpperCase() + category.slice(1) + ' Devices',
          description: `Devices related to ${category}`
        };
        
        // Get category overview if available
        const categoryOverview = allData.categoryOverviews[subsystem.id]?.[category] || {};
        
        subsystemIndex.categories[category] = {
          id: category,
          name: categoryInfo.name,
          description: categoryInfo.description,
          title: categoryOverview.title || categoryInfo.name,
          summary: categoryOverview.summary || categoryInfo.description,
          components: categoryComponents.map(comp => ({
            id: comp.id,
            name: comp.name,
            description: comp.description || ''
          }))
        };
        
        // Add architectural patterns if available
        if (categoryOverview.architecturalPatterns && categoryOverview.architecturalPatterns.length > 0) {
          subsystemIndex.categories[category].architecturalPatterns = categoryOverview.architecturalPatterns;
        }
        
        // Add key components if available
        if (categoryOverview.keyComponents && categoryOverview.keyComponents.length > 0) {
          subsystemIndex.categories[category].keyComponents = categoryOverview.keyComponents;
        }
      }
    }
    
    // Write subsystem index file
    const subsystemIndexPath = path.join(targetPath, 'index.json');
    fs.writeFileSync(subsystemIndexPath, JSON.stringify(subsystemIndex, null, 2));
    console.log(`✅ Generated index for ${subsystem.name} with ${components.length} components`);
  }
  
  // STEP 4: Create main index file
  console.log('\nGenerating main index file...');
  
  const mainIndex = {
    subsystems: SUBSYSTEMS.map(subsystem => {
      const components = allData.components[subsystem.id] || [];
      
      // For devices, count by category
      let categoryStats = {};
      if (subsystem.id === 'devices') {
        categoryStats = components.reduce((stats, comp) => {
          const category = comp.category || 'other';
          stats[category] = (stats[category] || 0) + 1;
          return stats;
        }, {});
      }
      
      return {
        id: subsystem.id,
        name: subsystem.name,
        description: subsystem.description,
        componentCount: components.length,
        categoryStats: Object.keys(categoryStats).length ? categoryStats : undefined
      };
    })
  };
  
  // Write main index file
  const mainIndexPath = path.join(TARGET_BASE_DIR, 'index.json');
  fs.writeFileSync(mainIndexPath, JSON.stringify(mainIndex, null, 2));
  console.log(`✅ Generated main index with ${mainIndex.subsystems.length} subsystems`);
  
  // STEP 5: Write statistics file
  const statsPath = path.join(TARGET_BASE_DIR, 'parsing_stats.json');
  fs.writeFileSync(statsPath, JSON.stringify(stats, null, 2));
  
  // Print summary
  console.log('\n=========== PARSING SUMMARY ===========');
  console.log(`Total overview files: ${stats.totalOverviews}`);
  console.log(`Total component files: ${stats.totalComponents}`);
  console.log('\nComponents by subsystem:');
  
  for (const subsystem of SUBSYSTEMS) {
    const count = stats.subsystemStats[subsystem.id]?.components || 0;
    console.log(`  ${subsystem.name}: ${count} components`);
  }
  
  console.log('\n🎉 Parsing complete!');
  console.log('\nNext steps:');
  console.log('1. Run `npm run build` to rebuild the website with the updated subsystem data');
  console.log('2. View the subsystems pages to verify the data is correctly displayed');
}

// Run the main function
parseAllSubsystems();
