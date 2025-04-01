const fs = require('fs');
const path = require('path');
const matter = require('gray-matter');

/**
 * Parses device analysis markdown files and converts them to structured JSON
 * for use in the HyperBEAM documentation website
 */

// Configuration
const sourceDir = path.join(__dirname, 'device_analysis');
const outputDir = path.join(__dirname, 'parsed_devices');
const outputJsonFile = path.join(outputDir, 'devices.json');

// Ensure output directories exist
if (!fs.existsSync(outputDir)) {
  fs.mkdirSync(outputDir, { recursive: true });
}

// Parse a single markdown file into structured data
function parseDeviceFile(filePath) {
  try {
    console.log(`Processing ${filePath}...`);
    const content = fs.readFileSync(filePath, 'utf8');
    
    // Extract device name from file heading - try multiple patterns
    let titleMatch = content.match(/# (.*?) Analysis \(`(.*?)`\)/);
    
    // First fallback pattern - try without backticks
    if (!titleMatch) {
      titleMatch = content.match(/# (.*?) Analysis \((.*?)\)/);
    }
    
    // Second fallback pattern - any title with "Analysis" followed by filename
    if (!titleMatch) {
      titleMatch = content.match(/# `?(.*?)`? (?:Module )?Analysis/);
      
      // If we found a title this way, try to extract the filename separately
      if (titleMatch) {
        const filenameMatch = content.toLowerCase().match(/`(dev_[a-z_]+\.erl)`/) || 
                              content.toLowerCase().match(/module: ([a-z_]+)/) ||
                              content.toLowerCase().match(/\b(dev_[a-z_]+)\b/);
        
        return parseDeviceFileWithExtractedInfo(
          filePath, 
          titleMatch[1].trim(), 
          filenameMatch ? (filenameMatch[1].endsWith('.erl') ? filenameMatch[1] : filenameMatch[1] + '.erl') : path.basename(filePath).replace('.md', '.erl'),
          content
        );
      }
    }
    
    // Third fallback - last resort, use the filename as basis
    if (!titleMatch) {
      const baseFilename = path.basename(filePath, '.md');
      const match = baseFilename.match(/\d+_(.+)_analysis/);
      
      if (match) {
        const deviceModuleName = match[1];
        const prettyName = deviceModuleName
          .replace(/^dev_/, '')
          .split('_')
          .map(word => word.charAt(0).toUpperCase() + word.slice(1))
          .join(' ');
          
        return parseDeviceFileWithExtractedInfo(
          filePath, 
          prettyName, 
          `${deviceModuleName}.erl`, 
          content
        );
      } else {
        console.warn(`Could not extract title from ${filePath}`);
        return null;
      }
    }
    
    const deviceName = titleMatch[1].trim();
    const filename = titleMatch[2] ? titleMatch[2].trim() : `dev_${deviceName.toLowerCase().replace(/ /g, '_')}.erl`;

    return parseDeviceFileWithExtractedInfo(filePath, deviceName, filename, content);
  } catch (error) {
    console.error(`Error processing ${filePath}:`, error);
    return null;
  }
}

// Helper function to parse file once we've extracted title info
function parseDeviceFileWithExtractedInfo(filePath, deviceName, filename, content) {
  try {
    // Process sections
    const sections = {};
    
    // Helper to extract sections by header
    function extractSection(sectionTitle, rawContent) {
      const regex = new RegExp(`## ${sectionTitle}\\s*([\\s\\S]*?)(?=\\s*##\\s|$)`, 'i');
      const match = rawContent.match(regex);
      return match ? match[1].trim() : null;
    }
    
    // Extract all sections from the markdown file
    sections.overview = extractSection('Overview', content) || "";
    sections.keyCharacteristics = extractSection('Key Characteristics', content) || "";
    sections.dependencies = extractSection('Dependencies', content) || "";
    sections.implementationDetails = extractSection('Implementation Details', content) || "";
    sections.integrationWithHyperBEAM = extractSection('Integration with HyperBEAM', content) || "";
    sections.testingApproach = extractSection('Testing Approach', content) || "";
    sections.observations = extractSection('Observations and Insights', content) || "";
    sections.architecturalSignificance = extractSection('Architectural Significance', content) || "";
    sections.conclusion = extractSection('Conclusion', content) || "";
    
    // Extract common subsections if they exist
    sections.strengths = extractSubsection('Strengths', content) || "";
    sections.designPatterns = extractSubsection('Design Patterns', content) || "";
    sections.challenges = extractSubsection('Challenges and Limitations', content) || "";
    sections.futureOpportunities = extractSubsection('Future Opportunities', content) || "";
    
    // Extra helper to extract subsections (typically within Observations)
    function extractSubsection(subsectionTitle, rawContent) {
      const regex = new RegExp(`### ${subsectionTitle}\\s*([\\s\\S]*?)(?=\\s*###|\\s*##|$)`, 'i');
      const match = rawContent.match(regex);
      return match ? match[1].trim() : null;
    }
    
    // We're no longer separately extracting code samples - keeping them in their original context
    // Instead, we'll just preserve the full raw markdown content for each section
    
    // Determine device category based on filename or content analysis
    let category = 'utility'; // Default category
    
    // Simple heuristics for categorization
    if (filename.includes('scheduler') || content.toLowerCase().includes('schedule') && content.toLowerCase().includes('assignment')) {
      category = 'scheduler';
    } else if (filename.startsWith('dev_p') && (filename.includes('pay') || content.toLowerCase().includes('payment'))) {
      category = 'payment';
    } else if ((filename.includes('push') || filename.includes('relay') || filename.includes('lookup')) && 
               content.toLowerCase().includes('message')) {
      category = 'routing';
    } else if (filename.includes('wasm') || filename.includes('compute') || filename.includes('runtime')) {
      category = 'runtime';
    } else if (content.toLowerCase().includes('security') || content.toLowerCase().includes('attestation') ||
               content.toLowerCase().includes('crypto')) {
      category = 'security';
    } else if (filename.startsWith('dev_') && (
               filename.includes('message') || 
               filename.includes('process') || 
               filename.includes('converge'))) {
      category = 'core';
    }
    
    // Create the structured device data with more detailed information
    // Keep the original markdown content without separately extracting code blocks
    const deviceData = {
      id: filename.replace('.erl', ''),
      name: deviceName,
      filename: filename,
      category: category,
      // Just store the raw markdown content for each section
      sections: sections,
      // Add metadata for improved search and organization
      metadata: {
        hasTests: content.includes('_test') || sections.testingApproach.length > 0,
        dependencies: extractDependencies(content),
        analysisCompleteness: calculateCompleteness(sections),
        // Add source info for attribution
        source: {
          originalFile: path.basename(filePath),
          parsedDate: new Date().toISOString()
        }
      },
      // Store the full original markdown content for completeness
      originalContent: content
    };
    
    // Extract dependencies from content
    function extractDependencies(rawContent) {
      const deps = [];
      const depRegex = /`([a-zA-Z0-9_]+)`/g;
      const dependenciesSection = extractSection('Dependencies', rawContent) || "";
      
      let match;
      while ((match = depRegex.exec(dependenciesSection)) !== null) {
        deps.push(match[1]);
      }
      
      return deps;
    }
    
    // Calculate how complete the documentation is
    function calculateCompleteness(sections) {
      const totalSections = 9; // Number of main sections
      let filledSections = 0;
      
      for (const key in sections) {
        if (sections[key] && sections[key].length > 20) { // Only count non-empty sections
          filledSections++;
        }
      }
      
      return Math.round((filledSections / totalSections) * 100);
    }
    
    return deviceData;
  } catch (error) {
    console.error(`Error processing device data for ${filePath}:`, error);
    return null;
  }
}

// Process all files in the directory
function processAllDeviceFiles() {
  console.log(`Scanning directory: ${sourceDir}`);
  
  if (!fs.existsSync(sourceDir)) {
    console.error(`Directory ${sourceDir} does not exist!`);
    return [];
  }
  
  // Get all files recursively
  const getAllFiles = (dir) => {
    let results = [];
    const list = fs.readdirSync(dir);
    
    list.forEach(file => {
      const filePath = path.join(dir, file);
      const stat = fs.statSync(filePath);
      
      if (stat && stat.isDirectory()) {
        // Recurse into subdirectories
        results = results.concat(getAllFiles(filePath));
      } else {
        // Add file to the results
        if (file.endsWith('.md') && !file.includes('overview')) {
          results.push(filePath);
        }
      }
    });
    
    return results;
  };

  const deviceFilePaths = getAllFiles(sourceDir);
  console.log(`Found ${deviceFilePaths.length} device analysis files`);
  
  const devices = [];
  
  deviceFilePaths.forEach(filePath => {
    const file = path.basename(filePath);
    const deviceData = parseDeviceFile(filePath);
    
    if (deviceData) {
      devices.push(deviceData);
      
      // Also save individual JSON for each device
      const deviceJsonPath = path.join(outputDir, `${deviceData.id}.json`);
      fs.writeFileSync(deviceJsonPath, JSON.stringify(deviceData, null, 2));
    }
  });
  
  // Save the complete devices data
  fs.writeFileSync(outputJsonFile, JSON.stringify(devices, null, 2));
  
  console.log(`\nProcessed ${devices.length} device files`);
  console.log(`Output saved to ${outputJsonFile}`);
  
  // Generate a category summary
  const categories = {};
  devices.forEach(device => {
    if (!categories[device.category]) {
      categories[device.category] = [];
    }
    categories[device.category].push({
      id: device.id,
      name: device.name,
      filename: device.filename
    });
  });
  
  fs.writeFileSync(
    path.join(outputDir, 'categories.json'), 
    JSON.stringify(categories, null, 2)
  );
  
  return devices;
}

// Main execution
try {
  const devices = processAllDeviceFiles();
  console.log('Device analysis parsing complete!');
  console.log('----------------------------------------');
  console.log('Categories:');
  
  // Print summary of categories
  const categories = {};
  devices.forEach(device => {
    if (!categories[device.category]) {
      categories[device.category] = 0;
    }
    categories[device.category]++;
  });
  
  Object.keys(categories).sort().forEach(category => {
    console.log(`${category}: ${categories[category]} devices`);
  });
} catch (error) {
  console.error('Error processing device files:', error);
}
