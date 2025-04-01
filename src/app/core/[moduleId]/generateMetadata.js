import fs from 'fs';
import path from 'path';

// Generate metadata for core module pages
export async function generateMetadata({ params }) {
  const moduleId = params.moduleId;
  
  try {
    // Define patterns to search for the file
    const possiblePatterns = [];
    
    // Handle special case for hb.erl which has a data file named hb_core_analysis.json
    if (moduleId === 'hb') {
      possiblePatterns.push('hb_core_analysis');
    }
    
    // Add the standard pattern
    const moduleIdWithAnalysis = moduleId.endsWith('_analysis') ? moduleId : `${moduleId}_analysis`;
    possiblePatterns.push(moduleIdWithAnalysis);
    
    const dataDir = path.join(process.cwd(), 'src', 'data', 'core');
    
    let moduleData = null;
    
    // Try all possible patterns
    for (const pattern of possiblePatterns) {
      const jsonFilePath = path.join(dataDir, `${pattern}.json`);
      
      if (fs.existsSync(jsonFilePath)) {
        const fileContent = fs.readFileSync(jsonFilePath, 'utf8');
        moduleData = JSON.parse(fileContent);
        break; // Exit loop once we find a match
      }
    }
    
    // Default metadata if no data is found
    if (!moduleData) {
      return {
        title: `${moduleId}.erl | HyperBEAM Documentation`,
        description: `Core module documentation for ${moduleId}.erl`,
        openGraph: {
          title: `${moduleId}.erl | HyperBEAM Documentation`,
          description: `Core module documentation for ${moduleId}.erl`,
          type: 'article',
        },
      };
    }
    
    // Metadata with actual module data
    return {
      title: `${moduleData.filename} | HyperBEAM Documentation`,
      description: moduleData.description || `Documentation for the ${moduleData.filename} core module`,
      openGraph: {
        title: `${moduleData.filename} | HyperBEAM Core Documentation`,
        description: moduleData.description || `Documentation for the ${moduleData.filename} core module`,
        type: 'article',
      },
    };
  } catch (error) {
    console.error(`Error generating metadata for core/${moduleId}:`, error);
    
    // Fallback metadata
    return {
      title: `${moduleId} | HyperBEAM Documentation`,
      description: `Core module documentation`,
    };
  }
}
