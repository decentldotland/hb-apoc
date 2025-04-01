import { NextResponse } from 'next/server';
import fs from 'fs';
import path from 'path';

// Dynamic API route for fetching core module data
export async function GET(request, { params }) {
  // Await params before accessing properties - required in Next.js App Router
  const resolvedParams = await params;
  const moduleId = resolvedParams.moduleId;
  
  // Determine possible file patterns to try
  const possibleModulePatterns = [];
  
  // Handle special case for hb.erl which has a data file named hb_core_analysis.json
  if (moduleId === 'hb') {
    possibleModulePatterns.push('hb_core_analysis');
  }
  
  // Add _analysis suffix if not already there
  const moduleIdWithAnalysis = moduleId.endsWith('_analysis') ? moduleId : `${moduleId}_analysis`;
  possibleModulePatterns.push(moduleIdWithAnalysis);
  
  try {
    // Get the core directory
    const dataDir = path.join(process.cwd(), 'src', 'data', 'core');
    
    // Check if any of the possible file patterns exist
    for (const pattern of possibleModulePatterns) {
      const jsonFilePath = path.join(dataDir, `${pattern}.json`);
      
      if (fs.existsSync(jsonFilePath)) {
        // Read the JSON file if it exists
        const fileContent = fs.readFileSync(jsonFilePath, 'utf8');
        const moduleData = JSON.parse(fileContent);
        
        // Return the actual module data
        return NextResponse.json(moduleData);
      }
    }
    
    // If none of the patterns match, list the directory to see what's available
    let availableFiles = [];
    if (fs.existsSync(dataDir)) {
      availableFiles = fs.readdirSync(dataDir);
      console.log(`Available files in core:`, availableFiles);
    }
    
    // If JSON file doesn't exist, create mock data as fallback
    const mockModuleData = {
      name: moduleId.replace('_analysis', ''),
      subsystem: 'core',
      filename: `${moduleId.replace('_analysis', '')}.erl`,
      description: `Documentation for the ${moduleId.replace('_analysis', '')} core module.`,
      content: `
# ${moduleId.replace('_analysis', '')}

## Overview

This is a sample documentation for the ${moduleId.replace('_analysis', '')} core module.

## Functions

- \`function_one/1\`: Basic function implementation
- \`function_two/2\`: Advanced function with multiple parameters
- \`function_three/3\`: Complex function for data processing

## Example Usage

\`\`\`erlang
Result = ${moduleId.replace('_analysis', '')}:function_one(InputData).
\`\`\`

## Notes

Core module documentation is still being compiled for this module.
      `
    };
    
    // Return the mock data with a note
    console.log(`JSON file not found for core/${moduleId}, using mock data`);
    return NextResponse.json(mockModuleData);
  } catch (error) {
    console.error(`Error fetching module data for core/${moduleId}:`, error);
    
    // Return error response
    return NextResponse.json(
      { 
        error: 'Failed to load module data', 
        message: error.message,
        subsystem: 'core',
        moduleId
      },
      { status: 500 }
    );
  }
}
