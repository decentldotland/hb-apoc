import { NextResponse } from 'next/server';
import fs from 'fs';
import path from 'path';

// Dynamic API route for fetching module data from any subsystem
export async function GET(request, { params }) {
  // Await params before accessing properties - required in Next.js App Router
  const resolvedParams = await params;
  const subsystemId = resolvedParams.subsystemId;
  const moduleId = resolvedParams.moduleId;
  
  // Determine possible file patterns to try based on the subsystem
  const possibleModulePatterns = [];
  
  // Add _analysis suffix if not already there
  const moduleIdWithAnalysis = moduleId.endsWith('_analysis') ? moduleId : `${moduleId}_analysis`;
  possibleModulePatterns.push(moduleIdWithAnalysis);
  
  // Special patterns for arweave subsystem
  if (subsystemId === 'arweave') {
    possibleModulePatterns.push(moduleIdWithAnalysis.replace(/^hb_/, 'ar_'));
    possibleModulePatterns.push(moduleId.replace(/^hb_/, 'ar_') + '_analysis');
  }
  
  // Special patterns for codec subsystem
  if (subsystemId === 'codec') {
    possibleModulePatterns.push(moduleIdWithAnalysis.replace(/^hb_/, 'dev_codec_'));
    possibleModulePatterns.push(moduleId.replace(/^hb_/, 'dev_codec_') + '_analysis');
    possibleModulePatterns.push('dev_' + moduleId + '_analysis');
  }
  
  try {
    // Get the subsystem directory
    const dataDir = path.join(process.cwd(), 'src', 'data', 'subsystems', subsystemId);
    
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
      console.log(`Available files in ${subsystemId}:`, availableFiles);
    }
    
    // If JSON file doesn't exist, create mock data as fallback
    const mockModuleData = {
      name: moduleId.replace('_analysis', ''),
      subsystem: subsystemId,
      filename: `${moduleId.replace('_analysis', '')}.erl`,
      description: `Documentation for the ${moduleId.replace('_analysis', '')} module in the ${subsystemId} subsystem.`,
      content: `
# ${moduleId.replace('_analysis', '')}

## Overview

This is a sample module documentation for ${moduleId.replace('_analysis', '')} in the ${subsystemId} subsystem.

## Functions

- \`function_one/1\`: Basic function implementation
- \`function_two/2\`: Advanced function with multiple parameters
- \`function_three/3\`: Complex function for data processing

## Example Usage

\`\`\`erlang
Result = ${moduleId.replace('_analysis', '')}:function_one(InputData).
\`\`\`

## Notes

Documentation is still being compiled for this module by the HyperBEAM apocryphal community.
      `
    };
    
    // Return the mock data with a note
    console.log(`JSON file not found for ${subsystemId}/${moduleId}, using mock data`);
    return NextResponse.json(mockModuleData);
  } catch (error) {
    console.error(`Error fetching module data for ${subsystemId}/${moduleId}:`, error);
    
    // Return error response
    return NextResponse.json(
      { 
        error: 'Failed to load module data', 
        message: error.message,
        subsystemId,
        moduleId
      },
      { status: 500 }
    );
  }
}
