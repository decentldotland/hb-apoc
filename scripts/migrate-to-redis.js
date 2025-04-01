/**
 * HyperBEAM Documentation Redis Migration Script
 * 
 * This script migrates documentation data from the filesystem to Redis,
 * with support for versioning and incremental updates.
 * 
 * Usage:
 *   node migrate-to-redis.js                    # Migrate base v1.0 data
 *   node migrate-to-redis.js --version=v2.0     # Create a new v2.0 version
 *   node migrate-to-redis.js --version=v2.0 --base-version=v1.0  # Specify base version
 */

import * as dataProvider from '../src/lib/data-provider.js';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

// Get __dirname equivalent in ES modules
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Parse command line arguments
const args = process.argv.slice(2);
const versionArg = args.find(arg => arg.startsWith('--version='));
const baseVersionArg = args.find(arg => arg.startsWith('--base-version='));

const version = versionArg ? versionArg.replace('--version=', '') : 'v1.0';
const baseVersion = baseVersionArg ? baseVersionArg.replace('--base-version=', '') : 'v1.0';

/**
 * Recursively process a directory and migrate files to Redis
 * 
 * @param {string} dir - Directory to process
 * @param {string} baseDir - Base directory for calculating relative paths
 * @param {string} version - Version to store in Redis
 * @param {Set<string>} processedPaths - Set of paths already processed
 * @returns {Promise<number>} - Number of files processed
 */
async function processDirectory(dir, baseDir, version, processedPaths = new Set()) {
  console.log(`Processing directory: ${dir}`);
  let count = 0;
  
  try {
    // Skip if directory doesn't exist
    if (!fs.existsSync(dir)) {
      console.log(`Directory not found: ${dir}`);
      return 0;
    }
    
    const entries = fs.readdirSync(dir);
    
    for (const entry of entries) {
      const fullPath = path.join(dir, entry);
      const stats = fs.statSync(fullPath);
      
      if (stats.isDirectory()) {
        // Skip version directories when processing base directory
        if (entry.match(/^v\d+\.\d+$/) && dir === baseDir) {
          console.log(`Skipping version directory: ${entry}`);
          continue;
        }
        
        // Recursively process subdirectories
        count += await processDirectory(fullPath, baseDir, version, processedPaths);
      } else if (stats.isFile() && entry.endsWith('.json')) {
        // Process JSON files
        const relativePath = path.relative(baseDir, fullPath)
          .replace(/^v\d+\.\d+\//, ''); // Remove version prefix if present
          
        // Skip if we've already processed this path
        if (processedPaths.has(relativePath)) {
          console.log(`Skipping already processed: ${relativePath}`);
          continue;
        }
        
        try {
          // Read the file content
          const content = fs.readFileSync(fullPath, 'utf8');
          const data = JSON.parse(content);
          
          // Add version information to the data
          if (typeof data === 'object' && data !== null) {
            data.version = version;
          }
          
          // Store in Redis
          await dataProvider.writeData(relativePath, data, version);
          processedPaths.add(relativePath);
          count++;
          
          console.log(`Processed: ${relativePath} (${version})`);
        } catch (error) {
          console.error(`Error processing ${fullPath}:`, error);
        }
      }
    }
  } catch (error) {
    console.error(`Error processing directory ${dir}:`, error);
  }
  
  return count;
}

/**
 * Migrate base v1.0 version data
 */
async function migrateBaseVersion() {
  console.log('Starting migration of base v1.0 data');
  
  // Base data directory (assuming it's relative to this script's parent directory)
  const baseDir = path.resolve(__dirname, '..', 'src', 'data');
  
  // Process all files recursively
  const count = await processDirectory(baseDir, baseDir, 'v1.0');
  
  // Set up version management keys
  await dataProvider.setVersions(['v1.0', 'latest']);
  await dataProvider.setLatestVersion('v1.0');
  
  console.log(`Completed migration of v1.0 with ${count} files`);
}

/**
 * Migrate a new version with fallback to previous version
 */
async function migrateNewVersion() {
  console.log(`Migrating version ${version} with fallback to ${baseVersion}`);
  
  // 1. Get current versions and add the new one
  const currentVersions = await dataProvider.getVersions();
  if (!currentVersions.includes(version)) {
    const newVersions = currentVersions.filter(v => v !== 'latest');
    newVersions.push(version);
    newVersions.push('latest');
    await dataProvider.setVersions(newVersions);
  }
  
  // 2. Set latest version to point to the new version
  await dataProvider.setLatestVersion(version);
  
  // 3. Get list of all keys from the base version to copy
  console.log(`Copying data from ${baseVersion} to ${version}...`);
  
  // Get all keys from base version
  const baseDir = path.resolve(__dirname, '..', 'src', 'data');
  const versionDir = path.join(baseDir, version);
  
  // Track which paths we've already processed
  const processedPaths = new Set();
  
  // Only duplicate files from baseVersion if it's not the same as the new version
  if (baseVersion !== version) {
    // Create a set to track file paths we've copied from the base version
    // Store paths in the format they'll be stored in Redis (without version prefix)
    const processedFromBase = await duplicateFromBaseVersion(baseVersion, version, baseDir);
    
    // Add all paths from processedFromBase to processedPaths
    for (const path of processedFromBase) {
      processedPaths.add(path);
    }
  }
  
  // 4. Process the version-specific directory if it exists
  // Process the version-specific directory if it exists
  if (fs.existsSync(versionDir)) {
    const count = await processDirectory(versionDir, baseDir, version, processedPaths);
    console.log(`Updated ${count} files from ${version} directory`);
  } else {
    console.log(`No directory found for version ${version}`);
  }
  
  console.log(`Completed migration to ${version}`);
}

/**
 * Duplicate data from a base version to a new version
 */
async function duplicateFromBaseVersion(baseVersion, newVersion, baseDir) {
  console.log(`Copying data from ${baseVersion} to ${newVersion}...`);
  
  // Get all files in the base version
  const processedPaths = new Set();
  
  // Recursively find all data files for the base version
  async function findBaseVersionFiles(dir, relativePath = '') {
    if (!fs.existsSync(dir)) return;
    
    const entries = fs.readdirSync(dir);
    
    for (const entry of entries) {
      const fullPath = path.join(dir, entry);
      const stats = fs.statSync(fullPath);
      
      // Skip version directories
      if (stats.isDirectory()) {
        if (entry.match(/^v\d+\.\d+$/)) continue;
        
        // Recursively process subdirectories
        await findBaseVersionFiles(
          fullPath, 
          relativePath ? path.join(relativePath, entry) : entry
        );
      } else if (stats.isFile() && entry.endsWith('.json')) {
        // Process JSON files
        const filePath = relativePath ? path.join(relativePath, entry) : entry;
        
        try {
          // Read the base version data
          const data = await dataProvider.readData(filePath, baseVersion);
          
          // Update version information
          if (typeof data === 'object' && data !== null) {
            data.version = newVersion;
          }
          
          // Write to the new version
          await dataProvider.writeData(filePath, data, newVersion);
          
          // Track that we've processed this path
          processedPaths.add(filePath);
          
          console.log(`Copied: ${filePath} from ${baseVersion} to ${newVersion}`);
        } catch (error) {
          console.error(`Error copying ${filePath}:`, error);
        }
      }
    }
  }
  
  await findBaseVersionFiles(path.join(baseDir, baseVersion));
  
  // If baseVersion is v1.0, also process the base directory without version prefix
  if (baseVersion === 'v1.0') {
    await findBaseVersionFiles(baseDir);
  }
  
  console.log(`Copied ${processedPaths.size} files from ${baseVersion} to ${newVersion}`);
  return processedPaths;
}

// Initialize the data provider with mock data (if needed)
await dataProvider.initMockData();

// Run migration based on arguments
if (version === 'v1.0') {
  await migrateBaseVersion();
} else {
  await migrateNewVersion();
}

console.log('Migration completed successfully!');
process.exit(0);
