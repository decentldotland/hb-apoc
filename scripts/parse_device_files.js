/**
 * Script to parse device analysis files and integrate them into the website
 * This script serves as a wrapper to run the main parser in the HB analysis stage 2 directory
 */
const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');

// Path to the HB analysis stage 2 directory
const analysisDir = path.resolve(__dirname, '../../HB analysis stage 2');
const parserPath = path.join(analysisDir, 'parse_device_analysis.js');
const parsedDevicesDir = path.join(analysisDir, 'parsed_devices');

console.log('🚀 HyperBEAM Device Analysis Parser');
console.log('----------------------------------');

// Check if the parser exists
if (!fs.existsSync(parserPath)) {
  console.error(`❌ Parser script not found at: ${parserPath}`);
  process.exit(1);
}

// Check if the device_analysis directory exists
const deviceAnalysisDir = path.join(analysisDir, 'device_analysis');
if (!fs.existsSync(deviceAnalysisDir)) {
  console.error(`❌ Device analysis directory not found at: ${deviceAnalysisDir}`);
  process.exit(1);
}

// Count the number of analysis files
const analysisFiles = fs.readdirSync(deviceAnalysisDir)
  .filter(file => file.endsWith('.md') && !file.includes('overview'));
console.log(`📋 Found ${analysisFiles.length} device analysis files`);

try {
  // Run the parser script
  console.log('⏳ Parsing device analysis files...');
  execSync(`node ${parserPath}`, { stdio: 'inherit', cwd: analysisDir });
  
  // Check if the parsed files were created
  if (fs.existsSync(parsedDevicesDir)) {
    const parsedFiles = fs.readdirSync(parsedDevicesDir)
      .filter(file => file.endsWith('.json'));
    console.log(`✅ Successfully generated ${parsedFiles.length} JSON files`);
    
    // Create the devices data directory in the website
    const targetDir = path.resolve(__dirname, '../src/data/devices');
    
    if (!fs.existsSync(path.dirname(targetDir))) {
      fs.mkdirSync(path.dirname(targetDir), { recursive: true });
    }
    
    if (!fs.existsSync(targetDir)) {
      fs.mkdirSync(targetDir, { recursive: true });
    }
    
    // Always copy the files to ensure we have the latest version
    console.log(`📂 Copying device files to website data directory...`);
    
    let copiedCount = 0;
    
    // Copy individual device JSON files
    parsedFiles.forEach(file => {
      const sourceFile = path.join(parsedDevicesDir, file);
      const targetFile = path.join(targetDir, file);
      fs.copyFileSync(sourceFile, targetFile);
      copiedCount++;
    });
    
    // Copy categories.json
    if (fs.existsSync(path.join(parsedDevicesDir, 'categories.json'))) {
      fs.copyFileSync(
        path.join(parsedDevicesDir, 'categories.json'),
        path.join(targetDir, 'categories.json')
      );
      copiedCount++;
    }
    
    // Copy devices.json 
    if (fs.existsSync(path.join(parsedDevicesDir, 'devices.json'))) {
      fs.copyFileSync(
        path.join(parsedDevicesDir, 'devices.json'),
        path.join(targetDir, 'devices.json')
      );
      copiedCount++;
    }
    
    console.log(`📂 Copied ${copiedCount} files to: ${targetDir}`);
    
    // Check for existing device listings in the website
    const websiteDevicesList = path.resolve(__dirname, '../src/data/device-list.json');
    if (fs.existsSync(websiteDevicesList)) {
      try {
        console.log(`🔄 Updating device-list.json with new device information...`);
        const deviceList = JSON.parse(fs.readFileSync(websiteDevicesList, 'utf8'));
        
        // Get our newly parsed devices
        const parsedDevices = JSON.parse(fs.readFileSync(path.join(targetDir, 'devices.json'), 'utf8'));
        
        // Map devices from our parser to match the existing format
        const updatedDevices = parsedDevices.map(device => ({
          id: device.id,
          name: device.name,
          description: device.sections.overview.split('\r\n')[0] || '', // Just take the first paragraph
          category: device.category,
          filename: device.filename
        }));
        
        // Merge with existing devices, preferring our new data for duplicates
        const mergedDevices = [...deviceList];
        
        // Update or add devices
        updatedDevices.forEach(newDevice => {
          const existingIndex = mergedDevices.findIndex(d => d.id === newDevice.id);
          if (existingIndex >= 0) {
            mergedDevices[existingIndex] = newDevice;
          } else {
            mergedDevices.push(newDevice);
          }
        });
        
        // Write back the updated list
        fs.writeFileSync(websiteDevicesList, JSON.stringify(mergedDevices, null, 2));
        console.log(`✅ Updated device list with ${updatedDevices.length} devices`);
      } catch (err) {
        console.warn(`⚠️ Could not update device-list.json: ${err.message}`);
      }
    }
    
    console.log('');
    console.log('🎉 Device analysis integration complete!');
    console.log('');
    console.log('Next steps:');
    console.log('1. Run `npm run build` to rebuild the website with the updated device data');
    console.log('2. Visit /devices/{deviceId} to view the device documentation');
  } else {
    console.error(`❌ No parsed files generated at: ${parsedDevicesDir}`);
  }
} catch (error) {
  console.error('❌ Error running parser script:', error.message);
  process.exit(1);
}
