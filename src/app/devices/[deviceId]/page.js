import fs from 'fs';
import path from 'path';
import Link from 'next/link';
import DeviceTemplate from '../../../components/DeviceTemplate';

// Dynamic metadata for SEO
export async function generateMetadata({ params }) {
  const { deviceId } = params;
  
  try {
    const dataDir = path.join(process.cwd(), 'src/data/devices/parsed_devices');
    const filePath = path.join(dataDir, `${deviceId}.json`);
    
    if (fs.existsSync(filePath)) {
      const fileContents = fs.readFileSync(filePath, 'utf8');
      const device = JSON.parse(fileContents);
      
      return {
        title: `${device.name} - HyperBEAM Device Documentation`,
        description: device.description || `Documentation for the ${device.name} device in HyperBEAM system.`
      };
    }
    
    return {
      title: 'Device Not Found - HyperBEAM Documentation',
      description: 'The requested device documentation could not be found.'
    };
  } catch (error) {
    console.error(`Error generating metadata for ${deviceId}:`, error);
    return {
      title: 'Error - HyperBEAM Device Documentation',
      description: 'There was an error loading the device documentation.'
    };
  }
}

// Generate static params at build time (equivalent to getStaticPaths)
export async function generateStaticParams() {
  try {
    const dataDir = path.join(process.cwd(), 'src/data/devices/parsed_devices');
    
    // If the directory doesn't exist yet, return empty params
    if (!fs.existsSync(dataDir)) {
      return [];
    }
    
    // Get all JSON files except categories.json and devices.json
    const deviceFiles = fs.readdirSync(dataDir)
      .filter(file => file.endsWith('.json') && 
                      file !== 'categories.json' && 
                      file !== 'devices.json');
    
    // Create params for each device
    return deviceFiles.map(file => ({
      deviceId: file.replace('.json', '')
    }));
  } catch (error) {
    console.error('Error generating static params:', error);
    return [];
  }
}

// Get device data (equivalent to getStaticProps)
async function getDeviceData(deviceId) {
  try {
    // Path to the parsed device data
    const dataDir = path.join(process.cwd(), 'src/data/devices/parsed_devices');
    const filePath = path.join(dataDir, `${deviceId}.json`);
    
    // Check if the device file exists
    if (!fs.existsSync(filePath)) {
      return { 
        device: null,
        error: `Device ${deviceId} not found` 
      };
    }
    
    // Read and parse the device data
    const fileContents = fs.readFileSync(filePath, 'utf8');
    const device = JSON.parse(fileContents);
    
    // Get related devices from the same category
    const categoriesPath = path.join(dataDir, 'categories.json');
    let relatedDevices = [];
    
    if (fs.existsSync(categoriesPath)) {
      const categories = JSON.parse(fs.readFileSync(categoriesPath, 'utf8'));
      const categoryDevices = categories[device.category] || [];
      
      // Filter out the current device and take up to 5 related devices
      relatedDevices = categoryDevices
        .filter(d => d.id !== deviceId)
        .slice(0, 5);
    }
    
    return {
      device: {
        ...device,
        relatedDevices
      },
      error: null
    };
  } catch (error) {
    console.error(`Error loading device data for ${deviceId}:`, error);
    return {
      device: null,
      error: 'Failed to load device data'
    };
  }
}

// Dynamic route component
export default async function DevicePage({ params }) {
  const { deviceId } = params;
  const { device, error } = await getDeviceData(deviceId);

  return (
    <>
      {error ? (
        <div className="bg-[#252526] border-l-4 border-red-500 p-4 rounded mb-6">
          <div className="flex">
            <div className="flex-shrink-0">
              <svg className="h-5 w-5 text-red-500" viewBox="0 0 20 20" fill="currentColor">
                <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
              </svg>
            </div>
            <div className="ml-3">
              <p className="text-sm text-terminal-text">{error}</p>
              <p className="mt-2 text-xs text-terminal-accent">
                <Link href="/devices" className="text-terminal-green hover:underline">
                  Return to devices list
                </Link>
              </p>
            </div>
          </div>
        </div>
      ) : device ? (
        <DeviceTemplate device={device} />
      ) : (
        <div className="bg-[#252526] border-l-4 border-terminal-yellow p-4 rounded mb-6">
          <div className="flex">
            <div className="flex-shrink-0">
              <svg className="h-5 w-5 text-terminal-yellow" viewBox="0 0 20 20" fill="currentColor">
                <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7 4a1 1 0 11-2 0 1 1 0 012 0zm-1-9a1 1 0 00-1 1v4a1 1 0 102 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
              </svg>
            </div>
            <div className="ml-3">
              <p className="text-sm text-terminal-text">Device not found: {deviceId}</p>
              <p className="mt-2 text-xs text-terminal-accent">
                <Link href="/devices" className="text-terminal-green hover:underline">
                  Browse available devices
                </Link>
              </p>
            </div>
          </div>
        </div>
      )}
    </>
  );
}
