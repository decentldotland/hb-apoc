export const generateMetadata = async ({ params }) => {
  // Safely extract params using object destructuring to avoid direct access warnings
  const subsystemId = params?.subsystemId;
  const moduleId = params?.moduleId;

  try {
    // Try to fetch module data from API to generate metadata
    const response = await fetch(`${process.env.NEXT_PUBLIC_BASE_URL || ''}/api/subsystems/${subsystemId}/${moduleId}`);
    
    if (!response.ok) {
      // Default metadata if API request fails
      return {
        title: `${moduleId} Module - HyperBEAM Documentation`,
        description: `Documentation for the ${moduleId} module in the ${subsystemId} subsystem of HyperBEAM.`
      };
    }
    
    const moduleData = await response.json();
    
    return {
      title: `${moduleData.name || moduleId} - HyperBEAM Documentation`,
      description: moduleData.description || `Documentation for the ${moduleId} module in the ${subsystemId} subsystem of HyperBEAM.`
    };
  } catch (error) {
    console.error(`Error generating metadata for ${subsystemId}/${moduleId}:`, error);
    
    // Fallback metadata if there's an error
    return {
      title: `${moduleId} Module - HyperBEAM Documentation`,
      description: `Documentation for the ${moduleId} module in the ${subsystemId} subsystem of HyperBEAM.`
    };
  }
};
