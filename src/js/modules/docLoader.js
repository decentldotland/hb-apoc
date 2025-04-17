// Load available versions
export async function loadVersions() {
    const response = await fetch('/versions.json');
    if (!response.ok) {
        throw new Error(`Failed to fetch versions: ${response.statusText}`);
    }
    return response.json();
}

// Load documentation chunks and folder overviews for a specific version
export async function loadChunks(version) {
    const versionsData = await loadVersions();
    const versionInfo = versionsData[version];
    
    if (!versionInfo) {
        throw new Error(`Version ${version} not found`);
    }
    
    // Load folder overviews directly from metadata
    const folderOverviews = versionInfo.metadata?.overviewMap || {};
    
    const chunks = await Promise.all(
        versionInfo.chunks.map(async chunkFile => {
            const response = await fetch(`/versions/${version}/${chunkFile}`);
            if (!response.ok) {
                throw new Error(`Failed to load chunk ${chunkFile}: ${response.statusText}`);
            }
            return response.json();
        })
    );
    
    const files = Object.assign({}, ...chunks);
    
    return {
        files,
        folderOverviews
    };
}

// Flatten file structure for easier access
export function flattenFileStructure(files) {
    const flattened = {};
    
    for (const [path, content] of Object.entries(files)) {
        // Keep original paths for all files
        flattened[path] = content;
    }
    
    return flattened;
}

// Load and process a specific version
export async function loadVersion(version) {
    const versionsData = await loadVersions();
    const versionInfo = versionsData[version];
    const { files, folderOverviews } = await loadChunks(version);
    
    // Keep original files for metadata paths that include .md extension
    return {
        files,
        folderOverviews,
        metadata: versionInfo.metadata || {}
    };
}
