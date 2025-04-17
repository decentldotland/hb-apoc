const fs = require('fs');
const path = require('path');

const CONTENT_DIR = path.join(__dirname, '../content');
const PUBLIC_DIR = path.join(__dirname, '../public');
const CHUNK_SIZE = 50 * 1024; // 50KB per chunk

function getVersions() {
    const versions = fs.readdirSync(CONTENT_DIR)
        .filter(file => fs.statSync(path.join(CONTENT_DIR, file)).isDirectory());
    
    return versions.map(version => {
        const metadataPath = path.join(CONTENT_DIR, version, 'version.json');
        let metadata = {};
        
        if (fs.existsSync(metadataPath)) {
            metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
        }
        
        return {
            name: version,
            metadata
        };
    });
}

// Keywords that indicate a file is an overview
const OVERVIEW_KEYWORDS = ['overview', 'summary'];

function findFirstFolderMarkdown(files) {
    const filePaths = Object.keys(files);
    
    // First look for any markdown file at root level
    const rootMarkdowns = filePaths.filter(path => 
        !path.includes('/') && // No directory separators = root level
        path.endsWith('.md')
    );
    
    if (rootMarkdowns.length > 0) {
        return rootMarkdowns[0];
    }
    
    return null;
}

function findOverviewFiles(files) {
    const overviewMap = {};
    
    // First pass: collect all files with overview keywords
    for (const filePath of Object.keys(files)) {
        // Ensure consistent forward slashes
        const normalizedPath = filePath.replace(/\\/g, '/');
        const parts = normalizedPath.split('/');
        const fileName = parts[parts.length - 1].toLowerCase();
        
        // Skip if this is a file in the root directory
        if (parts.length < 2) continue;
        
        // Check if this file contains any overview keywords
        if (OVERVIEW_KEYWORDS.some(keyword => fileName.includes(keyword))) {
            // Get the parent folder path
            const parentFolder = parts.slice(0, -1).join('/');
            
            // Count how many keywords this file matches
            const keywordCount = OVERVIEW_KEYWORDS.filter(k => fileName.includes(k)).length;
            
            // If we haven't found an overview for this folder yet, or if this one has more keywords
            if (!overviewMap[parentFolder] || 
                keywordCount > OVERVIEW_KEYWORDS.filter(k => overviewMap[parentFolder].toLowerCase().includes(k)).length) {
                // Store both the folder path and overview file path with consistent forward slashes
                overviewMap[parentFolder] = normalizedPath;
                console.log(`Found overview file for ${parentFolder}:`, {
                    folder: parentFolder,
                    file: normalizedPath,
                    keywordCount,
                    keywords: OVERVIEW_KEYWORDS.filter(k => fileName.includes(k))
                });
                console.log('Current overviewMap:', overviewMap);
            }
        }
    }
    
    return overviewMap;
}

function processMarkdownFile(filePath) {
    const content = fs.readFileSync(filePath, 'utf8');
    return {
        content: content
    };
}

function walkDirectory(dir) {
    const files = {};
    
    function walk(currentPath, basePath = '') {
        const entries = fs.readdirSync(currentPath);
        
        // Sort entries to ensure consistent order
        entries.sort((a, b) => {
            // Extract numbers from start of filenames (if they exist)
            const aNum = parseInt(a.match(/^(\d+)/)?.[1] || '0');
            const bNum = parseInt(b.match(/^(\d+)/)?.[1] || '0');
            
            if (aNum !== bNum) return aNum - bNum;
            return a.localeCompare(b);
        });
        
        for (const entry of entries) {
            const fullPath = path.join(currentPath, entry);
            const relativePath = path.join(basePath, entry).replace(/\\/g, '/');
            const stat = fs.statSync(fullPath);
            
            if (stat.isDirectory()) {
                walk(fullPath, relativePath);
            } else if (entry.endsWith('.md')) {
                const content = processMarkdownFile(fullPath);
                files[relativePath] = content;
            }
        }
    }
    
    walk(dir);
    return files;
}

function createChunks(files) {
    const chunks = [{}];
    let currentSize = 0;
    let currentChunk = 0;
    
    for (const [path, content] of Object.entries(files)) {
        const contentSize = JSON.stringify(content).length;
        
        if (currentSize + contentSize > CHUNK_SIZE && Object.keys(chunks[currentChunk]).length > 0) {
            currentChunk++;
            chunks[currentChunk] = {};
            currentSize = 0;
        }
        
        chunks[currentChunk][path] = content;
        currentSize += contentSize;
    }
    
    return chunks;
}

function buildVersion(version) {
    const versionDir = path.join(CONTENT_DIR, version.name);
    const files = walkDirectory(versionDir);
    const overviewMap = findOverviewFiles(files);
    const chunks = createChunks(files);
    const firstFolderMd = findFirstFolderMarkdown(files);
    
    // Create version directory in public
    const publicVersionDir = path.join(PUBLIC_DIR, 'versions', version.name);
    fs.mkdirSync(publicVersionDir, { recursive: true });
    
    // Write chunks as static JSON files
    chunks.forEach((chunk, i) => {
        fs.writeFileSync(
            path.join(publicVersionDir, `chunk.${i}.json`),
            JSON.stringify(chunk)
        );
    });
    
    if (firstFolderMd) {
        console.log(`Found first folder markdown file: ${firstFolderMd}`);
    }

    return {
        chunks: chunks.map((_, i) => `chunk.${i}.json`),
        metadata: {
            overviewMap,
            homePage: firstFolderMd // Store which file should be shown on home page
        }
    };
}

function build() {
    // Create public directory
    fs.mkdirSync(PUBLIC_DIR, { recursive: true });
    
    // Get all versions with metadata
    const versions = getVersions();
    const versionData = {};
    
    // Build each version
    for (const version of versions) {
        console.log(`Building version ${version.name}...`);
        const buildResult = buildVersion(version);
        versionData[version.name] = {
            chunks: buildResult.chunks,
            metadata: {
                ...version.metadata,
                overviewMap: buildResult.overviewMap,
                homePage: buildResult.metadata.homePage
            }
        };
        console.log(`- Created ${versionData[version.name].chunks.length} chunks`);
    }
    
    // Write versions.json
    const versionsJson = {
        versions: versions.map(v => v.name),
        metadata: Object.fromEntries(versions.map(v => [v.name, {
            ...v.metadata,
            homePage: versionData[v.name].metadata.homePage,
            overviewMap: versionData[v.name].metadata.overviewMap
        }])),
        ...versionData
    };
    fs.writeFileSync(
        path.join(PUBLIC_DIR, 'versions.json'),
        JSON.stringify(versionsJson)
    );
    
    console.log('\nBuild complete!');
    console.log(`- Processed ${versions.length} versions`);
    console.log(`- Output directory: ${path.relative(process.cwd(), PUBLIC_DIR)}`);
}

// Run build
build();
