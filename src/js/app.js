import '../css/main.css';
import '../css/blog.css';
import '../css/mobile.css';
import { initializeFileTree, updateActiveFile } from './modules/fileTree';
import { initVersionSelector, updateVersionInfo } from './modules/versionSelector';
import { loadBlogPosts } from './modules/blog';
import { initializeSidebar } from './modules/sidebar';
import { loadVersion, loadVersions, flattenFileStructure } from './modules/docLoader';

// Initialize marked with options and syntax highlighting
marked.setOptions({
    breaks: true,
    gfm: true,
    highlight: function(code, lang) {
        if (lang === 'mermaid') {
            return `<div class="mermaid">${code}</div>`;
        }
        if (lang && hljs.getLanguage(lang)) {
            return hljs.highlight(code, { language: lang }).value;
        }
        return hljs.highlightAuto(code).value;
    }
});

// Initialize mermaid with dark theme
mermaid.initialize({ 
    startOnLoad: true,
    theme: 'dark',
    themeVariables: {
        primaryColor: '#4ec9b0',
        primaryTextColor: '#f0f0f0',
        primaryBorderColor: '#333',
        lineColor: '#f0f0f0',
        secondaryColor: '#569cd6',
        tertiaryColor: '#ce9178'
    }
});

// Handle file selection
function selectFile(files, path) {
    try {
        const file = files[path];
        if (!file) {
            throw new Error(`File not found: ${path}`);
        }

        // Pass the full path to updateActiveFile
        updateActiveFile(path);

        const content = document.getElementById('content');
        if (!content) {
            throw new Error('Content element not found');
        }

        const html = marked.parse(file.content);
        content.innerHTML = html;
        
        // Initialize syntax highlighting
        document.querySelectorAll('pre code').forEach(block => {
            hljs.highlightBlock(block);
        });

        // Re-initialize mermaid diagrams
        mermaid.init(undefined, document.querySelectorAll('.mermaid'));

        // Update URL with file path
        const params = new URLSearchParams(window.location.search);
        params.set('file', path);
        window.history.replaceState({}, '', '?' + params.toString());
    } catch (error) {
        console.error('Error selecting file:', error);
        const content = document.getElementById('content');
        if (content) {
            content.innerHTML = `
                <div class="error-message">
                    <h1 style="color: var(--accent-color)">Error: Failed to load file</h1>
                    <p style="color: var(--terminal-output); margin-top: 1rem;">${error.message}</p>
                </div>
            `;
        }
    }
}

// Load specific version
async function loadVersionContent(version) {
    const loading = document.getElementById('loading');
    const viewer = document.getElementById('viewer');
    const fileTree = document.getElementById('file-tree');

    if (!loading || !viewer || !fileTree) {
        throw new Error('Required DOM elements not found');
    }

    try {
        loading.style.display = 'flex';
        viewer.style.display = 'none';
        
        // Clear existing data
        window.markdownFiles = null;
        window.flattenedFiles = null;
        
        // Load and flatten files
        const { files, folderOverviews, metadata } = await loadVersion(version);
        if (!files || Object.keys(files).length === 0) {
            throw new Error('No files found in this version');
        }
        
        // Store files globally - flatten paths for file tree
        window.markdownFiles = files;
        window.flattenedFiles = flattenFileStructure(files);
        
        // Initialize file tree with folder overviews
        initializeFileTree(window.markdownFiles, (path) => {
            selectFile(window.markdownFiles, path);
        }, folderOverviews);
        
        // Update version info
        await updateVersionInfo(version, Object.keys(files).length);

        // Show terminal home page by default, or load file from URL
        const params = new URLSearchParams(window.location.search);
        const filePath = params.get('file');
        if (filePath) {
            selectFile(window.markdownFiles, filePath);
        } else {
            // Show terminal home page and first folder markdown if available
            const content = document.getElementById('content');
            if (content) {
                content.innerHTML = `
                    <div class="terminal">
                        <div class="terminal-line">> initialising... hyperBEAM Apocryphal v1.0</div>
                        <div class="terminal-line">> extracting collective knowledge from the hyperBEAM repo</div>
                        <div class="terminal-line">> no centralised gospels detected. <strong>code remains god</strong>.</div>
                        <div class="terminal-line">> invite: critique, evolve, experiment.</div>
                        <div class="terminal-line">> disclaimer: no answers found. only paths to discovery.</div>
                        <div class="terminal-line terminal-cursor">> awaiting input... let's eat glass together.</div>
                    </div>
                `;

                // Check if there's a home page file specified in metadata
                const homePage = metadata.homePage;
                // Make sure we have the .md extension when accessing files
                if (homePage && window.markdownFiles[homePage]) {
                    console.log('Found home page:', homePage);
                    console.log('Available files:', Object.keys(window.markdownFiles));
                    // Add the markdown content after the terminal
                    const mdContent = marked.parse(window.markdownFiles[homePage].content);
                    content.innerHTML += `<div class="markdown-content">${mdContent}</div>`;

                    // Initialize syntax highlighting for the markdown content
                    document.querySelectorAll('pre code').forEach(block => {
                        hljs.highlightBlock(block);
                    });

                    // Re-initialize mermaid diagrams
                    mermaid.init(undefined, document.querySelectorAll('.mermaid'));
                }
            }
        }

        // Show content
        loading.style.display = 'none';
        viewer.style.display = 'block';
        
        console.log('Documentation viewer initialized with', Object.keys(files).length, 'files');
    } catch (error) {
        console.error('Failed to load version:', error);
        loading.innerHTML = `
            <p style="color: var(--accent-color)">Error: Failed to load documentation</p>
            <p style="color: var(--terminal-output); font-size: 14px; margin-top: 10px;">${error.message}</p>
        `;
    }
}

// Load page content
async function loadPage(path) {
    const viewer = document.getElementById('viewer');
    const loading = document.getElementById('loading');
    const content = document.getElementById('content');
    
    try {
        loading.style.display = 'flex';
        viewer.style.display = 'none';

        // Update active states
        document.querySelectorAll('.file-tree span, .static-links a').forEach(el => {
            el.classList.remove('active');
        });

        if (path === '/blog') {
            // Load blog template
            const template = document.getElementById('blog-template');
            content.innerHTML = template.innerHTML;
            
            // Update active state
            document.querySelector(`.static-links a[href="${path}"]`).classList.add('active');
            
            // Load blog posts
            await loadBlogPosts();
        } else {
            // Handle documentation pages
            const file = path.replace(/^\//, '');
            if (window.markdownFiles && window.markdownFiles[file]) {
                selectFile(window.markdownFiles, file);
            } else {
                content.innerHTML = `
                    <h1>Page Not Found</h1>
                    <p style="color: var(--terminal-output)">The requested page could not be found.</p>
                `;
            }
        }
    } catch (error) {
        console.error('Failed to load page:', error);
        content.innerHTML = `
            <p style="color: var(--accent-color)">Failed to load page</p>
            <p style="color: var(--terminal-output); font-size: 14px; margin-top: 10px;">
                ${error.message}
            </p>
        `;
    } finally {
        loading.style.display = 'none';
        viewer.style.display = 'block';
    }
}

// Initialize the viewer
async function init() {
    try {
        // Get version from URL or initialize selector
        const params = new URLSearchParams(window.location.search);
        const version = params.get('version') || await initVersionSelector(loadVersionContent);
        
        // Load selected version
        await loadVersionContent(version);

        // Handle initial page load
        const path = window.location.pathname;
        if (path && path !== '/') {
            await loadPage(path);
        }

        // Handle navigation
        document.querySelectorAll('.static-links a').forEach(link => {
            link.addEventListener('click', async (e) => {
                e.preventDefault();
                const path = link.getAttribute('href');
                window.history.pushState({}, '', path);
                await loadPage(path);
            });
        });

        // Handle browser back/forward
        window.addEventListener('popstate', async () => {
            await loadPage(window.location.pathname);
        });
    } catch (error) {
        console.error('Failed to initialize documentation viewer:', error);
        document.getElementById('loading').innerHTML = `
            <p style="color: #dc3545">Failed to load documentation: ${error.message}</p>
        `;
    }
}

// Initialize sidebar functionality
initializeSidebar();

// Start initialization
init();
