// Build file tree from files
function buildFileTree(files) {
    const tree = {};
    
    for (const [path, file] of Object.entries(files)) {
        // Ensure consistent forward slashes
        const normalizedPath = path.replace(/\\/g, '/');
        const parts = normalizedPath.split('/');
        let current = tree;
        
        // Build the tree structure using the actual folder names
        for (let i = 0; i < parts.length; i++) {
            const part = parts[i];
            
            if (i === parts.length - 1) {
                // Keep original name with .md extension
                current[part] = {
                    ...file,
                    originalPath: normalizedPath // Store normalized path for selection
                };
            } else {
                // Create folder if it doesn't exist
                current[part] = current[part] || {};
                current = current[part];
            }
        }
    }
    
    return tree;
}

// Render file tree HTML
function renderFileTree(tree, parent = '', folderOverviews = {}) {
    let html = '<ul class="collapsed">';
    for (const [name, value] of Object.entries(tree)) {
        const path = parent ? `${parent}/${name}` : name;
        if (value.originalPath) {
            // Skip overview files since they're shown when clicking the folder
            const isOverviewFile = Object.values(folderOverviews).some(overview => overview === value.originalPath);
            if (!isOverviewFile) {
                // Remove .md extension only for display
                const displayName = name.replace(/\.md$/, '');
                html += `<li data-path="${value.originalPath}"><span class="file" data-path="${value.originalPath}">${displayName}</span></li>`;
            }
        } else {
            // Check if this folder has an overview file by logging ALL keys in folderOverviews
            const normalizedPath = path.replace(/\\/g, '/');
            
            // Debug logging all keys in folderOverviews
            console.log('All overview keys:', Object.keys(folderOverviews));
            
            // Try both with and without trailing slash
            const pathWithSlash = normalizedPath.endsWith('/') ? normalizedPath : normalizedPath + '/';
            const pathWithoutSlash = normalizedPath.endsWith('/') ? normalizedPath.slice(0, -1) : normalizedPath;
            
            // Check if any version of the path exists in folderOverviews
            const hasOverview = folderOverviews[normalizedPath] !== undefined || 
                               folderOverviews[pathWithSlash] !== undefined ||
                               folderOverviews[pathWithoutSlash] !== undefined;
            
            let overviewPath;
            if (folderOverviews[normalizedPath] !== undefined) {
                overviewPath = folderOverviews[normalizedPath];
            } else if (folderOverviews[pathWithSlash] !== undefined) {
                overviewPath = folderOverviews[pathWithSlash];
            } else if (folderOverviews[pathWithoutSlash] !== undefined) {
                overviewPath = folderOverviews[pathWithoutSlash];
            }
            
            const folderClass = hasOverview ? 'folder has-overview' : 'folder';
            
            console.log('Folder overview detailed check:', { 
                normalizedPath, 
                pathWithSlash,
                pathWithoutSlash,
                hasOverview, 
                overviewPath,
                folderOverviewsKeys: Object.keys(folderOverviews)
            });
            
            html += `
                <li>
                    <span class="${folderClass}" data-path="${normalizedPath}" ${hasOverview ? `data-overview="${overviewPath}"` : ''}>
                        ${name} ${hasOverview ? '📄' : ''}
                    </span>
                    ${renderFileTree(value, normalizedPath, folderOverviews)}
                </li>`;
        }
    }
    html += '</ul>';
    return html;
}

// Handle truncation and tooltips
function handleTooltip(span) {
    const li = span.closest('li');
    const isOverflowing = span.scrollWidth > span.clientWidth;
    
    if (isOverflowing) {
        span.classList.add('truncated');
        const rect = span.getBoundingClientRect();
        const tooltipY = rect.top + (rect.height / 2);
        li.style.setProperty('--tooltip-y', `${tooltipY}px`);
        
        // Only show full path if it's different from the visible text
        const visibleText = span.textContent.trim();
        const fullPath = span.dataset.path;
        if (fullPath !== visibleText) {
            li.setAttribute('data-path', fullPath);
        }
    } else {
        span.classList.remove('truncated');
        li.removeAttribute('data-path');
    }
}

// Update active file in tree
export function updateActiveFile(path) {
    document.querySelectorAll('.file-tree span').forEach(span => {
        span.classList.remove('active');
        if (span.dataset.path === path) {
            span.classList.add('active');
            
            // Expand parent folders
            let parent = span.closest('ul');
            while (parent) {
                parent.classList.remove('collapsed');
                const folderSpan = parent.previousElementSibling;
                if (folderSpan && folderSpan.classList.contains('folder')) {
                    folderSpan.classList.add('expanded');
                }
                parent = parent.parentElement.closest('ul');
            }
        }
    });
}

// Initialize file tree with click handlers
export function initializeFileTree(files, onSelect, folderOverviews = {}) {
    const fileTree = document.getElementById('file-tree');
    if (!fileTree) return;

    // Build and render tree
    const tree = buildFileTree(files);
    fileTree.innerHTML = renderFileTree(tree, '', folderOverviews);
    
    // Expand root level
    fileTree.querySelector('ul').classList.remove('collapsed');

    // Add click handlers
    document.querySelectorAll('.file-tree span').forEach(span => {
        if (span.classList.contains('file')) {
            span.addEventListener('click', () => onSelect(span.dataset.path));
        } else {
            // For folders
            span.addEventListener('click', (e) => {
                e.stopPropagation();
                
                // Always load overview file immediately on click if available
                if (span.classList.contains('has-overview')) {
                    const overviewPath = span.dataset.overview;
                    console.log('Loading overview file for folder:', { overviewPath });
                    if (overviewPath) {
                        onSelect(overviewPath);
                    }
                }
                
                // Toggle folder expansion regardless of overview status
                // This ensures folders expand/collapse on click even if they have overviews
                const wasExpanded = span.classList.contains('expanded');
                span.classList.toggle('expanded');
                const ul = span.nextElementSibling;
                if (ul) {
                    ul.classList.toggle('collapsed');
                }
            });
        }

        // Initial tooltip check
        handleTooltip(span);

        // Update on events
        span.addEventListener('mouseenter', () => handleTooltip(span));
        span.addEventListener('mousemove', () => handleTooltip(span));
        span.addEventListener('mouseleave', () => {
            span.closest('li').removeAttribute('data-path');
        });
    });

    // Update tooltips on resize
    window.addEventListener('resize', () => {
        document.querySelectorAll('.file-tree span').forEach(handleTooltip);
    });
}
