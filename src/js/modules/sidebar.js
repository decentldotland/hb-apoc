// Handle sidebar functionality
export function initializeSidebar() {
    const sidebar = document.getElementById('sidebar');
    const sidebarToggle = document.getElementById('sidebar-toggle');
    
    // Create close button
    const closeButton = document.createElement('button');
    closeButton.className = 'sidebar-close';
    closeButton.innerHTML = '×';
    sidebar.appendChild(closeButton);

    // Ensure sidebar starts closed
    sidebar.classList.remove('active');
    sidebarToggle.innerHTML = '☰';

    // Handle version selector on mobile
    if (window.innerWidth <= 768) {
        const headerVersionWrapper = document.querySelector('.header-content .select-wrapper');
        if (headerVersionWrapper) {
            // Create new version selector for sidebar
            const sidebarVersionWrapper = document.createElement('div');
            sidebarVersionWrapper.className = 'select-wrapper mobile-version-selector';
            
            const sidebarSelect = document.createElement('select');
            sidebarSelect.className = 'version-select';
            sidebarSelect.id = 'mobile-version-selector';
            
            // Add arrow
            const selectArrow = document.createElement('div');
            selectArrow.className = 'select-arrow';
            selectArrow.textContent = '>';
            
            sidebarVersionWrapper.appendChild(sidebarSelect);
            sidebarVersionWrapper.appendChild(selectArrow);
            
            // Insert at the top of sidebar
            sidebar.insertBefore(sidebarVersionWrapper, sidebar.firstChild);
            
            // Initialize version selector
            const headerSelect = headerVersionWrapper.querySelector('select');
            if (headerSelect) {
                // Copy options from header selector
                Array.from(headerSelect.options).forEach(opt => {
                    const option = document.createElement('option');
                    option.value = opt.value;
                    option.text = opt.text;
                    option.selected = opt.selected;
                    sidebarSelect.appendChild(option);
                });
                
                // Keep them in sync
                sidebarSelect.value = headerSelect.value;
                
                sidebarSelect.addEventListener('change', (e) => {
                    headerSelect.value = e.target.value;
                    headerSelect.dispatchEvent(new Event('change'));
                });
                
                headerSelect.addEventListener('change', (e) => {
                    sidebarSelect.value = e.target.value;
                });
                
                // Create version info container
                const versionInfo = document.createElement('div');
                versionInfo.className = 'version-info';
                sidebarVersionWrapper.appendChild(versionInfo);
                
                // Copy version info if it exists
                const headerVersionInfo = headerVersionWrapper.querySelector('.version-info');
                if (headerVersionInfo) {
                    versionInfo.textContent = headerVersionInfo.textContent;
                }
            }
        }
    }

    // Toggle sidebar
    sidebarToggle.addEventListener('click', () => {
        sidebar.classList.toggle('active');
        sidebarToggle.innerHTML = sidebar.classList.contains('active') ? '☰' : '☰';
    });

    // Close sidebar
    closeButton.addEventListener('click', () => {
        sidebar.classList.remove('active');
        sidebarToggle.innerHTML = '☰';
    });

    // Auto-close on selection
    sidebar.addEventListener('click', (e) => {
        const isLink = e.target.tagName === 'A' || 
                      e.target.closest('a') ||
                      e.target.classList.contains('file') ||
                      e.target.closest('.file');
        
        const isVersionSelect = e.target.tagName === 'SELECT' ||
                              e.target.closest('.select-wrapper');
        
        // Don't close if clicking version selector
        if (isLink && !isVersionSelect) {
            setTimeout(() => {
                sidebar.classList.remove('active');
                sidebarToggle.innerHTML = '☰';
            }, 150); // Small delay for better UX
        }
    });

    // Handle contributors section
    const contributorsToggle = document.querySelector('.contributors-toggle');
    const contributorsContent = document.querySelector('.contributors-content');
    
    if (contributorsToggle && contributorsContent) {
        contributorsToggle.addEventListener('click', () => {
            contributorsContent.classList.toggle('collapsed');
            const icon = contributorsToggle.querySelector('.toggle-icon');
            if (icon) {
                icon.textContent = contributorsContent.classList.contains('collapsed') ? '▼' : '▲';
            }
        });

        // Keep contributors collapsed by default
        contributorsContent.classList.add('collapsed');
        const icon = contributorsToggle.querySelector('.toggle-icon');
        if (icon) {
            icon.textContent = '▼';
        }
    }
}
