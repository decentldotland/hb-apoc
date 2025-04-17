// Handle sidebar functionality
export function initializeSidebar() {
    const sidebar = document.getElementById('sidebar');
    const sidebarToggle = document.getElementById('sidebar-toggle');
    
    // Create close button
    const closeButton = document.createElement('button');
    closeButton.className = 'sidebar-close';
    closeButton.innerHTML = '×';
    sidebar.appendChild(closeButton);

    // Handle version selector on mobile
    if (window.innerWidth <= 768) {
        const headerVersionWrapper = document.querySelector('.header-content .select-wrapper');
        if (headerVersionWrapper) {
            // Clone version selector for sidebar
            const sidebarVersionWrapper = headerVersionWrapper.cloneNode(true);
            sidebarVersionWrapper.classList.add('mobile-version-selector');
            
            // Insert at the top of sidebar
            sidebar.insertBefore(sidebarVersionWrapper, sidebar.firstChild);
            
            // Sync version selectors
            const sidebarSelect = sidebarVersionWrapper.querySelector('select');
            const headerSelect = headerVersionWrapper.querySelector('select');
            
            if (sidebarSelect && headerSelect) {
                // Keep them in sync
                sidebarSelect.value = headerSelect.value;
                
                sidebarSelect.addEventListener('change', (e) => {
                    headerSelect.value = e.target.value;
                    headerSelect.dispatchEvent(new Event('change'));
                });
                
                headerSelect.addEventListener('change', (e) => {
                    sidebarSelect.value = e.target.value;
                });
            }
        }
    }

    // Toggle sidebar
    sidebarToggle.addEventListener('click', () => {
        sidebar.classList.add('active');
    });

    // Close sidebar
    closeButton.addEventListener('click', () => {
        sidebar.classList.remove('active');
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

        // Show contributors by default on mobile
        if (window.innerWidth <= 768) {
            contributorsContent.classList.remove('collapsed');
            const icon = contributorsToggle.querySelector('.toggle-icon');
            if (icon) {
                icon.textContent = '▲';
            }
        }
    }
}
