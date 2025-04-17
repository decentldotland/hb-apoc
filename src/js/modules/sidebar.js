// Handle sidebar functionality
export function initializeSidebar() {
    const sidebar = document.getElementById('sidebar');
    const sidebarToggle = document.getElementById('sidebar-toggle');
    
    // Create close button
    const closeButton = document.createElement('button');
    closeButton.className = 'sidebar-close';
    closeButton.innerHTML = '×';
    sidebar.appendChild(closeButton);

    // Move version selector to sidebar on mobile
    if (window.innerWidth <= 768) {
        const versionWrapper = document.querySelector('.select-wrapper');
        if (versionWrapper) {
            const sidebarVersionWrapper = versionWrapper.cloneNode(true);
            sidebar.insertBefore(sidebarVersionWrapper, sidebar.firstChild);
            
            // Re-initialize version selector in sidebar
            const select = sidebarVersionWrapper.querySelector('select');
            if (select) {
                select.addEventListener('change', (e) => {
                    const originalSelect = document.querySelector('.header-content .version-select');
                    if (originalSelect) {
                        originalSelect.value = e.target.value;
                        originalSelect.dispatchEvent(new Event('change'));
                    }
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
        
        if (isLink) {
            setTimeout(() => {
                sidebar.classList.remove('active');
            }, 150); // Small delay for better UX
        }
    });

    // Show contributors section by default on mobile
    const contributorsContent = document.querySelector('.contributors-content');
    if (window.innerWidth <= 768) {
        contributorsContent.classList.remove('collapsed');
    }
}
