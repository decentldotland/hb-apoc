// Handle sidebar functionality
export function initializeSidebar() {
    const sidebar = document.getElementById('sidebar');
    const menuToggle = document.getElementById('nav-menu-toggle');
    
    // Create overlay
    const overlay = document.createElement('div');
    overlay.className = 'sidebar-overlay';
    document.body.appendChild(overlay);
    
    // Create close button
    const closeButton = document.createElement('button');
    closeButton.className = 'sidebar-close';
    closeButton.innerHTML = '×';
   // sidebar.appendChild(closeButton);

    // Ensure sidebar starts closed
    sidebar.classList.remove('active');
    overlay.classList.remove('active');
    menuToggle.innerHTML = '☰';

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

    // Function to open sidebar
    function openSidebar() {
        sidebar.classList.add('active');
        overlay.classList.add('active');
        menuToggle.innerHTML = '✕';
        document.body.style.overflow = 'hidden'; // Prevent background scrolling
    }

    // Function to close sidebar
    function closeSidebar() {
        sidebar.classList.remove('active');
        overlay.classList.remove('active');
        menuToggle.innerHTML = '☰';
        document.body.style.overflow = ''; // Restore scrolling
    }

    // Toggle sidebar
    menuToggle.addEventListener('click', () => {
        if (sidebar.classList.contains('active')) {
            closeSidebar();
        } else {
            openSidebar();
        }
    });

    // Close sidebar when overlay is clicked
    overlay.addEventListener('click', closeSidebar);

    // Close sidebar when close button is clicked
    closeButton.addEventListener('click', closeSidebar);

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
                closeSidebar();
            }, 150); // Small delay for better UX
        }
    });

    // Initialize contributors section
    function initializeContributors() {
        const contributorsToggle = document.querySelector('.contributors-toggle');
        const contributorsContent = document.querySelector('.contributors-content');
        
        if (!contributorsToggle || !contributorsContent) return;

        // Start collapsed
        contributorsContent.classList.add('collapsed');
        const icon = contributorsToggle.querySelector('.toggle-icon');
        if (icon) icon.textContent = '▼';

        // Handle toggle click
        contributorsToggle.addEventListener('click', (e) => {
            e.preventDefault();
            e.stopPropagation();
            
            contributorsContent.classList.toggle('collapsed');
            if (icon) {
                icon.textContent = contributorsContent.classList.contains('collapsed') ? '▼' : '▲';
            }
        });
    }

    // Initialize contributors section
    initializeContributors();
}
