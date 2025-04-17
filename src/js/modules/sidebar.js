// Handle sidebar functionality
export function initializeSidebar() {
    const sidebar = document.getElementById('sidebar');
    const sidebarToggle = document.getElementById('sidebar-toggle');
    
    // Create close button
    const closeButton = document.createElement('button');
    closeButton.className = 'sidebar-close';
    closeButton.innerHTML = '×';
    sidebar.appendChild(closeButton);

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
