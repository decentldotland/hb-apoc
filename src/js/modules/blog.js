// Load blog posts with optional feed filtering and pagination
export async function loadBlogPosts(selectedFeed = '', page = 1) {
    try {
        const response = await fetch('/api/rss');
        if (!response.ok) {
            throw new Error(`Failed to fetch RSS feed: ${response.statusText}`);
        }
        
        const data = await response.json();
        const blogPosts = document.getElementById('blog-posts');
        const template = document.getElementById('blog-post-template');
        const feedFilter = document.getElementById('feed-filter');
        
        // Initialize feed selector and info if needed
        if (data.feeds && !feedFilter.children.length) {
            initializeFeedSelector(data.feeds, feedFilter);
            initializeFeedList(data.feeds);
            initializeFeedSearch();
            initializeFeedToggle();
        }
        
        // Display blog posts
        displayBlogPosts(data, selectedFeed, page);
        
    } catch (error) {
        document.getElementById('blog-posts').innerHTML = `
            <p style="color: var(--accent-color)">Failed to load blog posts</p>
            <p style="color: var(--terminal-output); font-size: 14px; margin-top: 10px;">
                ${error.message}
            </p>
        `;
    }
}

function initializeFeedSelector(feeds, feedFilter) {
    // Update feed selector
    feedFilter.innerHTML = `
        <option value="">All Feeds</option>
        ${feeds.map(feed => 
            `<option value="${feed.name}">${feed.name} - ${feed.description}</option>`
        ).join('')}
    `;

    // Add change handler
    feedFilter.addEventListener('change', (e) => {
        const selectedFeed = e.target.value;
        loadBlogPosts(selectedFeed);
        
        // Update feed items active state
        document.querySelectorAll('.feed-item').forEach(item => {
            item.classList.toggle('active', item.dataset.feed === selectedFeed);
        });

        // Clear search when changing feeds
        document.getElementById('feed-search').value = '';
        document.querySelectorAll('.feed-item').forEach(item => {
            item.style.display = 'flex';
        });
    });
}

function initializeFeedList(feeds) {
    const feedList = document.querySelector('.feed-list');
    feedList.innerHTML = `
        <div class="feed-count">
            <span class="count">${feeds.length}</span> Active Feeds
        </div>
        ${feeds.map(feed => `
            <div class="feed-item" data-feed="${feed.name}">
                <span class="feed-name">${feed.name}</span>
                <span class="feed-description">${feed.description}</span>
            </div>
        `).join('')}
    `;

    // Add click handler to feed items
    document.querySelectorAll('.feed-item').forEach(item => {
        item.addEventListener('click', () => {
            const feedName = item.dataset.feed;
            const isActive = item.classList.contains('active');
            
            // Update feed selector
            document.getElementById('feed-filter').value = isActive ? '' : feedName;
            
            // Update feed items active state
            document.querySelectorAll('.feed-item').forEach(otherItem => {
                otherItem.classList.toggle('active', !isActive && otherItem === item);
            });
            
            // Load filtered posts
            loadBlogPosts(isActive ? '' : feedName);
        });
    });
}

function initializeFeedSearch() {
    const searchInput = document.getElementById('feed-search');
    searchInput.addEventListener('input', (e) => {
        const searchTerm = e.target.value.toLowerCase().trim();
        let matchCount = 0;
        
        // Remove existing no results message
        const existingNoResults = document.querySelector('.no-results');
        if (existingNoResults) {
            existingNoResults.remove();
        }
        
        // Update feed items visibility
        document.querySelectorAll('.feed-item').forEach(item => {
            const feedName = item.querySelector('.feed-name').textContent.toLowerCase();
            const feedDesc = item.querySelector('.feed-description').textContent.toLowerCase();
            const matches = feedName.includes(searchTerm) || feedDesc.includes(searchTerm);
            item.style.display = matches ? 'flex' : 'none';
            if (matches) matchCount++;
        });

        updateFeedCount(matchCount, searchTerm);
        showNoResultsMessage(matchCount, searchTerm);
    });
}

function updateFeedCount(matchCount, searchTerm) {
    const feedCount = document.querySelector('.feed-count');
    const totalFeeds = document.querySelectorAll('.feed-item').length;
    
    if (searchTerm) {
        feedCount.innerHTML = `
            <span class="count">${matchCount}</span> matching feed${matchCount !== 1 ? 's' : ''} 
            <span class="search-term">(search: "${searchTerm}")</span>
        `;
    } else {
        feedCount.innerHTML = `
            <span class="count">${totalFeeds}</span> Active Feeds
        `;
    }
}

function showNoResultsMessage(matchCount, searchTerm) {
    if (matchCount === 0 && searchTerm) {
        const message = document.createElement('div');
        message.className = 'no-results';
        message.innerHTML = `
            <p>No feeds found matching "${searchTerm}"</p>
            <button class="clear-search">Clear Search</button>
        `;
        document.querySelector('.feed-list').appendChild(message);
        
        message.querySelector('.clear-search').addEventListener('click', () => {
            const searchInput = document.getElementById('feed-search');
            searchInput.value = '';
            searchInput.dispatchEvent(new Event('input'));
            searchInput.focus();
        });
    }
}

function initializeFeedToggle() {
    const feedToggle = document.querySelector('.feed-toggle');
    const feedContent = document.querySelector('.feed-content');
    const toggleIcon = document.querySelector('.toggle-icon');
    const toggleText = document.querySelector('.toggle-text');

    feedToggle.addEventListener('click', () => {
        feedContent.classList.toggle('collapsed');
        toggleIcon.textContent = feedContent.classList.contains('collapsed') ? '▼' : '▲';
        toggleText.textContent = feedContent.classList.contains('collapsed') ? 'Show RSS Feeds' : 'Hide RSS Feeds';
    });
}

function displayBlogPosts(data, selectedFeed, page) {
    const blogPosts = document.getElementById('blog-posts');
    blogPosts.innerHTML = '';
    
    if (!data.items || data.items.length === 0) {
        blogPosts.innerHTML = `
            <p style="color: var(--terminal-output)">No blog posts found.</p>
        `;
        return;
    }
    
    // Filter posts if a feed is selected
    const posts = selectedFeed 
        ? data.items.filter(post => post.feed === selectedFeed)
        : data.items;
    
    if (posts.length === 0) {
        blogPosts.innerHTML = `
            <p style="color: var(--terminal-output)">No posts found in the selected feed.</p>
        `;
        return;
    }

    // Pagination
    const postsPerPage = 5;
    const totalPages = Math.ceil(posts.length / postsPerPage);
    const startIndex = (page - 1) * postsPerPage;
    const endIndex = startIndex + postsPerPage;
    const currentPosts = posts.slice(startIndex, endIndex);

    // Update pagination controls
    updatePaginationControls(page, totalPages, selectedFeed);
    
    // Display posts
    currentPosts.forEach(post => {
        const postElement = createPostElement(post, selectedFeed);
        blogPosts.appendChild(postElement);
    });
}

function updatePaginationControls(page, totalPages, selectedFeed) {
    const pagination = document.querySelector('.pagination');
    const prevButton = pagination.querySelector('.prev-page');
    const nextButton = pagination.querySelector('.next-page');

    // Set page info
    pagination.setAttribute('data-current-page', page);
    pagination.setAttribute('data-total-pages', totalPages);

    // Update button states
    prevButton.disabled = page === 1;
    nextButton.disabled = page === totalPages;

    // Remove old event listeners
    const newPrevButton = prevButton.cloneNode(true);
    const newNextButton = nextButton.cloneNode(true);
    prevButton.parentNode.replaceChild(newPrevButton, prevButton);
    nextButton.parentNode.replaceChild(newNextButton, nextButton);

    // Add new event listeners
    newPrevButton.addEventListener('click', () => {
        if (page > 1) {
            loadBlogPosts(selectedFeed, page - 1);
            window.scrollTo(0, 0);
        }
    });

    newNextButton.addEventListener('click', () => {
        if (page < totalPages) {
            loadBlogPosts(selectedFeed, page + 1);
            window.scrollTo(0, 0);
        }
    });
}

function createPostElement(post, selectedFeed) {
    const template = document.getElementById('blog-post-template');
    const postElement = template.content.cloneNode(true);
    
    const title = postElement.querySelector('.post-title a');
    title.href = post.link;
    title.textContent = post.title;
    
    const meta = postElement.querySelector('.post-meta');
    const date = postElement.querySelector('.post-date');
    date.textContent = new Date(post.pubDate).toLocaleDateString();
    
    const author = postElement.querySelector('.post-author');
    author.textContent = post.author;
    
    // Add feed name if showing all feeds
    if (!selectedFeed) {
        const feedSpan = document.createElement('span');
        feedSpan.className = 'post-feed';
        feedSpan.textContent = post.feed;
        meta.appendChild(feedSpan);
    }
    
    const description = postElement.querySelector('.post-description');
    description.innerHTML = post.description;
    
    return postElement;
}
