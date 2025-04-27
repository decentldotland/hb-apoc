const Parser = require('rss-parser');

const FEEDS = [
  {
    url: 'https://odysee.com/$/rss/@Pierre:eb9',
    name: 'Pierre',
    description: 'Technical deep dives and analysis'
  },
  {
    url: 'https://odysee.com/$/rss/@hyperBEAM:c',
    name: 'hyperBEAM',
    description: 'Official hyperBEAM updates'
  },
  {
    url: 'https://odysee.com/$/rss/@EntityC:8',
    name: 'EntityC',
    description: 'Community updates and tutorials'
  },
  {
    url: 'https://odysee.com/$/rss/@apus_network:2',
    name: 'Apus Network',
    description: 'Community updates and tutorials'
  }
];

module.exports = async function handler(req, res) {
  // Enable CORS
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type');

  // Handle OPTIONS request for CORS
  if (req.method === 'OPTIONS') {
    res.status(200).end();
    return;
  }

  try {
    const parser = new Parser();
    const allItems = [];
    
    // Fetch all feeds in parallel
    await Promise.all(FEEDS.map(async (feed) => {
      try {
        const parsedFeed = await parser.parseURL(feed.url);
        const items = parsedFeed.items.map(item => ({
          title: item.title,
          link: item.link,
          pubDate: item.pubDate,
          description: item.contentSnippet || item.content,
          author: feed.name,
          feed: feed.name,
          feedDescription: feed.description
        }));
        allItems.push(...items);
      } catch (error) {
        console.error(`Error fetching feed ${feed.name}:`, error);
      }
    }));

    // Sort all items by date
    allItems.sort((a, b) => new Date(b.pubDate) - new Date(a.pubDate));

    // Set cache headers
    res.setHeader('Cache-Control', 's-maxage=300'); // Cache for 5 minutes
    
    res.status(200).json({
      items: allItems,
      feeds: FEEDS.map(feed => ({
        name: feed.name,
        url: feed.url,
        description: feed.description
      }))
    });
  } catch (error) {
    console.error('API Error:', error);
    res.status(500).json({ error: 'Failed to fetch RSS feeds' });
  }
}
