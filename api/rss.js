import Parser from 'rss-parser';

export default async function handler(req, res) {
  try {
    const parser = new Parser();
    const feed = await parser.parseURL('https://odysee.com/$/rss/@Pierre:eb9');
    
    res.setHeader('Cache-Control', 's-maxage=300'); // Cache for 5 minutes
    res.status(200).json({
      title: feed.title,
      description: feed.description,
      items: feed.items.map(item => ({
        title: item.title,
        link: item.link,
        pubDate: item.pubDate,
        description: item.contentSnippet || item.content,
        author: item.creator
      }))
    });
  } catch (error) {
    res.status(500).json({ error: 'Failed to fetch RSS feed' });
  }
}
