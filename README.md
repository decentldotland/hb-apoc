# hyperBEAM Apocryphal

A public good initiative by several teams from the Arweave and AO ecosystem. This project provides a documentation viewer and blog system for hyperBEAM-related content.

## Project Overview

The project consists of:
- Documentation viewer with version control
- Blog system with RSS feed integration
- Markdown rendering with syntax highlighting
- File tree navigation
- Mobile-responsive design

## Tech Stack

- Frontend: HTML, CSS, JavaScript
- Build Tools: Webpack
- Markdown Processing: marked.js
- Syntax Highlighting: highlight.js
- RSS Feed Integration: Node.js serverless function

## Project Structure

```
hb-apoc/
├── api/                     # Serverless API functions
│   └── rss.js              # RSS feed handler
│
├── content/                 # Documentation markdown files
│   ├── latest/             # Latest version docs
│   ├── v1.0.0/            # Version 1.0.0 docs
│   └── v2.0.0/            # Version 2.0.0 docs
│
├── public/                 # Generated files
│   └── versions/          # Generated JSON chunks
│
├── src/                   # Source code
│   ├── css/              # Stylesheets
│   ├── js/               # JavaScript modules
│   └── index.html        # Main HTML template
│
└── scripts/              # Build scripts
    └── buildDocs.js      # Documentation processor
```

## Development

1. Install dependencies:
```bash
npm install
```

2. Start development server:
```bash
npm run dev
```

This will:
- Process documentation files
- Start webpack dev server
- Enable hot reloading

## Building for Production

```bash
npm run build
```

This command:
- Processes documentation
- Creates optimized bundles
- Outputs to `/dist` directory

## Deployment

This project is configured for deployment on Vercel:

1. Connect your GitHub repository to Vercel
2. Vercel will automatically:
   - Detect the project type
   - Install dependencies
   - Build the project
   - Deploy to a production URL

The project uses Vercel's serverless functions for the RSS feed API endpoint.

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is open source and available under the [MIT License](LICENSE).

## Contributors

- EntityC HQ
- Decentland Labs
- Apus Network
