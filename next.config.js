/** @type {import('next').NextConfig} */
const path = require('path');

const nextConfig = {
  experimental: {
    // Using a minimal turbo config to enable basic functionality
    turbo: {}
  },
  reactStrictMode: false,
  images: {
    domains: ['github.com'],
  },
  // Configure to properly handle the dual structure with renamed app directory
  async rewrites() {
    return [
      {
        source: '/src/app/:path*',
        destination: '/:path*',
      },
      {
        source: '/',
        destination: '/src/app',
      }
    ]
  },
  webpack(config) {
    config.resolve.alias['@'] = path.join(__dirname, 'src');
    
    // Add special handling for modules that need browser environment
    config.resolve.fallback = {
      ...config.resolve.fallback,
      fs: false,
      path: false,
      os: false,
      crypto: false,
      stream: false,
    };
    
    // Don't externalize modules that need to be bundled
    config.externals = [
      ...(config.externals || []),
      function({ context, request }, callback) {
        // Ensure these packages are bundled
        if (/mermaid|xterm|xterm-addon/.test(request)) {
          return callback();
        }
        callback(null, 'commonjs ' + request);
      },
    ];
    
    return config;
  },
  // Configure compiler to optimize output
  compiler: {
    styledComponents: true,
  },
  // Standard page extensions
  pageExtensions: ['js', 'jsx', 'ts', 'tsx'],
};

module.exports = nextConfig;
