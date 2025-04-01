import '../styles/globals.css'
// Note: The Layout component needs 'use client' directive
// since it uses React hooks like useState
import Layout from '../components/Layout'

// This file is a server component

export const metadata = {
  title: 'HyperBEAM Documentation Portal',
  description: 'Documentation for the HyperBEAM system, a powerful device-based architecture.'
};

export default function RootLayout({ children }) {
  return (
    <html lang="en">
      <body>
        <Layout>
          {children}
        </Layout>
      </body>
    </html>
  );
}
