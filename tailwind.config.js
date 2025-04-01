/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./src/pages/**/*.{js,ts,jsx,tsx,md,mdx}",
    "./src/components/**/*.{js,ts,jsx,tsx,md,mdx}",
    "./src/app/**/*.{js,ts,jsx,tsx,md,mdx}",
  ],
  theme: {
    extend: {
      colors: {
        terminal: {
          bg: '#1e1e1e',
          text: '#f0f0f0',
          primary: '#569cd6',
          secondary: '#4ec9b0',
          accent: '#ce9178',
          green: '#4ec9b0',
          prompt: '#569cd6',
          command: '#ce9178',
          output: '#dcdcaa',
        },
      },
      fontFamily: {
        mono: ['Consolas', 'Courier New', 'monospace'],
      },
    },
  },
  plugins: [],
};
