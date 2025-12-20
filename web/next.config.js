/** @type {import('next').NextConfig} */
const isProd = process.env.NODE_ENV === 'production';
const defaultApiBase = isProd ? 'https://api.zimlegend.online' : 'http://localhost:8000';

const nextConfig = {
  output: 'standalone',
  reactStrictMode: true,
  turbopack: {}, // Enable Turbopack with default config
  compiler: {
    removeConsole: process.env.NODE_ENV === 'production',
  },
  
  images: {
    remotePatterns: [
      {
        protocol: 'https',
        hostname: '**.cloudfront.net',
      },
      {
        protocol: 'https',
        hostname: '**.amazonaws.com',
      },
    ],
    unoptimized: false,
  },

  env: {
    NEXT_PUBLIC_API_BASE_URL: process.env.NEXT_PUBLIC_API_BASE_URL || defaultApiBase,
    NEXT_PUBLIC_MAPBOX_TOKEN: process.env.NEXT_PUBLIC_MAPBOX_TOKEN,
  },

  headers: async () => [
    {
      source: '/api/:path*',
      headers: [
        { key: 'Access-Control-Allow-Origin', value: '*' },
        { key: 'Access-Control-Allow-Methods', value: 'GET,POST,PUT,DELETE,OPTIONS' },
      ],
    },
  ],
};

module.exports = nextConfig;
