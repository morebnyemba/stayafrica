const createNextIntlPlugin = require('next-intl/plugin');
const withNextIntl = createNextIntlPlugin('./src/i18n/request.ts');

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
      source: '/:path*',
      headers: [
        { key: 'X-Frame-Options', value: 'DENY' },
        { key: 'X-Content-Type-Options', value: 'nosniff' },
        { key: 'Referrer-Policy', value: 'strict-origin-when-cross-origin' },
        { key: 'Permissions-Policy', value: 'camera=(), microphone=(), geolocation=(self)' },
        {
          key: 'Content-Security-Policy',
          value: [
            "default-src 'self'",
            "script-src 'self' 'unsafe-inline' 'unsafe-eval' https://api.mapbox.com https://js.stripe.com",
            "style-src 'self' 'unsafe-inline' https://api.mapbox.com https://fonts.googleapis.com",
            "img-src 'self' data: blob: https://*.mapbox.com https://*.amazonaws.com https://*.cloudfront.net",
            "font-src 'self' https://fonts.gstatic.com",
            "connect-src 'self' https://api.mapbox.com https://*.mapbox.com https://api.stripe.com https://exp.host https://api.zimlegend.online https://api.stayafrica.app wss://*.stayafrica.app",
            "frame-src 'self' https://js.stripe.com https://hooks.stripe.com",
            "object-src 'none'",
            "base-uri 'self'",
            "form-action 'self'",
            "frame-ancestors 'none'",
            "upgrade-insecure-requests",
          ].join('; '),
        },
        {
          key: 'Strict-Transport-Security',
          value: 'max-age=63072000; includeSubDomains; preload',
        },
      ],
    },
    {
      source: '/api/:path*',
      headers: [
        { key: 'Access-Control-Allow-Methods', value: 'GET,POST,PUT,DELETE,OPTIONS' },
      ],
    },
  ],
};

module.exports = withNextIntl(nextConfig);
