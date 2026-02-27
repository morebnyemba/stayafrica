import type { Metadata } from 'next';
import type { ReactNode } from 'react';
import './globals.css';
import 'mapbox-gl/dist/mapbox-gl.css';
import { Providers } from '@/context/providers';
import { AppShell } from '@/components/common/AppShell';
import { ErrorBoundary } from '@/components/common/ErrorBoundary';

export const metadata: Metadata = {
  title: {
    default: 'StayAfrica - Unique Accommodations Across Africa',
    template: '%s | StayAfrica',
  },
  description: 'Discover and book unique properties across Zimbabwe, South Africa, Botswana, Namibia, and Zambia. Stay Anywhere. Africa Awaits.',
  keywords: ['Africa accommodation', 'Zimbabwe hotels', 'South Africa rentals', 'African vacation', 'StayAfrica', 'holiday rentals Africa'],
  metadataBase: new URL('https://stayafrica.app'),
  openGraph: {
    type: 'website',
    locale: 'en_US',
    url: 'https://stayafrica.app',
    siteName: 'StayAfrica',
    title: 'StayAfrica - Unique Accommodations Across Africa',
    description: 'Discover and book unique properties across Zimbabwe, South Africa, and more. Stay Anywhere. Africa Awaits.',
    images: [
      {
        url: '/og-image.png',
        width: 1200,
        height: 630,
        alt: 'StayAfrica - Stay Anywhere. Africa Awaits.',
      },
    ],
  },
  twitter: {
    card: 'summary_large_image',
    title: 'StayAfrica - Unique Accommodations Across Africa',
    description: 'Discover and book unique properties across Africa.',
    images: ['/og-image.png'],
  },
  robots: {
    index: true,
    follow: true,
    googleBot: {
      index: true,
      follow: true,
      'max-video-preview': -1,
      'max-image-preview': 'large',
      'max-snippet': -1,
    },
  },
  icons: {
    icon: [
      { url: '/favicon.ico', sizes: 'any' },
      { url: '/logo.svg', type: 'image/svg+xml' },
    ],
    apple: '/logo.png',
  },
};

export default function RootLayout({
  children,
}: {
  children: ReactNode;
}) {
  return (
    <html lang="en" suppressHydrationWarning>
      <head>
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <link rel="manifest" href="/manifest.json" />
        <meta name="theme-color" content="#2D5016" />
        <meta name="mobile-web-app-capable" content="yes" />
        <meta name="apple-mobile-web-app-status-bar-style" content="default" />
        <meta name="apple-mobile-web-app-title" content="StayAfrica" />
        <link rel="preconnect" href="https://fonts.googleapis.com" />
        <link rel="preconnect" href="https://fonts.gstatic.com" crossOrigin="anonymous" />
      </head>
      <body className="antialiased bg-sand-100 text-primary-900">
        <ErrorBoundary>
          <Providers>
            <AppShell>{children}</AppShell>
          </Providers>
        </ErrorBoundary>
      </body>
    </html>
  );
}

