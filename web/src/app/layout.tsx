import type { Metadata } from 'next';
import type { ReactNode } from 'react';
import './globals.css';
import 'mapbox-gl/dist/mapbox-gl.css';
import { Providers } from '@/context/providers';
import { Navigation, BottomNav } from '@/components/common/navigation';
import { Footer } from '@/components/common/footer';
import { ErrorBoundary } from '@/components/common/ErrorBoundary';

export const metadata: Metadata = {
  title: 'StayAfrica - Unique Accommodations Across Africa',
  description: 'Discover and book unique properties across Zimbabwe, South Africa, and more',
  viewport: 'width=device-width, initial-scale=1',
  icons: {
    icon: '/favicon.ico',
  },
};

export default function RootLayout({
  children,
}: {
  children: ReactNode;
}) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body className="antialiased bg-sand-100 text-primary-900">
        <ErrorBoundary>
          <Providers>
            <div className="flex flex-col min-h-screen pb-16 lg:pb-0">
              <Navigation />
              <main id="main-content" className="flex-grow">
                {children}
              </main>
              <BottomNav />
              <Footer />
            </div>
          </Providers>
        </ErrorBoundary>
      </body>
    </html>
  );
}
