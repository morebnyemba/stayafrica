import type { Metadata } from 'next';
import type { ReactNode } from 'react';
import './globals.css';
import { Providers } from '@/context/providers';
import { Navigation } from '@/components/common/navigation';
import { Footer } from '@/components/common/footer';

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
        <Providers>
          <div className="flex flex-col min-h-screen">
            <Navigation />
            <main className="flex-grow">
              {children}
            </main>
            <Footer />
          </div>
        </Providers>
      </body>
    </html>
  );
}
