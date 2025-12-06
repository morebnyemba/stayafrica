import type { Metadata } from 'next';
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
  children: React.ReactNode;
}) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body className="bg-white text-gray-900 antialiased">
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
