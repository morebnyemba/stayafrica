'use client';

import type { ReactNode } from 'react';
import { usePathname } from 'next/navigation';
import { Navigation, BottomNav } from '@/components/common/navigation';
import { Footer } from '@/components/common/footer';
import { CookieNotice } from '@/components/common/cookie-notice';

export function AppShell({ children }: { children: ReactNode }) {
  const pathname = usePathname();
  const isAdminRoute = pathname?.startsWith('/admin');

  return (
    <div className={`flex flex-col min-h-screen ${isAdminRoute ? '' : 'pb-16 lg:pb-0'} relative overflow-x-hidden w-full`}>
      {!isAdminRoute && <Navigation />}
      <main id="main-content" className={isAdminRoute ? 'flex-grow h-screen' : 'flex-grow'}>
        {children}
      </main>
      {!isAdminRoute && <BottomNav />}
      {!isAdminRoute && <Footer />}
      <CookieNotice />
    </div>
  );
}
