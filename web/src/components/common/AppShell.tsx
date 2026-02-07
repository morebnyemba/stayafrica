'use client';

import type { ReactNode } from 'react';
import { usePathname } from 'next/navigation';
import { Navigation, BottomNav } from '@/components/common/navigation';
import { Footer } from '@/components/common/footer';

export function AppShell({ children }: { children: ReactNode }) {
  const pathname = usePathname();
  const isAdminRoute = pathname?.startsWith('/admin');

  return (
    <div className="flex flex-col min-h-screen pb-16 lg:pb-0">
      {!isAdminRoute && <Navigation />}
      <main id="main-content" className="flex-grow">
        {children}
      </main>
      {!isAdminRoute && <BottomNav />}
      {!isAdminRoute && <Footer />}
    </div>
  );
}
