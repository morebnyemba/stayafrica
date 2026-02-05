import type { ReactNode } from 'react';
import { Providers } from '@/context/providers';
import { ErrorBoundary } from '@/components/common/ErrorBoundary';

export default function AdminRootLayout({
  children,
}: {
  children: ReactNode;
}) {
  return (
    <ErrorBoundary>
      <Providers>
        {children}
      </Providers>
    </ErrorBoundary>
  );
}
