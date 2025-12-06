'use client';

import React from 'react';
import { AuthProvider } from './auth-context';
import { QueryClientProvider } from 'react-query';
import { queryClient } from '@/services/query-client';

export function Providers({ children }: { children: React.ReactNode }) {
  return (
    <QueryClientProvider client={queryClient}>
      <AuthProvider>
        {children}
      </AuthProvider>
    </QueryClientProvider>
  );
}
