'use client';

import React from 'react';
import { AuthProvider } from './auth-context';
import { ThemeProvider } from './theme-context';
import { QueryClientProvider } from '@tanstack/react-query';
import { queryClient } from '@/services/query-client';

export function Providers({ children }: { children: React.ReactNode }) {
  return (
    <QueryClientProvider client={queryClient}>
      <ThemeProvider>
        <AuthProvider>
          {children}
        </AuthProvider>
      </ThemeProvider>
    </QueryClientProvider>
  );
}
