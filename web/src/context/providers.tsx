'use client';

import React from 'react';
import { AuthProvider } from './auth-context';
import { ThemeProvider } from './theme-context';
import { QueryClientProvider } from '@tanstack/react-query';
import { queryClient } from '@/services/query-client';
import { Toaster } from 'react-hot-toast';

export function Providers({ children }: { children: React.ReactNode }) {
  return (
    <QueryClientProvider client={queryClient}>
      <ThemeProvider>
        <AuthProvider>
          <Toaster
            position="top-center"
            toastOptions={{
              duration: 4000,
              style: {
                background: 'var(--color-bg-secondary)',
                color: 'var(--color-text-primary)',
                border: '1px solid var(--color-border)',
              },
              success: {
                iconTheme: {
                  primary: '#d9b168',
                  secondary: '#fff',
                },
              },
            }}
          />
          {children}
        </AuthProvider>
      </ThemeProvider>
    </QueryClientProvider>
  );
}
