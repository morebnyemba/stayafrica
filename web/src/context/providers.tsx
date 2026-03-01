'use client';

import React from 'react';
import { NextIntlClientProvider } from 'next-intl';
import { ThemeProvider } from './theme-context';
import { QueryClientProvider } from '@tanstack/react-query';
import { queryClient } from '@/services/query-client';
import { Toaster } from 'react-hot-toast';

export function Providers({
  children,
  locale,
  messages,
}: {
  children: React.ReactNode;
  locale?: string;
  messages?: Record<string, unknown>;
}) {
  return (
    <QueryClientProvider client={queryClient}>
      <NextIntlClientProvider locale={locale} messages={messages}>
        <ThemeProvider>
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
      </ThemeProvider>
      </NextIntlClientProvider>
    </QueryClientProvider>
  );
}
