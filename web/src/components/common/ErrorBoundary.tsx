/**
 * Error Boundary - Catches React errors and displays fallback UI
 */
'use client';

import React, { ReactNode } from 'react';
import { Button } from './ui/Button';

interface ErrorBoundaryProps {
  children: ReactNode;
  fallback?: ReactNode;
}

interface ErrorBoundaryState {
  hasError: boolean;
  error: Error | null;
}

export class ErrorBoundary extends React.Component<ErrorBoundaryProps, ErrorBoundaryState> {
  constructor(props: ErrorBoundaryProps) {
    super(props);
    this.state = { hasError: false, error: null };
  }

  static getDerivedStateFromError(error: Error): ErrorBoundaryState {
    return { hasError: true, error };
  }

  componentDidCatch(error: Error, errorInfo: React.ErrorInfo) {
    console.error('ErrorBoundary caught:', error, errorInfo);
  }

  render() {
    if (this.state.hasError) {
      return (
        this.props.fallback || (
          <div className="flex items-center justify-center min-h-screen bg-neutral-50">
            <div className="text-center">
              <h1 className="text-4xl font-bold text-neutral-900 mb-2">Something Went Wrong</h1>
              <p className="text-neutral-600 mb-6">{this.state.error?.message}</p>
              <Button onClick={() => window.location.reload()}>Reload Page</Button>
            </div>
          </div>
        )
      );
    }

    return this.props.children;
  }
}
