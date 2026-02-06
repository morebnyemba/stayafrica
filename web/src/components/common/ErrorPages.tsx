                                      Q/**
 * Error Pages - 404 and 500 error pages
 */
'use client';

import { Button } from '@/components/ui/Button';
import Link from 'next/link';
import { AlertCircle, Home } from 'lucide-react';

export function NotFound() {
  return (
    <div className="flex items-center justify-center min-h-screen bg-gradient-to-br from-neutral-50 to-neutral-100">
      <div className="text-center">
        <AlertCircle className="h-20 w-20 text-error-500 mx-auto mb-6" />
        <h1 className="text-5xl font-bold text-neutral-900 mb-2">404</h1>
        <h2 className="text-2xl font-semibold text-neutral-700 mb-4">Page Not Found</h2>
        <p className="text-neutral-600 mb-8 max-w-md mx-auto">
          The page you're looking for doesn't exist or has been moved. Let's help you get back on track.
        </p>
        <div className="flex gap-4 justify-center">
          <Link href="/">
            <Button leftIcon={<Home className="h-5 w-5" />}>Go Home</Button>
          </Link>
          <Button variant="outline" onClick={() => window.history.back()}>
            Go Back
          </Button>
        </div>
      </div>
    </div>
  );
}

export function ServerError() {
  return (
    <div className="flex items-center justify-center min-h-screen bg-gradient-to-br from-neutral-50 to-neutral-100">
      <div className="text-center">
        <AlertCircle className="h-20 w-20 text-error-600 mx-auto mb-6" />
        <h1 className="text-5xl font-bold text-neutral-900 mb-2">500</h1>
        <h2 className="text-2xl font-semibold text-neutral-700 mb-4">Server Error</h2>
        <p className="text-neutral-600 mb-8 max-w-md mx-auto">
          Something went wrong on our end. Our team has been notified and is working on a fix.
        </p>
        <div className="flex gap-4 justify-center">
          <Link href="/">
            <Button leftIcon={<Home className="h-5 w-5" />}>Go Home</Button>
          </Link>
          <Button variant="outline" onClick={() => window.location.reload()}>
            Retry
          </Button>
        </div>
      </div>
    </div>
  );
}
