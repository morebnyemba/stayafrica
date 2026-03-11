'use client';

import { useEffect } from 'react';
import { useRouter, usePathname, useSearchParams } from 'next/navigation';
import { useAuth } from '@/store/auth-store';
import { Skeleton } from '@/components/ui/skeleton';

interface ProtectedRouteProps {
  children: React.ReactNode;
  requireAuth?: boolean;
  requiredRole?: string;
}

export function ProtectedRoute({ children, requireAuth = true, requiredRole }: ProtectedRouteProps) {
  const { isAuthenticated, isLoading, user } = useAuth();
  const router = useRouter();
  const pathname = usePathname();
  const searchParams = useSearchParams();

  useEffect(() => {
    if (isLoading) return;

    if (requireAuth && !isAuthenticated) {
      // Build the full return URL including query params (e.g. booking details)
      // but strip any existing 'redirect' param to prevent infinite loops
      const params = new URLSearchParams(searchParams.toString());
      params.delete('redirect');
      const queryString = params.toString();
      const returnPath = queryString ? `${pathname}?${queryString}` : pathname;
      const loginUrl = `/login?redirect=${encodeURIComponent(returnPath)}`;
      router.replace(loginUrl);
      return;
    }

    if (requiredRole && user?.role !== requiredRole) {
      router.replace(requiredRole === 'host' ? '/host' : '/');
    }
  }, [isAuthenticated, isLoading, requireAuth, requiredRole, router, user?.role, pathname, searchParams]);

  if (isLoading) {
    return (
      <div className="min-h-screen bg-sand-100">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          <div className="space-y-6">
            <Skeleton className="h-12 w-64" />
            <Skeleton className="h-96 w-full" />
          </div>
        </div>
      </div>
    );
  }

  if (requireAuth && !isAuthenticated) {
    return null;
  }

  if (requiredRole && user?.role !== requiredRole) {
    return null;
  }

  return <>{children}</>;
}
