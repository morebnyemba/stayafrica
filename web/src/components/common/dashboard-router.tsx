'use client';

import { useAuth } from '@/store/auth-store';
import { useRouter } from 'next/navigation';
import { useEffect } from 'react';
import { DashboardContent } from '@/components/common/dashboard-content';
import { HostDashboard } from '@/components/host/host-dashboard';

export function DashboardRouter() {
  const { user, isLoading } = useAuth();
  const router = useRouter();

  useEffect(() => {
    // Don't do anything while loading
    if (isLoading) return;

    // If user is a host, redirect to host dashboard
    if (user?.role === 'host') {
      router.replace('/host/dashboard');
    }
  }, [user, isLoading, router]);

  // Show loading state while checking auth
  if (isLoading) {
    return (
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-secondary-600 mx-auto mb-4"></div>
          <p className="text-primary-600 dark:text-sand-300">Loading...</p>
        </div>
      </div>
    );
  }

  // For hosts, show their dashboard (or it will redirect)
  if (user?.role === 'host') {
    return <HostDashboard />;
  }

  // For guests and others, show the regular dashboard
  return <DashboardContent />;
}
