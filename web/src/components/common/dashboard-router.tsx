'use client';

import { useAuth } from '@/store/auth-store';
import { useRouter } from 'next/navigation';
import { useEffect } from 'react';
import { DashboardContent } from '@/components/common/dashboard-content';
import { HostDashboard } from '@/components/host/host-dashboard';

export function DashboardRouter() {
  const { user, isLoading } = useAuth();
  const router = useRouter();

  const activeProfile = user?.active_profile ?? 'guest';

  useEffect(() => {
    // Don't do anything while loading
    if (isLoading) return;

    // Only redirect to host dashboard if actively in host mode
    if (activeProfile === 'host') {
      router.replace('/host/dashboard');
    }
  }, [user, isLoading, router, activeProfile]);

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

  // If active profile is host, show host dashboard (or it will redirect)
  if (activeProfile === 'host') {
    return <HostDashboard />;
  }

  // For guests (or hosts in guest/travelling mode), show the traveller dashboard
  return <DashboardContent />;
}
