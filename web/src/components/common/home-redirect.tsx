'use client';

import { useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/store/auth-store';

export function HomeRedirect() {
  const { user, isLoading } = useAuth();
  const router = useRouter();

  useEffect(() => {
    if (isLoading) return;
    if (!user) return;

    if (user.active_profile === 'host') {
      router.replace('/host/dashboard');
    }
  }, [user, isLoading, router]);

  return null;
}
