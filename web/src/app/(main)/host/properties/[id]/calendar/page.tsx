'use client';

import { ProtectedRoute } from '@/components/auth/protected-route';
import { HostCalendarContent } from '@/components/host/host-calendar-content';

export default function PropertyCalendarPage() {
  return (
    <ProtectedRoute requiredRole="host">
      <HostCalendarContent />
    </ProtectedRoute>
  );
}
