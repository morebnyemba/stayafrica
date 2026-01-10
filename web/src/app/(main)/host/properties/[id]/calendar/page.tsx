'use client';

import { useEffect, useState } from 'react';
import { ProtectedRoute } from '@/components/auth/protected-route';

export default function PropertyCalendarPage({ params }: { params: Promise<{ id: string }> }) {
  const [propertyId, setPropertyId] = useState<string | null>(null);

  useEffect(() => {
    async function initializeParams() {
      const resolvedParams = await params;
      setPropertyId(resolvedParams.id);
    }
    initializeParams();
  }, [params]);

  return (
    <ProtectedRoute requiredRole="host">
      <div className="max-w-4xl mx-auto px-4 py-12 pt-20 sm:pt-12">
        <h1 className="text-3xl font-bold mb-8">Manage Calendar</h1>
        <div className="bg-white rounded-lg p-8 border border-gray-200 dark:bg-primary-900 dark:border-primary-700">
          <p className="text-gray-600 dark:text-gray-400">
            {propertyId ? `Calendar management for property ${propertyId} - coming soon...` : 'Loading...'}
          </p>
        </div>
      </div>
    </ProtectedRoute>
  );
}
