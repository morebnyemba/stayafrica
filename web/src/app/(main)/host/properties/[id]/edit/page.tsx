'use client';

import { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { ProtectedRoute } from '@/components/auth/protected-route';
import { PropertyForm } from '@/components/host/property-form';
import { apiClient } from '@/services/api-client';

export default function EditPropertyPage({ params }: { params: { id: string } }) {
  const router = useRouter();
  const [property, setProperty] = useState<any>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    async function loadProperty() {
      try {
        setLoading(true);
        const response = await apiClient.getPropertyById(params.id);
        setProperty(response.data);
        setError(null);
      } catch (err) {
        setError(err instanceof Error ? err.message : 'Failed to load property');
        setProperty(null);
      } finally {
        setLoading(false);
      }
    }

    loadProperty();
  }, [params.id]);

  const handleSuccess = () => {
    router.push(`/host/properties`);
  };

  return (
    <ProtectedRoute requiredRole="host">
      <div className="max-w-4xl mx-auto px-4 py-12">
        <h1 className="text-3xl font-bold mb-8">Edit Property</h1>

        {loading && (
          <div className="bg-white rounded-lg p-8 border border-gray-200 dark:bg-primary-900 dark:border-primary-700">
            <p className="text-gray-600 dark:text-gray-400">Loading property details...</p>
          </div>
        )}

        {error && (
          <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg p-4 mb-8">
            <p className="text-red-800 dark:text-red-200">{error}</p>
          </div>
        )}

        {property && !loading && (
          <PropertyForm
            initialData={property}
            propertyId={params.id}
            onSuccess={handleSuccess}
            isEdit={true}
          />
        )}
      </div>
    </ProtectedRoute>
  );
}
