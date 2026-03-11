'use client';

import { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { ProtectedRoute } from '@/components/auth/protected-route';
import { PropertyForm } from '@/components/host/property-form-multistep';
import { apiClient } from '@/services/api-client';

export default function EditPropertyPage({ params }: { params: Promise<{ id: string }> }) {
  const router = useRouter();
  const [property, setProperty] = useState<any>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [propertyId, setPropertyId] = useState<string | null>(null);

  useEffect(() => {
    async function initializeParams() {
      const resolvedParams = await params;
      setPropertyId(resolvedParams.id);
    }
    initializeParams();
  }, [params]);

  useEffect(() => {
    if (!propertyId) return;
    
    async function loadProperty() {
      try {
        setLoading(true);
        const response = await apiClient.getHostPropertyById(propertyId!);
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
  }, [propertyId]);

  const handleSuccess = () => {
    router.push(`/host/properties`);
  };

  return (
    <ProtectedRoute requiredRole="host">
      <div className="max-w-4xl mx-auto px-4 py-12">
        <h1 className="text-3xl font-bold text-primary-900 mb-2">Edit Property</h1>
        {property && <p className="text-lg text-primary-600 mb-8">{property.title}</p>}

        {loading && (
          <div className="bg-white rounded-lg p-8 border border-sand-200/50">
            <p className="text-primary-500">Loading property details...</p>
          </div>
        )}

        {error && (
          <div className="bg-red-50 border border-red-200 rounded-lg p-4 mb-8">
            <p className="text-red-800">{error}</p>
          </div>
        )}

        {property && !loading && propertyId && (
          <PropertyForm
            initialData={property}
            propertyId={propertyId}
            onSuccess={handleSuccess}
            isEdit={true}
          />
        )}
      </div>
    </ProtectedRoute>
  );
}
