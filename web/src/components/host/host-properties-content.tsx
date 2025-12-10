'use client';

import { useAuth } from '@/context/auth-context';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import Link from 'next/link';
import { Building, Plus, Edit, Trash2, Eye, Calendar, DollarSign } from 'lucide-react';
import { ProtectedRoute } from '@/components/auth/protected-route';
import { useState } from 'react';

export function HostPropertiesContent() {
  const { user, isAuthenticated } = useAuth();
  const queryClient = useQueryClient();
  const [deletingId, setDeletingId] = useState<string | null>(null);

  // Fetch host properties
  const { data: propertiesData, isLoading } = useQuery({
    queryKey: ['host', 'properties'],
    queryFn: async () => {
      const response = await apiClient.getHostProperties();
      return response.data;
    },
    enabled: isAuthenticated && user?.role === 'host',
  });

  // Delete property mutation
  const deleteMutation = useMutation({
    mutationFn: (propertyId: string) => apiClient.deleteProperty(propertyId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host', 'properties'] });
      setDeletingId(null);
    },
    onError: (error) => {
      console.error('Failed to delete property:', error);
      setDeletingId(null);
    },
  });

  const handleDelete = (propertyId: string) => {
    if (window.confirm('Are you sure you want to delete this property? This action cannot be undone.')) {
      setDeletingId(propertyId);
      deleteMutation.mutate(propertyId);
    }
  };

  const properties = propertiesData?.results || [];

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Header */}
          <div className="flex items-center justify-between mb-8">
            <div>
              <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                My Properties
              </h1>
              <p className="text-lg text-primary-600 dark:text-sand-300">
                Manage your property listings
              </p>
            </div>
            <Link href="/host/properties/new" className="btn-primary px-6 py-3 flex items-center gap-2">
              <Plus className="w-5 h-5" />
              Add Property
            </Link>
          </div>

          {/* Properties Grid */}
          {isLoading ? (
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
              {[1, 2, 3].map((i) => (
                <div key={i} className="card p-6 animate-pulse">
                  <div className="h-48 bg-primary-200 dark:bg-primary-700 rounded-lg mb-4"></div>
                  <div className="h-6 bg-primary-200 dark:bg-primary-700 rounded mb-2"></div>
                  <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded"></div>
                </div>
              ))}
            </div>
          ) : properties.length > 0 ? (
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
              {properties.map((property: any) => (
                <div key={property.id} className="card overflow-hidden hover:shadow-lg transition-shadow">
                  {/* Property Image */}
                  <div className="relative h-48 bg-primary-200 dark:bg-primary-700">
                    {property.main_image ? (
                      <img
                        src={property.main_image}
                        alt={property.title}
                        className="w-full h-full object-cover"
                      />
                    ) : (
                      <div className="w-full h-full flex items-center justify-center">
                        <Building className="w-16 h-16 text-primary-400" />
                      </div>
                    )}
                    
                    {/* Status Badge */}
                    <div className="absolute top-3 right-3">
                      <span
                        className={`px-3 py-1 text-xs font-semibold rounded-full ${
                          property.status === 'active'
                            ? 'bg-green-100 dark:bg-green-900/30 text-green-800 dark:text-green-300'
                            : property.status === 'pending_approval'
                            ? 'bg-yellow-100 dark:bg-yellow-900/30 text-yellow-800 dark:text-yellow-300'
                            : 'bg-gray-100 dark:bg-gray-900/30 text-gray-800 dark:text-gray-300'
                        }`}
                      >
                        {property.status.replace('_', ' ')}
                      </span>
                    </div>
                  </div>

                  {/* Property Details */}
                  <div className="p-4">
                    <h3 className="text-lg font-bold text-primary-900 dark:text-sand-50 mb-2">
                      {property.title}
                    </h3>
                    <p className="text-sm text-primary-600 dark:text-sand-300 mb-3">
                      {property.city}, {property.country}
                    </p>

                    <div className="flex items-center gap-4 text-sm text-primary-700 dark:text-sand-200 mb-4">
                      <span>{property.bedrooms} bed</span>
                      <span>•</span>
                      <span>{property.bathrooms} bath</span>
                      <span>•</span>
                      <span>{property.max_guests} guests</span>
                    </div>

                    <div className="flex items-center justify-between mb-4">
                      <div className="flex items-center gap-1 text-secondary-600 dark:text-secondary-400 font-semibold">
                        <DollarSign className="w-4 h-4" />
                        <span>{property.price_per_night}</span>
                        <span className="text-sm text-primary-600 dark:text-sand-400">/night</span>
                      </div>
                    </div>

                    {/* Action Buttons */}
                    <div className="flex gap-2">
                      <Link
                        href={`/property/${property.id}`}
                        className="flex-1 btn-secondary py-2 text-sm flex items-center justify-center gap-1"
                      >
                        <Eye className="w-4 h-4" />
                        View
                      </Link>
                      <Link
                        href={`/host/properties/${property.id}/edit`}
                        className="flex-1 btn-primary py-2 text-sm flex items-center justify-center gap-1"
                      >
                        <Edit className="w-4 h-4" />
                        Edit
                      </Link>
                      <button
                        onClick={() => handleDelete(property.id)}
                        disabled={deletingId === property.id}
                        className="px-3 py-2 bg-red-600 hover:bg-red-700 text-white rounded-lg transition-colors disabled:opacity-50"
                      >
                        <Trash2 className="w-4 h-4" />
                      </button>
                    </div>

                    {/* Additional Actions */}
                    <div className="mt-3 pt-3 border-t border-primary-200 dark:border-primary-700">
                      <Link
                        href={`/host/properties/${property.id}/calendar`}
                        className="w-full text-sm text-secondary-600 dark:text-secondary-400 hover:underline flex items-center justify-center gap-1"
                      >
                        <Calendar className="w-4 h-4" />
                        Manage Calendar
                      </Link>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          ) : (
            <div className="card p-12 text-center">
              <Building className="w-20 h-20 text-primary-300 dark:text-primary-700 mx-auto mb-4" />
              <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                No Properties Yet
              </h2>
              <p className="text-primary-600 dark:text-sand-300 mb-6 max-w-md mx-auto">
                Start earning by listing your first property on StayAfrica. It only takes a few minutes!
              </p>
              <Link href="/host/properties/new" className="btn-primary px-8 py-3 inline-flex items-center gap-2">
                <Plus className="w-5 h-5" />
                List Your First Property
              </Link>
            </div>
          )}

          {/* Summary Stats */}
          {properties.length > 0 && (
            <div className="mt-8 grid grid-cols-1 md:grid-cols-4 gap-4">
              <div className="card p-4">
                <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Total Properties</div>
                <div className="text-2xl font-bold text-primary-900 dark:text-sand-50">
                  {properties.length}
                </div>
              </div>
              <div className="card p-4">
                <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Active</div>
                <div className="text-2xl font-bold text-green-600">
                  {properties.filter((p: any) => p.status === 'active').length}
                </div>
              </div>
              <div className="card p-4">
                <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Pending Approval</div>
                <div className="text-2xl font-bold text-yellow-600">
                  {properties.filter((p: any) => p.status === 'pending_approval').length}
                </div>
              </div>
              <div className="card p-4">
                <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Inactive</div>
                <div className="text-2xl font-bold text-gray-600">
                  {properties.filter((p: any) => p.status === 'inactive').length}
                </div>
              </div>
            </div>
          )}
        </div>
      </div>
    </ProtectedRoute>
  );
}
