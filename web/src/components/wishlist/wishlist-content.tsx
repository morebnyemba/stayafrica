'use client';

import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { MapPin, Star, X, Heart } from 'lucide-react';
import { useRouter } from 'next/navigation';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });

export function WishlistContent() {
  const router = useRouter();
  const queryClient = useQueryClient();

  const { data: savedPropertiesData, isLoading, error } = useQuery({
    queryKey: ['properties', 'saved'],
    queryFn: async () => {
      const response = await apiClient.getSavedProperties();
      return response.data?.results || [];
    },
  });

  const unsaveMutation = useMutation({
    mutationFn: (propertyId: string) => apiClient.unsaveProperty(propertyId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['properties', 'saved'] });
    },
  });

  const handleRemove = (propertyId: string, e: React.MouseEvent) => {
    e.stopPropagation();
    if (confirm('Remove this property from your wishlist?')) {
      unsaveMutation.mutate(propertyId);
    }
  };

  const handlePropertyClick = (propertyId: string) => {
    router.push(`/property?id=${propertyId}`);
  };

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Header */}
          <div className="mb-8">
            <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
              My Wishlist
            </h1>
            <p className="text-lg text-primary-600 dark:text-sand-300">
              Your saved properties for future trips
            </p>
          </div>

          {/* Content */}
          {isLoading ? (
            <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-6">
              {[1, 2, 3].map((i) => (
                <div key={i} className="animate-pulse">
                  <div className="bg-primary-200 dark:bg-primary-700 h-64 rounded-lg"></div>
                </div>
              ))}
            </div>
          ) : error ? (
            <div className="bg-white dark:bg-primary-800 p-12 rounded-lg text-center border border-primary-200 dark:border-primary-700">
              <p className="text-primary-600 dark:text-sand-300 mb-4">
                Unable to load your wishlist. Please try again later.
              </p>
              <button
                onClick={() => queryClient.invalidateQueries({ queryKey: ['properties', 'saved'] })}
                className="bg-secondary-600 hover:bg-secondary-700 text-white px-6 py-2 rounded-lg transition"
              >
                Retry
              </button>
            </div>
          ) : savedPropertiesData?.length === 0 ? (
            <div className="bg-white dark:bg-primary-800 p-12 rounded-lg text-center border border-primary-200 dark:border-primary-700">
              <Heart className="w-16 h-16 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
              <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
                Your wishlist is empty
              </h3>
              <p className="text-primary-600 dark:text-sand-300 mb-6">
                Start saving properties you love for future trips
              </p>
              <button
                onClick={() => router.push('/explore')}
                className="bg-secondary-600 hover:bg-secondary-700 text-white px-6 py-3 rounded-lg transition"
              >
                Explore Properties
              </button>
            </div>
          ) : (
            <>
              <div className="mb-6 text-primary-600 dark:text-sand-300">
                {savedPropertiesData?.length} {savedPropertiesData?.length === 1 ? 'property' : 'properties'} saved
              </div>
              <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-6">
                {savedPropertiesData?.map((saved: any) => {
                  const property = saved.property;
                  return (
                    <article
                      key={saved.id}
                      onClick={() => handlePropertyClick(property.id)}
                      className="bg-white dark:bg-primary-800 rounded-lg group overflow-hidden border border-primary-100/80 dark:border-primary-700 hover:border-secondary-400 transition cursor-pointer relative"
                    >
                      {/* Remove button */}
                      <button
                        onClick={(e) => handleRemove(property.id, e)}
                        className="absolute top-4 right-4 z-10 bg-white dark:bg-primary-800 p-2 rounded-full shadow-lg hover:bg-red-50 dark:hover:bg-red-900/30 transition group/btn"
                        title="Remove from wishlist"
                      >
                        <X className="w-5 h-5 text-red-600 dark:text-red-400 group-hover/btn:scale-110 transition-transform" />
                      </button>

                      <div className="relative h-56 overflow-hidden">
                        <img
                          src={
                            property.main_image ||
                            property.images?.[0]?.image_url ||
                            'https://images.unsplash.com/photo-1512917774080-9991f1c52e1d'
                          }
                          alt={property.title}
                          className="w-full h-full object-cover transition duration-700 group-hover:scale-105"
                        />
                        <div className="absolute inset-0 bg-gradient-to-t from-primary-900/70 via-primary-900/10 to-transparent opacity-0 group-hover:opacity-100 transition" />
                        {property.average_rating && (
                          <div className="absolute top-4 left-4 inline-flex items-center gap-1 bg-primary-900/80 backdrop-blur px-3 py-1 rounded-full text-sm font-semibold text-sand-50">
                            <Star className="w-4 h-4 text-secondary-300" fill="currentColor" />
                            <span>{property.average_rating.toFixed(1)}</span>
                          </div>
                        )}
                      </div>

                      <div className="p-6 space-y-4">
                        <div>
                          <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2 group-hover:text-secondary-600 transition">
                            {property.title}
                          </h3>
                          <div className="flex items-center gap-2 text-primary-600 dark:text-sand-300 text-sm">
                            <MapPin className="w-4 h-4" />
                            <span>
                              {property.city}, {property.country}
                            </span>
                          </div>
                        </div>

                        <div className="flex justify-between items-end">
                          <div>
                            <span className="text-2xl font-semibold text-primary-900 dark:text-sand-50">
                              ${property.price_per_night}
                            </span>
                            <span className="text-sm text-primary-500 dark:text-sand-400 ml-1 font-medium">
                              /night
                            </span>
                          </div>
                          <button
                            type="button"
                            onClick={(e) => {
                              e.stopPropagation();
                              handlePropertyClick(property.id);
                            }}
                            className="bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white px-4 py-2 rounded-lg text-sm transition"
                          >
                            View details
                          </button>
                        </div>

                        <div className="text-xs text-primary-500 dark:text-sand-500 pt-2 border-t border-primary-100 dark:border-primary-700">
                          Saved {new Date(saved.created_at).toLocaleDateString()}
                        </div>
                      </div>
                    </article>
                  );
                })}
              </div>
            </>
          )}
        </div>
      </div>
    </ProtectedRoute>
  );
}
