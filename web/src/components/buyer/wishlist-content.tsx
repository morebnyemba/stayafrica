'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useRouter } from 'next/navigation';
import { ProtectedRoute } from '@/components/auth/protected-route';
import Link from 'next/link';
import { Heart, MapPin, Star, Users, Bed, DollarSign, Trash2, X } from 'lucide-react';
import { toast } from 'react-hot-toast';

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
      toast.success('Removed from wishlist');
    },
    onError: () => {
      toast.error('Failed to remove from wishlist');
    },
  });

  const removeFromWishlist = (propertyId: string, e?: React.MouseEvent) => {
    if (e) e.stopPropagation();
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
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          {/* Header */}
          <div className="mb-8">
            <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
              My Wishlist
            </h1>
            <p className="text-lg text-primary-600 dark:text-sand-200">
              Properties you&apos;ve saved for later
            </p>
          </div>

          {/* Wishlist Grid */}
          {isLoading ? (
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
              {[1, 2, 3].map((i) => (
                <div key={i} className="card overflow-hidden animate-pulse">
                  <div className="h-48 bg-primary-200 dark:bg-primary-700"></div>
                  <div className="p-4 space-y-3">
                    <div className="h-6 bg-primary-200 dark:bg-primary-700 rounded"></div>
                    <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded w-2/3"></div>
                    <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded w-1/2"></div>
                  </div>
                </div>
              ))}
            </div>
          ) : error ? (
            <div className="card p-12 text-center">
              <p className="text-primary-600 dark:text-sand-300 mb-4">
                Unable to load your wishlist. Please try again later.
              </p>
              <button
                onClick={() => queryClient.invalidateQueries({ queryKey: ['properties', 'saved'] })}
                className="btn-primary px-6 py-2"
              >
                Retry
              </button>
            </div>
          ) : savedPropertiesData?.length === 0 ? (
            <div className="card p-12 text-center">
              <Heart className="w-20 h-20 text-primary-300 dark:text-primary-700 mx-auto mb-4" />
              <h3 className="text-2xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
                Your wishlist is empty
              </h3>
              <p className="text-primary-600 dark:text-sand-300 mb-8 max-w-md mx-auto">
                Start exploring properties and save your favorites here for easy access later
              </p>
              <Link href="/explore" className="btn-primary px-8 py-3 inline-block">
                Explore Properties
              </Link>
            </div>
          ) : (
            <>
              <div className="mb-6 text-primary-600 dark:text-sand-300">
                {savedPropertiesData?.length} {savedPropertiesData?.length === 1 ? 'property' : 'properties'} saved
              </div>
              <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
                {savedPropertiesData?.map((saved: any) => {
                  const property = saved.property;
                  return (
                    <article
                      key={saved.id}
                      onClick={() => handlePropertyClick(property.id)}
                      className="card overflow-hidden group cursor-pointer hover:shadow-lg transition"
                    >
                      {/* Property Image */}
                      <div className="relative h-48 overflow-hidden">
                        <img
                          src={
                            property.main_image ||
                            property.images?.[0]?.image_url ||
                            'https://images.unsplash.com/photo-5129917774080-9991f1c52e1d'
                          }
                          alt={property.title}
                          className="w-full h-full object-cover group-hover:scale-110 transition-transform duration-300"
                        />
                        <button
                          onClick={(e) => removeFromWishlist(property.id, e)}
                          className="absolute top-3 right-3 p-2 bg-white/90 dark:bg-primary-800/90 rounded-full hover:bg-red-50 dark:hover:bg-red-900/30 transition z-10"
                          title="Remove from wishlist"
                        >
                          <X className="w-5 h-5 text-red-600 dark:text-red-400" />
                        </button>
                        {property.average_rating && (
                          <div className="absolute top-3 left-3 px-3 py-1 bg-white/90 dark:bg-primary-800/90 backdrop-blur rounded-full">
                            <div className="flex items-center gap-1">
                              <Star className="w-4 h-4 text-yellow-500 fill-current" />
                              <span className="text-sm font-semibold text-primary-900 dark:text-sand-50">
                                {property.average_rating.toFixed(1)}
                              </span>
                            </div>
                          </div>
                        )}
                      </div>

                      {/* Property Info */}
                      <div className="p-4">
                        <h3 className="font-semibold text-lg text-primary-900 dark:text-sand-50 mb-2 line-clamp-1">
                          {property.title}
                        </h3>
                        
                        <div className="flex items-center gap-2 text-primary-600 dark:text-sand-300 text-sm mb-3">
                          <MapPin className="w-4 h-4" />
                          <span className="line-clamp-1">
                            {property.city}, {property.country}
                          </span>
                        </div>

                        {(property.max_guests || property.bedrooms) && (
                          <div className="flex items-center gap-4 text-sm text-primary-600 dark:text-sand-300 mb-4">
                            {property.max_guests && (
                              <div className="flex items-center gap-1">
                                <Users className="w-4 h-4" />
                                <span>{property.max_guests} guests</span>
                              </div>
                            )}
                            {property.bedrooms && (
                              <div className="flex items-center gap-1">
                                <Bed className="w-4 h-4" />
                                <span>{property.bedrooms} beds</span>
                              </div>
                            )}
                          </div>
                        )}

                        <div className="flex items-center justify-between pt-3 border-t border-primary-200 dark:border-primary-700">
                          <div>
                            <div className="flex items-center gap-1">
                              <span className="text-2xl font-bold text-primary-900 dark:text-sand-50">
                                ${property.price_per_night}
                              </span>
                            </div>
                            <span className="text-xs text-primary-600 dark:text-sand-400">per night</span>
                          </div>
                          <button
                            onClick={(e) => {
                              e.stopPropagation();
                              handlePropertyClick(property.id);
                            }}
                            className="btn-primary px-4 py-2 text-sm"
                          >
                            View Details
                          </button>
                        </div>

                        <div className="text-xs text-primary-500 dark:text-sand-500 pt-2 mt-2 border-t border-primary-100 dark:border-primary-700">
                          Saved {new Date(saved.created_at).toLocaleDateString()}
                        </div>
                      </div>
                    </article>
                  );
                })}
              </div>
            </>
          )}

          {/* Stats */}
          {wishlistItems.length > 0 && (
            <div className="mt-8 card p-6">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm text-primary-600 dark:text-sand-400 mb-1">
                    Total Saved Properties
                  </p>
                  <p className="text-3xl font-bold text-primary-900 dark:text-sand-50">
                    {wishlistItems.length}
                  </p>
                </div>
                <Heart className="w-12 h-12 text-red-500 fill-current opacity-20" />
              </div>
            </div>
          )}
        </div>
      </div>
    </ProtectedRoute>
  );
}
