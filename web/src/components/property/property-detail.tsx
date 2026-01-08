'use client';

import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { PropertyImageCarousel } from '@/components/property/property-image-carousel';
import { PropertyAmenities } from '@/components/property/property-amenities';
import { PropertyHostCard } from '@/components/property/property-host-card';
import { BookingCard } from '@/components/booking/booking-card';
import { Skeleton } from '@/components/ui/skeleton';
import { Button } from '@/components/ui';
import { Heart, MapPin, Share2, Star } from 'lucide-react';
import Link from 'next/link';

type Review = {
  id: string;
  guest?: { first_name?: string; last_name?: string };
  rating: number;
  text: string;
  created_at: string;
};

type PropertyDetailContentProps = {
  propertyId: string;
  useHostEndpoint?: boolean;
};

export function PropertyDetailContent({ propertyId, useHostEndpoint = false }: PropertyDetailContentProps) {
  const { data: property, isLoading, error } = useQuery({
    queryKey: ['property', propertyId, useHostEndpoint],
    queryFn: async () => {
      if (!propertyId) throw new Error('Property ID not found');
      const response = useHostEndpoint
        ? await apiClient.getHostPropertyById(propertyId)
        : await apiClient.getPropertyDetails(propertyId);
      return response.data;
    },
    enabled: !!propertyId,
  });

  // Fetch property reviews
  const { data: reviews } = useQuery<Review[]>({
    queryKey: ['property-reviews', propertyId],
    queryFn: async () => {
      if (!propertyId) throw new Error('Property ID not found');
      const response = await apiClient.getPropertyReviews(propertyId);
      return response.data as Review[];
    },
    enabled: !!propertyId,
  });

  if (!propertyId) {
    return (
      <div className="max-w-7xl mx-auto px-4 py-12">
        <div className="text-center">
          <p className="text-primary-900 dark:text-sand-100 mb-4">Property not found</p>
          <Link href="/explore" className="text-secondary-600 hover:text-secondary-700">
            Back to explore
          </Link>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="max-w-7xl mx-auto px-4 py-12">
        <div className="text-center">
          <p className="text-red-600 dark:text-red-400 mb-4">Error loading property</p>
          <Button
            onClick={() => window.location.reload()}
            className="mb-4"
          >
            Retry
          </Button>
          <Link href="/explore" className="text-secondary-600 hover:text-secondary-700">
            Back to explore
          </Link>
        </div>
      </div>
    );
  }

  if (isLoading) {
    return (
      <div className="max-w-7xl mx-auto px-4 py-12">
        <div className="space-y-6">
          {/* Image skeleton */}
          <Skeleton className="w-full h-96 rounded-2xl" />
          {/* Title skeleton */}
          <Skeleton className="h-8 w-64" />
          <Skeleton className="h-6 w-48 mb-8" />
          {/* Content skeletons */}
          <div className="grid grid-cols-3 gap-8">
            <div className="col-span-2 space-y-6">
              <Skeleton className="h-64 w-full" />
              <Skeleton className="h-64 w-full" />
            </div>
            <Skeleton className="h-96 w-full" />
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="max-w-7xl mx-auto px-4 py-12">
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
        {/* Main content */}
        <div className="lg:col-span-2 space-y-8">
          {/* Image carousel */}
          <PropertyImageCarousel
            images={property?.images || []}
            title={property?.title}
          />

          {/* Basic info */}
          <div>
            <div className="flex items-start justify-between mb-4">
              <div className="flex-1">
                <h1 className="text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                  {property?.title}
                </h1>
                <div className="flex items-center space-x-4 text-sm text-primary-600 dark:text-sand-300">
                  <div className="flex items-center space-x-1">
                    <MapPin className="w-4 h-4" />
                    <span>{property?.city}, {property?.country}</span>
                  </div>
                  {property?.average_rating && (
                    <div className="flex items-center space-x-1">
                      <Star className="w-4 h-4 fill-secondary-500 text-secondary-500" />
                      <span>{property?.average_rating.toFixed(1)} ({property?.review_count} reviews)</span>
                    </div>
                  )}
                </div>
              </div>
              <div className="flex gap-2">
                <button className="p-3 rounded-lg border border-primary-200 dark:border-primary-700 hover:bg-primary-50 dark:hover:bg-primary-800 transition">
                  <Heart className="w-5 h-5 text-primary-600" />
                </button>
                <button className="p-3 rounded-lg border border-primary-200 dark:border-primary-700 hover:bg-primary-50 dark:hover:bg-primary-800 transition">
                  <Share2 className="w-5 h-5 text-primary-600" />
                </button>
              </div>
            </div>
          </div>

          {/* Description */}
          <div>
            <h2 className="text-2xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
              About this property
            </h2>
            <p className="text-primary-700 dark:text-sand-200 leading-relaxed whitespace-pre-line">
              {property?.description}
            </p>
          </div>

          {/* Property type & capacity */}
          <div className="grid grid-cols-2 gap-4 sm:grid-cols-4">
            <div className="bg-white dark:bg-primary-800 p-4 rounded-lg border border-primary-200 dark:border-primary-700">
              <p className="text-sm text-primary-600 dark:text-sand-400">Type</p>
              <p className="text-lg font-semibold text-primary-900 dark:text-sand-50 capitalize">
                {property?.property_type}
              </p>
            </div>
            {property?.bedrooms && (
              <div className="bg-white dark:bg-primary-800 p-4 rounded-lg border border-primary-200 dark:border-primary-700">
                <p className="text-sm text-primary-600 dark:text-sand-400">Bedrooms</p>
                <p className="text-lg font-semibold text-primary-900 dark:text-sand-50">
                  {property?.bedrooms}
                </p>
              </div>
            )}
            {property?.bathrooms && (
              <div className="bg-white dark:bg-primary-800 p-4 rounded-lg border border-primary-200 dark:border-primary-700">
                <p className="text-sm text-primary-600 dark:text-sand-400">Bathrooms</p>
                <p className="text-lg font-semibold text-primary-900 dark:text-sand-50">
                  {property?.bathrooms}
                </p>
              </div>
            )}
            {property?.max_guests && (
              <div className="bg-white dark:bg-primary-800 p-4 rounded-lg border border-primary-200 dark:border-primary-700">
                <p className="text-sm text-primary-600 dark:text-sand-400">Guests</p>
                <p className="text-lg font-semibold text-primary-900 dark:text-sand-50">
                  {property?.max_guests}
                </p>
              </div>
            )}
          </div>

          {/* Amenities */}
          {property?.amenities && property.amenities.length > 0 && (
            <PropertyAmenities amenities={property.amenities} />
          )}

          {/* Host info */}
          <PropertyHostCard host={property?.host} />

          {/* Reviews section */}
          {reviews && reviews.length > 0 && (
            <div>
              <h2 className="text-2xl font-semibold text-primary-900 dark:text-sand-50 mb-6">
                Guest Reviews
              </h2>
              <div className="space-y-4">
                {reviews.map((review) => (
                  <div key={review.id} className="border border-primary-200 dark:border-primary-700 rounded-lg p-4">
                    <div className="flex items-start justify-between mb-2">
                      <div>
                        <p className="font-semibold text-primary-900 dark:text-sand-50">
                          {review.guest?.first_name} {review.guest?.last_name}
                        </p>
                        <p className="text-sm text-primary-600 dark:text-sand-400">
                          {new Date(review.created_at).toLocaleDateString()}
                        </p>
                      </div>
                      <div className="flex items-center gap-1">
                        {[...Array(5)].map((_, i) => (
                          <Star
                            key={i}
                            className={`w-4 h-4 ${
                              i < review.rating
                                ? 'fill-secondary-500 text-secondary-500'
                                : 'text-primary-300 dark:text-primary-600'
                            }`}
                          />
                        ))}
                      </div>
                    </div>
                    <p className="text-primary-700 dark:text-sand-200">{review.text}</p>
                  </div>
                ))}
              </div>
            </div>
          )}
        </div>

        {/* Booking card */}
        <div>
          <BookingCard property={property} />
        </div>
      </div>
    </div>
  );
}
