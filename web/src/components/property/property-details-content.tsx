'use client';

import { useState, useEffect } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery, useMutation } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/store/auth-store';
import { Skeleton } from '@/components/ui/skeleton';
import { PropertyImageCarousel } from '@/components/property/property-image-carousel';
import { PropertyAmenities } from '@/components/property/property-amenities';
import { PropertyHostCard } from '@/components/property/property-host-card';
import { BookingCard } from '@/components/booking/booking-card';
import { Heart, MapPin, Share2, Star, Users, Loader2 } from 'lucide-react';
import Link from 'next/link';
import { toast } from 'react-hot-toast';

type Review = {
  id: string;
  guest?: { first_name?: string; last_name?: string };
  rating: number;
  text: string;
  created_at: string;
};

export function PropertyDetailsContent() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { user } = useAuth();
  const propertyId = searchParams.get('id') ?? '';
  const [isSaved, setIsSaved] = useState(false);
  const [checkingSaved, setCheckingSaved] = useState(true);
  const [contactingHost, setContactingHost] = useState(false);

  // Check if property is saved on mount
  useEffect(() => {
    if (!propertyId) return;
    setCheckingSaved(true);
    apiClient.isPropertySaved(propertyId)
      .then((isSaved: boolean) => setIsSaved(isSaved))
      .catch(() => setIsSaved(false))
      .finally(() => setCheckingSaved(false));
  }, [propertyId]);

  // Save/Unsave mutations
  const saveMutation = useMutation({
    mutationFn: async () => apiClient.saveProperty(propertyId),
    onSuccess: () => setIsSaved(true),
  });
  const unsaveMutation = useMutation({
    mutationFn: async () => apiClient.unsaveProperty(propertyId),
    onSuccess: () => setIsSaved(false),
  });

  // Fetch property details
  const { data: property, isLoading, error } = useQuery({
    queryKey: ['property', propertyId],
    queryFn: async () => {
      if (!propertyId) throw new Error('Property ID not found');
      const response = await apiClient.getPropertyDetails(propertyId);
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

  // Contact Host logic
  const contactHost = async () => {
    if (!property || !property.host_id || !user?.id) {
      toast.error('Host or user information missing');
      return;
    }
    setContactingHost(true);
    try {
      const response = await apiClient.createConversation({
        participants: [property.host_id, user.id],
        property: property.id,
        subject: `Inquiry about ${property.title}`,
      });
      const conversationId = response.data.id;
      toast.success('Conversation started!');
      router.push(`/messaging/conversations/${conversationId}`);
    } catch (error: any) {
      toast.error(error.response?.data?.error || 'Failed to start conversation');
    } finally {
      setContactingHost(false);
    }
  };

  if (!propertyId) {
    return (
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
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
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
        <div className="text-center">
          <p className="text-red-600 dark:text-red-400 mb-4">Error loading property</p>
          <button
            className="btn-primary px-6 py-2 mb-4"
            onClick={() => window.location.reload()}
          >
            Retry
          </button>
          <Link href="/explore" className="text-secondary-600 hover:text-secondary-700">
            Back to explore
          </Link>
        </div>
      </div>
    );
  }

  if (isLoading) {
    return (
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 py-12">
        <div className="max-w-6xl mx-auto px-4 sm:px-6 lg:px-8">
          {/* Hero skeleton */}
          <Skeleton className="w-full h-96 mb-8 rounded-2xl" />
          {/* Title skeleton */}
          <Skeleton className="h-8 w-64 mb-4" />
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
    <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
      {/* Header with back button */}
      <div className="bg-white dark:bg-primary-800 border-b border-primary-200 dark:border-primary-700 sticky top-16 z-40">
        <div className="max-w-6xl mx-auto px-4 sm:px-6 lg:px-8 py-4 flex items-center justify-between">
          <button
            onClick={() => router.back()}
            className="text-primary-600 dark:text-sand-300 hover:text-primary-900 dark:hover:text-sand-50"
          >
            ← Back
          </button>
          <div className="flex items-center space-x-4">
            <button
              disabled={checkingSaved || saveMutation.isPending || unsaveMutation.isPending}
              onClick={() => {
                if (isSaved) {
                  unsaveMutation.mutate();
                } else {
                  saveMutation.mutate();
                }
              }}
              className={`flex items-center space-x-2 px-4 py-2 rounded-lg transition ${
                isSaved
                  ? 'bg-secondary-100 dark:bg-secondary-900/30 text-secondary-600'
                  : 'bg-sand-100 dark:bg-primary-700 text-primary-600 dark:text-sand-300'
              } ${checkingSaved ? 'opacity-50 cursor-not-allowed' : ''}`}
            >
              <Heart className={`w-5 h-5 ${isSaved ? 'fill-current' : ''}`} />
              <span>{isSaved ? 'Saved' : 'Save'}</span>
            </button>
            <button className="flex items-center space-x-2 px-4 py-2 bg-sand-100 dark:bg-primary-700 text-primary-600 dark:text-sand-300 rounded-lg hover:bg-sand-200 dark:hover:bg-primary-600 transition">
              <Share2 className="w-5 h-5" />
              <span>Share</span>
            </button>
            {user && (
              <button
                onClick={contactHost}
                disabled={contactingHost}
                className="flex items-center space-x-2 px-4 py-2 bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white rounded-lg transition disabled:opacity-50 disabled:cursor-not-allowed"
              >
                {contactingHost ? (
                  <>
                    <Loader2 className="w-5 h-5 animate-spin" />
                    <span>Contacting...</span>
                  </>
                ) : (
                  <>
                    <Users className="w-5 h-5" />
                    <span>Contact Host</span>
                  </>
                )}
              </button>
            )}
          </div>
        </div>
      </div>

      <div className="max-w-6xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
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
              <h1 className="text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                {property?.title}
              </h1>
              <div className="flex items-center space-x-4 text-sm text-primary-600 dark:text-sand-300">
                <div className="flex items-center space-x-1">
                  <MapPin className="w-4 h-4" />
                  <span>{property?.city}, {property?.country}</span>
                </div>
                {property?.average_rating > 0 && (
                  <div className="flex items-center space-x-1">
                    <Star className="w-4 h-4 fill-secondary-500 text-secondary-500" />
                    <span>{property?.average_rating.toFixed(1)} ({property?.review_count} reviews)</span>
                  </div>
                )}
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
                <p className="text-sm text-primary-600 dark:text-sand-400">Property Type</p>
                <p className="text-lg font-semibold text-primary-900 dark:text-sand-50">
                  {property?.property_type}
                </p>
              </div>
              {property?.max_guests && (
                <div className="bg-white dark:bg-primary-800 p-4 rounded-lg border border-primary-200 dark:border-primary-700">
                  <p className="text-sm text-primary-600 dark:text-sand-400">Max Guests</p>
                  <p className="text-lg font-semibold text-primary-900 dark:text-sand-50">
                    {property?.max_guests}
                  </p>
                </div>
              )}
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
            </div>

            {/* Amenities */}
            {property?.amenities && property.amenities.length > 0 && (
              <PropertyAmenities amenities={property.amenities} />
            )}

            {/* Host info */}
            <PropertyHostCard host={property?.host} propertyId={propertyId} />

            {/* Reviews section */}
            {reviews && reviews.length > 0 && (
              <div>
                <h2 className="text-2xl font-semibold text-primary-900 dark:text-sand-50 mb-6">
                  Reviews
                </h2>
                <div className="space-y-4">
                  {reviews.map((review: Review) => (
                    <div key={review.id} className="bg-white dark:bg-primary-800 p-6 rounded-lg border border-primary-200 dark:border-primary-700">
                      <div className="flex items-start justify-between mb-2">
                        <div>
                          <p className="font-semibold text-primary-900 dark:text-sand-50">
                            {review.guest?.first_name} {review.guest?.last_name}
                          </p>
                          <p className="text-sm text-primary-600 dark:text-sand-400">
                            {new Date(review.created_at).toLocaleDateString()}
                          </p>
                        </div>
                        <div className="flex items-center space-x-1">
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

          {/* Sidebar - Booking card */}
          <div className="lg:col-span-1">
            <BookingCard property={property} />
          </div>
        </div>
      </div>
    </div>
  );
}
