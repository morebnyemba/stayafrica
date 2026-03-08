'use client';

import { useState, useEffect } from 'react';
import { useSearchParams } from 'next/navigation';
import { useQuery, useMutation } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Skeleton } from '@/components/ui/skeleton';
import { Button } from '@/components/ui';
import { PropertyImageCarousel } from '@/components/property/property-image-carousel';
import { PropertyAmenities } from '@/components/property/property-amenities';
import { PropertyHostCard } from '@/components/property/property-host-card';
import { BookingPanel } from '@/components/booking';
import POIList from '@/components/poi/POIList';
import { PropertyLocationMap } from '@/components/property/property-location-map';
import { SimilarProperties } from '@/components/property/similar-properties';
import {
  Heart, MapPin, Share2, Star,
  Home, Bed, UserCheck,
} from 'lucide-react';
import Link from 'next/link';
import { toast } from 'react-hot-toast';

type Review = {
  id: string;
  guest?: { first_name?: string; last_name?: string };
  rating: number;
  text: string;
  created_at: string;
};

interface PropertyDetailsContentProps {
  propertyId?: string;
}

export function PropertyDetailsContent({ propertyId: propId }: PropertyDetailsContentProps = {}) {
  const searchParams = useSearchParams();
  const propertyId = propId || searchParams.get('id') || '';
  const [isSaved, setIsSaved] = useState(false);
  const [checkingSaved, setCheckingSaved] = useState(true);
  const [showFullDescription, setShowFullDescription] = useState(false);
  const [showAllReviews, setShowAllReviews] = useState(false);

  // Check if property is saved on mount (only if logged in)
  useEffect(() => {
    if (!propertyId) return;
    const token = typeof window !== 'undefined' ? localStorage.getItem('access_token') : null;
    if (!token) {
      setCheckingSaved(false);
      return;
    }
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

  // Review rating breakdown
  const ratingBreakdown = reviews?.reduce((acc, r) => {
    const star = Math.min(5, Math.max(1, Math.round(r.rating)));
    acc[star] = (acc[star] || 0) + 1;
    return acc;
  }, {} as Record<number, number>) || {};

  const avgRating = reviews && reviews.length > 0
    ? (reviews.reduce((sum, r) => sum + r.rating, 0) / reviews.length).toFixed(1)
    : null;

  if (!propertyId) {
    return (
      <div className="min-h-screen bg-white dark:bg-primary-900 flex items-center justify-center">
        <div className="text-center">
          <p className="text-primary-900 dark:text-sand-100 mb-4">Property not found</p>
          <Link href="/explore" className="text-secondary-600 hover:text-secondary-700 underline">
            Back to explore
          </Link>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen bg-white dark:bg-primary-900 flex items-center justify-center">
        <div className="text-center">
          <p className="text-red-600 dark:text-red-400 mb-4">Error loading property</p>
          <Button onClick={() => window.location.reload()} className="mb-4">Retry</Button>
          <Link href="/explore" className="text-secondary-600 hover:text-secondary-700 underline">
            Back to explore
          </Link>
        </div>
      </div>
    );
  }

  if (isLoading) {
    return (
      <div className="min-h-screen bg-white dark:bg-primary-900 py-8">
        <div className="max-w-[1120px] mx-auto px-6">
          <Skeleton className="h-8 w-80 mb-2" />
          <Skeleton className="h-5 w-48 mb-6" />
          <Skeleton className="w-full h-[400px] mb-8 rounded-xl" />
          <div className="grid grid-cols-1 lg:grid-cols-3 gap-12">
            <div className="lg:col-span-2 space-y-6">
              <Skeleton className="h-48 w-full" />
              <Skeleton className="h-48 w-full" />
            </div>
            <Skeleton className="h-96 w-full rounded-xl" />
          </div>
        </div>
      </div>
    );
  }

  const isLongDescription = (property?.description?.length || 0) > 300;

  const displayedReviews = showAllReviews ? reviews : reviews?.slice(0, 6);

  return (
    <div className="min-h-screen bg-white dark:bg-primary-900">
      <div className="max-w-[1120px] mx-auto px-6 pt-6 pb-12">
        {/* Breadcrumb */}
        {property && (
          <nav className="text-sm text-primary-600 dark:text-sand-400 mb-3 flex items-center gap-1.5">
            <Link href="/explore" className="hover:text-primary-900 dark:hover:text-sand-50 hover:underline">
              Explore
            </Link>
            <span>›</span>
            {property.country && (
              <>
                <Link
                  href={`/explore?country=${encodeURIComponent(property.country)}`}
                  className="hover:text-primary-900 dark:hover:text-sand-50 hover:underline"
                >
                  {property.country}
                </Link>
                <span>›</span>
              </>
            )}
            {property.city && (
              <>
                <Link
                  href={`/explore?city=${encodeURIComponent(property.city)}`}
                  className="hover:text-primary-900 dark:hover:text-sand-50 hover:underline"
                >
                  {property.city}
                </Link>
                <span>›</span>
              </>
            )}
            <span className="text-primary-900 dark:text-sand-50">{property.property_type || 'Property'}</span>
          </nav>
        )}
        {/* Title & Actions Row */}
        <div className="mb-6">
          <h1 className="text-2xl font-semibold text-primary-900 dark:text-sand-50 mb-1">
            {property?.title}
          </h1>
          <div className="flex items-center justify-between flex-wrap gap-2">
            <div className="flex items-center gap-2 text-sm">
              {property?.average_rating > 0 && (
                <>
                  <Star className="w-4 h-4 fill-primary-900 dark:fill-sand-50 text-primary-900 dark:text-sand-50" />
                  <span className="font-semibold text-primary-900 dark:text-sand-50">
                    {property.average_rating.toFixed(1)}
                  </span>
                  <span className="text-primary-600 dark:text-sand-400">·</span>
                  <button
                    onClick={() => document.getElementById('reviews')?.scrollIntoView({ behavior: 'smooth' })}
                    className="underline text-primary-900 dark:text-sand-50 font-medium hover:text-primary-700"
                  >
                    {property?.review_count || 0} review{(property?.review_count || 0) !== 1 ? 's' : ''}
                  </button>
                  <span className="text-primary-600 dark:text-sand-400">·</span>
                </>
              )}
              <span className="flex items-center gap-1 text-primary-900 dark:text-sand-50 font-medium">
                <MapPin className="w-4 h-4" />
                {property?.city}, {property?.country}
              </span>
            </div>
            <div className="flex items-center gap-2">
              <button
                onClick={() => {
                  if (navigator.share) {
                    navigator.share({ title: property?.title, url: window.location.href });
                  } else {
                    navigator.clipboard.writeText(window.location.href);
                    toast.success('Link copied!');
                  }
                }}
                className="flex items-center gap-1.5 px-3 py-2 rounded-lg hover:bg-sand-100 dark:hover:bg-primary-800 transition text-sm font-medium text-primary-900 dark:text-sand-50 underline"
              >
                <Share2 className="w-4 h-4" />
                Share
              </button>
              <button
                disabled={checkingSaved || saveMutation.isPending || unsaveMutation.isPending}
                onClick={() => isSaved ? unsaveMutation.mutate() : saveMutation.mutate()}
                className="flex items-center gap-1.5 px-3 py-2 rounded-lg hover:bg-sand-100 dark:hover:bg-primary-800 transition text-sm font-medium text-primary-900 dark:text-sand-50 underline disabled:opacity-50"
              >
                <Heart className={`w-4 h-4 ${isSaved ? 'fill-red-500 text-red-500' : ''}`} />
                {isSaved ? 'Saved' : 'Save'}
              </button>
            </div>
          </div>
        </div>

        {/* Photo Grid / Carousel */}
        <PropertyImageCarousel
          images={property?.images || []}
          title={property?.title}
        />

        {/* Main Content Grid */}
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-x-12 mt-8">
          {/* Left Column */}
          <div className="lg:col-span-2">
            {/* Property Type + Host Intro */}
            <div className="flex items-center justify-between py-6 border-b border-primary-200 dark:border-primary-700">
              <div>
                <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50">
                  {property?.property_type} hosted by {property?.host?.first_name || 'Host'}
                </h2>
                <p className="text-sm text-primary-600 dark:text-sand-400 mt-1">
                  {[
                    property?.max_guests && `${property.max_guests} guest${property.max_guests !== 1 ? 's' : ''}`,
                    property?.bedrooms && `${property.bedrooms} bedroom${property.bedrooms !== 1 ? 's' : ''}`,
                    property?.bathrooms && `${property.bathrooms} bathroom${property.bathrooms !== 1 ? 's' : ''}`,
                  ].filter(Boolean).join(' · ')}
                </p>
              </div>
              {property?.host?.profile_picture ? (
                <img
                  src={property.host.profile_picture}
                  alt={property.host.first_name}
                  className="w-14 h-14 rounded-full object-cover flex-shrink-0"
                />
              ) : (
                <div className="w-14 h-14 rounded-full bg-primary-800 dark:bg-primary-600 flex items-center justify-center flex-shrink-0">
                  <span className="text-white text-lg font-semibold">
                    {property?.host?.first_name?.[0] || 'H'}
                  </span>
                </div>
              )}
            </div>

            {/* Property Highlights */}
            <div className="py-6 border-b border-primary-200 dark:border-primary-700 space-y-5">
              {property?.host?.is_verified && (
                <div className="flex items-start gap-4">
                  <UserCheck className="w-6 h-6 text-primary-900 dark:text-sand-50 flex-shrink-0 mt-0.5" />
                  <div>
                    <p className="font-medium text-primary-900 dark:text-sand-50">Verified Host</p>
                    <p className="text-sm text-primary-600 dark:text-sand-400">
                      This host has completed identity verification
                    </p>
                  </div>
                </div>
              )}
              {property?.property_type && (
                <div className="flex items-start gap-4">
                  <Home className="w-6 h-6 text-primary-900 dark:text-sand-50 flex-shrink-0 mt-0.5" />
                  <div>
                    <p className="font-medium text-primary-900 dark:text-sand-50">Entire {property.property_type.toLowerCase()}</p>
                    <p className="text-sm text-primary-600 dark:text-sand-400">
                      You&apos;ll have the {property.property_type.toLowerCase()} to yourself
                    </p>
                  </div>
                </div>
              )}
              {property?.bedrooms && (
                <div className="flex items-start gap-4">
                  <Bed className="w-6 h-6 text-primary-900 dark:text-sand-50 flex-shrink-0 mt-0.5" />
                  <div>
                    <p className="font-medium text-primary-900 dark:text-sand-50">
                      {property.bedrooms} dedicated bedroom{property.bedrooms !== 1 ? 's' : ''}
                    </p>
                    <p className="text-sm text-primary-600 dark:text-sand-400">
                      Comfortable sleeping arrangements for {property.max_guests || 2} guest{(property.max_guests || 2) !== 1 ? 's' : ''}
                    </p>
                  </div>
                </div>
              )}
            </div>

            {/* Description with Show More */}
            <div className="py-6 border-b border-primary-200 dark:border-primary-700">
              <div className={`relative ${!showFullDescription && isLongDescription ? 'max-h-[120px] overflow-hidden' : ''}`}>
                <p className="text-primary-700 dark:text-sand-200 leading-relaxed whitespace-pre-line text-[15px]">
                  {property?.description}
                </p>
                {!showFullDescription && isLongDescription && (
                  <div className="absolute bottom-0 left-0 right-0 h-12 bg-gradient-to-t from-white dark:from-primary-900 to-transparent" />
                )}
              </div>
              {isLongDescription && (
                <button
                  onClick={() => setShowFullDescription(!showFullDescription)}
                  className="mt-3 text-primary-900 dark:text-sand-50 font-semibold text-sm underline hover:text-primary-700 dark:hover:text-sand-200"
                >
                  {showFullDescription ? 'Show less' : 'Show more'}
                </button>
              )}
            </div>

            {/* Amenities */}
            {property?.amenities && property.amenities.length > 0 && (
              <div className="py-6 border-b border-primary-200 dark:border-primary-700">
                <PropertyAmenities amenities={property.amenities} />
              </div>
            )}

            {/* Host Card */}
            <div className="py-6 border-b border-primary-200 dark:border-primary-700">
              <PropertyHostCard host={property?.host} propertyId={propertyId} />
            </div>

            {/* Reviews Section */}
            <div id="reviews" className="py-6 border-b border-primary-200 dark:border-primary-700">
              {reviews && reviews.length > 0 ? (
                <>
                  {/* Rating Summary */}
                  <div className="flex items-center gap-3 mb-6">
                    <Star className="w-6 h-6 fill-primary-900 dark:fill-sand-50 text-primary-900 dark:text-sand-50" />
                    <span className="text-2xl font-semibold text-primary-900 dark:text-sand-50">
                      {avgRating}
                    </span>
                    <span className="text-primary-600 dark:text-sand-400 text-lg">·</span>
                    <span className="text-2xl font-semibold text-primary-900 dark:text-sand-50">
                      {reviews.length} review{reviews.length !== 1 ? 's' : ''}
                    </span>
                  </div>

                  {/* Rating Breakdown Bars */}
                  <div className="grid grid-cols-1 sm:grid-cols-2 gap-x-16 gap-y-2 mb-8 max-w-lg">
                    {[5, 4, 3, 2, 1].map((star) => {
                      const count = ratingBreakdown[star] || 0;
                      const pct = reviews.length > 0 ? (count / reviews.length) * 100 : 0;
                      return (
                        <div key={star} className="flex items-center gap-2">
                          <span className="text-xs text-primary-900 dark:text-sand-50 w-3">{star}</span>
                          <div className="flex-1 h-1 bg-primary-200 dark:bg-primary-700 rounded-full overflow-hidden">
                            <div
                              className="h-full bg-primary-900 dark:bg-sand-100 rounded-full transition-all"
                              style={{ width: `${pct}%` }}
                            />
                          </div>
                        </div>
                      );
                    })}
                  </div>

                  {/* Review Cards */}
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
                    {displayedReviews?.map((review: Review) => (
                      <div key={review.id}>
                        <div className="flex items-center gap-3 mb-3">
                          {/* Avatar */}
                          <div className="w-10 h-10 rounded-full bg-primary-800 dark:bg-primary-600 flex items-center justify-center flex-shrink-0">
                            <span className="text-white text-sm font-semibold">
                              {review.guest?.first_name?.[0] || '?'}{review.guest?.last_name?.[0] || ''}
                            </span>
                          </div>
                          <div>
                            <p className="font-semibold text-sm text-primary-900 dark:text-sand-50">
                              {review.guest?.first_name} {review.guest?.last_name}
                            </p>
                            <p className="text-xs text-primary-500 dark:text-sand-500">
                              {new Date(review.created_at).toLocaleDateString('en-US', { month: 'long', year: 'numeric' })}
                            </p>
                          </div>
                        </div>
                        <div className="flex items-center gap-0.5 mb-2">
                          {[...Array(5)].map((_, i) => (
                            <Star
                              key={i}
                              className={`w-3 h-3 ${
                                i < review.rating
                                  ? 'fill-primary-900 dark:fill-sand-50 text-primary-900 dark:text-sand-50'
                                  : 'text-primary-200 dark:text-primary-600'
                              }`}
                            />
                          ))}
                        </div>
                        <p className="text-[15px] text-primary-700 dark:text-sand-200 line-clamp-4 leading-relaxed">
                          {review.text}
                        </p>
                      </div>
                    ))}
                  </div>

                  {reviews.length > 6 && (
                    <button
                      onClick={() => setShowAllReviews(!showAllReviews)}
                      className="mt-6 px-6 py-3 border border-primary-900 dark:border-sand-50 rounded-lg text-primary-900 dark:text-sand-50 font-semibold text-sm hover:bg-sand-50 dark:hover:bg-primary-800 transition"
                    >
                      {showAllReviews ? 'Show less' : `Show all ${reviews.length} reviews`}
                    </button>
                  )}
                </>
              ) : (
                <>
                  <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">Reviews</h2>
                  <div className="flex items-center gap-3 text-primary-500 dark:text-sand-400">
                    <Star className="w-5 h-5" />
                    <p className="text-[15px]">No reviews yet. Be the first to review this property!</p>
                  </div>
                </>
              )}
            </div>

            {/* Where you'll be — Location Map */}
            {property && (
              <div className="py-6 border-b border-primary-200 dark:border-primary-700">
                <PropertyLocationMap
                  location={property.location}
                  city={property.city}
                  country={property.country}
                  suburb={property.suburb}
                />
              </div>
            )}

            {/* Nearby Points of Interest */}
            {property && (
              <div className="py-6">
                <POIList propertyId={property.id} radiusKm={2} />
              </div>
            )}
          </div>

          {/* Right Column — Booking Panel */}
          <div className="lg:col-span-1">
            <div className="sticky top-24">
              {property && (
                <BookingPanel
                  propertyId={property.id}
                  pricePerNight={property.price_per_night}
                  maxGuests={property.max_guests}
                  minStay={property.min_stay_nights || 1}
                  hostVerified={property.host?.is_verified || false}
                  hostRating={property.average_rating || 0}
                  country={property.country}
                />
              )}
            </div>
          </div>
        </div>

        {/* Similar Properties — Full Width */}
        {property && (
          <div className="mt-12 pt-8 border-t border-primary-200 dark:border-primary-700">
            <SimilarProperties
              propertyId={property.id}
              city={property.city}
              country={property.country}
            />
          </div>
        )}
      </div>
    </div>
  );
}
