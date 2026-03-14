'use client';

import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Star, MessageCircle } from 'lucide-react';
import Link from 'next/link';

interface Review {
  id: string;
  guest_first_name?: string;
  guest_last_name?: string;
  property_id?: string;
  property_title?: string;
  experience_id?: string;
  experience_title?: string;
  rating: number;
  text: string;
  created_at: string;
  host_response?: string;
  host_response_date?: string;
}

export function HostReviews() {
  const [selectedProperty, setSelectedProperty] = useState<string>('all');
  const [sortBy, setSortBy] = useState<'recent' | 'rating'>('recent');

  // Fetch host reviews
  const { data: reviewsData, isLoading } = useQuery({
    queryKey: ['host', 'reviews', selectedProperty, sortBy],
    queryFn: async () => {
      const response = await apiClient.get('/reviews/', {
        params: {
          property_id: selectedProperty !== 'all' ? selectedProperty : undefined,
          sort_by: sortBy,
        },
      });
      return response.data;
    },
  });

  // Fetch host properties for filter
  const { data: propertiesData } = useQuery({
    queryKey: ['host', 'properties'],
    queryFn: async () => {
      const response = await apiClient.getHostProperties();
      return response.data;
    },
  });

  const reviews: Review[] = reviewsData?.results || reviewsData || [];
  const properties = propertiesData?.results || propertiesData || [];

  // Calculate stats
  const totalReviews = reviews.length;
  const averageRating = reviews.length > 0
    ? reviews.reduce((sum, r) => sum + r.rating, 0) / reviews.length
    : 0;

  const ratingDistribution = [5, 4, 3, 2, 1].map((rating) => ({
    rating,
    count: reviews.filter((r) => r.rating === rating).length,
    percentage: totalReviews > 0
      ? (reviews.filter((r) => r.rating === rating).length / totalReviews) * 100
      : 0,
  }));

  if (isLoading) {
    return (
      <div className="min-h-screen bg-sand-100 py-8">
        <div className="max-w-6xl mx-auto px-4">
          <div className="mb-8">
            <div className="h-9 w-52 bg-primary-200 rounded animate-pulse mb-2" />
            <div className="h-5 w-72 bg-primary-200 rounded animate-pulse" />
          </div>
          {/* Stats skeleton */}
          <div className="card p-4 sm:p-6 mb-6 animate-pulse">
            <div className="flex flex-col sm:flex-row items-center sm:items-start gap-6 sm:gap-8">
              <div className="text-center">
                <div className="h-14 w-16 bg-primary-200 rounded mx-auto mb-2" />
                <div className="flex gap-1 mb-2 justify-center">
                  {[1,2,3,4,5].map(s => <div key={s} className="w-5 h-5 bg-primary-200 rounded" />)}
                </div>
              </div>
              <div className="flex-1 space-y-3">
                {[1,2,3,4,5].map(r => (
                  <div key={r} className="flex items-center gap-3">
                    <div className="w-8 h-4 bg-primary-200 rounded" />
                    <div className="flex-1 h-2 bg-primary-200 rounded-full" />
                    <div className="w-8 h-4 bg-primary-200 rounded" />
                  </div>
                ))}
              </div>
            </div>
          </div>
          {/* Review cards skeleton */}
          <div className="space-y-4">
            {[1,2,3].map(i => (
              <div key={i} className="card p-6 animate-pulse">
                <div className="flex items-start gap-4 mb-4">
                  <div className="w-12 h-12 bg-primary-200 rounded-full" />
                  <div className="flex-1 space-y-2">
                    <div className="h-5 w-32 bg-primary-200 rounded" />
                    <div className="h-4 w-48 bg-primary-200 rounded" />
                  </div>
                  <div className="flex gap-1">{[1,2,3,4,5].map(s => <div key={s} className="w-4 h-4 bg-primary-200 rounded" />)}</div>
                </div>
                <div className="h-4 w-full bg-primary-200 rounded mb-2" />
                <div className="h-4 w-3/4 bg-primary-200 rounded" />
              </div>
            ))}
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-sand-100 py-8">
      <div className="max-w-6xl mx-auto px-4">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-primary-900 mb-2">Guest Reviews</h1>
          <p className="text-primary-500">See what guests say about your properties</p>
        </div>

        {/* Stats Card */}
        <div className="card p-4 sm:p-6 mb-6">
          <div className="flex flex-col sm:flex-row items-center sm:items-start gap-6 sm:gap-8">
            {/* Average Rating */}
            <div className="text-center">
              <div className="text-5xl font-bold text-primary-900 mb-2">
                {averageRating.toFixed(1)}
              </div>
              <div className="flex gap-1 mb-2">
                {[1, 2, 3, 4, 5].map((star) => (
                  <Star
                    key={star}
                    className={`w-5 h-5 ${
                      star <= Math.round(averageRating)
                        ? 'fill-secondary-500 text-secondary-500'
                        : 'text-primary-200'
                    }`}
                  />
                ))}
              </div>
              <p className="text-sm text-primary-500">
                {totalReviews} {totalReviews === 1 ? 'review' : 'reviews'}
              </p>
            </div>

            {/* Rating Distribution */}
            <div className="flex-1">
              {ratingDistribution.map(({ rating, count, percentage }) => (
                <div key={rating} className="flex items-center gap-3 mb-2">
                  <span className="text-sm text-primary-500 w-8">{rating}</span>
                  <Star className="w-4 h-4 text-secondary-500 fill-secondary-500" />
                  <div className="flex-1 h-2 bg-primary-200 rounded-full overflow-hidden">
                    <div
                      className="h-full bg-secondary-500 rounded-full"
                      style={{ width: `${percentage}%` }}
                    />
                  </div>
                  <span className="text-sm text-primary-500 w-8 text-right">{count}</span>
                </div>
              ))}
            </div>
          </div>
        </div>

        {/* Filters */}
        <div className="flex flex-col sm:flex-row gap-3 mb-6">
          <div className="flex-1">
            <select
              value={selectedProperty}
              onChange={(e) => setSelectedProperty(e.target.value)}
              className="w-full px-4 py-3 bg-white border border-primary-300 rounded-xl focus:ring-2 focus:ring-secondary-500 focus:border-transparent text-sm"
            >
              <option value="all">All Properties</option>
              {properties.map((property: any) => (
                <option key={property.id} value={property.id}>
                  {property.title}
                </option>
              ))}
            </select>
          </div>

          <select
            value={sortBy}
            onChange={(e) => setSortBy(e.target.value as 'recent' | 'rating')}
            className="px-4 py-3 bg-white border border-primary-300 rounded-xl focus:ring-2 focus:ring-secondary-500 focus:border-transparent text-sm"
          >
            <option value="recent">Most Recent</option>
            <option value="rating">Highest Rating</option>
          </select>
        </div>

        {/* Reviews List */}
        <div className="space-y-4">
          {reviews.length === 0 ? (
            <div className="card p-12 text-center">
              <div className="w-16 h-16 bg-primary-100 rounded-full flex items-center justify-center mx-auto mb-4">
                <Star className="w-8 h-8 text-primary-300" />
              </div>
              <h3 className="text-xl font-semibold text-primary-900 mb-2">No Reviews Yet</h3>
              <p className="text-primary-500">
                Reviews from your guests will appear here after their stay
              </p>
            </div>
          ) : (
            reviews.map((review) => (
              <div
                key={review.id}
                className="card p-4 sm:p-6 hover:shadow-md transition-shadow"
              >
                {/* Review Header */}
                <div className="flex flex-col sm:flex-row sm:items-start justify-between gap-3 mb-4">
                  <div className="flex items-center gap-3 sm:gap-4">
                    <div className="w-12 h-12 bg-secondary-100 rounded-full flex items-center justify-center">
                      <span className="text-lg font-semibold text-secondary-700">
                        {review.guest_first_name?.[0] || '?'}
                      </span>
                    </div>
                    <div>
                      <h3 className="font-semibold text-primary-900">
                        {review.guest_first_name} {review.guest_last_name}
                      </h3>
                      {review.property_id ? (
                        <Link
                          href={`/host/properties/${review.property_id}`}
                          className="text-sm text-primary-500 hover:text-secondary-600"
                        >
                          {review.property_title}
                        </Link>
                      ) : (
                        <span className="text-sm text-primary-500">{review.experience_title}</span>
                      )}
                    </div>
                  </div>

                  <div className="flex items-center gap-2">
                    <div className="flex gap-0.5">
                      {[1, 2, 3, 4, 5].map((star) => (
                        <Star
                          key={star}
                          className={`w-4 h-4 ${
                            star <= review.rating
                              ? 'fill-secondary-500 text-secondary-500'
                              : 'text-primary-200'
                          }`}
                        />
                      ))}
                    </div>
                    <span className="text-sm font-semibold text-primary-900">
                      {review.rating.toFixed(1)}
                    </span>
                  </div>
                </div>

                {/* Review Text */}
                <p className="text-primary-700 mb-3">{review.text}</p>

                {/* Review Date */}
                <p className="text-sm text-primary-400 mb-4">
                  {new Date(review.created_at).toLocaleDateString('en-US', {
                    year: 'numeric',
                    month: 'long',
                    day: 'numeric',
                  })}
                </p>

                {/* Host Response */}
                {review.host_response ? (
                  <div className="bg-sand-50 rounded-xl p-4 border-l-4 border-secondary-600">
                    <div className="flex items-center gap-2 mb-2">
                      <MessageCircle className="w-4 h-4 text-secondary-600" />
                      <span className="text-sm font-semibold text-primary-900">Your Response</span>
                      <span className="text-xs text-primary-400">
                        {new Date(review.host_response_date!).toLocaleDateString()}
                      </span>
                    </div>
                    <p className="text-primary-700">{review.host_response}</p>
                  </div>
                ) : (
                  <button className="text-secondary-600 hover:text-secondary-700 font-medium text-sm flex items-center gap-2">
                    <MessageCircle className="w-4 h-4" />
                    Respond to review
                  </button>
                )}
              </div>
            ))
          )}
        </div>
      </div>
    </div>
  );
}
