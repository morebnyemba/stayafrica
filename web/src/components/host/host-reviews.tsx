'use client';

import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Star, MessageCircle } from 'lucide-react';
import Link from 'next/link';

interface Review {
  id: string;
  guest: {
    first_name: string;
    last_name: string;
  };
  property: {
    id: string;
    title: string;
  };
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
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-secondary-600 mx-auto mb-4"></div>
          <p className="text-primary-500 dark:text-sand-400">Loading reviews...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-sand-100 dark:bg-primary-900 py-8">
      <div className="max-w-6xl mx-auto px-4">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-2">Guest Reviews</h1>
          <p className="text-primary-500 dark:text-sand-400">See what guests say about your properties</p>
        </div>

        {/* Stats Card */}
        <div className="card p-6 mb-6">
          <div className="flex items-start gap-8">
            {/* Average Rating */}
            <div className="text-center">
              <div className="text-5xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                {averageRating.toFixed(1)}
              </div>
              <div className="flex gap-1 mb-2">
                {[1, 2, 3, 4, 5].map((star) => (
                  <Star
                    key={star}
                    className={`w-5 h-5 ${
                      star <= Math.round(averageRating)
                        ? 'fill-yellow-400 text-yellow-400'
                        : 'text-primary-200 dark:text-primary-600'
                    }`}
                  />
                ))}
              </div>
              <p className="text-sm text-primary-500 dark:text-sand-400">
                {totalReviews} {totalReviews === 1 ? 'review' : 'reviews'}
              </p>
            </div>

            {/* Rating Distribution */}
            <div className="flex-1">
              {ratingDistribution.map(({ rating, count, percentage }) => (
                <div key={rating} className="flex items-center gap-3 mb-2">
                  <span className="text-sm text-primary-500 dark:text-sand-400 w-8">{rating}</span>
                  <Star className="w-4 h-4 text-yellow-400 fill-yellow-400" />
                  <div className="flex-1 h-2 bg-primary-200 dark:bg-primary-700 rounded-full overflow-hidden">
                    <div
                      className="h-full bg-yellow-400 rounded-full"
                      style={{ width: `${percentage}%` }}
                    />
                  </div>
                  <span className="text-sm text-primary-500 dark:text-sand-400 w-8 text-right">{count}</span>
                </div>
              ))}
            </div>
          </div>
        </div>

        {/* Filters */}
        <div className="flex gap-4 mb-6">
          <div className="flex-1">
            <select
              value={selectedProperty}
              onChange={(e) => setSelectedProperty(e.target.value)}
              className="w-full px-4 py-3 bg-white dark:bg-primary-800/40 border border-primary-300 dark:border-primary-600 rounded-xl focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
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
            className="px-4 py-3 bg-white dark:bg-primary-800/40 border border-primary-300 dark:border-primary-600 rounded-xl focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
          >
            <option value="recent">Most Recent</option>
            <option value="rating">Highest Rating</option>
          </select>
        </div>

        {/* Reviews List */}
        <div className="space-y-4">
          {reviews.length === 0 ? (
            <div className="card p-12 text-center">
              <div className="w-16 h-16 bg-primary-100 dark:bg-primary-800 rounded-full flex items-center justify-center mx-auto mb-4">
                <Star className="w-8 h-8 text-primary-300 dark:text-primary-600" />
              </div>
              <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">No Reviews Yet</h3>
              <p className="text-primary-500 dark:text-sand-400">
                Reviews from your guests will appear here after their stay
              </p>
            </div>
          ) : (
            reviews.map((review) => (
              <div
                key={review.id}
                className="card p-6 hover:shadow-md transition-shadow"
              >
                {/* Review Header */}
                <div className="flex items-start justify-between mb-4">
                  <div className="flex items-center gap-4">
                    <div className="w-12 h-12 bg-secondary-100 dark:bg-secondary-900/30 rounded-full flex items-center justify-center">
                      <span className="text-lg font-semibold text-secondary-700 dark:text-secondary-300">
                        {review.guest.first_name[0]}
                      </span>
                    </div>
                    <div>
                      <h3 className="font-semibold text-primary-900 dark:text-sand-50">
                        {review.guest.first_name} {review.guest.last_name}
                      </h3>
                      <Link
                        href={`/host/properties/${review.property.id}`}
                        className="text-sm text-primary-500 dark:text-sand-400 hover:text-secondary-600 dark:hover:text-secondary-400"
                      >
                        {review.property.title}
                      </Link>
                    </div>
                  </div>

                  <div className="flex items-center gap-2">
                    <div className="flex gap-0.5">
                      {[1, 2, 3, 4, 5].map((star) => (
                        <Star
                          key={star}
                          className={`w-4 h-4 ${
                            star <= review.rating
                              ? 'fill-yellow-400 text-yellow-400'
                              : 'text-primary-200 dark:text-primary-600'
                          }`}
                        />
                      ))}
                    </div>
                    <span className="text-sm font-semibold text-primary-900 dark:text-sand-50">
                      {review.rating.toFixed(1)}
                    </span>
                  </div>
                </div>

                {/* Review Text */}
                <p className="text-primary-700 dark:text-sand-200 mb-3">{review.text}</p>

                {/* Review Date */}
                <p className="text-sm text-primary-400 dark:text-sand-500 mb-4">
                  {new Date(review.created_at).toLocaleDateString('en-US', {
                    year: 'numeric',
                    month: 'long',
                    day: 'numeric',
                  })}
                </p>

                {/* Host Response */}
                {review.host_response ? (
                  <div className="bg-sand-50 dark:bg-primary-800 rounded-xl p-4 border-l-4 border-secondary-600">
                    <div className="flex items-center gap-2 mb-2">
                      <MessageCircle className="w-4 h-4 text-secondary-600 dark:text-secondary-400" />
                      <span className="text-sm font-semibold text-primary-900 dark:text-sand-50">Your Response</span>
                      <span className="text-xs text-primary-400 dark:text-sand-500">
                        {new Date(review.host_response_date!).toLocaleDateString()}
                      </span>
                    </div>
                    <p className="text-primary-700 dark:text-sand-200">{review.host_response}</p>
                  </div>
                ) : (
                  <button className="text-secondary-600 dark:text-secondary-400 hover:text-secondary-700 dark:hover:text-secondary-300 font-medium text-sm flex items-center gap-2">
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
