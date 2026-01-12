'use client';

import { useState } from 'react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Star, ThumbsUp, MessageSquare } from 'lucide-react';
import { Button } from '@/components/ui';
import { useAuth } from '@/store/auth-store';
import toast from 'react-hot-toast';

interface Review {
  id: string;
  guest_name: string;
  rating: number;
  text: string;
  host_response?: string;
  host_response_date?: string;
  helpful_count: number;
  created_at: string;
  guest?: number;
}

interface ReviewListProps {
  propertyId: string;
  reviews: Review[];
  averageRating?: number;
  totalReviews?: number;
}

export function ReviewList({ propertyId, reviews, averageRating, totalReviews }: ReviewListProps) {
  const { user, isAuthenticated } = useAuth();
  const queryClient = useQueryClient();
  const [respondingTo, setRespondingTo] = useState<string | null>(null);
  const [responseText, setResponseText] = useState('');

  const voteMutation = useMutation({
    mutationFn: async ({ reviewId, voteType }: { reviewId: string; voteType: 'helpful' | 'unhelpful' }) => {
      return apiClient.voteReview(reviewId, voteType);
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['property-reviews', propertyId] });
      toast.success('Vote recorded');
    },
    onError: (error: any) => {
      toast.error(error.response?.data?.error || 'Failed to vote');
    },
  });

  const respondMutation = useMutation({
    mutationFn: async ({ reviewId, response }: { reviewId: string; response: string }) => {
      return apiClient.respondToReview(reviewId, response);
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['property-reviews', propertyId] });
      setRespondingTo(null);
      setResponseText('');
      toast.success('Response posted');
    },
    onError: (error: any) => {
      toast.error(error.response?.data?.error || 'Failed to post response');
    },
  });

  const handleVote = (reviewId: string) => {
    if (!isAuthenticated) {
      toast.error('Please log in to vote');
      return;
    }
    voteMutation.mutate({ reviewId, voteType: 'helpful' });
  };

  const handleRespond = (reviewId: string) => {
    if (!responseText.trim()) {
      toast.error('Please enter a response');
      return;
    }
    respondMutation.mutate({ reviewId, response: responseText });
  };

  const renderStars = (rating: number) => {
    return (
      <div className="flex items-center gap-1">
        {[1, 2, 3, 4, 5].map((star) => (
          <Star
            key={star}
            className={`w-4 h-4 ${
              star <= rating
                ? 'fill-secondary-500 text-secondary-500'
                : 'text-primary-300 dark:text-primary-600'
            }`}
          />
        ))}
      </div>
    );
  };

  return (
    <div className="space-y-6">
      {/* Header with average rating */}
      {averageRating !== undefined && totalReviews !== undefined && totalReviews > 0 && (
        <div className="card p-6 mb-6">
          <div className="flex items-center gap-4">
            <div className="text-center">
              <div className="text-4xl font-bold text-primary-900 dark:text-sand-50">
                {averageRating.toFixed(1)}
              </div>
              <div className="flex items-center gap-1 mt-2">
                {renderStars(Math.round(averageRating))}
              </div>
            </div>
            <div className="flex-1 border-l border-primary-200 dark:border-primary-700 pl-4">
              <p className="text-lg font-semibold text-primary-900 dark:text-sand-50">
                {totalReviews} {totalReviews === 1 ? 'Review' : 'Reviews'}
              </p>
              <p className="text-sm text-primary-600 dark:text-sand-400">
                Based on verified bookings
              </p>
            </div>
          </div>
        </div>
      )}

      {/* Reviews list */}
      {reviews.length === 0 ? (
        <div className="card p-12 text-center">
          <MessageSquare className="w-12 h-12 text-primary-400 dark:text-sand-500 mx-auto mb-4" />
          <h3 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-2">
            No Reviews Yet
          </h3>
          <p className="text-primary-600 dark:text-sand-300">
            Be the first to book and leave a review!
          </p>
        </div>
      ) : (
        <div className="space-y-4">
          {reviews.map((review) => (
            <article key={review.id} className="card p-6">
              {/* Review header */}
              <div className="flex items-start justify-between mb-3">
                <div>
                  <div className="flex items-center gap-3 mb-2">
                    <span className="font-semibold text-primary-900 dark:text-sand-50">
                      {review.guest_name}
                    </span>
                    {renderStars(review.rating)}
                  </div>
                  <time className="text-sm text-primary-600 dark:text-sand-400">
                    {new Date(review.created_at).toLocaleDateString('en-US', {
                      year: 'numeric',
                      month: 'long',
                      day: 'numeric',
                    })}
                  </time>
                </div>
              </div>

              {/* Review text */}
              <p className="text-primary-900 dark:text-sand-100 mb-4">{review.text}</p>

              {/* Host response */}
              {review.host_response && (
                <div className="mt-4 pl-4 border-l-2 border-secondary-500 bg-sand-50 dark:bg-primary-800 p-4 rounded-r">
                  <p className="text-sm font-semibold text-primary-900 dark:text-sand-50 mb-1">
                    Response from host
                  </p>
                  <p className="text-sm text-primary-700 dark:text-sand-300 mb-2">
                    {review.host_response}
                  </p>
                  {review.host_response_date && (
                    <time className="text-xs text-primary-600 dark:text-sand-400">
                      {new Date(review.host_response_date).toLocaleDateString()}
                    </time>
                  )}
                </div>
              )}

              {/* Actions */}
              <div className="mt-4 flex items-center gap-4 pt-4 border-t border-primary-200 dark:border-primary-700">
                <button
                  onClick={() => handleVote(review.id)}
                  disabled={voteMutation.isPending || !isAuthenticated}
                  className="flex items-center gap-2 text-sm text-primary-600 dark:text-sand-400 hover:text-primary-900 dark:hover:text-sand-50 transition-colors disabled:opacity-50"
                >
                  <ThumbsUp className="w-4 h-4" />
                  <span>Helpful ({review.helpful_count})</span>
                </button>

                {/* Show respond button only to host */}
                {isAuthenticated && user?.role === 'host' && !review.host_response && (
                  <button
                    onClick={() => setRespondingTo(respondingTo === review.id ? null : review.id)}
                    className="flex items-center gap-2 text-sm text-primary-600 dark:text-sand-400 hover:text-primary-900 dark:hover:text-sand-50 transition-colors"
                  >
                    <MessageSquare className="w-4 h-4" />
                    <span>Respond</span>
                  </button>
                )}
              </div>

              {/* Response form */}
              {respondingTo === review.id && (
                <div className="mt-4 space-y-3">
                  <textarea
                    value={responseText}
                    onChange={(e) => setResponseText(e.target.value)}
                    placeholder="Write your response..."
                    rows={3}
                    className="w-full px-3 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
                  />
                  <div className="flex gap-2">
                    <Button
                      onClick={() => handleRespond(review.id)}
                      disabled={respondMutation.isPending || !responseText.trim()}
                      size="sm"
                    >
                      {respondMutation.isPending ? 'Posting...' : 'Post Response'}
                    </Button>
                    <Button
                      onClick={() => {
                        setRespondingTo(null);
                        setResponseText('');
                      }}
                      variant="outline"
                      size="sm"
                    >
                      Cancel
                    </Button>
                  </div>
                </div>
              )}
            </article>
          ))}
        </div>
      )}
    </div>
  );
}
