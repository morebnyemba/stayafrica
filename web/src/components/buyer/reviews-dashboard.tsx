'use client';

import React from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Star, MessageSquare } from 'lucide-react';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });

export function ReviewsDashboard() {
  const queryClient = useQueryClient();
  const { data: writtenReviews = [], isLoading: loadingWritten, error: errorWritten } = useQuery({
    queryKey: ['reviews', 'written'],
    queryFn: async () => {
      const response = await apiClient.getReviews({ written_by_user: true });
      return response.data?.results || [];
    },
  });

  const { data: receivedReviews = [], isLoading: loadingReceived, error: errorReceived } = useQuery({
    queryKey: ['reviews', 'received'],
    queryFn: async () => {
      const response = await apiClient.getReviews({ received_by_user: true });
      return response.data?.results || [];
    },
  });

  // Reply mutation
  const replyMutation = useMutation<
    any,
    Error,
    { reviewId: string; reply: string }
  >({
    mutationFn: async (params: { reviewId: string; reply: string }) => {
      return apiClient.replyToReview(params.reviewId, params.reply);
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['reviews', 'received'] });
    },
  });  // Local state for reply fields
  const [replyFields, setReplyFields] = React.useState<Record<string, string>>({});

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          <header className="mb-6">
            <h1 className="text-2xl sm:text-3xl font-bold text-primary-900 dark:text-sand-50">My Reviews</h1>
          </header>
          <section className="mb-8" aria-labelledby="reviews-written-heading">
            <h2 id="reviews-written-heading" className="text-xl sm:text-2xl font-semibold mb-4 text-primary-900 dark:text-sand-50">Reviews Written</h2>
            {loadingWritten ? (
              <div className="space-y-4" aria-busy="true" aria-live="polite">
                {[1,2].map(i => (
                  <div key={i} className="animate-pulse h-12 bg-primary-200 dark:bg-primary-700 rounded-lg" aria-label="Loading reviews" />
                ))}
              </div>
            ) : errorWritten ? (
              <div className="bg-white dark:bg-primary-800 p-6 sm:p-8 rounded-lg text-center border border-primary-200 dark:border-primary-700" role="alert">
                <p className="text-primary-600 dark:text-sand-300 mb-4">Unable to load your reviews.</p>
                <button className="btn-primary px-6 py-2 min-h-[44px]" onClick={() => window.location.reload()} aria-label="Retry loading reviews">Retry</button>
              </div>
            ) : writtenReviews.length === 0 ? (
              <div className="bg-white dark:bg-primary-800 p-6 sm:p-8 rounded-lg text-center border border-primary-200 dark:border-primary-700">
                <MessageSquare className="w-8 h-8 text-primary-400 dark:text-sand-500 mx-auto mb-2" aria-hidden="true" />
                <p className="text-primary-600 dark:text-sand-300">You have not written any reviews yet.</p>
              </div>
            ) : (
              <div className="space-y-4">
                {writtenReviews.map((review: any) => (
                  <article key={review.id} className="bg-white dark:bg-primary-800 p-4 sm:p-6 rounded-lg border border-primary-100 dark:border-primary-700" aria-label={`Review: ${review.rating} out of 5 stars`}>
                    <div className="flex items-center gap-2 mb-2">
                      <Star className="w-5 h-5 text-yellow-500" aria-hidden="true" />
                      <span className="font-semibold" aria-label={`Rating: ${review.rating} out of 5`}>{review.rating} / 5</span>
                    </div>
                    <p className="text-primary-900 dark:text-sand-50 mb-2">{review.text}</p>
                    <time className="text-xs text-primary-500 dark:text-sand-400" dateTime={review.created_at}>{new Date(review.created_at).toLocaleDateString()}</time>
                  </article>
                ))}
              </div>
            )}
          </section>
          <section aria-labelledby="reviews-received-heading">
            <h2 id="reviews-received-heading" className="text-xl sm:text-2xl font-semibold mb-4 text-primary-900 dark:text-sand-50">Reviews Received</h2>
            {loadingReceived ? (
              <div className="space-y-4" aria-busy="true" aria-live="polite">
                {[1,2].map(i => (
                  <div key={i} className="animate-pulse h-12 bg-primary-200 dark:bg-primary-700 rounded-lg" aria-label="Loading reviews" />
                ))}
              </div>
            ) : errorReceived ? (
              <div className="bg-white dark:bg-primary-800 p-6 sm:p-8 rounded-lg text-center border border-primary-200 dark:border-primary-700" role="alert">
                <p className="text-primary-600 dark:text-sand-300 mb-4">Unable to load received reviews.</p>
                <button className="btn-primary px-6 py-2 min-h-[44px]" onClick={() => window.location.reload()} aria-label="Retry loading reviews">Retry</button>
              </div>
            ) : receivedReviews.length === 0 ? (
              <div className="bg-white dark:bg-primary-800 p-6 sm:p-8 rounded-lg text-center border border-primary-200 dark:border-primary-700">
                <MessageSquare className="w-8 h-8 text-primary-400 dark:text-sand-500 mx-auto mb-2" aria-hidden="true" />
                <p className="text-primary-600 dark:text-sand-300">No reviews received yet.</p>
              </div>
            ) : (
              <div className="space-y-4 sm:space-y-6">
                {receivedReviews.map((review: any) => (
                  <article key={review.id} className="bg-white dark:bg-primary-800 p-4 sm:p-6 rounded-lg border border-primary-100 dark:border-primary-700" aria-label={`Review received: ${review.rating} out of 5 stars`}>
                    <div className="flex items-center gap-2 mb-2">
                      <Star className="w-5 h-5 text-yellow-500" aria-hidden="true" />
                      <span className="font-semibold" aria-label={`Rating: ${review.rating} out of 5`}>{review.rating} / 5</span>
                    </div>
                    <p className="text-primary-900 dark:text-sand-50 mb-2">{review.text}</p>
                    <time className="text-xs text-primary-500 dark:text-sand-400 block mb-3" dateTime={review.created_at}>{new Date(review.created_at).toLocaleDateString()}</time>
                    {/* Reply section */}
                    <div className="mt-3 pt-3 border-t border-primary-100 dark:border-primary-700">
                      <label htmlFor={`reply-${review.id}`} className="block text-sm font-medium text-primary-700 dark:text-sand-200 mb-2">Reply:</label>
                      <textarea
                        id={`reply-${review.id}`}
                        className="w-full rounded-lg border border-primary-200 dark:border-primary-700 bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 p-3 mb-3 focus:ring-2 focus:ring-secondary-500 focus:outline-none min-h-[80px]"
                        rows={3}
                        value={replyFields[review.id] || ''}
                        onChange={e => setReplyFields((f: Record<string, string>) => ({ ...f, [review.id]: e.target.value }))}
                        aria-label={`Reply to review ${review.id}`}
                        placeholder="Write your reply here..."
                      />
                      <button
                        className="btn-primary px-4 sm:px-6 py-2 text-sm sm:text-base min-h-[44px]"
                        disabled={replyMutation.isPending || !replyFields[review.id]?.trim()}
                        onClick={() => replyMutation.mutate({ reviewId: review.id, reply: replyFields[review.id] })}
                        aria-label={`Submit reply to review ${review.id}`}
                      >
                        {replyMutation.isPending ? 'Submitting...' : 'Submit Reply'}
                      </button>
                      {review.reply && (
                        <div className="mt-3 p-3 bg-primary-50 dark:bg-primary-700 rounded-lg text-primary-600 dark:text-sand-300 text-sm">
                          <span className="font-semibold">Your reply:</span> {review.reply}
                        </div>
                      )}
                    </div>
                  </article>
                ))}
              </div>
            )}
          </section>
        </div>
      </div>
    </ProtectedRoute>
  );
}
