'use client';

import React from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Star, MessageSquare } from 'lucide-react';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { Button } from '@/components/ui/Button';

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
      <div className="min-h-screen bg-sand-100">
        <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          <header className="mb-6">
            <h1 className="text-2xl sm:text-3xl font-bold text-primary-900">My Reviews</h1>
          </header>
          <section className="mb-8" aria-labelledby="reviews-written-heading">
            <h2 id="reviews-written-heading" className="text-xl sm:text-2xl font-semibold mb-4 text-primary-900">Reviews Written</h2>
            {loadingWritten ? (
              <div className="space-y-4" aria-busy="true" aria-live="polite">
                {[1,2].map(i => (
                  <div key={i} className="animate-pulse h-12 bg-primary-200 rounded-lg" aria-label="Loading reviews" />
                ))}
              </div>
            ) : errorWritten ? (
              <div className="bg-white p-6 sm:p-8 rounded-lg text-center border border-primary-200" role="alert">
                <p className="text-primary-600 mb-4">Unable to load your reviews.</p>
                <Button variant="primary" size="lg" onClick={() => window.location.reload()} aria-label="Retry loading reviews">Retry</Button>
              </div>
            ) : writtenReviews.length === 0 ? (
              <div className="bg-white p-6 sm:p-8 rounded-lg text-center border border-primary-200">
                <MessageSquare className="w-8 h-8 text-primary-400 mx-auto mb-2" aria-hidden="true" />
                <p className="text-primary-600">You have not written any reviews yet.</p>
              </div>
            ) : (
              <div className="space-y-4">
                {writtenReviews.map((review: any) => (
                  <article key={review.id} className="bg-white p-4 sm:p-6 rounded-lg border border-primary-100" aria-label={`Review: ${review.rating} out of 5 stars`}>
                    <div className="flex items-center gap-2 mb-2">
                      <Star className="w-5 h-5 text-yellow-500" aria-hidden="true" />
                      <span className="font-semibold" aria-label={`Rating: ${review.rating} out of 5`}>{review.rating} / 5</span>
                    </div>
                    <p className="text-primary-900 mb-2">{review.text}</p>
                    <time className="text-xs text-primary-500" dateTime={review.created_at}>{new Date(review.created_at).toLocaleDateString()}</time>
                  </article>
                ))}
              </div>
            )}
          </section>
          <section aria-labelledby="reviews-received-heading">
            <h2 id="reviews-received-heading" className="text-xl sm:text-2xl font-semibold mb-4 text-primary-900">Reviews Received</h2>
            {loadingReceived ? (
              <div className="space-y-4" aria-busy="true" aria-live="polite">
                {[1,2].map(i => (
                  <div key={i} className="animate-pulse h-12 bg-primary-200 rounded-lg" aria-label="Loading reviews" />
                ))}
              </div>
            ) : errorReceived ? (
              <div className="bg-white p-6 sm:p-8 rounded-lg text-center border border-primary-200" role="alert">
                <p className="text-primary-600 mb-4">Unable to load received reviews.</p>
                <Button variant="primary" size="lg" onClick={() => window.location.reload()} aria-label="Retry loading reviews">Retry</Button>
              </div>
            ) : receivedReviews.length === 0 ? (
              <div className="bg-white p-6 sm:p-8 rounded-lg text-center border border-primary-200">
                <MessageSquare className="w-8 h-8 text-primary-400 mx-auto mb-2" aria-hidden="true" />
                <p className="text-primary-600">No reviews received yet.</p>
              </div>
            ) : (
              <div className="space-y-4 sm:space-y-6">
                {receivedReviews.map((review: any) => (
                  <article key={review.id} className="bg-white p-4 sm:p-6 rounded-lg border border-primary-100" aria-label={`Review received: ${review.rating} out of 5 stars`}>
                    <div className="flex items-center gap-2 mb-2">
                      <Star className="w-5 h-5 text-yellow-500" aria-hidden="true" />
                      <span className="font-semibold" aria-label={`Rating: ${review.rating} out of 5`}>{review.rating} / 5</span>
                    </div>
                    <p className="text-primary-900 mb-2">{review.text}</p>
                    <time className="text-xs text-primary-500 block mb-3" dateTime={review.created_at}>{new Date(review.created_at).toLocaleDateString()}</time>
                    {/* Reply section */}
                    <div className="mt-3 pt-3 border-t border-primary-100">
                      <label htmlFor={`reply-${review.id}`} className="block text-sm font-medium text-primary-700 mb-2">Reply:</label>
                      <textarea
                        id={`reply-${review.id}`}
                        className="w-full rounded-lg border border-primary-200 bg-sand-50 text-primary-900 p-3 mb-3 focus:ring-2 focus:ring-secondary-500 focus:outline-none min-h-[80px]"
                        rows={3}
                        value={replyFields[review.id] || ''}
                        onChange={e => setReplyFields((f: Record<string, string>) => ({ ...f, [review.id]: e.target.value }))}
                        aria-label={`Reply to review ${review.id}`}
                        placeholder="Write your reply here..."
                      />
                      <Button
                        variant="primary"
                        size="md"
                        disabled={replyMutation.isPending || !replyFields[review.id]?.trim()}
                        onClick={() => replyMutation.mutate({ reviewId: review.id, reply: replyFields[review.id] })}
                        aria-label={`Submit reply to review ${review.id}`}
                      >
                        {replyMutation.isPending ? 'Submitting...' : 'Submit Reply'}
                      </Button>
                      {review.reply && (
                        <div className="mt-3 p-3 bg-primary-50 rounded-lg text-primary-600 text-sm">
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
