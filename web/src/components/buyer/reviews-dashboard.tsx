'use client';

import React from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { Star, MessageSquare } from 'lucide-react';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { Button } from '@/components/ui/Button';
import { NoticeModal } from '@/components/common/notice-modal';

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

  const [editingReviewId, setEditingReviewId] = React.useState<string | null>(null);
  const [editRating, setEditRating] = React.useState<number>(0);
  const [editText, setEditText] = React.useState<string>('');
  const [noticeMessage, setNoticeMessage] = React.useState('');

  const updateMutation = useMutation<any, Error, { reviewId: string; rating: number; text: string }>({
    mutationFn: async (params) => {
      return apiClient.updateReview(params.reviewId, { rating: params.rating, text: params.text });
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['reviews', 'written'] });
      setEditingReviewId(null);
    },
    onError: (error: any) => {
      setNoticeMessage(error?.response?.data?.error || 'Failed to update review. It may be past the allowed edit window.');
    }
  });

  const handleEditClick = (review: any) => {
    setEditingReviewId(review.id);
    setEditRating(review.rating);
    setEditText(review.text);
  };

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
                    {editingReviewId === review.id ? (
                      <div className="space-y-3">
                        <div className="flex gap-1">
                          {[1, 2, 3, 4, 5].map((star) => (
                            <button
                              key={star}
                              type="button"
                              onClick={() => setEditRating(star)}
                              className="focus:outline-none"
                            >
                              <Star className={`w-6 h-6 ${star <= editRating ? 'text-yellow-500 fill-yellow-500' : 'text-gray-300'}`} />
                            </button>
                          ))}
                        </div>
                        <textarea
                          className="w-full rounded-lg border border-primary-200 p-3 min-h-[100px] text-primary-900 focus:ring-2 focus:ring-secondary-500 focus:outline-none"
                          value={editText}
                          onChange={(e) => setEditText(e.target.value)}
                        />
                        <div className="flex gap-2">
                          <Button variant="primary" size="sm" onClick={() => updateMutation.mutate({ reviewId: review.id, rating: editRating, text: editText })} disabled={updateMutation.isPending}>
                            {updateMutation.isPending ? 'Saving...' : 'Save'}
                          </Button>
                          <Button variant="outline" size="sm" onClick={() => setEditingReviewId(null)}>Cancel</Button>
                        </div>
                      </div>
                    ) : (
                      <>
                        <div className="flex items-center justify-between mb-2">
                          <div className="flex items-center gap-2">
                            <Star className="w-5 h-5 text-yellow-500 fill-yellow-500" aria-hidden="true" />
                            <span className="font-semibold" aria-label={`Rating: ${review.rating} out of 5`}>{review.rating} / 5</span>
                          </div>
                          <Button variant="outline" size="sm" onClick={() => handleEditClick(review)}>Edit</Button>
                        </div>
                        {(review.property_title || review.experience_title) && (
                          <p className="text-sm font-medium text-secondary-600 mb-2">
                            {review.property_title || review.experience_title}
                            {review.booking_ref && <span className="text-primary-400 font-normal ml-2">Ref: {review.booking_ref}</span>}
                          </p>
                        )}
                        <p className="text-primary-900 mb-2">{review.text}</p>
                        <time className="text-xs text-primary-500" dateTime={review.created_at}>{new Date(review.created_at).toLocaleDateString()}</time>
                      </>
                    )}
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
                    {/* Add Context to Received Reviews */}
                    {(review.property_title || review.experience_title) && (
                      <p className="text-sm font-medium text-secondary-600 mb-2">
                        {review.property_title || review.experience_title}
                        {review.guest_first_name && (
                          <span className="text-primary-500 font-normal ml-2">
                            (Guest: {review.guest_first_name} {review.guest_last_name})
                          </span>
                        )}
                      </p>
                    )}
                    <p className="text-primary-900 mb-2">{review.text}</p>
                    <time className="text-xs text-primary-500 block mb-3" dateTime={review.created_at}>{new Date(review.created_at).toLocaleDateString()}</time>
                  </article>
                ))}
              </div>
            )}
          </section>
        </div>
      </div>
      <NoticeModal
        isOpen={Boolean(noticeMessage)}
        title="Unable to Update Review"
        message={noticeMessage}
        onClose={() => setNoticeMessage('')}
      />
    </ProtectedRoute>
  );
}
