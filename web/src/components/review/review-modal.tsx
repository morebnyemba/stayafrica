'use client';

import { useState } from 'react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { Star } from 'lucide-react';
import { apiClient } from '@/services/api-client';
import toast from 'react-hot-toast';
import { Button } from '@/components/ui';

// Custom Dialog to avoid Radix UI dependency issues if not installed
interface ReviewModalProps {
  isOpen: boolean;
  onClose: () => void;
  bookingId: number;
  propertyTitle: string;
}

export function ReviewModal({ isOpen, onClose, bookingId, propertyTitle }: ReviewModalProps) {
  const queryClient = useQueryClient();
  const [rating, setRating] = useState<number>(0);
  const [hoverRating, setHoverRating] = useState<number>(0);
  const [text, setText] = useState('');

  const submitMutation = useMutation({
    mutationFn: async () => {
      return apiClient.createReview({
        booking_id: bookingId,
        rating,
        text,
      });
    },
    onSuccess: () => {
      toast.success('Review submitted successfully!');
      // Invalidate relevant queries
      queryClient.invalidateQueries({ queryKey: ['bookings'] });
      queryClient.invalidateQueries({ queryKey: ['reviews'] });
      
      // Reset form and close
      setRating(0);
      setText('');
      onClose();
    },
    onError: (error: any) => {
      let msg = 'Failed to submit review';
      if (error.response?.data) {
        if (typeof error.response.data.error === 'string') {
          msg = error.response.data.error;
        } else if (typeof error.response.data.detail === 'string') {
          msg = error.response.data.detail;
        } else {
          // Handle DRF list format e.g. {"non_field_errors": ["..."]}
          const firstKey = Object.keys(error.response.data)[0];
          if (firstKey && Array.isArray(error.response.data[firstKey])) {
            msg = error.response.data[firstKey][0];
          }
        }
      }
      toast.error(msg);
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (rating === 0) {
      toast.error('Please select a rating');
      return;
    }
    if (!text.trim()) {
      toast.error('Please write a review');
      return;
    }
    submitMutation.mutate();
  };

  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center overflow-y-auto overflow-x-hidden bg-black/50 p-4">
      <div className="relative w-full max-w-md rounded-xl bg-white p-6 shadow-2xl">
        <div className="mb-4">
          <h3 className="text-xl font-bold text-primary-900">Review your stay</h3>
          <p className="mt-1 text-sm text-primary-600">
            How was your experience at {propertyTitle}? Your feedback helps other guests.
          </p>
        </div>

        <form onSubmit={handleSubmit} className="space-y-6">
          {/* Star Rating Section */}
          <div className="flex flex-col items-center justify-center space-y-3 p-4 bg-sand-50 rounded-lg border border-primary-100">
            <div className="flex items-center gap-2">
              {[1, 2, 3, 4, 5].map((star) => (
                <button
                  key={star}
                  type="button"
                  onClick={() => setRating(star)}
                  onMouseEnter={() => setHoverRating(star)}
                  onMouseLeave={() => setHoverRating(0)}
                  className="p-1 focus:outline-none focus-visible:ring-2 focus-visible:ring-secondary-500 rounded-full transition-transform hover:scale-110"
                >
                  <Star
                    className={`w-10 h-10 transition-colors ${
                      star <= (hoverRating || rating)
                        ? 'fill-amber-400 text-amber-400'
                        : 'text-primary-200'
                    }`}
                  />
                </button>
              ))}
            </div>
            <span className="text-sm font-semibold h-5 text-primary-700">
              {rating === 0 ? 'Tap a star to rate' : 
               rating === 1 ? 'Terrible' : 
               rating === 2 ? 'Poor' : 
               rating === 3 ? 'Okay' : 
               rating === 4 ? 'Good' : 'Excellent'}
            </span>
          </div>

          {/* Text Area Section */}
          <div className="space-y-2">
            <label htmlFor="review-text" className="text-sm font-semibold text-primary-900">
              Share details of your own experience
            </label>
            <textarea
              id="review-text"
              value={text}
              onChange={(e) => setText(e.target.value)}
              placeholder="What did you like? What could be improved? Would you recommend it to others?"
              className="w-full min-h-[120px] p-3 rounded-lg border border-primary-200 bg-white text-primary-900 focus:ring-2 focus:ring-secondary-500 focus:border-transparent resize-y"
              disabled={submitMutation.isPending}
            />
          </div>

          {/* Actions */}
          <div className="flex justify-end gap-3 pt-2">
            <Button
              type="button"
              variant="outline"
              onClick={onClose}
              disabled={submitMutation.isPending}
            >
              Cancel
            </Button>
            <Button
              type="submit"
              disabled={rating === 0 || !text.trim() || submitMutation.isPending}
            >
              {submitMutation.isPending ? 'Submitting...' : 'Submit Review'}
            </Button>
          </div>
        </form>
      </div>
    </div>
  );
}
