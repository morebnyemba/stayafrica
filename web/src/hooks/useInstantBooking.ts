import { useQuery, useMutation } from '@tanstack/react-query';
import { useState } from 'react';
import apiClient from '@/services/api-client';

interface QualificationDetails {
  is_qualified: boolean;
  requirements_met: {
    id_verified: boolean;
    min_reviews: boolean;
    min_rating: boolean;
    completed_bookings: boolean;
    payment_method: boolean;
  };
  guest_stats: {
    review_count: number;
    average_rating: number;
    completed_bookings_count: number;
  };
  required_reviews?: number;
  required_rating?: number;
}

interface InstantBookingData {
  check_in: string;
  check_out: string;
  number_of_guests: number;
}

export function useInstantBooking(propertyId: string) {
  const [bookingError, setBookingError] = useState<string | null>(null);

  const { data: qualificationDetails, isLoading } = useQuery({
    queryKey: ['instant-booking-qualification', propertyId],
    queryFn: async () => {
      const response = await apiClient.get<QualificationDetails>(
        `/properties/${propertyId}/instant_booking_info/`
      );
      return response.data;
    },
  });

  const bookMutation = useMutation({
    mutationFn: async (bookingData: InstantBookingData) => {
      const response = await apiClient.post('/bookings/', {
        rental_property: propertyId,
        ...bookingData,
      });
      return response.data;
    },
    onError: (error: any) => {
      setBookingError(
        error.response?.data?.message || 
        error.response?.data?.detail || 
        'Failed to create instant booking'
      );
    },
    onSuccess: () => {
      setBookingError(null);
    },
  });

  const bookInstantly = async (bookingData: InstantBookingData) => {
    if (!qualificationDetails?.is_qualified) {
      setBookingError('You do not meet the requirements for instant booking');
      return null;
    }

    return bookMutation.mutateAsync(bookingData);
  };

  return {
    isQualified: qualificationDetails?.is_qualified || false,
    isLoading,
    qualificationDetails,
    bookInstantly,
    isBooking: bookMutation.isPending,
    bookingError,
  };
}
