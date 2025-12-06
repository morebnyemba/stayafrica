'use client';

import { useQuery, useMutation, useQueryClient } from 'react-query';
import { apiClient } from './api-client';

// Properties
export function useProperties(filters?: any) {
  return useQuery(['properties', filters], () => apiClient.getProperties(filters));
}

export function useProperty(id: string) {
  return useQuery(['property', id], () => apiClient.getPropertyById(id));
}

export function useNearbyProperties(latitude: number, longitude: number, radius: number = 10) {
  return useQuery(
    ['properties-nearby', latitude, longitude, radius],
    () => apiClient.searchNearby(latitude, longitude, radius)
  );
}

export function useCreateProperty() {
  const queryClient = useQueryClient();
  return useMutation((data: any) => apiClient.createProperty(data), {
    onSuccess: () => {
      queryClient.invalidateQueries('properties');
    },
  });
}

// Bookings
export function useBookings(filters?: any) {
  return useQuery(['bookings', filters], () => apiClient.getBookings(filters));
}

export function useBooking(id: string) {
  return useQuery(['booking', id], () => apiClient.getBookingById(id));
}

export function useCreateBooking() {
  const queryClient = useQueryClient();
  return useMutation((data: any) => apiClient.createBooking(data), {
    onSuccess: () => {
      queryClient.invalidateQueries('bookings');
    },
  });
}

export function useConfirmBooking() {
  const queryClient = useQueryClient();
  return useMutation((id: string) => apiClient.confirmBooking(id), {
    onSuccess: () => {
      queryClient.invalidateQueries('bookings');
    },
  });
}

export function useCancelBooking() {
  const queryClient = useQueryClient();
  return useMutation((id: string) => apiClient.cancelBooking(id), {
    onSuccess: () => {
      queryClient.invalidateQueries('bookings');
    },
  });
}

// Payments
export function useInitiatePayment() {
  return useMutation((data: { bookingId: string; provider: string }) =>
    apiClient.initiatePayment(data.bookingId, data.provider)
  );
}

export function usePaymentStatus(paymentId: string) {
  return useQuery(['payment', paymentId], () => apiClient.getPaymentStatus(paymentId));
}

// Reviews
export function useReviews(params?: any) {
  return useQuery(['reviews', params], () => apiClient.getReviews(params));
}

export function useCreateReview() {
  const queryClient = useQueryClient();
  return useMutation((data: any) => apiClient.createReview(data), {
    onSuccess: () => {
      queryClient.invalidateQueries('reviews');
    },
  });
}

// Messages
export function useMessages(params?: any) {
  return useQuery(['messages', params], () => apiClient.getMessages(params));
}

export function useSendMessage() {
  const queryClient = useQueryClient();
  return useMutation((data: any) => apiClient.sendMessage(data), {
    onSuccess: () => {
      queryClient.invalidateQueries('messages');
    },
  });
}

export function useConversations() {
  return useQuery(['conversations'], () => apiClient.getConversations());
}

export function useUnreadCount() {
  return useQuery(['unread-count'], () => apiClient.getUnreadCount());
}

// Users
export function useUserProfile() {
  return useQuery(['user-profile'], () => apiClient.getUserProfile());
}

export function useUpdateProfile() {
  const queryClient = useQueryClient();
  return useMutation((data: any) => apiClient.updateUserProfile(data), {
    onSuccess: () => {
      queryClient.invalidateQueries('user-profile');
    },
  });
}

// Admin
export function useAdminStats() {
  return useQuery(['admin-stats'], () => apiClient.getAdminStats());
}

export function useAuditLogs(params?: any) {
  return useQuery(['audit-logs', params], () => apiClient.getAuditLogs(params));
}
