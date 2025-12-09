'use client';

import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';

// Properties
export function useProperties(filters?: any) {
  return useQuery({
    queryKey: ['properties', filters],
    queryFn: () => apiClient.getProperties(filters),
  });
}

export function useProperty(id: string) {
  return useQuery({
    queryKey: ['property', id],
    queryFn: () => apiClient.getPropertyById(id),
  });
}

export function useNearbyProperties(latitude: number, longitude: number, radius: number = 10) {
  return useQuery({
    queryKey: ['properties-nearby', latitude, longitude, radius],
    queryFn: () => apiClient.searchNearby(latitude, longitude, radius),
  });
}

export function useCreateProperty() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (data: any) => apiClient.createProperty(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['properties'] });
    },
  });
}

// Bookings
export function useBookings(filters?: any) {
  return useQuery({
    queryKey: ['bookings', filters],
    queryFn: () => apiClient.getBookings(filters),
  });
}

export function useBooking(id: string) {
  return useQuery({
    queryKey: ['booking', id],
    queryFn: () => apiClient.getBookingById(id),
  });
}

export function useCreateBooking() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (data: any) => apiClient.createBooking(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bookings'] });
    },
  });
}

export function useConfirmBooking() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (id: string) => apiClient.confirmBooking(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bookings'] });
    },
  });
}

export function useCancelBooking() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (id: string) => apiClient.cancelBooking(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bookings'] });
    },
  });
}

// Payments
export function useInitiatePayment() {
  return useMutation({
    mutationFn: (data: { bookingId: string; provider: string }) =>
      apiClient.initiatePayment(data.bookingId, data.provider),
  });
}

export function usePaymentStatus(paymentId: string) {
  return useQuery({
    queryKey: ['payment', paymentId],
    queryFn: () => apiClient.getPaymentStatus(paymentId),
  });
}

// Reviews
export function useReviews(params?: any) {
  return useQuery({
    queryKey: ['reviews', params],
    queryFn: () => apiClient.getReviews(params),
  });
}

export function useCreateReview() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (data: any) => apiClient.createReview(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['reviews'] });
    },
  });
}

// Messages
export function useMessages(params?: any) {
  return useQuery({
    queryKey: ['messages', params],
    queryFn: () => apiClient.getMessages(params),
  });
}

export function useSendMessage() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (data: any) => apiClient.sendMessage(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['messages'] });
    },
  });
}

export function useConversations() {
  return useQuery({
    queryKey: ['conversations'],
    queryFn: () => apiClient.getConversations(),
  });
}

export function useUnreadCount() {
  return useQuery({
    queryKey: ['unread-count'],
    queryFn: () => apiClient.getUnreadCount(),
  });
}

// Users
export function useUserProfile() {
  return useQuery({
    queryKey: ['user-profile'],
    queryFn: () => apiClient.getUserProfile(),
  });
}

export function useUpdateProfile() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (data: any) => apiClient.updateUserProfile(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['user-profile'] });
    },
  });
}

// Admin
export function useAdminStats() {
  return useQuery({
    queryKey: ['admin-stats'],
    queryFn: () => apiClient.getAdminStats(),
  });
}

export function useAuditLogs(params?: any) {
  return useQuery({
    queryKey: ['audit-logs', params],
    queryFn: () => apiClient.getAuditLogs(params),
  });
}
