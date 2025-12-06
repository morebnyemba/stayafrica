import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';

// Properties
export function useNearbyProperties(latitude: number, longitude: number, radius: number) {
  return useQuery({
    queryKey: ['properties', 'nearby', latitude, longitude, radius],
    queryFn: () => apiClient.getNearbyProperties(latitude, longitude, radius),
    enabled: !!latitude && !!longitude,
  });
}

export function usePropertyById(id: string) {
  return useQuery({
    queryKey: ['properties', id],
    queryFn: () => apiClient.getPropertyById(id),
    enabled: !!id,
  });
}

// Bookings
export function useBookings(status?: string) {
  return useQuery({
    queryKey: ['bookings', status],
    queryFn: () => apiClient.getBookings(status),
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

export function useCancelBooking() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (id: string) => apiClient.cancelBooking(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bookings'] });
    },
  });
}

// User
export function useUserProfile() {
  return useQuery({
    queryKey: ['user', 'profile'],
    queryFn: () => apiClient.getUserProfile(),
  });
}

// Messages
export function useConversations() {
  return useQuery({
    queryKey: ['conversations'],
    queryFn: () => apiClient.getConversations(),
  });
}

export function useSendMessage() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: ({ conversationId, message }: { conversationId: string; message: string }) =>
      apiClient.sendMessage(conversationId, message),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['conversations'] });
    },
  });
}

// Reviews
export function useSubmitReview() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: ({ bookingId, data }: { bookingId: string; data: any }) =>
      apiClient.submitReview(bookingId, data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bookings'] });
    },
  });
}
