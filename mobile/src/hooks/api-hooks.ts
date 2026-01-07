import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import type {
  User,
  Property,
  Booking,
  Review,
  UpdateProfileRequest,
  CreateBookingRequest,
  CreatePropertyRequest,
  UpdatePropertyRequest,
  SubmitReviewRequest,
  WalletBalance,
  Transaction,
  HostEarnings,
} from '@/types';

// Properties
export function useProperties() {
  return useQuery({
    queryKey: ['properties'],
    queryFn: () => apiClient.getProperties(),
  });
}

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
  return useQuery<{ results: Booking[] }>({
    queryKey: ['bookings', status],
    queryFn: () => apiClient.getBookings(status),
  });
}

export function useCreateBooking() {
  const queryClient = useQueryClient();
  return useMutation<Booking, Error, CreateBookingRequest>({
    mutationFn: (data: CreateBookingRequest) => apiClient.createBooking(data),
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
  return useQuery<User>({
    queryKey: ['user', 'profile'],
    queryFn: () => apiClient.getUserProfile(),
  });
}

export function useUpdateProfile() {
  const queryClient = useQueryClient();
  return useMutation<User, Error, UpdateProfileRequest>({
    mutationFn: (data: UpdateProfileRequest) => apiClient.updateUserProfile(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['user', 'profile'] });
    },
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
  return useMutation<Review, Error, SubmitReviewRequest>({
    mutationFn: (data: SubmitReviewRequest) =>
      apiClient.submitReview(data.booking_id, data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bookings'] });
    },
  });
}

export function usePropertyReviews(propertyId: string) {
  return useQuery<{ results: Review[] }>({
    queryKey: ['reviews', 'property', propertyId],
    queryFn: () => apiClient.getPropertyReviews(propertyId),
    enabled: !!propertyId,
  });
}

// Wishlist
export function useWishlist() {
  return useQuery<{ results: Property[] }>({
    queryKey: ['wishlist'],
    queryFn: () => apiClient.getWishlist(),
  });
}

export function useAddToWishlist() {
  const queryClient = useQueryClient();
  return useMutation<{ message: string }, Error, string>({
    mutationFn: (propertyId: string) => apiClient.addToWishlist(propertyId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['wishlist'] });
    },
  });
}

export function useRemoveFromWishlist() {
  const queryClient = useQueryClient();
  return useMutation<void, Error, string>({
    mutationFn: (propertyId: string) => apiClient.removeFromWishlist(propertyId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['wishlist'] });
    },
  });
}

// Host - Properties
export function useHostProperties() {
  return useQuery<{ results: Property[] }>({
    queryKey: ['host', 'properties'],
    queryFn: () => apiClient.getHostProperties(),
  });
}

export function useCreateProperty() {
  const queryClient = useQueryClient();
  return useMutation<Property, Error, CreatePropertyRequest>({
    mutationFn: (data: CreatePropertyRequest) => apiClient.createProperty(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host', 'properties'] });
    },
  });
}

export function useUpdateProperty() {
  const queryClient = useQueryClient();
  return useMutation<Property, Error, { id: string; data: UpdatePropertyRequest }>({
    mutationFn: ({ id, data }: { id: string; data: UpdatePropertyRequest }) => 
      apiClient.updateProperty(id, data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host', 'properties'] });
    },
  });
}

export function useDeleteProperty() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (id: string) => apiClient.deleteProperty(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host', 'properties'] });
    },
  });
}

// Host - Bookings
export function useHostBookings() {
  return useQuery<{ results: Booking[] }>({
    queryKey: ['host', 'bookings'],
    queryFn: () => apiClient.getHostBookings(),
  });
}

// Host - Earnings
export function useHostEarnings() {
  return useQuery<HostEarnings>({
    queryKey: ['host', 'earnings'],
    queryFn: () => apiClient.getHostEarnings(),
  });
}

// Wallet/Payments
export function useWalletBalance() {
  return useQuery<WalletBalance>({
    queryKey: ['wallet', 'balance'],
    queryFn: () => apiClient.getWalletBalance(),
  });
}

export function useTransactions() {
  return useQuery<{ results: Transaction[] }>({
    queryKey: ['wallet', 'transactions'],
    queryFn: () => apiClient.getTransactions(),
  });
}

export function useWithdrawFunds() {
  const queryClient = useQueryClient();
  return useMutation<void, Error, number>({
    mutationFn: (amount: number) => apiClient.withdrawFunds(amount),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['wallet'] });
    },
  });
}
