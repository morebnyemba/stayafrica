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
  Experience,
  ExperienceBooking,
  AppNotification,
  NotificationPreference,
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

export function useBookingById(id: string) {
  return useQuery<Booking>({
    queryKey: ['booking', id],
    queryFn: () => apiClient.getBookingById(id),
    enabled: !!id,
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
      queryClient.invalidateQueries({ queryKey: ['host', 'bookings'] });
    },
  });
}

export function useConfirmBooking() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (id: string) => apiClient.confirmBooking(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['bookings'] });
      queryClient.invalidateQueries({ queryKey: ['host', 'bookings'] });
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
    refetchInterval: 10000,
  });
}

export function useCreateConversation() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (propertyId: string) => apiClient.createConversation(propertyId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['conversations'] });
    },
  });
}

export function useConversationMessages(conversationId: string) {
  return useQuery({
    queryKey: ['conversations', conversationId, 'messages'],
    queryFn: () => apiClient.getConversationMessages(conversationId),
    enabled: !!conversationId,
    refetchInterval: 5000,
  });
}

export function useSendMessage() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: ({ conversationId, receiverId, message }: { conversationId: string; receiverId: string; message: string }) =>
      apiClient.sendMessage(conversationId, receiverId, message),
    onSuccess: (_, variables) => {
      queryClient.invalidateQueries({ queryKey: ['conversations'] });
      if (variables?.conversationId) {
        queryClient.invalidateQueries({ queryKey: ['conversations', variables.conversationId, 'messages'] });
      }
    },
  });
}

export function useMarkConversationAsRead() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (conversationId: string) => apiClient.markConversationAsRead(conversationId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['conversations'] });
      queryClient.invalidateQueries({ queryKey: ['unread-count'] });
    },
  });
}

export function useArchiveConversation() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (conversationId: string) => apiClient.archiveConversation(conversationId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['conversations'] });
    },
  });
}

export function useEditMessage() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: ({ messageId, text }: { messageId: string; text: string }) =>
      apiClient.editMessage(messageId, text),
    onSuccess: () => {
      // Invalidate all conversations since the API doesn't return conversation ID in the response
      // and we need to update both the conversation list and message details
      queryClient.invalidateQueries({ queryKey: ['conversations'] });
    },
  });
}

export function useDeleteMessage() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (messageId: string) => apiClient.deleteMessage(messageId),
    onSuccess: () => {
      // Invalidate all conversations since the API doesn't return conversation ID in the response
      // and we need to update both the conversation list and message details
      queryClient.invalidateQueries({ queryKey: ['conversations'] });
    },
  });
}

export function useUnreadCount() {
  return useQuery({
    queryKey: ['unread-count'],
    queryFn: () => apiClient.getTotalUnreadCount(),
    refetchInterval: 15000,
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
export function useHostEarnings(period: string = 'month') {
  return useQuery<{ earnings: any[] }>({
    queryKey: ['host', 'earnings', period],
    queryFn: () => apiClient.getHostEarnings(period),
  });
}

// Host - Analytics & Dashboard
export function useHostAnalytics() {
  return useQuery({
    queryKey: ['host', 'analytics'],
    queryFn: () => apiClient.getHostAnalytics(),
  });
}

export function usePropertyPerformance() {
  return useQuery({
    queryKey: ['host', 'property-performance'],
    queryFn: () => apiClient.getPropertyPerformance(),
  });
}

export function useUpcomingCheckins(days: number = 7) {
  return useQuery({
    queryKey: ['host', 'checkins', days],
    queryFn: () => apiClient.getUpcomingCheckins(days),
  });
}

export function usePendingActions() {
  return useQuery({
    queryKey: ['host', 'pending-actions'],
    queryFn: () => apiClient.getPendingActions(),
  });
}

export function useBookingCalendar(propertyId: string, start?: string, end?: string) {
  return useQuery({
    queryKey: ['host', 'calendar', propertyId, start, end],
    queryFn: () => apiClient.getBookingCalendar(propertyId, start, end),
    enabled: !!propertyId,
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
  return useMutation<{ message: string; balance: number }, Error, number>({
    mutationFn: (amount: number) => apiClient.withdrawFunds(amount),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['wallet'] });
    },
  });
}

// Experiences - Guest
export function useExperiences(params?: any) {
  return useQuery({
    queryKey: ['experiences', params],
    queryFn: () => apiClient.getExperiences(params),
  });
}

export function useExperienceById(id: string) {
  return useQuery({
    queryKey: ['experiences', id],
    queryFn: () => apiClient.getExperienceById(id),
    enabled: !!id,
  });
}

export function useExperienceCategories() {
  return useQuery({
    queryKey: ['experience-categories'],
    queryFn: () => apiClient.getExperienceCategories(),
  });
}

export function useNearbyExperiences(lat: number, lng: number, radius: number = 50) {
  return useQuery({
    queryKey: ['experiences', 'nearby', lat, lng, radius],
    queryFn: () => apiClient.getNearbyExperiences(lat, lng, radius),
    enabled: !!lat && !!lng,
  });
}

export function useExperienceBookings(params?: any) {
  return useQuery({
    queryKey: ['experience-bookings', params],
    queryFn: () => apiClient.getExperienceBookings(params),
  });
}

export function useBookExperience() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (data: { experience: string; booking_date: string; num_participants: number; special_requests?: string }) =>
      apiClient.bookExperience(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['experience-bookings'] });
    },
  });
}

export function useCancelExperienceBooking() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (id: number) => apiClient.cancelExperienceBooking(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['experience-bookings'] });
    },
  });
}

// Experiences - Host
export function useHostExperiences(params?: any) {
  return useQuery({
    queryKey: ['host-experiences', params],
    queryFn: () => apiClient.getHostExperiences(params),
  });
}

export function useHostExperienceBookings(params?: any) {
  return useQuery({
    queryKey: ['host-experience-bookings', params],
    queryFn: () => apiClient.getHostExperienceBookings(params),
  });
}

export function useCreateExperience() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (data: FormData | Record<string, any>) => apiClient.createExperience(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host-experiences'] });
    },
  });
}

export function useUpdateExperience() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: ({ id, data }: { id: string; data: FormData | Record<string, any> }) =>
      apiClient.updateExperience(id, data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host-experiences'] });
      queryClient.invalidateQueries({ queryKey: ['experiences'] });
    },
  });
}

export function useDeleteExperience() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (id: string) => apiClient.deleteExperience(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host-experiences'] });
    },
  });
}

export function useConfirmExperienceBooking() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (id: number) => apiClient.confirmExperienceBooking(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host-experience-bookings'] });
    },
  });
}

export function useCompleteExperienceBooking() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (id: number) => apiClient.completeExperienceBooking(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['host-experience-bookings'] });
    },
  });
}

// ── Fee Configuration ──────────────────────────────────────────────────

export interface FeeConfiguration {
  commission_rate: number;
  service_fee: number;
  default_currency: string;
  max_advance_booking_days: number;
  max_stay_duration_days: number;
  review_window_days: number;
}

export interface BookingCostCalculation {
  basePrice: number;
  serviceFee: number;
  commissionFee: number;
  commissionRate: number;
  cleaningFee: number;
  total: number;
}

export function useFeeConfiguration() {
  return useQuery<FeeConfiguration>({
    queryKey: ['fees'],
    queryFn: () => apiClient.getFeeConfiguration(),
    staleTime: 1000 * 60 * 60, // Cache for 1 hour
  });
}

export function useUnavailableDates(propertyId: string) {
  return useQuery<{ unavailable_dates: string[] }>({
    queryKey: ['unavailable-dates', propertyId],
    queryFn: () => apiClient.getUnavailableDates(propertyId),
    enabled: !!propertyId,
  });
}

export function calculateBookingCost(
  pricePerNight: number,
  nights: number,
  feeConfig: FeeConfiguration,
  cleaningFee: number = 0
): BookingCostCalculation {
  const basePrice = nights * pricePerNight;
  const serviceFee = parseFloat(feeConfig.service_fee.toString());
  const commissionRate = parseFloat(feeConfig.commission_rate.toString());
  const commissionFee = (basePrice + serviceFee) * commissionRate;
  const total = basePrice + serviceFee + commissionFee + cleaningFee;

  return {
    basePrice,
    serviceFee,
    commissionFee,
    commissionRate,
    cleaningFee,
    total,
  };
}

// ── Notifications ──────────────────────────────────────────────────────

export function useNotifications() {
  return useQuery<{ results: AppNotification[] }>({
    queryKey: ['notifications'],
    queryFn: () => apiClient.getNotifications(),
    refetchInterval: 30000, // Poll every 30 seconds
  });
}

export function useUnreadNotificationCount() {
  return useQuery<{ unread_count: number }>({
    queryKey: ['notification-unread-count'],
    queryFn: () => apiClient.getUnreadNotificationCount(),
    refetchInterval: 30000,
  });
}

export function useMarkNotificationRead() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (id: string) => apiClient.markNotificationRead(id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['notifications'] });
      queryClient.invalidateQueries({ queryKey: ['notification-unread-count'] });
    },
  });
}

export function useMarkAllNotificationsRead() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => apiClient.markAllNotificationsRead(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['notifications'] });
      queryClient.invalidateQueries({ queryKey: ['notification-unread-count'] });
    },
  });
}

// ── Notification Preferences ────────────────────────────────────────────

export function useNotificationPreferences() {
  return useQuery<NotificationPreference>({
    queryKey: ['notification-preferences'],
    queryFn: () => apiClient.getNotificationPreferences(),
  });
}

export function useUpdateNotificationPreferences() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (data: Partial<NotificationPreference>) =>
      apiClient.updateNotificationPreferences(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['notification-preferences'] });
    },
  });
}
