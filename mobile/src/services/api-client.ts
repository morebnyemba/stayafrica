import axios, { AxiosInstance, AxiosError } from 'axios';
import AsyncStorage from '@react-native-async-storage/async-storage';
import * as SecureStore from 'expo-secure-store';
import { Platform } from 'react-native';
import type {
  User,
  Property,
  Booking,
  Review,
  Message,
  Conversation,
  UpdateProfileRequest,
  CreateBookingRequest,
  CreatePropertyRequest,
  UpdatePropertyRequest,
  SubmitReviewRequest,
  WalletBalance,
  Transaction,
  HostEarnings,
  Wallet,
  WithdrawalRequest,
  WithdrawalResponse,
  VerificationData,
  VerificationResponse,
  ApiListResponse,
  PaymentInitiationRequest,
  PaymentInitiationResponse,
  PaymentStatusResponse,
  BankAccount,
  CreateBankAccountRequest,
  HostAnalytics,
  PropertyPerformance,
  BookingCalendarEvent,
  UpcomingCheckin,
  PendingAction,
  Experience,
  ExperienceCategory,
  ExperienceBooking,
  AppNotification,
  NotificationPreference,
  PushTokenData,
} from '@/types';

const API_VERSION = process.env.EXPO_PUBLIC_API_VERSION || 'v1';
const DEFAULT_API_BASE = 'https://api.stayafrica.app/api';
const API_BASE_URL = `${process.env.EXPO_PUBLIC_API_BASE_URL || DEFAULT_API_BASE}/${API_VERSION}`;

// Secure storage wrapper that falls back to AsyncStorage on web
const secureStorage = {
  async setItemAsync(key: string, value: string): Promise<void> {
    if (Platform.OS === 'web') {
      await AsyncStorage.setItem(key, value);
    } else {
      await SecureStore.setItemAsync(key, value);
    }
  },
  async getItemAsync(key: string): Promise<string | null> {
    if (Platform.OS === 'web') {
      return await AsyncStorage.getItem(key);
    } else {
      return await SecureStore.getItemAsync(key);
    }
  },
  async deleteItemAsync(key: string): Promise<void> {
    if (Platform.OS === 'web') {
      await AsyncStorage.removeItem(key);
    } else {
      await SecureStore.deleteItemAsync(key);
    }
  },
};

interface TokenResponse {
  access: string;
  refresh: string;
}

class APIClient {
  public client: AxiosInstance;
  private refreshPromise: Promise<string> | null = null;

  constructor() {
    this.client = axios.create({
      baseURL: API_BASE_URL,
      timeout: 30000,
      headers: {
        'Content-Type': 'application/json',
      },
    });

    // Request interceptor - add token (skip for auth endpoints)
    this.client.interceptors.request.use(
      async (config) => {
        const requestUrl = config.url || '';
        // Skip adding token for auth endpoints
        const isAuthEndpoint = requestUrl.includes('/auth/') || requestUrl.includes('/users/register');
        
        if (!isAuthEndpoint) {
          const token = await this.getAccessToken();
          if (token) {
            config.headers.Authorization = `Bearer ${token}`;
          }
        }
        return config;
      },
      (error) => Promise.reject(error)
    );

    // Response interceptor - handle 401
    this.client.interceptors.response.use(
      (response) => response,
      async (error: AxiosError) => {
        const originalRequest = error.config as any;
        const requestUrl = originalRequest?.url || '';
        
        // Skip token refresh for auth endpoints (login, register, refresh)
        const isAuthEndpoint = requestUrl.includes('/auth/') || requestUrl.includes('/users/register');

        if (error.response?.status === 401 && !originalRequest._retry && !isAuthEndpoint) {
          originalRequest._retry = true;

          try {
            const newToken = await this.refreshAccessToken();
            if (newToken) {
              this.client.defaults.headers.common.Authorization = `Bearer ${newToken}`;
              originalRequest.headers.Authorization = `Bearer ${newToken}`;
              return this.client(originalRequest);
            }
          } catch (refreshError) {
            // Refresh failed - clear tokens and redirect to login
            await this.clearTokens();
            return Promise.reject(error); // Return original error, not refresh error
          }
        }

        return Promise.reject(error);
      }
    );
  }

  async login(email: string, password: string): Promise<TokenResponse> {
    const response = await this.client.post('/auth/login/', {
      email,
      password,
    });

    // Check if 2FA is required
    if (response.data?.two_factor_required) {
      return response.data; // Return without saving tokens — caller must verify 2FA
    }

    const { access, refresh } = response.data || {};
    if (access && refresh) {
      await this.saveTokens(String(access), String(refresh));
    }
    return response.data;
  }

  async loginWith2FA(email: string, password: string, token: string): Promise<TokenResponse> {
    const response = await this.client.post('/auth/login/2fa/', {
      email,
      password,
      token,
    });
    const { access, refresh } = response.data || {};
    if (access && refresh) {
      await this.saveTokens(String(access), String(refresh));
    }
    return response.data;
  }

  async loginWithBackupCode(email: string, password: string, backupCode: string): Promise<TokenResponse> {
    const response = await this.client.post('/auth/login/2fa/', {
      email,
      password,
      backup_code: backupCode,
    });
    const { access, refresh } = response.data || {};
    if (access && refresh) {
      await this.saveTokens(String(access), String(refresh));
    }
    return response.data;
  }

  async register(userData: any): Promise<TokenResponse> {
    const response = await this.client.post('/users/register/', userData);
    const { access, refresh } = response.data || {};
    if (access && refresh) {
      await this.saveTokens(String(access), String(refresh));
    }
    return response.data;
  }

  async refreshAccessToken(): Promise<string> {
    if (this.refreshPromise) {
      return this.refreshPromise;
    }

    this.refreshPromise = (async () => {
      const refreshToken = await this.getRefreshToken();
      if (!refreshToken) {
        throw new Error('No refresh token available');
      }

      const response = await this.client.post('/auth/refresh/', {
        refresh: refreshToken,
      });

      const { access } = response.data || {};
      if (access) {
        await secureStorage.setItemAsync('accessToken', String(access));
      }
      return access;
    })();

    try {
      return await this.refreshPromise;
    } finally {
      this.refreshPromise = null;
    }
  }

  async saveTokens(accessToken: string, refreshToken: string): Promise<void> {
    // Ensure tokens are strings before saving
    if (accessToken && typeof accessToken === 'string') {
      await secureStorage.setItemAsync('accessToken', accessToken);
    }
    if (refreshToken && typeof refreshToken === 'string') {
      await secureStorage.setItemAsync('refreshToken', refreshToken);
    }
  }

  async getAccessToken(): Promise<string | null> {
    return await secureStorage.getItemAsync('accessToken');
  }

  async getRefreshToken(): Promise<string | null> {
    return await secureStorage.getItemAsync('refreshToken');
  }

  async clearTokens(): Promise<void> {
    await secureStorage.deleteItemAsync('accessToken');
    await secureStorage.deleteItemAsync('refreshToken');
  }

  async hasValidToken(): Promise<boolean> {
    const token = await this.getAccessToken();
    return !!token;
  }

  // Properties
  async getProperties(): Promise<ApiListResponse<Property>> {
    return (await this.client.get('/properties/')).data;
  }

  async getNearbyProperties(
    latitude: number,
    longitude: number,
    radius: number
  ): Promise<ApiListResponse<Property>> {
    return (
      await this.client.get('/properties/', {
        params: { latitude, longitude, radius },
      })
    ).data;
  }

  async getPropertyById(id: string): Promise<Property> {
    return (await this.client.get(`/properties/${id}/`)).data;
  }

  // Bookings
  async getBookings(status?: string): Promise<ApiListResponse<Booking>> {
    return (
      await this.client.get('/bookings/', {
        params: { status },
      })
    ).data;
  }

  async getBookingById(id: string): Promise<Booking> {
    return (await this.client.get(`/bookings/${id}/`)).data;
  }

  async createBooking(data: CreateBookingRequest): Promise<Booking> {
    return (await this.client.post('/bookings/', data)).data;
  }

  async cancelBooking(id: string): Promise<{ message: string; booking: Booking }> {
    return (await this.client.post(`/bookings/${id}/cancel/`)).data;
  }

  async confirmBooking(id: string): Promise<{ message: string; booking: Booking }> {
    return (await this.client.post(`/bookings/${id}/confirm/`)).data;
  }

  // User
  async getUserProfile(): Promise<User> {
    return (await this.client.get('/users/profile/')).data;
  }

  async updateUserProfile(data: UpdateProfileRequest): Promise<User> {
    return (await this.client.patch('/users/profile/', data)).data;
  }

  // Messages
  async getConversations(): Promise<any> {
    try {
      return (await this.client.get('/messaging/conversations/')).data;
    } catch (error: any) {
      if (error?.response?.status === 404) {
        return { results: [] };
      }
      throw error;
    }
  }

  async getConversationMessages(conversationId: string): Promise<any> {
    try {
      return (
        await this.client.get('/messaging/messages/', {
          params: { conversation: conversationId },
        })
      ).data;
    } catch (error: any) {
      if (error?.response?.status === 404) {
        return { results: [] };
      }
      throw error;
    }
  }

  async createConversation(propertyId: string): Promise<any> {
    try {
      return (
        await this.client.post('/messaging/conversations/', {
          property_id: propertyId,
        })
      ).data;
    } catch (error: any) {
      if (error?.response?.status === 404) {
        return null;
      }
      throw error;
    }
  }

  async sendMessage(conversationId: string, receiverId: string, message: string): Promise<any> {
    return (
      await this.client.post('/messaging/messages/', {
        conversation: Number(conversationId),
        receiver: Number(receiverId),
        text: message,
      })
    ).data;
  }

  async markConversationAsRead(conversationId: string): Promise<any> {
    return (
      await this.client.post(`/messaging/conversations/${conversationId}/mark_as_read/`)
    ).data;
  }

  async archiveConversation(conversationId: string): Promise<any> {
    return (
      await this.client.post(`/messaging/conversations/${conversationId}/archive/`)
    ).data;
  }

  async editMessage(messageId: string, text: string): Promise<any> {
    return (
      await this.client.put(`/messaging/messages/${messageId}/edit/`, { text })
    ).data;
  }

  async deleteMessage(messageId: string): Promise<any> {
    return (
      await this.client.delete(`/messaging/messages/${messageId}/`)
    ).data;
  }

  async getTotalUnreadCount(): Promise<{ unread_count: number }> {
    return (
      await this.client.get('/messaging/conversations/unread_count/')
    ).data;
  }

  // Reviews
  async submitReview(bookingId: string, data: SubmitReviewRequest): Promise<Review> {
    return (await this.client.post(`/reviews/`, { booking_id: bookingId, ...data })).data;
  }

  async getPropertyReviews(propertyId: string): Promise<ApiListResponse<Review>> {
    return (await this.client.get(`/reviews/`, { params: { property_id: propertyId } })).data;
  }

  // Wishlist
  async getWishlist(): Promise<ApiListResponse<Property>> {
    return (await this.client.get('/users/wishlist/')).data;
  }

  async addToWishlist(propertyId: string): Promise<{ message: string }> {
    return (await this.client.post('/users/wishlist/', { property_id: propertyId })).data;
  }

  async removeFromWishlist(propertyId: string): Promise<void> {
    return (await this.client.delete(`/users/wishlist/${propertyId}/`)).data;
  }

  // Host - Properties
  async getHostProperties(): Promise<ApiListResponse<Property>> {
    return (await this.client.get('/properties/host_properties/')).data;
  }

  async createProperty(data: CreatePropertyRequest): Promise<Property> {
    return (await this.client.post('/properties/', data)).data;
  }

  async updateProperty(id: string, data: UpdatePropertyRequest): Promise<Property> {
    return (await this.client.patch(`/properties/${id}/`, data)).data;
  }

  async deleteProperty(id: string): Promise<void> {
    return (await this.client.delete(`/properties/${id}/`)).data;
  }

  // Host - Analytics & Dashboard
  async getHostAnalytics(): Promise<HostAnalytics> {
    return (await this.client.get('/properties/host_analytics/')).data;
  }

  async getHostEarnings(period: string = 'month'): Promise<HostEarnings> {
    return (await this.client.get('/properties/host_earnings/', { params: { period } })).data;
  }

  async getPropertyPerformance(): Promise<ApiListResponse<PropertyPerformance>> {
    return (await this.client.get('/properties/property_performance/')).data;
  }

  async getBookingCalendar(propertyId: string, start?: string, end?: string): Promise<ApiListResponse<BookingCalendarEvent>> {
    return (await this.client.get(`/properties/${propertyId}/booking_calendar/`, {
      params: { start, end },
    })).data;
  }

  async getUpcomingCheckins(days: number = 7): Promise<ApiListResponse<UpcomingCheckin>> {
    return (await this.client.get('/properties/upcoming_checkins/', { params: { days } })).data;
  }

  async getPendingActions(): Promise<ApiListResponse<PendingAction>> {
    return (await this.client.get('/properties/pending_actions/')).data;
  }

  // Host - Bookings
  async getHostBookings(): Promise<ApiListResponse<Booking>> {
    return (await this.client.get('/bookings/')).data;
  }

  // Wallet/Payments
  async getWalletBalance(): Promise<WalletBalance> {
    return (await this.client.get('/wallets/my_wallet/')).data;
  }

  async getTransactions(): Promise<ApiListResponse<Transaction>> {
    return (await this.client.get('/transactions/')).data;
  }

  async withdrawFunds(amount: number): Promise<{ message: string; balance: number }> {
    return (await this.client.post('/payments/withdraw/', { amount })).data;
  }

  // Payment initiation for bookings
  async initiatePayment(bookingId: string, provider: string): Promise<PaymentInitiationResponse> {
    return (await this.client.post('/payments/initiate/', {
      booking_id: bookingId,
      provider,
    })).data;
  }

  async getPaymentStatus(paymentId: string): Promise<PaymentStatusResponse> {
    return (await this.client.get(`/payments/${paymentId}/`)).data;
  }

  // Payment history
  async getPaymentHistory(params?: any): Promise<ApiListResponse<Payment>> {
    return (await this.client.get('/payments/', { params })).data;
  }

  // Bank accounts
  async getBankAccounts(): Promise<ApiListResponse<BankAccount>> {
    return (await this.client.get('/bank-accounts/')).data;
  }

  async createBankAccount(data: CreateBankAccountRequest): Promise<BankAccount> {
    return (await this.client.post('/bank-accounts/', data)).data;
  }

  async updateBankAccount(accountId: string, data: Partial<CreateBankAccountRequest>): Promise<BankAccount> {
    return (await this.client.patch(`/bank-accounts/${accountId}/`, data)).data;
  }

  async deleteBankAccount(accountId: string): Promise<void> {
    return (await this.client.delete(`/bank-accounts/${accountId}/`)).data;
  }

  async setPrimaryBankAccount(accountId: string): Promise<BankAccount> {
    return (await this.client.post(`/bank-accounts/${accountId}/set_primary/`)).data;
  }

  // Wallet management
  async getMyWallet(): Promise<Wallet> {
    return (await this.client.get('/wallets/my_wallet/')).data;
  }

  async getWalletTransactions(walletId: string, params?: any): Promise<ApiListResponse<Transaction>> {
    return (await this.client.get(`/wallets/${walletId}/transactions/`, { params })).data;
  }

  // Withdrawals
  async getWithdrawals(params?: any): Promise<ApiListResponse<WithdrawalResponse>> {
    return (await this.client.get('/withdrawals/', { params })).data;
  }

  async initiateWithdrawal(data: WithdrawalRequest): Promise<WithdrawalResponse> {
    return (await this.client.post('/withdrawals/', data)).data;
  }

  // Two-Factor Authentication
  async get2FAStatus(): Promise<{
    two_factor_enabled: boolean;
    has_backup_codes: boolean;
    backup_codes_remaining: number;
  }> {
    return (await this.client.get('/2fa/status/')).data;
  }

  async setup2FA(): Promise<{
    secret: string;
    qr_code: string;
    backup_codes: string[];
    message: string;
  }> {
    return (await this.client.post('/2fa/setup/')).data;
  }

  async enable2FA(token: string): Promise<{ message: string; two_factor_enabled: boolean }> {
    return (await this.client.post('/2fa/enable/', { token })).data;
  }

  async disable2FA(password: string): Promise<{ message: string; two_factor_enabled: boolean }> {
    return (await this.client.post('/2fa/disable/', { password })).data;
  }

  async verify2FAToken(token: string): Promise<{ message: string; valid: boolean }> {
    return (await this.client.post('/2fa/verify/', { token })).data;
  }

  async regenerateBackupCodes(password: string): Promise<{ message: string; backup_codes: string[] }> {
    return (await this.client.post('/2fa/backup-codes/regenerate/', { password })).data;
  }

  // Experiences - Guest
  async getExperiences(params?: any): Promise<ApiListResponse<Experience>> {
    return (await this.client.get('/experiences/', { params })).data;
  }

  async getExperienceById(id: string): Promise<Experience> {
    return (await this.client.get(`/experiences/${id}/`)).data;
  }

  async getExperienceCategories(): Promise<ExperienceCategory[]> {
    const res = (await this.client.get('/categories/')).data;
    return Array.isArray(res) ? res : res.results ?? [];
  }

  async getExperienceAvailability(id: string): Promise<any> {
    return (await this.client.get(`/experiences/${id}/availability/`)).data;
  }

  async getNearbyExperiences(lat: number, lng: number, radius: number = 50): Promise<ApiListResponse<Experience>> {
    return (await this.client.get('/experiences/nearby/', { params: { lat, lng, radius } })).data;
  }

  async bookExperience(data: {
    experience: string;
    booking_date: string;
    num_participants: number;
    special_requests?: string;
  }): Promise<ExperienceBooking> {
    return (await this.client.post('/experience-bookings/', data)).data;
  }

  async getExperienceBookings(params?: any): Promise<ApiListResponse<ExperienceBooking>> {
    return (await this.client.get('/experience-bookings/', { params })).data;
  }

  async cancelExperienceBooking(id: number): Promise<ExperienceBooking> {
    return (await this.client.post(`/experience-bookings/${id}/cancel/`)).data;
  }

  // Experiences - Host
  async getHostExperiences(params?: any): Promise<ApiListResponse<Experience>> {
    return (await this.client.get('/experiences/mine/', { params })).data;
  }

  async createExperience(data: FormData | Record<string, any>): Promise<Experience> {
    const headers = data instanceof FormData ? { 'Content-Type': 'multipart/form-data' } : {};
    return (await this.client.post('/experiences/', data, { headers })).data;
  }

  async updateExperience(id: string, data: FormData | Record<string, any>): Promise<Experience> {
    const headers = data instanceof FormData ? { 'Content-Type': 'multipart/form-data' } : {};
    return (await this.client.patch(`/experiences/${id}/`, data, { headers })).data;
  }

  async deleteExperience(id: string): Promise<void> {
    return (await this.client.delete(`/experiences/${id}/`)).data;
  }

  async getHostExperienceBookings(params?: any): Promise<ApiListResponse<ExperienceBooking>> {
    return (await this.client.get('/experience-bookings/', { params: { ...params, role: 'host' } })).data;
  }

  async confirmExperienceBooking(id: number): Promise<ExperienceBooking> {
    return (await this.client.post(`/experience-bookings/${id}/confirm/`)).data;
  }

  async completeExperienceBooking(id: number): Promise<ExperienceBooking> {
    return (await this.client.post(`/experience-bookings/${id}/complete/`)).data;
  }

  // ── Notifications ──────────────────────────────────────────────────────

  async getNotifications(params?: { page?: number }): Promise<ApiListResponse<AppNotification>> {
    return (await this.client.get('/notifications/', { params })).data;
  }

  async getUnreadNotificationCount(): Promise<{ unread_count: number }> {
    return (await this.client.get('/notifications/unread_count/')).data;
  }

  async markNotificationRead(id: string): Promise<AppNotification> {
    return (await this.client.post(`/notifications/${id}/mark_read/`)).data;
  }

  async markAllNotificationsRead(): Promise<{ message: string }> {
    return (await this.client.post('/notifications/mark_all_read/')).data;
  }

  // ── Notification Preferences ────────────────────────────────────────────

  async getNotificationPreferences(): Promise<NotificationPreference> {
    return (await this.client.get('/preferences/')).data;
  }

  async updateNotificationPreferences(data: Partial<NotificationPreference>): Promise<NotificationPreference> {
    return (await this.client.put('/preferences/', data)).data;
  }

  // ── Push Tokens ────────────────────────────────────────────────────────

  async registerPushToken(data: PushTokenData): Promise<any> {
    return (await this.client.post('/tokens/', data)).data;
  }

  async deactivatePushToken(tokenId: string): Promise<any> {
    return (await this.client.post(`/tokens/${tokenId}/deactivate/`)).data;
  }

  // Generic request method
  get<T = any>(url: string, config?: any) {
    return this.client.get<T>(url, config);
  }

  post<T = any>(url: string, data?: any, config?: any) {
    return this.client.post<T>(url, data, config);
  }

  put<T = any>(url: string, data?: any, config?: any) {
    return this.client.put<T>(url, data, config);
  }

  patch<T = any>(url: string, data?: any, config?: any) {
    return this.client.patch<T>(url, data, config);
  }

  delete<T = any>(url: string, config?: any) {
    return this.client.delete<T>(url, config);
  }
}

export const apiClient = new APIClient();
