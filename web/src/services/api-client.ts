import axios, { AxiosInstance } from 'axios';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || (process.env.NODE_ENV === 'production' ? 'https://api.zimlegend.online' : 'http://localhost:8000');

// Remove any trailing /api/v1 from the base URL to prevent duplication
const cleanBaseUrl = API_BASE_URL.replace(/\/api\/v1\/?$/, '');

class ApiClient {
  private client: AxiosInstance;

  private assertId(id: string | undefined | null, label = 'ID') {
    if (!id) throw new Error(`${label} is required`);
    return id;
  }

  // Reply to review
  async replyToReview(reviewId: string, reply: string) {
    return this.client.post(`/reviews/${reviewId}/reply/`, { reply });
  }

  constructor() {
    this.client = axios.create({
      baseURL: `${cleanBaseUrl}/api/v1`,
      headers: {
        'Content-Type': 'application/json',
      },
    });

    // Request interceptor
    this.client.interceptors.request.use((config) => {
      const token = localStorage.getItem('access_token');
      if (token) {
        config.headers.Authorization = `Bearer ${token}`;
      }
      return config;
    });

    // Response interceptor
    this.client.interceptors.response.use(
      (response) => response,
      async (error) => {
        const originalRequest = error.config;

        if (error.response?.status === 401 && !originalRequest._retry) {
          originalRequest._retry = true;

          try {
            const refreshToken = localStorage.getItem('refresh_token');
            const response = await this.client.post('/auth/refresh/', {
              refresh: refreshToken,
            });

            const { access } = response.data;
            localStorage.setItem('access_token', access);

            originalRequest.headers.Authorization = `Bearer ${access}`;
            return this.client(originalRequest);
          } catch (refreshError) {
            localStorage.removeItem('access_token');
            localStorage.removeItem('refresh_token');
            window.location.href = '/login';
            return Promise.reject(refreshError);
          }
        }

        return Promise.reject(error);
      }
    );
  }

  // Properties
  async getProperties(params?: any) {
    return this.client.get('/properties/', { params });
  }

  async getAmenities() {
    return this.client.get('/amenities/');
  }

  async getPropertyById(id: string) {
    const safeId = this.assertId(id, 'Property ID');
    return this.client.get(`/properties/${safeId}/`);
  }

  async getPropertyDetails(id: string) {
    const safeId = this.assertId(id, 'Property ID');
    return this.getPropertyById(safeId);
  }

  async getUnavailableDates(propertyId: string, start?: string, end?: string) {
    const safeId = this.assertId(propertyId, 'Property ID');
    return this.client.get(`/properties/${safeId}/unavailable-dates/`, {
      params: { start, end },
    });
  }

  async getHostPropertyById(id: string) {
    const safeId = this.assertId(id, 'Property ID');
    return this.client.get(`/properties/${safeId}/host_detail/`);
  }

  async searchNearby(latitude: number, longitude: number, radiusKm: number = 10) {
    return this.client.get('/properties/search_nearby/', {
      params: { latitude, longitude, radius_km: radiusKm },
    });
  }

  async createProperty(data: any) {
    return this.client.post('/properties/', data);
  }

  async updateProperty(id: string, data: any) {
    const safeId = this.assertId(id, 'Property ID');
    return this.client.put(`/properties/${safeId}/`, data);
  }

  async uploadPropertyImages(propertyId: string, formData: FormData) {
    const safeId = this.assertId(propertyId, 'Property ID');
    return this.client.post(`/properties/${safeId}/upload_images/`, formData, {
      headers: {
        'Content-Type': 'multipart/form-data',
      },
    });
  }

  async deleteProperty(id: string) {
    const safeId = this.assertId(id, 'Property ID');
    return this.client.delete(`/properties/${safeId}/`);
  }

  // Bookings
  async getBookings(params?: any) {
    return this.client.get('/bookings/', { params });
  }

  async getBookingById(id: string) {
    const safeId = this.assertId(id, 'Booking ID');
    return this.client.get(`/bookings/${safeId}/`);
  }

  async createBooking(data: any) {
    return this.client.post('/bookings/', data);
  }

  async confirmBooking(id: string) {
    return this.client.post(`/bookings/${id}/confirm/`);
  }

  async cancelBooking(id: string) {
    return this.client.post(`/bookings/${id}/cancel/`);
  }

  // Payments
  async initiatePayment(bookingId: string, provider: string) {
    return this.client.post('/payments/initiate/', {
      booking_id: bookingId,
      provider,
    });
  }

  async getPaymentStatus(paymentId: string) {
    const safeId = this.assertId(paymentId, 'Payment ID');
    return this.client.get(`/payments/${safeId}/`);
  }

  async capturePaypalOrder(orderId: string) {
    return this.client.post('/payments/capture-paypal/', { order_id: orderId });
  }

  async getPaymentByGatewayRef(gatewayRef: string) {
    return this.client.get('/payments/', { params: { gateway_ref: gatewayRef } });
  }

  async lookupPaymentByRef(gatewayRef: string) {
    return this.client.get('/payments/lookup/', { params: { gateway_ref: gatewayRef } });
  }

  // Reviews
  async getReviews(params?: any) {
    return this.client.get('/reviews/', { params });
  }

  async createReview(data: any) {
    return this.client.post('/reviews/', data);
  }

  async getPropertyReviews(propertyId: string) {
    const safeId = this.assertId(propertyId, 'Property ID');
    return this.client.get(`/properties/${safeId}/reviews/`);
  }

  // Messages & Conversations
  async getConversations(params?: any) {
    return this.client.get('/messaging/conversations/', { params });
  }

  async getConversation(conversationId: string) {
    const safeId = this.assertId(conversationId, 'Conversation ID');
    return this.client.get(`/messaging/conversations/${safeId}/`);
  }

  async createConversation(data: {
    participants: number[];
    property?: number;
    booking?: number;
    subject?: string;
  }) {
    return this.client.post('/messaging/conversations/', data);
  }

  async markConversationAsRead(conversationId: string) {
    const safeId = this.assertId(conversationId, 'Conversation ID');
    return this.client.post(`/messaging/conversations/${safeId}/mark_as_read/`);
  }

  async archiveConversation(conversationId: string) {
    const safeId = this.assertId(conversationId, 'Conversation ID');
    return this.client.post(`/messaging/conversations/${safeId}/archive/`);
  }

  async getConversationMessages(conversationId: string, params?: any) {
    return this.client.get('/messaging/messages/', { 
      params: { ...params, conversation: conversationId } 
    });
  }

  async sendMessage(data: {
    conversation: number;
    receiver: number;
    text: string;
    message_type?: string;
    metadata?: any;
  }) {
    return this.client.post('/messaging/messages/', data);
  }

  async markMessageAsRead(messageId: string) {
    const safeId = this.assertId(messageId, 'Message ID');
    return this.client.post(`/messaging/messages/${safeId}/mark_as_read/`);
  }

  async editMessage(messageId: string, text: string) {
    const safeId = this.assertId(messageId, 'Message ID');
    return this.client.put(`/messaging/messages/${safeId}/edit/`, { text });
  }

  async deleteMessage(messageId: string) {
    const safeId = this.assertId(messageId, 'Message ID');
    return this.client.delete(`/messaging/messages/${safeId}/`);
  }

  async getUnreadMessagesCount() {
    return this.client.get('/messaging/messages/unread/');
  }

  async getTotalUnreadCount() {
    return this.client.get('/messaging/conversations/unread_count/');
  }

  async getMessageTemplates() {
    return this.client.get('/messaging/templates/');
  }

  async getMessages(params?: any) {
    return this.client.get('/messaging/messages/', { params });
  }

  async getUnreadCount() {
    return this.client.get('/messaging/conversations/unread_count/');
  }

  // Users
  async getUserProfile() {
    return this.client.get('/users/profile/');
  }

  async updateUserProfile(data: any) {
    return this.client.put('/users/profile/', data);
  }

  async changePassword(data: any) {
    return this.client.post('/users/change_password/', data);
  }

  // Wishlist
  async getSavedProperties() {
    return this.client.get('/properties/saved/');
  }

  async isPropertySaved(propertyId: string) {
    const safeId = this.assertId(propertyId, 'Property ID');
    const response = await this.getSavedProperties();
    const results = response.data?.results ?? response.data ?? [];

    return Array.isArray(results)
      ? results.some((item: any) => item?.property?.id === safeId || item?.property_id === safeId)
      : false;
  }

  async saveProperty(propertyId: string) {
    const safeId = this.assertId(propertyId, 'Property ID');
    return this.client.post('/properties/saved/', { property_id: safeId });
  }

  async unsaveProperty(propertyId: string) {
    const safeId = this.assertId(propertyId, 'Property ID');
    return this.client.delete(`/properties/saved/${safeId}/`);
  }

  // Reviews - User specific
  async getReviewsWritten(params?: any) {
    return this.client.get('/reviews/', { params: { ...params, written_by_user: true } });
  }

  async getReviewsReceived(params?: any) {
    return this.client.get('/reviews/', { params: { ...params, received_by_user: true } });
  }

  // Payments - User specific
  async getPaymentHistory(params?: any) {
    return this.client.get('/payments/', { params });
  }

  // Admin
  async getAdminStats() {
    return this.client.get('/admin/stats/dashboard/');
  }

  async getAuditLogs(params?: any) {
    return this.client.get('/admin/audit-logs/', { params });
  }

  // System Configuration
  async getFeeConfiguration() {
    return this.client.get('/admin/config/fees/');
  }

  // Get available payment providers based on user country
  async getAvailableProviders(country?: string) {
    return this.client.get('/payments/providers/', { params: { country } });
  }

  // Geocoding & Location Services
  async geocodeAddress(address: string, country?: string) {
    return this.client.post('/properties/geocode/', { address, country });
  }

  async reverseGeocode(latitude: number, longitude: number) {
    return this.client.post('/properties/reverse_geocode/', { latitude, longitude });
  }

  async getLocationSuggestions(query: string, country?: string, limit?: number) {
    return this.client.get('/properties/location_suggestions/', {
      params: { q: query, country, limit },
    });
  }

  // Host Dashboard & Analytics
  async getHostProperties() {
    return this.client.get('/properties/host_properties/');
  }

  async getHostAnalytics() {
    return this.client.get('/properties/host_analytics/');
  }

  async getHostEarnings(period: string = 'month') {
    return this.client.get('/properties/host_earnings/', { params: { period } });
  }

  async getPropertyPerformance() {
    return this.client.get('/properties/property_performance/');
  }

  async getBookingCalendar(propertyId: string, start?: string, end?: string) {
    const safeId = this.assertId(propertyId, 'Property ID');
    return this.client.get(`/properties/${safeId}/booking_calendar/`, {
      params: { start, end },
    });
  }

  async getUpcomingCheckins(days: number = 7) {
    return this.client.get('/properties/upcoming_checkins/', { params: { days } });
  }

  async getPendingActions() {
    return this.client.get('/properties/pending_actions/');
  }

  // Host Booking Management
  async getHostBookings(params?: any) {
    return this.client.get('/bookings/', { params });
  }

  // Wallet Management
  async getMyWallet() {
    return this.client.get('/payments/wallets/my_wallet/');
  }

  async getWalletBalance(walletId: string) {
    const safeId = this.assertId(walletId, 'Wallet ID');
    return this.client.get(`/payments/wallets/${safeId}/balance/`);
  }

  async getWalletTransactions(walletId: string, params?: any) {
    const safeId = this.assertId(walletId, 'Wallet ID');
    return this.client.get(`/payments/wallets/${safeId}/transactions/`, { params });
  }

  // Bank Account Management
  async getBankAccounts() {
    return this.client.get('/payments/bank-accounts/');
  }

  async createBankAccount(data: {
    bank_name: string;
    account_name: string;
    account_number: string;
    branch_code?: string;
    country?: string;
    is_primary?: boolean;
  }) {
    return this.client.post('/payments/bank-accounts/', data);
  }

  async updateBankAccount(accountId: string, data: any) {
    const safeId = this.assertId(accountId, 'Bank Account ID');
    return this.client.patch(`/payments/bank-accounts/${safeId}/`, data);
  }

  async deleteBankAccount(accountId: string) {
    const safeId = this.assertId(accountId, 'Bank Account ID');
    return this.client.delete(`/payments/bank-accounts/${safeId}/`);
  }

  async setPrimaryBankAccount(accountId: string) {
    const safeId = this.assertId(accountId, 'Bank Account ID');
    return this.client.post(`/payments/bank-accounts/${safeId}/set_primary/`);
  }

  // Withdrawal Management
  async getWithdrawals(params?: any) {
    return this.client.get('/payments/withdrawals/', { params });
  }

  async initiateWithdrawal(data: {
    wallet: string;
    bank_account: string;
    amount: string;
    currency: string;
    notes?: string;
  }) {
    return this.client.post('/payments/withdrawals/', data);
  }

  // Transaction History
  async getTransactions(params?: any) {
    return this.client.get('/payments/transactions/', { params });
  }

  // Experiences
  async getExperiences(params?: any) {
    return this.client.get('/experiences/', { params });
  }

  async getExperienceById(id: string) {
    const safeId = this.assertId(id, 'Experience ID');
    return this.client.get(`/experiences/${safeId}/`);
  }

  async getExperienceCategories() {
    return this.client.get('/categories/');
  }

  async getNearbyExperiences(latitude: number, longitude: number, radiusKm: number = 50) {
    return this.client.get('/experiences/nearby/', {
      params: { lat: latitude, lng: longitude, radius: radiusKm },
    });
  }

  async createExperience(data: FormData | Record<string, any>) {
    return this.client.post('/experiences/', data, {
      headers: data instanceof FormData ? { 'Content-Type': 'multipart/form-data' } : {},
    });
  }

  async updateExperience(id: string, data: FormData | Record<string, any>) {
    const safeId = this.assertId(id, 'Experience ID');
    return this.client.patch(`/experiences/${safeId}/`, data, {
      headers: data instanceof FormData ? { 'Content-Type': 'multipart/form-data' } : {},
    });
  }

  async deleteExperience(id: string) {
    const safeId = this.assertId(id, 'Experience ID');
    return this.client.delete(`/experiences/${safeId}/`);
  }

  async getExperienceAvailability(id: string) {
    const safeId = this.assertId(id, 'Experience ID');
    return this.client.get(`/experiences/${safeId}/availability/`);
  }

  async getHostExperienceBookings(params?: any) {
    // Host sees bookings for their own experiences
    return this.client.get('/experience-bookings/', { params: { ...params, role: 'host' } });
  }

  async updateExperienceBookingStatus(bookingId: number, status: string) {
    return this.client.patch(`/experience-bookings/${bookingId}/`, { status });
  }

  async confirmExperienceBooking(bookingId: number) {
    return this.client.post(`/experience-bookings/${bookingId}/confirm/`);
  }

  async completeExperienceBooking(bookingId: number) {
    return this.client.post(`/experience-bookings/${bookingId}/complete/`);
  }

  async cancelExperienceBooking(bookingId: number) {
    return this.client.post(`/experience-bookings/${bookingId}/cancel/`);
  }

  async getHostExperiences(params?: any) {
    return this.client.get('/experiences/mine/', { params });
  }

  async createExperienceBooking(data: any) {
    return this.client.post('/experience-bookings/', data);
  }

  async getExperienceBookings(params?: any) {
    return this.client.get('/experience-bookings/', { params });
  }

  // Review voting
  async voteReview(reviewId: string, voteType: 'helpful' | 'unhelpful') {
    return this.client.post(`/reviews/${reviewId}/vote/`, { vote_type: voteType });
  }

  async respondToReview(reviewId: string, response: string) {
    return this.client.post(`/reviews/${reviewId}/respond/`, { response });
  }

  // User Preferences
  async getUserPreferences() {
    return this.client.get('/preferences/my_preferences/');
  }

  async updateUserPreferences(data: any) {
    return this.client.post('/preferences/update_preferences/', data);
  }

  async updateUserLocation(latitude: number, longitude: number) {
    return this.client.post('/preferences/update_location/', {
      latitude,
      longitude,
    });
  }

  // User Property Interactions
  async trackPropertyView(propertyId: string, durationSeconds?: number) {
    return this.client.post('/interactions/track_view/', {
      property_id: propertyId,
      duration_seconds: durationSeconds,
    });
  }

  async getUserInteractions() {
    return this.client.get('/interactions/');
  }

  // Flexible Date Search
  async flexibleSearch(params: {
    check_in: string;
    check_out: string;
    flexibility?: 'exact' | 'flexible_days' | 'weekend' | 'month';
    days?: number;
    property_type?: string;
    min_price?: number;
    max_price?: number;
    guests?: number;
  }) {
    return this.client.get('/properties/flexible_search/', { params });
  }

  // Instant Booking
  async getInstantBookingInfo(propertyId: string) {
    const safeId = this.assertId(propertyId, 'Property ID');
    return this.client.get(`/properties/${safeId}/instant_booking_info/`);
  }

  async toggleInstantBooking(propertyId: string, data: {
    enabled: boolean;
    requirements?: {
      require_verified?: boolean;
      min_reviews?: number;
      min_rating?: number;
      require_completed_bookings?: boolean;
      require_payment_method?: boolean;
    };
  }) {
    const safeId = this.assertId(propertyId, 'Property ID');
    return this.client.post(`/properties/${safeId}/toggle_instant_booking/`, data);
  }

  // Analytics
  async get(url: string, config?: any) {
    return this.client.get(url, config);
  }

  async post(url: string, data?: any, config?: any) {
    return this.client.post(url, data, config);
  }

  async put(url: string, data?: any, config?: any) {
    return this.client.put(url, data, config);
  }

  async patch(url: string, data?: any, config?: any) {
    return this.client.patch(url, data, config);
  }

  async delete(url: string, config?: any) {
    return this.client.delete(url, config);
  }
}

export const apiClient = new ApiClient();
