import axios, { AxiosInstance } from 'axios';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

class ApiClient {
  private client: AxiosInstance;

  constructor() {
    this.client = axios.create({
      baseURL: `${API_BASE_URL}/api/v1`,
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

  async getPropertyById(id: string) {
    return this.client.get(`/properties/${id}/`);
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
    return this.client.put(`/properties/${id}/`, data);
  }

  async deleteProperty(id: string) {
    return this.client.delete(`/properties/${id}/`);
  }

  // Bookings
  async getBookings(params?: any) {
    return this.client.get('/bookings/', { params });
  }

  async getBookingById(id: string) {
    return this.client.get(`/bookings/${id}/`);
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
    return this.client.get(`/payments/${paymentId}/`);
  }

  // Reviews
  async getReviews(params?: any) {
    return this.client.get('/reviews/', { params });
  }

  async createReview(data: any) {
    return this.client.post('/reviews/', data);
  }

  // Messages
  async getMessages(params?: any) {
    return this.client.get('/messages/', { params });
  }

  async sendMessage(data: any) {
    return this.client.post('/messages/', data);
  }

  async getConversations() {
    return this.client.get('/messages/conversations/');
  }

  async getUnreadCount() {
    return this.client.get('/messages/unread/');
  }

  async markMessageAsRead(messageId: string) {
    return this.client.patch(`/messages/${messageId}/`, { is_read: true });
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

  async saveProperty(propertyId: string) {
    return this.client.post('/properties/saved/', { property_id: propertyId });
  }

  async unsaveProperty(propertyId: string) {
    return this.client.delete(`/properties/saved/${propertyId}/`);
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
    return this.client.get(`/properties/${propertyId}/booking_calendar/`, {
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

  // Reply to review
  async replyToReview(reviewId: string, reply: string) {
    return this.client.post(`/reviews/${reviewId}/reply/`, { reply });
  }
}

export const apiClient = new ApiClient();
