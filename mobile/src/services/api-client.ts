import axios, { AxiosInstance, AxiosError } from 'axios';
import AsyncStorage from '@react-native-async-storage/async-storage';
import * as SecureStore from 'expo-secure-store';
import { Platform } from 'react-native';
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
  private client: AxiosInstance;
  private refreshPromise: Promise<string> | null = null;

  constructor() {
    this.client = axios.create({
      baseURL: API_BASE_URL,
      timeout: 30000,
      headers: {
        'Content-Type': 'application/json',
      },
    });

    // Request interceptor - add token
    this.client.interceptors.request.use(
      async (config) => {
        const token = await this.getAccessToken();
        if (token) {
          config.headers.Authorization = `Bearer ${token}`;
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

        if (error.response?.status === 401 && !originalRequest._retry) {
          originalRequest._retry = true;

          try {
            const newToken = await this.refreshAccessToken();
            this.client.defaults.headers.common.Authorization = `Bearer ${newToken}`;
            originalRequest.headers.Authorization = `Bearer ${newToken}`;
            return this.client(originalRequest);
          } catch (refreshError) {
            // Refresh failed - clear tokens and redirect to login
            await this.clearTokens();
            return Promise.reject(refreshError);
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
    const { access, refresh } = response.data;
    await this.saveTokens(access, refresh);
    return response.data;
  }

  async register(userData: any): Promise<TokenResponse> {
    const response = await this.client.post('/users/register/', userData);
    const { access, refresh } = response.data;
    await this.saveTokens(access, refresh);
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

      const { access } = response.data;
      await secureStorage.setItemAsync('accessToken', access);
      return access;
    })();

    try {
      return await this.refreshPromise;
    } finally {
      this.refreshPromise = null;
    }
  }

  async saveTokens(accessToken: string, refreshToken: string): Promise<void> {
    await secureStorage.setItemAsync('accessToken', accessToken);
    await secureStorage.setItemAsync('refreshToken', refreshToken);
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
  async getProperties(): Promise<any> {
    return (await this.client.get('/properties/')).data;
  }

  async getNearbyProperties(
    latitude: number,
    longitude: number,
    radius: number
  ): Promise<any> {
    return (
      await this.client.get('/properties/', {
        params: { latitude, longitude, radius },
      })
    ).data;
  }

  async getPropertyById(id: string): Promise<any> {
    return (await this.client.get(`/properties/${id}/`)).data;
  }

  // Bookings
  async getBookings(status?: string): Promise<any> {
    return (
      await this.client.get('/bookings/', {
        params: { status },
      })
    ).data;
  }

  async createBooking(data: any): Promise<any> {
    return (await this.client.post('/bookings/', data)).data;
  }

  async cancelBooking(id: string): Promise<any> {
    return (await this.client.post(`/bookings/${id}/cancel/`)).data;
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
    return (await this.client.get('/messaging/conversations/')).data;
  }

  async getConversationMessages(conversationId: string): Promise<any> {
    return (
      await this.client.get(`/messaging/conversations/${conversationId}/messages/`)
    ).data;
  }

  async sendMessage(conversationId: string, message: string): Promise<any> {
    return (
      await this.client.post(`/messaging/conversations/${conversationId}/messages/`, {
        content: message,
      })
    ).data;
  }

  // Reviews
  async submitReview(bookingId: string, data: any): Promise<any> {
    return (await this.client.post(`/reviews/`, { booking_id: bookingId, ...data })).data;
  }

  async getPropertyReviews(propertyId: string): Promise<{ results: Review[] }> {
    return (await this.client.get(`/reviews/`, { params: { property_id: propertyId } })).data;
  }

  // Wishlist
  async getWishlist(): Promise<{ results: Property[] }> {
    return (await this.client.get('/users/wishlist/')).data;
  }

  async addToWishlist(propertyId: string): Promise<{ message: string }> {
    return (await this.client.post('/users/wishlist/', { property_id: propertyId })).data;
  }

  async removeFromWishlist(propertyId: string): Promise<void> {
    return (await this.client.delete(`/users/wishlist/${propertyId}/`)).data;
  }

  // Host - Properties
  async getHostProperties(): Promise<{ results: Property[] }> {
    return (await this.client.get('/properties/host/')).data;
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

  // Host - Bookings
  async getHostBookings(): Promise<{ results: Booking[] }> {
    return (await this.client.get('/bookings/host/')).data;
  }

  // Host - Earnings
  async getHostEarnings(): Promise<HostEarnings> {
    return (await this.client.get('/payments/earnings/')).data;
  }

  // Wallet/Payments
  async getWalletBalance(): Promise<WalletBalance> {
    return (await this.client.get('/payments/wallet/')).data;
  }

  async getTransactions(): Promise<{ results: Transaction[] }> {
    return (await this.client.get('/payments/transactions/')).data;
  }

  async withdrawFunds(amount: number): Promise<{ message: string; balance: number }> {
    return (await this.client.post('/payments/withdraw/', { amount })).data;
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
