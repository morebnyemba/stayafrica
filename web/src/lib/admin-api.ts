import { apiClient } from '@/services/api-client';
import { AdminStats, AuditLog, SystemConfig } from '@/types/admin-types';
import { User, Property, Booking, Payment } from '@/types';

export const adminApi = {
  // Dashboard Stats
  async getStats(): Promise<AdminStats> {
    const response = await apiClient.get('/api/v1/admin/stats/');
    return response.data;
  },

  // User Management
  async getUsers(params?: { 
    search?: string; 
    role?: string; 
    is_verified?: boolean;
    page?: number;
    per_page?: number;
  }): Promise<{ results: User[]; count: number }> {
    const response = await apiClient.get('/api/v1/admin/users/', { params });
    return response.data;
  },

  async getUserById(id: string): Promise<User> {
    const response = await apiClient.get(`/api/v1/admin/users/${id}/`);
    return response.data;
  },

  async updateUser(id: string, data: Partial<User>): Promise<User> {
    const response = await apiClient.patch(`/api/v1/admin/users/${id}/`, data);
    return response.data;
  },

  async verifyUser(id: string): Promise<User> {
    const response = await apiClient.post(`/api/v1/admin/users/${id}/verify/`);
    return response.data;
  },

  async suspendUser(id: string, reason: string): Promise<User> {
    const response = await apiClient.post(`/api/v1/admin/users/${id}/suspend/`, { reason });
    return response.data;
  },

  async deleteUser(id: string): Promise<void> {
    await apiClient.delete(`/api/v1/admin/users/${id}/`);
  },

  // Property Management
  async getProperties(params?: {
    search?: string;
    status?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: Property[]; count: number }> {
    const response = await apiClient.get('/api/v1/admin/properties/', { params });
    return response.data;
  },

  async approveProperty(id: string): Promise<Property> {
    const response = await apiClient.post(`/api/v1/admin/properties/${id}/approve/`);
    return response.data;
  },

  async rejectProperty(id: string, reason: string): Promise<Property> {
    const response = await apiClient.post(`/api/v1/admin/properties/${id}/reject/`, { reason });
    return response.data;
  },

  async deleteProperty(id: string): Promise<void> {
    await apiClient.delete(`/api/v1/admin/properties/${id}/`);
  },

  // Booking Management
  async getBookings(params?: {
    search?: string;
    status?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: Booking[]; count: number }> {
    const response = await apiClient.get('/api/v1/admin/bookings/', { params });
    return response.data;
  },

  async getBookingById(id: string): Promise<Booking> {
    const response = await apiClient.get(`/api/v1/admin/bookings/${id}/`);
    return response.data;
  },

  async cancelBooking(id: string, reason: string): Promise<Booking> {
    const response = await apiClient.post(`/api/v1/admin/bookings/${id}/cancel/`, { reason });
    return response.data;
  },

  // Payment Management
  async getPayments(params?: {
    search?: string;
    status?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: Payment[]; count: number }> {
    const response = await apiClient.get('/api/v1/admin/payments/', { params });
    return response.data;
  },

  async getPaymentById(id: string): Promise<Payment> {
    const response = await apiClient.get(`/api/v1/admin/payments/${id}/`);
    return response.data;
  },

  async refundPayment(id: string, amount?: number): Promise<Payment> {
    const response = await apiClient.post(`/api/v1/admin/payments/${id}/refund/`, { amount });
    return response.data;
  },

  // Audit Logs
  async getAuditLogs(params?: {
    user_id?: string;
    action?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: AuditLog[]; count: number }> {
    const response = await apiClient.get('/api/v1/admin/audit-logs/', { params });
    return response.data;
  },

  // System Settings
  async getSystemConfig(): Promise<SystemConfig> {
    const response = await apiClient.get('/api/v1/admin/system-config/');
    return response.data;
  },

  async updateSystemConfig(data: Partial<SystemConfig>): Promise<SystemConfig> {
    const response = await apiClient.patch('/api/v1/admin/system-config/', data);
    return response.data;
  },

  // Analytics
  async getRevenueAnalytics(params?: {
    period?: 'daily' | 'weekly' | 'monthly' | 'yearly';
    start_date?: string;
    end_date?: string;
  }): Promise<any> {
    const response = await apiClient.get('/api/v1/admin/analytics/revenue/', { params });
    return response.data;
  },

  async getBookingAnalytics(params?: {
    period?: 'daily' | 'weekly' | 'monthly' | 'yearly';
    start_date?: string;
    end_date?: string;
  }): Promise<any> {
    const response = await apiClient.get('/api/v1/admin/analytics/bookings/', { params });
    return response.data;
  },

  async getUserAnalytics(params?: {
    period?: 'daily' | 'weekly' | 'monthly' | 'yearly';
    start_date?: string;
    end_date?: string;
  }): Promise<any> {
    const response = await apiClient.get('/api/v1/admin/analytics/users/', { params });
    return response.data;
  },
};
