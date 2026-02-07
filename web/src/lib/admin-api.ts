import { apiClient } from '@/services/api-client';
import { AdminStats, AuditLog, SystemConfig, IdentityVerification, VerificationStats } from '@/types/admin-types';
import { User, Property, Booking, Payment } from '@/types';

export const adminApi = {
  // Dashboard Stats - uses actual backend endpoint
  async getStats(): Promise<AdminStats> {
    const response = await apiClient.get('/admin/stats/dashboard/');
    return response.data;
  },

  // User Management - uses regular users endpoint (admin can access all)
  async getUsers(params?: { 
    search?: string; 
    role?: string; 
    is_verified?: boolean;
    page?: number;
    per_page?: number;
  }): Promise<{ results: User[]; count: number }> {
    const response = await apiClient.get('/users/', { params });
    return response.data;
  },

  async getUserById(id: string): Promise<User> {
    const response = await apiClient.get(`/users/${id}/`);
    return response.data;
  },

  async updateUser(id: string, data: Partial<User>): Promise<User> {
    const response = await apiClient.put(`/users/${id}/`, data);
    return response.data;
  },

  async verifyUser(id: string): Promise<User> {
    // Verify user by updating the is_verified field
    const response = await apiClient.patch(`/users/${id}/`, { is_verified: true });
    return response.data;
  },

  async suspendUser(id: string, reason: string): Promise<User> {
    const response = await apiClient.post(`/users/${id}/suspend/`, { reason });
    return response.data;
  },

  async deleteUser(id: string): Promise<void> {
    await apiClient.delete(`/users/${id}/`);
  },

  // Property Management - uses regular properties endpoint
  async getProperties(params?: {
    search?: string;
    status?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: Property[]; count: number }> {
    const response = await apiClient.get('/properties/', { params });
    return response.data;
  },

  async approveProperty(id: string): Promise<Property> {
    const response = await apiClient.post(`/properties/${id}/approve/`);
    return response.data;
  },

  async rejectProperty(id: string, reason: string): Promise<Property> {
    const response = await apiClient.post(`/properties/${id}/reject/`, { reason });
    return response.data;
  },

  async deleteProperty(id: string): Promise<void> {
    await apiClient.delete(`/properties/${id}/`);
  },

  // Booking Management - uses regular bookings endpoint
  async getBookings(params?: {
    search?: string;
    status?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: Booking[]; count: number }> {
    const response = await apiClient.get('/bookings/', { params });
    return response.data;
  },

  async getBookingById(id: string): Promise<Booking> {
    const response = await apiClient.get(`/bookings/${id}/`);
    return response.data;
  },

  async cancelBooking(id: string, reason: string): Promise<Booking> {
    const response = await apiClient.post(`/bookings/${id}/cancel/`, { reason });
    return response.data;
  },

  // Payment Management - uses regular payments endpoint
  async getPayments(params?: {
    search?: string;
    status?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: Payment[]; count: number }> {
    const response = await apiClient.get('/payments/', { params });
    return response.data;
  },

  async getPaymentById(id: string): Promise<Payment> {
    const response = await apiClient.get(`/payments/${id}/`);
    return response.data;
  },

  async refundPayment(id: string, amount?: number): Promise<Payment> {
    const response = await apiClient.post(`/payments/${id}/refund/`, { amount });
    return response.data;
  },

  // Audit Logs - uses actual backend endpoint
  async getAuditLogs(params?: {
    user_id?: string;
    action?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: AuditLog[]; count: number }> {
    const response = await apiClient.get('/admin/audit-logs/', { params });
    return response.data;
  },

  // System Settings - uses actual backend endpoint
  async getSystemConfig(): Promise<SystemConfig> {
    const response = await apiClient.get('/admin/config/fees/');
    return response.data;
  },

  async updateSystemConfig(data: Partial<SystemConfig>): Promise<SystemConfig> {
    const response = await apiClient.put('/admin/config/fees/', data);
    return response.data;
  },

  // Analytics - these endpoints don't exist yet, will return mock data for now
  async getRevenueAnalytics(_params?: {
    period?: 'daily' | 'weekly' | 'monthly' | 'yearly';
    start_date?: string;
    end_date?: string;
  }): Promise<any> {
    // TODO: Backend needs to implement these endpoints
    return { data: [] };
  },

  async getBookingAnalytics(_params?: {
    period?: 'daily' | 'weekly' | 'monthly' | 'yearly';
    start_date?: string;
    end_date?: string;
  }): Promise<any> {
    // TODO: Backend needs to implement these endpoints
    return { data: [] };
  },

  async getUserAnalytics(_params?: {
    period?: 'daily' | 'weekly' | 'monthly' | 'yearly';
    start_date?: string;
    end_date?: string;
  }): Promise<any> {
    // TODO: Backend needs to implement these endpoints
    return { data: [] };
  },

  // Identity Verification / KYC Management
  async getPendingVerifications(params?: {
    page?: number;
    per_page?: number;
  }): Promise<{ count: number; results: IdentityVerification[] }> {
    const response = await apiClient.get('/users/verification/pending_reviews/', { params });
    return response.data;
  },

  async getVerificationById(id: string | number): Promise<IdentityVerification> {
    const response = await apiClient.get(`/users/verification/${id}/`);
    return response.data;
  },

  async approveVerification(id: string | number, notes?: string): Promise<IdentityVerification> {
    const response = await apiClient.post(`/users/verification/${id}/review/`, {
      action: 'approve',
      notes: notes || '',
    });
    return response.data.verification;
  },

  async rejectVerification(id: string | number, reason: string, notes?: string): Promise<IdentityVerification> {
    const response = await apiClient.post(`/users/verification/${id}/review/`, {
      action: 'reject',
      reason,
      notes: notes || '',
    });
    return response.data.verification;
  },

  async getVerificationStats(): Promise<VerificationStats> {
    const response = await apiClient.get('/users/verification/statistics/');
    return response.data;
  },

  // Reviews Management
  async getReviews(params?: {
    search?: string;
    rating?: number;
    page?: number;
    per_page?: number;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/reviews/', { params });
    return response.data;
  },

  async moderateReview(id: string): Promise<any> {
    const response = await apiClient.patch(`/reviews/${id}/`, {
      text: '[MODERATED - This review has been hidden by administrators]'
    });
    return response.data;
  },

  async hideReview(id: string): Promise<any> {
    const response = await apiClient.patch(`/reviews/${id}/`, {
      text: '[HIDDEN]'
    });
    return response.data;
  },

  async deleteReview(id: string): Promise<void> {
    await apiClient.delete(`/reviews/${id}/`);
  },

  // Wallets Management
  async getWallets(params?: {
    search?: string;
    status?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/payments/wallets/', { params });
    return response.data;
  },

  async activateWallet(id: string): Promise<any> {
    const response = await apiClient.patch(`/payments/wallets/${id}/`, {
      status: 'active'
    });
    return response.data;
  },

  async suspendWallet(id: string): Promise<any> {
    const response = await apiClient.patch(`/payments/wallets/${id}/`, {
      status: 'suspended'
    });
    return response.data;
  },

  async closeWallet(id: string): Promise<any> {
    const response = await apiClient.patch(`/payments/wallets/${id}/`, {
      status: 'closed'
    });
    return response.data;
  },

  // Withdrawals Management
  async getWithdrawals(params?: {
    search?: string;
    status?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/payments/withdrawals/', { params });
    return response.data;
  },

  async markWithdrawalProcessing(id: string): Promise<any> {
    const response = await apiClient.patch(`/payments/withdrawals/${id}/`, {
      status: 'processing'
    });
    return response.data;
  },

  async markWithdrawalCompleted(id: string, notes?: string): Promise<any> {
    const response = await apiClient.patch(`/payments/withdrawals/${id}/`, {
      status: 'completed',
      admin_notes: notes
    });
    return response.data;
  },

  async markWithdrawalFailed(id: string, notes?: string): Promise<any> {
    const response = await apiClient.patch(`/payments/withdrawals/${id}/`, {
      status: 'failed',
      admin_notes: notes
    });
    return response.data;
  },

  // Tax Configuration Management
  async getTaxJurisdictions(params?: {
    search?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/payments/tax-jurisdictions/', { params });
    return response.data;
  },

  async getTaxRates(params?: {
    search?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/payments/tax-rates/', { params });
    return response.data;
  },
};
