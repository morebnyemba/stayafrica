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

  async createUser(data: Partial<User> & { password?: string }): Promise<User> {
    const response = await apiClient.post('/users/', data);
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

  async activateProperty(id: string): Promise<Property> {
    const response = await apiClient.patch(`/properties/${id}/`, { status: 'active' });
    return response.data;
  },

  async deactivateProperty(id: string): Promise<Property> {
    const response = await apiClient.patch(`/properties/${id}/`, { status: 'inactive' });
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

  async confirmBooking(id: string): Promise<Booking> {
    const response = await apiClient.post(`/bookings/${id}/confirm/`);
    return response.data;
  },

  async completeBooking(id: string): Promise<Booking> {
    const response = await apiClient.post(`/bookings/${id}/complete/`);
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

  // Analytics
  async getRevenueAnalytics(params?: {
    period?: 'daily' | 'weekly' | 'monthly' | 'yearly';
    start_date?: string;
    end_date?: string;
  }): Promise<any> {
    const response = await apiClient.get('/admin/stats/revenue_analytics/', { params });
    return response.data;
  },

  async getBookingAnalytics(params?: {
    period?: 'daily' | 'weekly' | 'monthly' | 'yearly';
    start_date?: string;
    end_date?: string;
  }): Promise<any> {
    const response = await apiClient.get('/admin/stats/booking_analytics/', { params });
    return response.data;
  },

  async getUserAnalytics(params?: {
    period?: 'daily' | 'weekly' | 'monthly' | 'yearly';
    start_date?: string;
    end_date?: string;
  }): Promise<any> {
    const response = await apiClient.get('/admin/stats/user_analytics/', { params });
    return response.data;
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
    const response = await apiClient.get('/wallets/', { params });
    return response.data;
  },

  async activateWallet(id: string): Promise<any> {
    const response = await apiClient.patch(`/wallets/${id}/`, {
      status: 'active'
    });
    return response.data;
  },

  async suspendWallet(id: string): Promise<any> {
    const response = await apiClient.patch(`/wallets/${id}/`, {
      status: 'suspended'
    });
    return response.data;
  },

  async closeWallet(id: string): Promise<any> {
    const response = await apiClient.patch(`/wallets/${id}/`, {
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
    const response = await apiClient.get('/withdrawals/', { params });
    return response.data;
  },

  async markWithdrawalProcessing(id: string): Promise<any> {
    const response = await apiClient.patch(`/withdrawals/${id}/`, {
      status: 'processing'
    });
    return response.data;
  },

  async markWithdrawalCompleted(id: string, notes?: string): Promise<any> {
    const response = await apiClient.patch(`/withdrawals/${id}/`, {
      status: 'completed',
      admin_notes: notes
    });
    return response.data;
  },

  async markWithdrawalFailed(id: string, notes?: string): Promise<any> {
    const response = await apiClient.patch(`/withdrawals/${id}/`, {
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
    const response = await apiClient.get('/tax/jurisdictions/', { params });
    return response.data;
  },

  async createTaxJurisdiction(data: any): Promise<any> {
    const response = await apiClient.post('/tax/jurisdictions/', data);
    return response.data;
  },

  async updateTaxJurisdiction(id: string, data: any): Promise<any> {
    const response = await apiClient.patch(`/tax/jurisdictions/${id}/`, data);
    return response.data;
  },

  async deleteTaxJurisdiction(id: string): Promise<void> {
    await apiClient.delete(`/tax/jurisdictions/${id}/`);
  },

  async getTaxRates(params?: {
    search?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/tax/rates/', { params });
    return response.data;
  },

  async createTaxRate(data: any): Promise<any> {
    const response = await apiClient.post('/tax/rates/', data);
    return response.data;
  },

  async updateTaxRate(id: string, data: any): Promise<any> {
    const response = await apiClient.patch(`/tax/rates/${id}/`, data);
    return response.data;
  },

  async deleteTaxRate(id: string): Promise<void> {
    await apiClient.delete(`/tax/rates/${id}/`);
  },

  async getTaxExemptions(params?: {
    search?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/tax/exemptions/', { params });
    return response.data;
  },

  async getTaxRemittances(params?: {
    search?: string;
    page?: number;
    per_page?: number;
    status?: string;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/tax/remittances/', { params });
    return response.data;
  },

  // POI Management
  async getPOIs(params?: {
    search?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/pois/', { params });
    return response.data;
  },

  async getPOICategories(params?: {
    search?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/poi-categories/', { params });
    return response.data;
  },

  async verifyPOI(id: string): Promise<any> {
    const response = await apiClient.patch(`/pois/${id}/`, {
      is_active: true
    });
    return response.data;
  },

  async unverifyPOI(id: string): Promise<any> {
    const response = await apiClient.patch(`/pois/${id}/`, {
      is_active: false
    });
    return response.data;
  },

  // Messaging Automation
  async getAutomatedMessages(params?: {
    search?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/messaging/automated-messages/', { params });
    return response.data;
  },

  async toggleAutomatedMessage(id: string, is_active: boolean): Promise<any> {
    const response = await apiClient.patch(`/messaging/automated-messages/${id}/`, { is_active });
    return response.data;
  },

  async deleteAutomatedMessage(id: string): Promise<void> {
    await apiClient.delete(`/messaging/automated-messages/${id}/`);
  },

  async getScheduledMessages(params?: {
    search?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/messaging/scheduled-messages/', { params });
    return response.data;
  },

  async deleteScheduledMessage(id: string): Promise<void> {
    await apiClient.delete(`/messaging/scheduled-messages/${id}/`);
  },

  async getQuickReplies(params?: {
    search?: string;
    page?: number;
    per_page?: number;
  }): Promise<{ results: any[]; count: number }> {
    const response = await apiClient.get('/messaging/quick-replies/', { params });
    return response.data;
  },

  async deleteQuickReply(id: string): Promise<void> {
    await apiClient.delete(`/messaging/quick-replies/${id}/`);
  },

  // POI Management
  async createPOI(data: any): Promise<any> {
    const response = await apiClient.post('/pois/', data);
    return response.data;
  },

  async updatePOI(id: string, data: any): Promise<any> {
    const response = await apiClient.patch(`/pois/${id}/`, data);
    return response.data;
  },

  async deletePOI(id: string): Promise<void> {
    await apiClient.delete(`/pois/${id}/`);
  },

  async createPOICategory(data: any): Promise<any> {
    const response = await apiClient.post('/poi-categories/', data);
    return response.data;
  },

  async updatePOICategory(id: string, data: any): Promise<any> {
    const response = await apiClient.patch(`/poi-categories/${id}/`, data);
    return response.data;
  },

  async deletePOICategory(id: string): Promise<void> {
    await apiClient.delete(`/poi-categories/${id}/`);
  },
};
