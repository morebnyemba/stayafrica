// Pricing API Service
import { apiClient } from './api-client';
import type { PricingRule, PricingRuleFormData } from '@/types/pricing-types';

export const pricingApi = {
  // --- Pricing Calendar ---

  async getPricingCalendar(propertyId: string, month?: number, year?: number) {
    const params: Record<string, number> = {};
    if (month !== undefined) params.month = month;
    if (year !== undefined) params.year = year;
    const response = await apiClient.get(
      `/properties/${propertyId}/pricing_calendar/`,
      { params }
    );
    return response.data;
  },

  async getDynamicPricing(propertyId: string, checkIn: string, checkOut: string) {
    const response = await apiClient.get(
      `/properties/${propertyId}/availability/`,
      { params: { check_in: checkIn, check_out: checkOut } }
    );
    return response.data;
  },

  async calculateBookingTotal(propertyId: string, checkIn: string, checkOut: string, guests: number) {
    const response = await apiClient.post(`/bookings/calculate_total/`, {
      rental_property: propertyId,
      check_in: checkIn,
      check_out: checkOut,
      number_of_guests: guests,
    });
    return response.data;
  },

  // --- Pricing Rules CRUD ---

  async getPricingRules(propertyId?: string | number): Promise<PricingRule[]> {
    const params: Record<string, string | number> = {};
    if (propertyId) params.property = propertyId;
    const response = await apiClient.get('/pricing-rules/', { params });
    return response.data?.results ?? response.data ?? [];
  },

  async createPricingRule(data: PricingRuleFormData): Promise<PricingRule> {
    const response = await apiClient.post('/pricing-rules/', data);
    return response.data;
  },

  async updatePricingRule(ruleId: number, data: Partial<PricingRuleFormData>): Promise<PricingRule> {
    const response = await apiClient.patch(`/pricing-rules/${ruleId}/`, data);
    return response.data;
  },

  async deletePricingRule(ruleId: number): Promise<void> {
    await apiClient.delete(`/pricing-rules/${ruleId}/`);
  },
};

export default pricingApi;
