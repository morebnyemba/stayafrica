// Pricing API Service
import { apiClient } from './api-client';
import { PricingCalendarResponse, DynamicPricing } from '@/types/pricing-types';

export const pricingApi = {
  /**
   * Get pricing calendar for a property
   */
  async getPricingCalendar(propertyId: string, month?: number, year?: number) {
    const params: any = {};
    if (month !== undefined) params.month = month;
    if (year !== undefined) params.year = year;
    
    const response = await apiClient.get<PricingCalendarResponse>(
      `/properties/${propertyId}/pricing_calendar/`,
      { params }
    );
    return response.data;
  },

  /**
   * Get dynamic pricing for specific dates
   */
  async getDynamicPricing(
    propertyId: string,
    checkIn: string,
    checkOut: string
  ) {
    const response = await apiClient.get<DynamicPricing>(
      `/properties/${propertyId}/availability/`,
      {
        params: {
          check_in: checkIn,
          check_out: checkOut,
        },
      }
    );
    return response.data;
  },

  /**
   * Calculate booking total with dynamic pricing
   */
  async calculateBookingTotal(
    propertyId: string,
    checkIn: string,
    checkOut: string,
    guests: number
  ) {
    const response = await apiClient.post<DynamicPricing>(
      `/bookings/calculate_total/`,
      {
        rental_property: propertyId,
        check_in: checkIn,
        check_out: checkOut,
        number_of_guests: guests,
      }
    );
    return response.data;
  },
};

export default pricingApi;
