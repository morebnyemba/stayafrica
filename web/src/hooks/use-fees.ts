import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';

export interface FeeConfiguration {
  commission_rate: number;
  service_fee: number;
  default_currency: string;
  max_advance_booking_days: number;
  max_stay_duration_days: number;
  review_window_days: number;
}

export function useFeeConfiguration() {
  return useQuery<FeeConfiguration>({
    queryKey: ['fees'],
    queryFn: async () => {
      const response = await apiClient.getFeeConfiguration();
      return response.data;
    },
    staleTime: 1000 * 60 * 60, // Cache for 1 hour
  });
}

export interface TaxEstimate {
  taxes: { name: string; tax_type: string; rate_percentage: number; flat_fee: number; applies_to_base_price: boolean }[];
  combined_rate: number;
}

export function useTaxEstimate(country?: string) {
  return useQuery<TaxEstimate>({
    queryKey: ['tax-estimate', country],
    queryFn: async () => {
      const response = await apiClient.getTaxEstimate(country!);
      return response.data;
    },
    enabled: !!country,
    staleTime: 1000 * 60 * 60,
  });
}

export interface BookingCostCalculation {
  basePrice: number;
  serviceFee: number;
  commissionFee: number;
  commissionRate: number;
  cleaningFee: number;
  taxes: number;
  taxRate: number;
  total: number;
}

export function calculateBookingCost(
  pricePerNight: number,
  nights: number,
  feeConfig: FeeConfiguration,
  cleaningFee: number = 0,
  taxRate: number = 0
): BookingCostCalculation {
  const basePrice = nights * pricePerNight;
  const serviceFee = parseFloat(feeConfig.service_fee.toString());
  const commissionRate = parseFloat(feeConfig.commission_rate.toString());
  const commissionFee = (basePrice + serviceFee) * commissionRate;
  const taxes = basePrice * (taxRate / 100);
  const total = basePrice + serviceFee + cleaningFee + taxes;

  return {
    basePrice,
    serviceFee,
    commissionFee,
    commissionRate,
    cleaningFee,
    taxes,
    taxRate,
    total,
  };
}
