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

export interface BookingCostCalculation {
  basePrice: number;
  serviceFee: number;
  commissionFee: number;
  commissionRate: number;
  cleaningFee: number;
  total: number;
}

export function calculateBookingCost(
  pricePerNight: number,
  nights: number,
  feeConfig: FeeConfiguration,
  cleaningFee: number = 0
): BookingCostCalculation {
  const basePrice = nights * pricePerNight;
  const serviceFee = parseFloat(feeConfig.service_fee.toString());
  const commissionRate = parseFloat(feeConfig.commission_rate.toString());
  const commissionFee = (basePrice + serviceFee) * commissionRate;
  const total = basePrice + serviceFee + commissionFee + cleaningFee;

  return {
    basePrice,
    serviceFee,
    commissionFee,
    commissionRate,
    cleaningFee,
    total,
  };
}
