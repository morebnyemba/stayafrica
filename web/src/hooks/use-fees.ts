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

export interface IndividualTax {
  name: string;
  amount: number;
}

export interface BookingCostCalculation {
  basePrice: number;
  serviceFee: number;
  commissionFee: number;
  commissionRate: number;
  cleaningFee: number;
  taxes: number;
  taxRate: number;
  individualTaxes: IndividualTax[];
  total: number;
}

export function calculateBookingCost(
  pricePerNight: number,
  nights: number,
  feeConfig: FeeConfiguration,
  cleaningFee: number = 0,
  taxEstimate: TaxEstimate | null = null
): BookingCostCalculation {
  const basePrice = nights * pricePerNight;
  const serviceFee = parseFloat(feeConfig.service_fee.toString());
  const commissionRate = parseFloat(feeConfig.commission_rate.toString());
  const commissionFee = (basePrice + serviceFee) * commissionRate;

  let taxes = 0;
  const individualTaxes: IndividualTax[] = [];

  if (taxEstimate?.taxes && taxEstimate.taxes.length > 0) {
    taxEstimate.taxes.forEach((tax) => {
      let taxAmount = 0;
      if (tax.rate_percentage > 0) {
        const basis = tax.applies_to_base_price ? basePrice : (basePrice + serviceFee + cleaningFee);
        taxAmount += basis * (tax.rate_percentage / 100);
      }
      if (tax.flat_fee > 0) {
        taxAmount += parseFloat(tax.flat_fee.toString());
      }
      if (taxAmount > 0) {
        individualTaxes.push({ name: tax.name, amount: taxAmount });
        taxes += taxAmount;
      }
    });
  } else if (taxEstimate?.combined_rate && taxEstimate.combined_rate > 0) {
    const taxRate = parseFloat(taxEstimate.combined_rate.toString());
    taxes = basePrice * (taxRate / 100);
    if (taxes > 0) {
      individualTaxes.push({ name: 'Taxes', amount: taxes });
    }
  }

  const taxRate = taxEstimate?.combined_rate ? parseFloat(taxEstimate.combined_rate.toString()) : 0;
  const total = basePrice + serviceFee + cleaningFee + taxes;

  return {
    basePrice,
    serviceFee,
    commissionFee,
    commissionRate,
    cleaningFee,
    taxes,
    taxRate,
    individualTaxes,
    total,
  };
}
