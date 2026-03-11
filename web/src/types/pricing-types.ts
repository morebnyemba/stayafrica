// Pricing Types for Dynamic Pricing Integration

export type PricingRuleType = 'seasonal' | 'weekend' | 'length_discount' | 'early_bird' | 'last_minute';

export interface PricingRule {
  id: number;
  property: number | string;
  name: string;
  rule_type: PricingRuleType;
  is_active: boolean;
  priority: number;
  start_date?: string | null;
  end_date?: string | null;
  adjustment_type: 'percentage' | 'fixed';
  adjustment_value: number;
  min_nights?: number | null;
  max_nights?: number | null;
  min_days_advance?: number | null;
  max_days_advance?: number | null;
  created_at?: string;
  updated_at?: string;
}

export interface PricingRuleFormData {
  property: number | string;
  name: string;
  rule_type: PricingRuleType;
  is_active: boolean;
  priority: number;
  start_date?: string | null;
  end_date?: string | null;
  adjustment_type: 'percentage' | 'fixed';
  adjustment_value: number;
  min_nights?: number | null;
  max_nights?: number | null;
  min_days_advance?: number | null;
  max_days_advance?: number | null;
}

export interface PropertyFee {
  id: number;
  property: number | string;
  fee_type: 'cleaning' | 'service' | 'pet' | 'extra_guest' | 'resort' | 'parking' | 'linen';
  name: string;
  amount: number;
  charge_type: 'per_booking' | 'per_night' | 'per_guest';
  is_mandatory: boolean;
  is_active: boolean;
  applies_after_guests?: number | null;
  created_at?: string;
  updated_at?: string;
}

export interface PropertyTax {
  id: number;
  property: number | string;
  tax_type: 'vat' | 'occupancy' | 'tourism' | 'city';
  name: string;
  rate: number;
  is_active: boolean;
  applies_to_base_price: boolean;
  applies_to_fees: boolean;
  created_at?: string;
  updated_at?: string;
}

export interface AppliedPricingRule {
  rule_name: string;
  rule_type: string;
  adjustment_type: string;
  adjustment_value: number;
  original_amount: number;
  adjusted_amount: number;
}

export interface DynamicPricing {
  base_price: number;
  adjusted_price: number;
  applied_rules: AppliedPricingRule[];
  fees: {
    name: string;
    amount: number;
  }[];
  taxes: {
    name: string;
    rate: number;
    amount: number;
  }[];
  total_price: number;
  breakdown_summary: string;
}

export interface PricingCalendarDay {
  date: string;
  base_price: number;
  dynamic_price: number;
  is_available: boolean;
  applied_rules: string[];
}

export interface PricingCalendarResponse {
  property_id: string;
  property_name: string;
  month: string;
  year: number;
  calendar: PricingCalendarDay[];
  min_price: number;
  max_price: number;
  avg_price: number;
}
