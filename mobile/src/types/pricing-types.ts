// Pricing Types for Dynamic Pricing Integration

export interface PricingRule {
  id: string;
  name: string;
  rule_type: 'seasonal' | 'weekend' | 'length_of_stay' | 'early_bird' | 'last_minute';
  adjustment_type: 'percentage' | 'fixed';
  adjustment_value: number;
  start_date?: string;
  end_date?: string;
  min_nights?: number;
  max_nights?: number;
  days_before_checkin?: number;
  is_active: boolean;
}

export interface PropertyFee {
  id: string;
  name: string;
  fee_type: 'cleaning' | 'service' | 'pet' | 'extra_guest' | 'other';
  amount: number;
  is_percentage: boolean;
  is_active: boolean;
}

export interface PropertyTax {
  id: string;
  name: string;
  rate: number;
  tax_type: 'vat' | 'occupancy' | 'tourism' | 'local';
  is_active: boolean;
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
