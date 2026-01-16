export interface TaxJurisdiction {
  jurisdiction: string;
  tax_type: string;
  rate: number;
  amount: number;
}

export interface BookingTax {
  booking_id: string;
  property_id: string;
  subtotal: number;
  total_tax: number;
  taxes: TaxJurisdiction[];
  grand_total: number;
}

export interface HostTaxReport {
  host_id: string;
  period_start: string;
  period_end: string;
  total_bookings: number;
  total_revenue: number;
  total_taxes_collected: number;
  tax_breakdown: {
    jurisdiction: string;
    tax_type: string;
    total_amount: number;
  }[];
}
