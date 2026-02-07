// Admin Dashboard Types

export interface AdminStats {
  total_revenue: number;
  total_bookings: number;
  total_users: number;
  active_hosts: number;
  total_properties: number;
  last_updated: string;
}

export interface AuditLog {
  id: string;
  user: {
    id: string;
    email: string;
    first_name: string;
    last_name: string;
  } | null;
  action: string;
  content_type: string | null;
  object_id: number | null;
  changes: Record<string, any>;
  timestamp: string;
}

export interface SystemConfig {
  id: number;
  commission_rate: string;
  service_fee: string;
  default_currency: string;
  paynow_integration_id: string;
  paynow_integration_key: string;
  paynow_webhook_secret: string;
  payfast_merchant_id: string;
  payfast_merchant_key: string;
  payfast_passphrase: string;
  payfast_webhook_secret: string;
  stripe_secret_key: string;
  stripe_publishable_key: string;
  stripe_webhook_secret: string;
  max_advance_booking_days: number;
  max_stay_duration_days: number;
  review_window_days: number;
  review_edit_window_days: number;
  admin_email: string;
  support_email: string;
  maintenance_mode: boolean;
  maintenance_message: string;
  created_at: string;
  updated_at: string;
}

export interface BookingAnalytics {
  total_bookings: number;
  confirmed_bookings: number;
  completed_bookings: number;
  cancelled_bookings: number;
  pending_bookings: number;
  bookings_by_month: Array<{
    month: string;
    count: number;
    revenue: number;
  }>;
}

export interface RevenueAnalytics {
  total_revenue: number;
  commission_earned: number;
  service_fees: number;
  revenue_by_month: Array<{
    month: string;
    revenue: number;
  }>;
}

export interface UserAnalytics {
  total_users: number;
  guests: number;
  hosts: number;
  verified_users: number;
  new_users_this_month: number;
  users_by_country: Array<{
    country: string;
    count: number;
  }>;
}

export interface PropertyAnalytics {
  total_properties: number;
  active_properties: number;
  pending_approval: number;
  inactive_properties: number;
  properties_by_type: Array<{
    type: string;
    count: number;
  }>;
}

// Identity Verification / KYC Types
export interface IdentityVerification {
  id: string | number;
  user: {
    id: string | number;
    email: string;
    first_name: string;
    last_name: string;
  };
  document_type: 'passport' | 'national_id' | 'drivers_license';
  document_number: string;
  document_country: string;
  document_expiry_date: string | null;
  document_front_image?: string;
  document_back_image?: string;
  selfie_image?: string;
  verification_method: 'manual' | 'automated' | 'third_party';
  status: 'pending' | 'under_review' | 'approved' | 'rejected' | 'expired';
  submitted_at: string;
  reviewed_at?: string;
  reviewed_by?: {
    id: string | number;
    email: string;
  };
  rejection_reason?: string;
  admin_notes?: string;
  is_expired?: boolean;
}

export interface VerificationStats {
  total_verifications: number;
  by_status: Record<string, number>;
  recent_submissions: number;
  recent_approved: number;
  approval_rate: number;
  pending_review: number;
}
