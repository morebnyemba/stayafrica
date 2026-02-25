// Auth Types
export interface User {
  id: string;
  email: string;
  first_name: string;
  last_name: string;
  phone_number: string;
  country_of_residence: string;
  role: 'guest' | 'host' | 'admin';
  active_profile: 'guest' | 'host';
  is_verified: boolean;
}

// Property Types
export interface Property {
  id: string;
  title: string;
  description: string;
  property_type: string;
  number_of_rooms: number;
  number_of_beds: number;
  number_of_bathrooms: number;
  max_guests: number;
  price_per_night: number;
  image_urls: string[];
  // Location can be either nested object or flat fields at top level
  location?: {
    address: string;
    city: string;
    country: string;
    latitude: number;
    longitude: number;
  };
  // Flat location fields (from backend)
  city?: string;
  country?: string;
  amenities: string[];
  rating: number;
  review_count: number;
  host_id: string;
}

// Booking Types
export interface Booking {
  id: string;
  booking_ref?: string;
  property_id?: string;
  rental_property?: string;
  property?: {
    id: string;
    title: string;
    city?: string;
    country?: string;
    location?: {
      address: string;
      city: string;
      country: string;
      latitude: number;
      longitude: number;
    };
  };
  property_title?: string;
  guest_id?: string;
  guest?: string;
  guest_email?: string;
  guest_first_name?: string;
  guest_last_name?: string;
  check_in: string;
  check_out: string;
  number_of_guests: number;
  number_of_nights?: number;
  nights?: number;
  price_per_night?: number;
  nightly_total?: number;
  total_before_tax?: number;
  tax?: number;
  service_fee?: number;
  commission_fee?: number;
  cleaning_fee?: number;
  grand_total: number;
  currency?: string;
  status: 'pending' | 'confirmed' | 'checked_in' | 'checked_out' | 'cancelled' | 'completed';
  special_requests?: string;
  created_at: string;
  updated_at?: string;
}

// Review Types
export interface Review {
  id: string;
  booking_id: string;
  reviewer_id: string;
  rating: number;
  title: string;
  comment: string;
  cleanliness_rating: number;
  communication_rating: number;
  accuracy_rating: number;
  location_rating: number;
  created_at: string;
}

// Experience Types
export type ExperienceDuration = 'hourly' | 'half_day' | 'full_day' | 'multi_day';
export type ExperienceDifficulty = 'easy' | 'moderate' | 'challenging';
export type ExperienceStatus = 'active' | 'inactive' | 'pending_approval';
export type ExperienceBookingStatus = 'pending' | 'confirmed' | 'cancelled' | 'completed';

export interface ExperienceCategory {
  id: number;
  name: string;
  slug: string;
  description: string;
  icon: string;
}

export interface Experience {
  id: string;
  title: string;
  description: string;
  host: string;
  host_name: string;
  category: number;
  category_name: string;
  country: string;
  city: string;
  address: string;
  latitude: number;
  longitude: number;
  price_per_person: number;
  currency: string;
  duration_type: ExperienceDuration;
  duration_hours: number;
  difficulty: ExperienceDifficulty;
  min_participants: number;
  max_participants: number;
  included_items: string[];
  requirements: string[];
  cancellation_policy: string;
  cover_image: string | null;
  images: string[];
  status: ExperienceStatus;
  average_rating: number;
  review_count: number;
  created_at: string;
  updated_at: string;
}

export interface ExperienceBooking {
  id: number;
  booking_ref: string;
  guest: number;
  guest_name: string;
  experience: string;
  experience_title: string;
  booking_date: string;
  booking_time: string | null;
  num_participants: number;
  price_per_person: number;
  service_fee: number;
  total_amount: number;
  currency: string;
  status: ExperienceBookingStatus;
  special_requests: string | null;
  created_at: string;
  updated_at: string;
}

// Message Types
export interface Message {
  id: string;
  conversation_id: string;
  sender_id: string;
  content: string;
  created_at: string;
  is_read: boolean;
}

export interface Conversation {
  id: string;
  participant_id: string;
  participant_name: string;
  participant_picture?: string;
  last_message: string;
  unread_count: number;
  updated_at: string;
}

// Payment Types
export interface Payment {
  id: string;
  booking_id: string;
  amount: number;
  currency: string;
  status: 'pending' | 'completed' | 'failed' | 'refunded';
  payment_method: string;
  transaction_id: string;
  created_at: string;
}

// API Request/Response Types
export interface UpdateProfileRequest {
  first_name?: string;
  last_name?: string;
  phone_number?: string;
  country_of_residence?: string;
}

export interface CreateBookingRequest {
  property_id: string;
  check_in_date: string;
  check_out_date: string;
  number_of_guests: number;
  special_requests?: string;
}

export interface CreatePropertyRequest {
  title: string;
  description: string;
  property_type: string;
  number_of_rooms: number;
  number_of_beds: number;
  number_of_bathrooms: number;
  max_guests: number;
  price_per_night: number;
  location: {
    address: string;
    city: string;
    country: string;
    latitude: number;
    longitude: number;
  };
  amenities: string[];
}

export interface UpdatePropertyRequest extends Partial<CreatePropertyRequest> { }

export interface SubmitReviewRequest {
  booking_id: string;
  rating: number;
  title?: string;
  comment: string;
  cleanliness_rating?: number;
  communication_rating?: number;
  accuracy_rating?: number;
  location_rating?: number;
}

export interface WalletBalance {
  total_earnings: number;
  available_balance: number;
  pending_balance: number;
}

export interface Transaction {
  id: string;
  type: 'credit' | 'debit';
  amount: number;
  description: string;
  created_at: string;
  status: 'completed' | 'pending' | 'failed';
}

export interface HostEarnings {
  total_earnings: number;
  available_balance: number;
  pending_balance: number;
  monthly_earnings: number;
  last_month_earnings: number;
  average_per_booking: number;
  recent_payouts: Array<{
    date: string;
    amount: number;
    status: 'paid' | 'pending';
    property: string;
  }>;
}

// Wallet Types
export interface Wallet {
  id: string;
  balance: number;
  currency: string;
  status: 'active' | 'suspended' | 'closed';
  user_id?: string;
  created_at?: string;
  updated_at?: string;
}

export interface WithdrawalRequest {
  amount: number;
  method: 'bank' | 'mobile' | 'paypal';
  account_id?: string;
}

export interface WithdrawalResponse {
  id: string;
  amount: number;
  method: string;
  status: 'pending' | 'processing' | 'completed' | 'failed';
  created_at: string;
  estimated_arrival?: string;
}

// Verification Types
export interface VerificationData {
  document_type: 'PASSPORT' | 'NATIONAL_ID' | 'DRIVERS_LICENSE';
  document_number: string;
  issued_country: string;
  expiry_date: string | null;
  front_image_url: string;
  back_image_url: string | null;
  selfie_url: string;
}

export interface VerificationResponse {
  id: string;
  status: 'pending' | 'approved' | 'rejected';
  submitted_at: string;
  reviewed_at?: string;
  rejection_reason?: string;
}

// API Response Wrapper Types
export interface ApiListResponse<T> {
  count?: number;
  next?: string | null;
  previous?: string | null;
  results: T[];
}

export interface ApiResponse<T> {
  data: T;
  message?: string;
  status: number;
}

// Payment Types Extended
export interface PaymentInitiationRequest {
  booking_id: string;
  provider: 'stripe' | 'paypal' | 'mobile_money';
  amount?: number;
}

export interface PaymentInitiationResponse {
  payment_id: string;
  client_secret?: string; // For Stripe
  checkout_url?: string; // For PayPal or other redirect-based providers
  mobile_money_reference?: string;
  status: 'pending' | 'requires_action' | 'processing';
}

export interface PaymentStatusResponse {
  payment_id: string;
  status: 'pending' | 'processing' | 'completed' | 'failed' | 'refunded';
  amount: number;
  currency: string;
  booking_id: string;
  created_at: string;
  completed_at?: string;
  failure_reason?: string;
}

// Bank Account Types
export interface BankAccount {
  id: string;
  account_holder_name: string;
  account_number: string;
  bank_name: string;
  bank_code?: string;
  is_primary: boolean;
  created_at: string;
}

export interface CreateBankAccountRequest {
  account_holder_name: string;
  account_number: string;
  bank_name: string;
  bank_code?: string;
}

// Host Analytics Types
export interface HostAnalytics {
  total_properties: number;
  active_properties: number;
  total_bookings: number;
  upcoming_bookings: number;
  total_revenue: number;
  occupancy_rate: number;
  average_rating: number;
  total_reviews: number;
}

export interface PropertyPerformance {
  property_id: string;
  property_title: string;
  total_bookings: number;
  revenue: number;
  occupancy_rate: number;
  average_rating: number;
  cancellation_rate: number;
}

// Booking Calendar Types
export interface BookingCalendarEvent {
  booking_id: string;
  guest_name: string;
  check_in: string;
  check_out: string;
  status: 'pending' | 'confirmed' | 'checked_in' | 'checked_out' | 'cancelled';
}

export interface UpcomingCheckin {
  booking_id: string;
  property_id: string;
  property_title: string;
  guest_name: string;
  guest_email: string;
  check_in: string;
  number_of_guests: number;
}

export interface PendingAction {
  type: 'booking_request' | 'review_needed' | 'verification_required' | 'payout_ready';
  id: string;
  title: string;
  description: string;
  created_at: string;
  priority: 'high' | 'medium' | 'low';
}

// Notification Types
export type NotificationType =
  | 'booking_confirmed'
  | 'booking_cancelled'
  | 'booking_reminder'
  | 'new_message'
  | 'payment_received'
  | 'payment_required'
  | 'review_received'
  | 'review_reminder'
  | 'price_drop'
  | 'system';

export interface AppNotification {
  id: string;
  user: string;
  notification_type: NotificationType;
  title: string;
  body: string;
  data: Record<string, any>;
  deep_link: string;
  status: 'pending' | 'sent' | 'delivered' | 'failed' | 'read';
  is_read: boolean;
  sent_at: string | null;
  delivered_at: string | null;
  read_at: string | null;
  created_at: string;
}

export interface NotificationPreference {
  id: string;
  user: string;
  booking_confirmed: boolean;
  booking_cancelled: boolean;
  booking_reminder: boolean;
  new_message: boolean;
  payment_received: boolean;
  payment_required: boolean;
  review_reminder: boolean;
  review_received: boolean;
  price_drop: boolean;
}

export interface PushTokenData {
  token: string;
  platform: 'ios' | 'android' | 'web';
  device_name?: string;
}
