// User Types
export interface User {
  id: string;
  email: string;
  phone_number: string;
  first_name: string;
  last_name: string;
  role: 'guest' | 'host' | 'admin';
  active_profile: 'guest' | 'host';
  is_staff?: boolean;
  profile_picture?: string;
  country_of_residence: string;
  username?: string;
  is_verified: boolean;
  is_active?: boolean;
  is_superuser?: boolean;
  bio?: string;
  last_login?: string;
  created_at: string;
  updated_at: string;
}

export interface AuthTokens {
  access: string;
  refresh: string;
}

// Property Types
export interface Property {
  id: string;
  title: string;
  description: string;
  price_per_night: number;
  location: {
    latitude: number;
    longitude: number;
    address: string;
    city: string;
    country: string;
  };
  property_type: 'apartment' | 'house' | 'villa' | 'cottage' | 'other';
  bedrooms: number;
  bathrooms: number;
  max_guests: number;
  amenities: Amenity[];
  images: PropertyImage[];
  status: 'active' | 'inactive' | 'pending_approval';
  host_id: string;
  rating: number;
  review_count: number;
  created_at: string;
  updated_at: string;
}

export interface PropertyImage {
  id: string;
  property_id: string;
  image_url: string;
  is_main: boolean;
  order: number;
}

export interface Amenity {
  id: string;
  name: string;
  icon?: string;
}

// Booking Types
export interface Booking {
  id: string;
  booking_ref: string;
  property_id: string;
  guest_id: string;
  check_in: string;
  check_out: string;
  number_of_guests: number;
  nightly_total: number;
  service_fee: number;
  commission_fee: number;
  cleaning_fee?: number;
  grand_total: number;
  status: 'pending' | 'confirmed' | 'completed' | 'cancelled';
  created_at: string;
  updated_at: string;
}

export interface BookingRequest {
  property_id: string;
  check_in: string;
  check_out: string;
  number_of_guests: number;
  cleaning_fee?: number;
}

// Payment Types
export interface Payment {
  id: string;
  booking_id: string;
  gateway_ref: string;
  provider: 'paynow' | 'payfast' | 'stripe' | 'ozow' | 'cash_on_arrival';
  amount: number;
  status: 'initiated' | 'success' | 'failed' | 'pending';
  created_at: string;
  updated_at: string;
}

// Review Types
export interface Review {
  id: string;
  booking_id: string;
  guest_id: string;
  host_id: string;
  rating: number;
  text: string;
  created_at: string;
  updated_at: string;
}

// Message Types
export interface Message {
  id: string;
  sender_id: string;
  receiver_id: string;
  text: string;
  is_read: boolean;
  created_at: string;
}

export interface Conversation {
  participant_id: string;
  participant_name: string;
  participant_picture?: string;
  last_message: string;
  last_message_time: string;
  unread_count: number;
}

// Search Filters
export interface PropertyFilters {
  city?: string;
  country?: string;
  property_type?: string;
  price_min?: number;
  price_max?: number;
  bedrooms?: number;
  bathrooms?: number;
  amenities?: string[];
  check_in?: string;
  check_out?: string;
  guests?: number;
  latitude?: number;
  longitude?: number;
  radius_km?: number;
}

// API Response Types
export interface ApiResponse<T> {
  data: T;
  message?: string;
  status: 'success' | 'error';
}

export interface PaginatedResponse<T> {
  results: T[];
  count: number;
  next?: string;
  previous?: string;
}

// ── Experiences ──────────────────────────────────────────────

export interface ExperienceCategory {
  id: number;
  name: string;
  description: string;
  icon: string;
}

export type ExperienceDuration = 'half_day' | 'full_day' | 'multi_day' | 'hourly';
export type ExperienceDifficulty = 'easy' | 'moderate' | 'challenging' | 'expert';
export type ExperienceStatus = 'active' | 'inactive' | 'pending_approval';

export interface ExperienceImage {
  id: number;
  image: string;
  caption: string;
  order: number;
}

export interface Experience {
  id: string;
  host: number;
  host_name: string;
  title: string;
  description: string;
  category: number | null;
  category_name: string;
  latitude: number | null;
  longitude: number | null;
  country: string;
  city: string;
  address: string;
  price_per_person: number;
  currency: string;
  duration: ExperienceDuration;
  duration_hours: number;
  difficulty: ExperienceDifficulty;
  min_participants: number;
  max_participants: number;
  included_items: string;
  requirements: string;
  cancellation_policy: string;
  main_image: string | null;
  images: ExperienceImage[];
  status: ExperienceStatus;
  is_available: boolean;
  created_at: string;
  updated_at: string;
}

export type ExperienceBookingStatus = 'pending' | 'confirmed' | 'cancelled' | 'completed';

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

export interface ExperienceAvailability {
  id: number;
  experience: string;
  weekday: number | null;
  specific_date: string | null;
  start_time: string;
  end_time: string;
  is_available: boolean;
}
