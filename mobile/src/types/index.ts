// Auth Types
export interface User {
  id: string;
  email: string;
  first_name: string;
  last_name: string;
  phone_number: string;
  country_of_residence: string;
  role: 'guest' | 'host' | 'admin';
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
  location: {
    address: string;
    city: string;
    country: string;
    latitude: number;
    longitude: number;
  };
  amenities: string[];
  rating: number;
  review_count: number;
  host_id: string;
}

// Booking Types
export interface Booking {
  id: string;
  property_id: string;
  guest_id: string;
  check_in: string;
  check_out: string;
  number_of_guests: number;
  number_of_nights: number;
  price_per_night: number;
  total_before_tax: number;
  tax: number;
  service_fee: number;
  grand_total: number;
  status: 'pending' | 'confirmed' | 'checked_in' | 'checked_out' | 'cancelled';
  special_requests: string;
  created_at: string;
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

export interface UpdatePropertyRequest extends Partial<CreatePropertyRequest> {}

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
