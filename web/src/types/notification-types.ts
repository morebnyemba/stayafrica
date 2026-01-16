export interface Notification {
  id: string;
  user: string;
  notification_type: 'BOOKING_CONFIRMED' | 'BOOKING_CANCELLED' | 'NEW_MESSAGE' | 
    'REVIEW_RECEIVED' | 'PAYOUT_COMPLETED' | 'PRICE_DROP' | 'WISHLIST_AVAILABLE' | 'SYSTEM_ALERT';
  title: string;
  message: string;
  data?: Record<string, unknown>;
  is_read: boolean;
  link?: string;
  created_at: string;
}

export interface NotificationPreferences {
  id: string;
  user: string;
  email_notifications: boolean;
  push_notifications: boolean;
  sms_notifications: boolean;
  booking_updates: boolean;
  messages: boolean;
  reviews: boolean;
  promotions: boolean;
  account_activity: boolean;
  updated_at: string;
}

export interface PushToken {
  id: string;
  user: string;
  token: string;
  device_type: 'web' | 'android' | 'ios';
  is_active: boolean;
  created_at: string;
}
