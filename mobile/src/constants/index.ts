export const API_ENDPOINTS = {
  // Auth
  LOGIN: '/auth/login/',
  REGISTER: '/auth/register/',
  REFRESH: '/auth/refresh/',
  LOGOUT: '/auth/logout/',

  // Properties
  PROPERTIES: '/properties/',
  PROPERTY_DETAIL: (id: string) => `/properties/${id}/`,

  // Bookings
  BOOKINGS: '/bookings/',
  BOOKING_DETAIL: (id: string) => `/bookings/${id}/`,
  CANCEL_BOOKING: (id: string) => `/bookings/${id}/cancel/`,

  // Users
  USER_PROFILE: '/users/profile/',

  // Messages
  CONVERSATIONS: '/messaging/conversations/',
  MESSAGES: (conversationId: string) =>
    `/messaging/conversations/${conversationId}/messages/`,

  // Reviews
  REVIEWS: '/reviews/',
};

export const COLORS = {
  primary: '#122f26',
  primaryDark: '#0a1a15',
  primaryLight: '#e3ece7',
  secondary: '#d9b168',
  secondaryDark: '#967c38',
  secondaryLight: '#f8e8c4',
  accent: '#3a5c50',
  background: '#f4f1ea',
  surface: '#ffffff',
  text: '#0a1a15',
  mutedText: '#3a5c50',
  success: '#10b981',
  error: '#ef4444',
  warning: '#f59e0b',
  gray50: '#f9fafb',
  gray100: '#f3f4f6',
  gray200: '#e5e7eb',
  gray300: '#d1d5db',
  gray400: '#9ca3af',
  gray500: '#6b7280',
  gray600: '#4b5563',
  gray700: '#374151',
  gray800: '#1f2937',
  gray900: '#111827',
  // Status colors (matching Tailwind)
  yellow100: '#fef3c7',
  yellow800: '#92400e',
  green100: '#dcfce7',
  green800: '#166534',
  blue100: '#dbeafe',
  blue800: '#1e40af',
  red100: '#fee2e2',
  red800: '#991b1b',
};

export const SIZES = {
  xs: 4,
  sm: 8,
  md: 12,
  lg: 16,
  xl: 24,
  xxl: 32,
  xxxl: 48,
};

export const BORDERS = {
  radius: {
    sm: 4,
    md: 8,
    lg: 12,
    xl: 16,
    full: 9999,
  },
};

export const STORAGE_KEYS = {
  ACCESS_TOKEN: 'accessToken',
  REFRESH_TOKEN: 'refreshToken',
  USER_PREFERENCES: 'userPreferences',
  FAVORITES: 'favorites',
  PUSH_TOKEN: 'pushToken',
};
