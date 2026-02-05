/**
 * Design Tokens - Centralized design system for StayAfrica
 * Colors, spacing, typography, shadows, animations
 */

export const colors = {
  // Primary - Moss green
  primary: {
    50: '#f2f5f4',
    100: '#e3ece7',
    200: '#c5d8cf',
    300: '#a2bcaf',
    400: '#789c8d',
    500: '#3a5c50',
    600: '#2d4a40',
    700: '#1d392f',
    800: '#122f26',
    900: '#0a1a15',
  },
  // Secondary - Gold/Amber
  secondary: {
    50: '#fdf7ec',
    100: '#f8e8c4',
    200: '#f3d99c',
    300: '#edcb74',
    400: '#e7bd4d',
    500: '#d9b168',
    600: '#bea04f',
    700: '#967c38',
    800: '#705a28',
    900: '#4c3a18',
  },
  // Neutrals - Sand
  sand: {
    50: '#fbf7f1',
    100: '#f4f1ea',
    200: '#e5dfd0',
    300: '#d6ceb7',
    400: '#c5bb9d',
    500: '#b4a683',
    600: '#968665',
    700: '#77674b',
    800: '#574933',
    900: '#392d1f',
  },
  // Semantic colors
  success: {
    50: '#f0fdf4',
    500: '#22c55e',
    600: '#16a34a',
    700: '#15803d',
  },
  error: {
    50: '#fef2f2',
    500: '#ef4444',
    600: '#dc2626',
    700: '#b91c1c',
  },
  warning: {
    50: '#fffbeb',
    500: '#f59e0b',
    600: '#d97706',
    700: '#b45309',
  },
  info: {
    50: '#f0f9ff',
    500: '#3b82f6',
    600: '#2563eb',
    700: '#1d4ed8',
  },
  // Neutrals - Gray
  neutral: {
    0: '#ffffff',
    50: '#f9fafb',
    100: '#f3f4f6',
    200: '#e5e7eb',
    300: '#d1d5db',
    400: '#9ca3af',
    500: '#6b7280',
    600: '#4b5563',
    700: '#374151',
    800: '#1f2937',
    900: '#111827',
  },
};

export const spacing = {
  0: '0',
  1: '0.25rem', // 4px
  2: '0.5rem', // 8px
  3: '0.75rem', // 12px
  4: '1rem', // 16px
  5: '1.25rem', // 20px
  6: '1.5rem', // 24px
  8: '2rem', // 32px
  10: '2.5rem', // 40px
  12: '3rem', // 48px
  16: '4rem', // 64px
  20: '5rem', // 80px
};

export const typography = {
  fontSize: {
    xs: ['0.75rem', { lineHeight: '1rem' }], // 12px
    sm: ['0.875rem', { lineHeight: '1.25rem' }], // 14px
    base: ['1rem', { lineHeight: '1.5rem' }], // 16px
    lg: ['1.125rem', { lineHeight: '1.75rem' }], // 18px
    xl: ['1.25rem', { lineHeight: '1.75rem' }], // 20px
    '2xl': ['1.5rem', { lineHeight: '2rem' }], // 24px
    '3xl': ['1.875rem', { lineHeight: '2.25rem' }], // 30px
    '4xl': ['2.25rem', { lineHeight: '2.5rem' }], // 36px
  },
  fontWeight: {
    light: 300,
    normal: 400,
    medium: 500,
    semibold: 600,
    bold: 700,
  },
};

export const shadows = {
  none: 'none',
  'sm': '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
  'base': '0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)',
  'md': '0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)',
  'lg': '0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05)',
  'xl': '0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04)',
  'elevated': '0 20px 40px rgba(0, 0, 0, 0.15)',
  'card': '0 4px 12px rgba(0, 0, 0, 0.08)',
  'hover': '0 8px 20px rgba(0, 0, 0, 0.12)',
};

export const borderRadius = {
  none: '0',
  sm: '0.25rem', // 4px
  base: '0.5rem', // 8px
  md: '0.75rem', // 12px
  lg: '1rem', // 16px
  xl: '1.25rem', // 20px
  '2xl': '1.5rem',
  '3xl': '1.875rem',
  full: '9999px',
};

export const transitions = {
  fast: '150ms cubic-bezier(0.4, 0, 0.2, 1)',
  base: '200ms cubic-bezier(0.4, 0, 0.2, 1)',
  slow: '300ms cubic-bezier(0.4, 0, 0.2, 1)',
};

export const animations = {
  fadeIn: {
    duration: 300,
    easing: 'cubic-bezier(0.4, 0, 1, 1)',
  },
  slideIn: {
    duration: 300,
    easing: 'cubic-bezier(0.4, 0, 0.2, 1)',
  },
  slideUp: {
    duration: 300,
    easing: 'cubic-bezier(0.4, 0, 0.2, 1)',
  },
  pulse: {
    duration: 2000,
    easing: 'cubic-bezier(0.4, 0, 0.6, 1)',
  },
};

export const zIndex = {
  hide: -1,
  auto: 'auto',
  base: 0,
  dropdown: 1000,
  sticky: 1020,
  fixed: 1030,
  modal: 1040,
  popover: 1050,
  tooltip: 1060,
};

// Size breakpoints
export const breakpoints = {
  xs: '480px',
  sm: '640px',
  md: '768px',
  lg: '1024px',
  xl: '1280px',
  '2xl': '1536px',
};
