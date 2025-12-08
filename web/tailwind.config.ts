import type { Config } from 'tailwindcss';

const config: Config = {
  content: [
    './src/pages/**/*.{js,ts,jsx,tsx,mdx}',
    './src/components/**/*.{js,ts,jsx,tsx,mdx}',
    './src/app/**/*.{js,ts,jsx,tsx,mdx}',
  ],
  theme: {
    extend: {
      colors: {
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
        moss: '#3a5c50',
        forest: '#122f26',
        savanna: '#0a1a15',
        gold: '#d9b168',
        ivory: '#ffffff',
      },
      spacing: {
        '128': '32rem',
        '144': '36rem',
      },
      borderRadius: {
        '3xl': '1.5rem',
      },
      boxShadow: {
        'lg-subtle': '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
        'elevated': '0 10px 15px -3px rgba(0, 0, 0, 0.1)',
      },
      animation: {
        'fade-in': 'fadeIn 0.5s ease-in-out',
        'slide-up': 'slideUp 0.3s ease-out',
      },
      keyframes: {
        fadeIn: {
          '0%': { opacity: '0' },
          '100%': { opacity: '1' },
        },
        slideUp: {
          '0%': { transform: 'translateY(20px)', opacity: '0' },
          '100%': { transform: 'translateY(0)', opacity: '1' },
        },
      },
    },
  },
  plugins: [],
};

export default config;
