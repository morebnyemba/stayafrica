/**
 * i18n configuration for StayAfrica.
 * Supports English, French, and Zulu.
 * Uses cookie-based locale detection (no URL prefix routing).
 */

export const locales = ['en', 'fr', 'zu'] as const;
export type Locale = (typeof locales)[number];
export const defaultLocale: Locale = 'en';

export const localeNames: Record<Locale, string> = {
  en: 'English',
  fr: 'Français',
  zu: 'isiZulu',
};
