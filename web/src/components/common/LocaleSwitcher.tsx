'use client';

import { useTransition } from 'react';
import { useLocale } from 'next-intl';
import { locales, localeNames, type Locale } from '@/i18n/config';
import { setUserLocale } from '@/i18n/locale';

export function LocaleSwitcher() {
  const locale = useLocale();
  const [isPending, startTransition] = useTransition();

  function onChange(value: string) {
    const newLocale = value as Locale;
    startTransition(async () => {
      await setUserLocale(newLocale);
      window.location.reload();
    });
  }

  return (
    <select
      value={locale}
      onChange={(e) => onChange(e.target.value)}
      disabled={isPending}
      className="rounded-md border border-gray-300 bg-white px-2 py-1 text-sm text-gray-700 focus:border-primary-500 focus:outline-none focus:ring-1 focus:ring-primary-500 disabled:opacity-50"
      aria-label="Select language"
    >
      {locales.map((loc) => (
        <option key={loc} value={loc}>
          {localeNames[loc]}
        </option>
      ))}
    </select>
  );
}
