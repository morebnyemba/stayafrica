const EUROPEAN_DATE_LOCALE = 'en-GB';
const europeanDateFormatter = new Intl.DateTimeFormat(EUROPEAN_DATE_LOCALE, {
  day: '2-digit',
  month: '2-digit',
  year: 'numeric',
});

function isValidDateInstance(value: Date): boolean {
  return Number.isFinite(value.getTime());
}

export function parseDate(value: Date | string | number): Date | null {
  const date = value instanceof Date ? value : new Date(value);
  return isValidDateInstance(date) ? date : null;
}

export function formatEuropeanDate(value: Date | string | number): string {
  const date = parseDate(value);
  if (!date) {
    return 'Invalid Date';
  }

  return europeanDateFormatter.format(date).replace(/\//g, '-');
}

export function installEuropeanDateFormatting(): void {
  // Intentionally a no-op.
  // Overriding Date.prototype.toLocaleDateString caused app-wide recursion and runtime crashes.
  // Use formatEuropeanDate explicitly or component-level picker/date configuration instead.
}
