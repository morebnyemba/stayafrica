const EUROPEAN_DATE_LOCALE = 'en-GB';

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

  return date
    .toLocaleDateString(EUROPEAN_DATE_LOCALE, {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric',
    })
    .replace(/\//g, '-');
}

export function installEuropeanDateFormatting(): void {
  if (typeof window === 'undefined') {
    return;
  }

  const patchedFlag = '__stayAfricaEuropeanDatePatched';
  const protoWithFlag = Date.prototype as Date & { [key: string]: unknown };

  if (protoWithFlag[patchedFlag]) {
    return;
  }

  const toEuropeanDateString = function (this: Date): string {
    return formatEuropeanDate(this);
  };

  Object.defineProperty(Date.prototype, 'toLocaleDateString', {
    value: toEuropeanDateString,
    configurable: true,
    writable: true,
  });

  protoWithFlag[patchedFlag] = true;
}
