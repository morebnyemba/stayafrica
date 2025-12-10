export const AFRICAN_COUNTRIES = [
  { code: 'ZW', name: 'Zimbabwe' },
  { code: 'ZA', name: 'South Africa' },
  { code: 'BW', name: 'Botswana' },
  { code: 'NA', name: 'Namibia' },
  { code: 'ZM', name: 'Zambia' },
  { code: 'KE', name: 'Kenya' },
  { code: 'TZ', name: 'Tanzania' },
  { code: 'UG', name: 'Uganda' },
  { code: 'MW', name: 'Malawi' },
  { code: 'MZ', name: 'Mozambique' },
  { code: 'NG', name: 'Nigeria' },
  { code: 'GH', name: 'Ghana' },
  { code: 'EG', name: 'Egypt' },
  { code: 'MA', name: 'Morocco' },
  { code: 'ET', name: 'Ethiopia' },
];

export const TARGET_COUNTRIES = [
  { code: 'ZW', name: 'Zimbabwe' },
  { code: 'ZA', name: 'South Africa' },
  { code: 'BW', name: 'Botswana' },
  { code: 'NA', name: 'Namibia' },
  { code: 'ZM', name: 'Zambia' },
];

export function getCountryName(code: string): string {
  const country = AFRICAN_COUNTRIES.find(c => c.code === code);
  return country?.name || code;
}

export function getCountryCode(name: string): string {
  const country = AFRICAN_COUNTRIES.find(c => c.name === name);
  return country?.code || name;
}
