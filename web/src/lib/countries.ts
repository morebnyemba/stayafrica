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

// World countries list for guest registration
export const WORLD_COUNTRIES = [
  ...AFRICAN_COUNTRIES,
  { code: 'US', name: 'United States' },
  { code: 'GB', name: 'United Kingdom' },
  { code: 'CA', name: 'Canada' },
  { code: 'AU', name: 'Australia' },
  { code: 'NZ', name: 'New Zealand' },
  { code: 'DE', name: 'Germany' },
  { code: 'FR', name: 'France' },
  { code: 'IT', name: 'Italy' },
  { code: 'ES', name: 'Spain' },
  { code: 'NL', name: 'Netherlands' },
  { code: 'SE', name: 'Sweden' },
  { code: 'NO', name: 'Norway' },
  { code: 'DK', name: 'Denmark' },
  { code: 'BE', name: 'Belgium' },
  { code: 'CH', name: 'Switzerland' },
  { code: 'AT', name: 'Austria' },
  { code: 'PL', name: 'Poland' },
  { code: 'CZ', name: 'Czech Republic' },
  { code: 'IE', name: 'Ireland' },
  { code: 'JP', name: 'Japan' },
  { code: 'CN', name: 'China' },
  { code: 'IN', name: 'India' },
  { code: 'BR', name: 'Brazil' },
  { code: 'MX', name: 'Mexico' },
  { code: 'AR', name: 'Argentina' },
  { code: 'SG', name: 'Singapore' },
  { code: 'MY', name: 'Malaysia' },
  { code: 'TH', name: 'Thailand' },
  { code: 'PH', name: 'Philippines' },
  { code: 'ID', name: 'Indonesia' },
  { code: 'VN', name: 'Vietnam' },
  { code: 'KR', name: 'South Korea' },
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

export function getCountriesByContext(role: 'guest' | 'host'): string[] {
  // Hosts can only operate in target countries
  // Guests can be from any country in the world
  if (role === 'host') {
    return TARGET_COUNTRIES.map(c => c.name);
  }
  return WORLD_COUNTRIES.map(c => c.name);
}

export async function getUserCountryByLocation(): Promise<string | null> {
  try {
    // Try to get user's location using IP geolocation
    const response = await fetch('https://ipapi.co/json/');
    const data = await response.json();
    
    // Check if country is in our target countries
    const countryCode = data.country_code;
    const country = TARGET_COUNTRIES.find(c => c.code === countryCode);
    return country?.name || null;
  } catch (error) {
    console.error('Failed to get user location:', error);
    return null;
  }
}
