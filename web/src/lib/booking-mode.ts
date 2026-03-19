import { User } from '@/types';

export function isHostMode(user?: Pick<User, 'role' | 'active_profile'> | null): boolean {
  return !!user && (user.role === 'host' || user.role === 'admin') && user.active_profile === 'host';
}

export function isOwnPropertyBooking(userId?: string | number | null, hostId?: string | number | null): boolean {
  return userId != null && hostId != null && String(userId) === String(hostId);
}

export async function confirmSwitchToTravelMode(options: {
  switchProfile: (mode: 'guest' | 'host') => Promise<void>;
  propertyTitle?: string;
}) {
  const propertySuffix = options.propertyTitle ? ` for ${options.propertyTitle}` : '';
  const confirmed = window.confirm(
    `You are currently in Hosting mode. Switch to Traveling mode to continue booking${propertySuffix}?`
  );

  if (!confirmed) {
    return false;
  }

  await options.switchProfile('guest');
  return true;
}