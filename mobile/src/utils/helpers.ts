import { Alert } from 'react-native';

type ProfileModeUser = {
  id?: string | number | null;
  role?: string | null;
  active_profile?: string | null;
};

// Common utility functions
export function formatCurrency(amount: number, currency: string = 'USD'): string {
  return new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency,
  }).format(amount);
}

export function isHostMode(user?: ProfileModeUser | null): boolean {
  return !!user && (user.role === 'host' || user.role === 'admin') && user.active_profile === 'host';
}

export function isOwnPropertyBooking(userId?: string | number | null, hostId?: string | number | null): boolean {
  return userId != null && hostId != null && String(userId) === String(hostId);
}

export function promptSwitchToTravelMode(options: {
  onConfirm: () => void | Promise<void>;
  onCancel?: () => void;
  propertyTitle?: string;
}) {
  const propertySuffix = options.propertyTitle ? ` for ${options.propertyTitle}` : '';

  Alert.alert(
    'Switch to Travel Mode',
    `You are currently in Hosting mode. Switch to Traveling mode to continue booking${propertySuffix}?`,
    [
      {
        text: 'Cancel',
        style: 'cancel',
        onPress: options.onCancel,
      },
      {
        text: 'Switch Mode',
        onPress: () => {
          void options.onConfirm();
        },
      },
    ]
  );
}

export function formatDate(date: string | Date): string {
  return new Date(date).toLocaleDateString('en-US', {
    year: 'numeric',
    month: 'short',
    day: 'numeric',
  });
}

export function formatTime(date: string | Date): string {
  return new Date(date).toLocaleTimeString('en-US', {
    hour: '2-digit',
    minute: '2-digit',
  });
}

export function calculateDaysUntil(date: string | Date): number {
  const now = new Date();
  const target = new Date(date);
  const diff = target.getTime() - now.getTime();
  return Math.ceil(diff / (1000 * 60 * 60 * 24));
}

export function validateEmail(email: string): boolean {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  return emailRegex.test(email);
}

export function validatePassword(password: string): boolean {
  // At least 8 characters, 1 uppercase, 1 number
  const passwordRegex = /^(?=.*[A-Z])(?=.*\d).{8,}$/;
  return passwordRegex.test(password);
}

export function truncateString(str: string, length: number): string {
  return str.length > length ? str.substring(0, length) + '...' : str;
}

export function debounce<T extends (...args: any[]) => any>(
  func: T,
  wait: number
): (...args: Parameters<T>) => void {
  let timeout: NodeJS.Timeout;

  return function executedFunction(...args: Parameters<T>) {
    const later = () => {
      clearTimeout(timeout);
      func(...args);
    };

    clearTimeout(timeout);
    timeout = setTimeout(later, wait);
  };
}
