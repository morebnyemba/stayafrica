export function validateEmail(email: string): { isValid: boolean; error?: string } {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  if (!emailRegex.test(email)) {
    return { isValid: false, error: 'Please enter a valid email address' };
  }
  return { isValid: true };
}

export function validatePassword(password: string): { isValid: boolean; error?: string } {
  if (password.length < 8) {
    return { isValid: false, error: 'Password must be at least 8 characters long' };
  }
  if (!/[A-Z]/.test(password)) {
    return { isValid: false, error: 'Password must contain at least one uppercase letter' };
  }
  if (!/[a-z]/.test(password)) {
    return { isValid: false, error: 'Password must contain at least one lowercase letter' };
  }
  if (!/[0-9]/.test(password)) {
    return { isValid: false, error: 'Password must contain at least one number' };
  }
  return { isValid: true };
}

export function validatePhoneNumber(phone: string): { isValid: boolean; error?: string } {
  // More flexible pattern that allows various international formats
  // Allows: +, optional country code (1-3 digits), and 9-15 total digits
  const cleaned = phone.replace(/[\s\-\(\)]/g, '');
  const phoneRegex = /^\+?[0-9]{9,15}$/;
  if (!phoneRegex.test(cleaned)) {
    return { isValid: false, error: 'Please enter a valid phone number' };
  }
  return { isValid: true };
}
