/**
 * Form Validation Utilities
 */

export interface ValidationRule {
  required?: { message?: string };
  minLength?: { value: number; message?: string };
  maxLength?: { value: number; message?: string };
  pattern?: { value: RegExp; message?: string };
  validate?: (value: any) => boolean | string;
}

export interface FormErrors {
  [key: string]: string;
}

export const validateField = (value: any, rules: ValidationRule): string => {
  if (rules.required && (!value || value.toString().trim() === '')) {
    return rules.required.message || 'This field is required';
  }

  if (value && rules.minLength && value.length < rules.minLength.value) {
    return rules.minLength.message || `Must be at least ${rules.minLength.value} characters`;
  }

  if (value && rules.maxLength && value.length > rules.maxLength.value) {
    return rules.maxLength.message || `Must be no more than ${rules.maxLength.value} characters`;
  }

  if (value && rules.pattern && !rules.pattern.value.test(value)) {
    return rules.pattern.message || 'Invalid format';
  }

  if (value && rules.validate) {
    const result = rules.validate(value);
    if (result !== true) {
      return typeof result === 'string' ? result : 'Invalid value';
    }
  }

  return '';
};

export const validateForm = (
  data: Record<string, any>,
  schema: Record<string, ValidationRule>
): FormErrors => {
  const errors: FormErrors = {};

  for (const [field, rules] of Object.entries(schema)) {
    const error = validateField(data[field], rules);
    if (error) {
      errors[field] = error;
    }
  }

  return errors;
};

// Common validation patterns
export const patterns = {
  email: /^[^\s@]+@[^\s@]+\.[^\s@]+$/,
  phone: /^[+]?[(]?[0-9]{3}[)]?[-\s.]?[0-9]{3}[-\s.]?[0-9]{4,6}$/,
  url: /^(https?:\/\/)?(www\.)?[-a-zA-Z0-9@:%._+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_+.~#?&/=]*)$/,
  password: /^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,}$/,
};

// Simple boolean validators for legacy callers
export const validateEmail = (email: string): boolean => patterns.email.test(email);

export const validatePassword = (password: string): boolean => password.length >= 8;

export const validatePhoneNumber = (phone: string): boolean => {
  const cleaned = phone.replace(/[\s\-()]/g, '');
  return /^\+?[0-9]{9,15}$/.test(cleaned);
};

// Common validators
export const validators = {
  email: (value: string): boolean | string => {
    if (!patterns.email.test(value)) {
      return 'Please enter a valid email address';
    }
    return true;
  },

  phone: (value: string): boolean | string => {
    if (!patterns.phone.test(value)) {
      return 'Please enter a valid phone number';
    }
    return true;
  },

  password: (value: string): boolean | string => {
    if (!patterns.password.test(value)) {
      return 'Password must contain uppercase, lowercase, number, and special character';
    }
    return true;
  },

  confirmPassword: (confirmPassword: string, password: string): boolean | string => {
    if (confirmPassword !== password) {
      return 'Passwords do not match';
    }
    return true;
  },

  age: (value: number): boolean | string => {
    if (value < 18) {
      return 'You must be at least 18 years old';
    }
    return true;
  },

  url: (value: string): boolean | string => {
    if (!patterns.url.test(value)) {
      return 'Please enter a valid URL';
    }
    return true;
  },
};
