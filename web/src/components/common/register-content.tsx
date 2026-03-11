'use client';

import { useState, useEffect } from 'react';
import { useAuth } from '@/store/auth-store';
import { useSearchParams } from 'next/navigation';
import Link from 'next/link';
import { Mail, Lock, User, Phone, MapPin, CheckCircle2, ArrowLeft, Luggage, Home } from 'lucide-react';
import { toast } from 'react-hot-toast';
import { Input, Button } from '@/components/ui';
import { validateEmail, validatePassword, validatePhoneNumber } from '@/lib/validation';
import { getCountriesByContext, getUserCountryByLocation } from '@/lib/countries';
import SocialAuthButtons from '@/components/auth/SocialAuthButtons';
import { AuthDivider } from '@/components/auth/AuthDivider';
import { AuthFooter } from '@/components/auth/AuthFooter';

type Step = 1 | 2 | 3;

function getRedirectUrl(searchParams: URLSearchParams): string {
  const redirect = searchParams.get('redirect');
  if (redirect && redirect.startsWith('/')) {
    return redirect;
  }
  return '/dashboard';
}

/**
 * Parse backend error response into user-friendly toast messages.
 * Backend returns errors like: { email: ["message"], password: ["message"], non_field_errors: ["message"] }
 */
function parseBackendErrors(errorData: Record<string, unknown>): string[] {
  const messages: string[] = [];

  if (typeof errorData === 'string') {
    return [errorData];
  }

  for (const [field, value] of Object.entries(errorData)) {
    if (field === 'detail' && typeof value === 'string') {
      messages.push(value);
    } else if (Array.isArray(value)) {
      const fieldLabel = field === 'non_field_errors'
        ? ''
        : `${field.replace(/_/g, ' ').replace(/\b\w/g, (c) => c.toUpperCase())}: `;
      for (const msg of value) {
        messages.push(`${fieldLabel}${msg}`);
      }
    } else if (typeof value === 'string') {
      const fieldLabel = field === 'non_field_errors'
        ? ''
        : `${field.replace(/_/g, ' ').replace(/\b\w/g, (c) => c.toUpperCase())}: `;
      messages.push(`${fieldLabel}${value}`);
    }
  }

  return messages.length > 0 ? messages : ['Registration failed. Please try again.'];
}

export function RegisterContent() {
  const { register } = useAuth();
  const searchParams = useSearchParams();
  const redirectUrl = getRedirectUrl(searchParams);
  const [isLoading, setIsLoading] = useState(false);
  const [currentStep, setCurrentStep] = useState<Step>(1);
  const [availableCountries, setAvailableCountries] = useState<string[]>([]);
  const [formData, setFormData] = useState({
    // Step 1: Personal Information
    first_name: '',
    last_name: '',
    phone_number: '',
    // Step 2: Account Credentials
    email: '',
    password: '',
    confirmPassword: '',
    // Step 3: Additional Details
    country_of_residence: '',
    role: 'guest' as 'guest' | 'host',
  });
  const [errors, setErrors] = useState<Record<string, string>>({});

  // Update available countries based on role
  useEffect(() => {
    const countries = getCountriesByContext(formData.role);
    setAvailableCountries(countries);
  }, [formData.role]);

  // Get user's current location if they select "host"
  useEffect(() => {
    if (formData.role === 'host') {
      getUserCountryByLocation().then((country) => {
        if (country && availableCountries.includes(country)) {
          setFormData((prev) => ({ ...prev, country_of_residence: country }));
        }
      });
    }
  }, [formData.role, availableCountries]);

  const validateStep1 = () => {
    const newErrors: Record<string, string> = {};

    if (!formData.first_name.trim()) {
      newErrors.first_name = 'First name is required';
    } else if (formData.first_name.trim().length < 2) {
      newErrors.first_name = 'First name must be at least 2 characters';
    } else if (formData.first_name.trim().length > 50) {
      newErrors.first_name = 'First name must be 50 characters or less';
    }

    if (!formData.last_name.trim()) {
      newErrors.last_name = 'Last name is required';
    } else if (formData.last_name.trim().length < 2) {
      newErrors.last_name = 'Last name must be at least 2 characters';
    } else if (formData.last_name.trim().length > 50) {
      newErrors.last_name = 'Last name must be 50 characters or less';
    }

    if (!formData.phone_number.trim()) {
      newErrors.phone_number = 'Phone number is required';
    } else {
      const phoneResult = validatePhoneNumber(formData.phone_number);
      if (!phoneResult.isValid) {
        newErrors.phone_number = phoneResult.error || 'Please enter a valid phone number';
      }
    }

    setErrors(newErrors);
    if (Object.keys(newErrors).length > 0) {
      toast.error(Object.values(newErrors)[0]);
    }
    return Object.keys(newErrors).length === 0;
  };

  const validateStep2 = () => {
    const newErrors: Record<string, string> = {};

    if (!formData.email.trim()) {
      newErrors.email = 'Email address is required';
    } else {
      const emailResult = validateEmail(formData.email);
      if (!emailResult.isValid) {
        newErrors.email = emailResult.error || 'Please enter a valid email address';
      }
    }

    if (!formData.password) {
      newErrors.password = 'Password is required';
    } else {
      const passwordResult = validatePassword(formData.password);
      if (!passwordResult.isValid) {
        newErrors.password = passwordResult.error || 'Password must be at least 8 characters';
      }
    }

    if (!formData.confirmPassword) {
      newErrors.confirmPassword = 'Please confirm your password';
    } else if (formData.password !== formData.confirmPassword) {
      newErrors.confirmPassword = 'Passwords do not match';
    }

    setErrors(newErrors);
    if (Object.keys(newErrors).length > 0) {
      toast.error(Object.values(newErrors)[0]);
    }
    return Object.keys(newErrors).length === 0;
  };

  const validateStep3 = () => {
    const newErrors: Record<string, string> = {};

    if (!formData.country_of_residence) {
      newErrors.country_of_residence = 'Please select your country';
    }

    setErrors(newErrors);
    if (Object.keys(newErrors).length > 0) {
      toast.error(Object.values(newErrors)[0]);
    }
    return Object.keys(newErrors).length === 0;
  };

  const handleBack = () => {
    if (currentStep > 1) {
      setCurrentStep((currentStep - 1) as Step);
    }
  };

  const handleNext = () => {
    let isValid = false;
    switch (currentStep) {
      case 1:
        isValid = validateStep1();
        break;
      case 2:
        isValid = validateStep2();
        break;
      case 3:
        isValid = validateStep3();
        break;
    }
    if (isValid && currentStep < 3) {
      setCurrentStep((currentStep + 1) as Step);
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!validateStep3()) {
      return;
    }
    setIsLoading(true);
    try {
      await register({
        email: formData.email,
        password: formData.password,
        first_name: formData.first_name,
        last_name: formData.last_name,
        phone_number: formData.phone_number,
        country_of_residence: formData.country_of_residence,
        role: formData.role,
      });
      toast.success('Account created successfully!');
      // Force a full-page navigation so middleware/SSR see the new cookie
      window.location.replace(redirectUrl);
    } catch (error: unknown) {
      // Parse field-specific errors from the backend
      if (error && typeof error === 'object' && 'fieldErrors' in error) {
        const fieldErrors = (error as { fieldErrors: Record<string, unknown> }).fieldErrors;
        const messages = parseBackendErrors(fieldErrors);
        messages.forEach((msg) => toast.error(msg));
      } else if (error instanceof Error) {
        toast.error(error.message || 'Registration failed. Please try again.');
      } else {
        toast.error('Registration failed. Please try again.');
      }
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 py-8 px-4 sm:py-12 sm:px-6 lg:px-8">
      <div className="max-w-lg mx-auto">
        {/* Logo */}
        <div className="text-center mb-6">
          <Link href="/" className="inline-flex items-center justify-center">
            <img src="/logo.png" alt="StayAfrica" className="h-16 sm:h-20 w-auto" />
          </Link>
        </div>

        <div className="bg-white rounded-2xl shadow-elevated border border-primary-100 p-5 sm:p-8">
          {/* Step indicator */}
          <div className="flex items-center justify-center gap-2 mb-4">
            {[1, 2, 3].map((step) => (
              <div key={step} className="flex items-center">
                <div
                  className={`w-8 h-8 sm:w-9 sm:h-9 rounded-full border-2 flex items-center justify-center text-sm font-semibold transition ${currentStep > step
                      ? 'border-secondary-500 bg-secondary-500 text-white'
                      : currentStep === step
                        ? 'border-secondary-500 bg-secondary-50 text-secondary-700'
                        : 'border-primary-200 text-primary-400'
                    }`}
                  aria-label={`Step ${step}`}
                >
                  {currentStep > step ? <CheckCircle2 className="w-5 h-5" /> : step}
                </div>
                {step < 3 && (
                  <div
                    className={`w-8 sm:w-12 h-0.5 mx-1.5 sm:mx-2 rounded-full transition ${currentStep > step ? 'bg-secondary-500' : 'bg-primary-200'
                      }`}
                  />
                )}
              </div>
            ))}
          </div>

          <h1 className="text-2xl sm:text-3xl font-bold mb-1 text-center text-primary-900">
            Create Your Account
          </h1>
          <p className="text-center text-sm text-primary-500 mb-6">
            Step {currentStep} of 3 &middot; {
              currentStep === 1 ? 'Personal Information' :
                currentStep === 2 ? 'Account Credentials' :
                  'Additional Details'
            }
          </p>

          <form
            onSubmit={(e) => {
              e.preventDefault();
              if (currentStep === 3) {
                handleSubmit(e);
              } else {
                handleNext();
              }
            }}
            className="space-y-5"
          >
            {/* Step 1: Personal Info (was Step 2) */}
            {currentStep === 1 && (
              <>
                <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
                  <Input
                    label="First Name"
                    type="text"
                    value={formData.first_name}
                    onChange={(e) => setFormData({ ...formData, first_name: e.target.value })}
                    placeholder="John"
                    error={errors.first_name}
                    icon={<User className="w-5 h-5" />}
                    required
                  />

                  <Input
                    label="Last Name"
                    type="text"
                    value={formData.last_name}
                    onChange={(e) => setFormData({ ...formData, last_name: e.target.value })}
                    placeholder="Doe"
                    error={errors.last_name}
                    icon={<User className="w-5 h-5" />}
                    required
                  />
                </div>

                <Input
                  label="Phone Number"
                  type="tel"
                  value={formData.phone_number}
                  onChange={(e) => setFormData({ ...formData, phone_number: e.target.value })}
                  placeholder="+1 (555) 123-4567"
                  error={errors.phone_number}
                  icon={<Phone className="w-5 h-5" />}
                  required
                />
              </>
            )}

            {/* Step 2: Credentials (was Step 1) */}
            {currentStep === 2 && (
              <>
                <Input
                  label="Email Address"
                  type="email"
                  value={formData.email}
                  onChange={(e) => setFormData({ ...formData, email: e.target.value })}
                  placeholder="you@example.com"
                  error={errors.email}
                  icon={<Mail className="w-5 h-5" />}
                  required
                />

                <Input
                  label="Password"
                  type="password"
                  value={formData.password}
                  onChange={(e) => setFormData({ ...formData, password: e.target.value })}
                  placeholder="••••••••"
                  error={errors.password}
                  icon={<Lock className="w-5 h-5" />}
                  required
                />

                <Input
                  label="Confirm Password"
                  type="password"
                  value={formData.confirmPassword}
                  onChange={(e) => setFormData({ ...formData, confirmPassword: e.target.value })}
                  placeholder="••••••••"
                  error={errors.confirmPassword}
                  icon={<Lock className="w-5 h-5" />}
                  required
                />
              </>
            )}

            {/* Step 3: Additional Details */}
            {currentStep === 3 && (
              <>
                <Input
                  label="Country of Residence"
                  select
                  options={[
                    { value: '', label: 'Select your country' },
                    ...availableCountries.map((c) => ({ value: c, label: c })),
                  ]}
                  value={formData.country_of_residence}
                  onChange={(e) => setFormData({ ...formData, country_of_residence: e.target.value })}
                  error={errors.country_of_residence}
                  icon={<MapPin className="w-5 h-5" />}
                  required
                />

                <div>
                  <label className="block text-sm font-medium text-primary-900 mb-3">
                    I want to
                  </label>
                  <div className="grid grid-cols-2 gap-3">
                    <button
                      type="button"
                      onClick={() => setFormData({ ...formData, role: 'guest' })}
                      className={`p-3 sm:p-4 border-2 rounded-xl transition-all ${formData.role === 'guest'
                          ? 'border-secondary-500 bg-secondary-50 shadow-sm'
                          : 'border-primary-200 hover:border-primary-300'
                        }`}
                    >
                      <div className="text-center">
                        <div className="flex justify-center mb-2">
                          <Luggage className="w-7 h-7 sm:w-8 sm:h-8 text-secondary-600" />
                        </div>
                        <div className="font-semibold text-sm sm:text-base text-primary-900">Book Stays</div>
                        <div className="text-xs sm:text-sm text-primary-500 mt-0.5">Find places to stay</div>
                      </div>
                    </button>
                    <button
                      type="button"
                      onClick={() => setFormData({ ...formData, role: 'host' })}
                      className={`p-3 sm:p-4 border-2 rounded-xl transition-all ${formData.role === 'host'
                          ? 'border-secondary-500 bg-secondary-50 shadow-sm'
                          : 'border-primary-200 hover:border-primary-300'
                        }`}
                    >
                      <div className="text-center">
                        <div className="flex justify-center mb-2">
                          <Home className="w-7 h-7 sm:w-8 sm:h-8 text-secondary-600" />
                        </div>
                        <div className="font-semibold text-sm sm:text-base text-primary-900">Host Properties</div>
                        <div className="text-xs sm:text-sm text-primary-500 mt-0.5">List my property</div>
                      </div>
                    </button>
                  </div>
                </div>
              </>
            )}

            {/* Navigation Buttons */}
            <div className="flex gap-3 pt-2">
              {currentStep > 1 && (
                <Button
                  type="button"
                  onClick={handleBack}
                  variant="outline"
                  size="lg"
                  className="flex-1"
                >
                  <ArrowLeft className="w-4 h-4 mr-1" />
                  Back
                </Button>
              )}

              <Button
                type="submit"
                variant="secondary"
                size="lg"
                fullWidth={currentStep === 1}
                isLoading={isLoading}
                disabled={isLoading}
                className={currentStep > 1 ? 'flex-1' : ''}
              >
                {currentStep === 3 ? 'Create Account' : 'Continue'}
              </Button>
            </div>
          </form>

          {/* Social Auth (only on step 1) */}
          {currentStep === 1 && (
            <>
              <AuthDivider text="or" />
              <SocialAuthButtons mode="signup" redirectUrl={redirectUrl} role={formData.role} />
            </>
          )}

          {/* Sign In Link */}
          <div className="mt-6 text-center">
            <p className="text-sm text-primary-500">
              Already have an account?{' '}
              <Link href={redirectUrl !== '/dashboard' ? `/login?redirect=${encodeURIComponent(redirectUrl)}` : '/login'} className="text-secondary-600 hover:text-secondary-700 font-semibold">
                Sign In
              </Link>
            </p>
          </div>
        </div>

        <AuthFooter action="signup" />
      </div>
    </div>
  );
}
