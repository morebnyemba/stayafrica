'use client';

import { useState, useEffect } from 'react';
import { useAuth } from '@/store/auth-store';
import Link from 'next/link';
import { Mail, Lock, User, Phone, MapPin, CheckCircle2, ArrowLeft } from 'lucide-react';
import { toast } from 'react-hot-toast';
import { Input, Button } from '@/components/ui';
import { validateEmail, validatePassword, validatePhoneNumber } from '@/lib/validation';
import { getCountriesByContext, getUserCountryByLocation } from '@/lib/countries';
import SocialAuthButtons from '@/components/auth/SocialAuthButtons';
import { AuthDivider } from '@/components/auth/AuthDivider';
import { AuthFooter } from '@/components/auth/AuthFooter';

type Step = 1 | 2 | 3;

export function RegisterContent() {
  const { register } = useAuth();
  const [isLoading, setIsLoading] = useState(false);
  const [currentStep, setCurrentStep] = useState<Step>(1);
  const [availableCountries, setAvailableCountries] = useState<string[]>([]);
  const [formData, setFormData] = useState({
    // Step 1
    email: '',
    password: '',
    confirmPassword: '',
    // Step 2
    first_name: '',
    last_name: '',
    phone_number: '',
    // Step 3
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
    
    if (!validateEmail(formData.email)) {
      newErrors.email = 'Please enter a valid email address';
    }
    
    if (!validatePassword(formData.password)) {
      newErrors.password = 'Password must be at least 8 characters';
    }
    
    if (formData.password !== formData.confirmPassword) {
      newErrors.confirmPassword = 'Passwords do not match';
    }
    
    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const validateStep2 = () => {
    const newErrors: Record<string, string> = {};
    
    if (!formData.first_name.trim()) {
      newErrors.first_name = 'First name is required';
    }
    
    if (!formData.last_name.trim()) {
      newErrors.last_name = 'Last name is required';
    }
    
    if (!validatePhoneNumber(formData.phone_number)) {
      newErrors.phone_number = 'Please enter a valid phone number';
    }
    
    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const validateStep3 = () => {
    const newErrors: Record<string, string> = {};
    
    if (!formData.country_of_residence) {
      newErrors.country_of_residence = 'Country is required';
    }
    
    setErrors(newErrors);
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
      window.location.replace('/dashboard');
    } catch (error) {
      toast.error('Registration failed. Please try again.');
      console.error('Registration error:', error);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="min-h-screen bg-primary-50 dark:bg-primary-950 py-12 px-4 sm:px-6 lg:px-8">
      <div className="max-w-3xl mx-auto">
        <div className="flex items-center justify-between mb-6">
          <Link href="/" className="text-secondary-600 dark:text-secondary-400 hover:underline font-medium">
            Home
          </Link>
          <Link href="/login" className="text-secondary-600 dark:text-secondary-400 hover:underline font-medium">
            Have an account? Sign in
          </Link>
        </div>

        <div className="bg-white dark:bg-primary-900 rounded-2xl shadow-xl border border-primary-100 dark:border-primary-800 p-6 sm:p-10">
          <div className="flex items-center justify-center gap-2 mb-4 text-secondary-600 dark:text-secondary-400">
            {[1, 2, 3].map((step) => (
              <div key={step} className="flex items-center">
                <div
                  className={`w-9 h-9 rounded-full border-2 flex items-center justify-center text-sm font-semibold transition ${
                    currentStep > step
                      ? 'border-secondary-500 bg-secondary-50 text-secondary-700'
                      : currentStep === step
                        ? 'border-secondary-500 text-secondary-700'
                        : 'border-primary-200 dark:border-primary-700 text-primary-400'
                  }`}
                  aria-label={`Step ${step}`}
                >
                  {currentStep > step ? <CheckCircle2 className="w-5 h-5" /> : step}
                </div>
                {step < 3 && (
                  <div
                    className={`w-12 h-1 mx-2 rounded-full transition ${
                      currentStep > step ? 'bg-secondary-500' : 'bg-primary-200 dark:bg-primary-700'
                    }`}
                  />
                )}
              </div>
            ))}
          </div>

          <h1 className="text-3xl font-bold mb-2 text-center text-primary-900 dark:text-sand-50">
            Create Your Account
          </h1>
          <p className="text-center text-primary-600 dark:text-sand-300 mb-8">
            Step {currentStep} of 3: {
              currentStep === 1 ? 'Account Credentials' :
              currentStep === 2 ? 'Personal Information' :
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
            className="space-y-6"
          >
            {/* Step 1: Credentials */}
            {currentStep === 1 && (
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

            {/* Step 2: Personal Info */}
            {currentStep === 2 && (
              <>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
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
                  <label className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-3">
                    I want to
                  </label>
                  <div className="grid grid-cols-2 gap-4">
                    <button
                      type="button"
                      onClick={() => setFormData({ ...formData, role: 'guest' })}
                      className={`p-4 border-2 rounded-lg transition ${
                        formData.role === 'guest'
                          ? 'border-secondary-500 bg-secondary-50 dark:bg-secondary-900/20'
                          : 'border-primary-200 dark:border-primary-700'
                      }`}
                    >
                      <div className="text-center">
                        <div className="text-2xl mb-2">🏖️</div>
                        <div className="font-semibold text-primary-900 dark:text-sand-100">Book Stays</div>
                        <div className="text-sm text-primary-600 dark:text-sand-300">I&apos;m looking for places to stay</div>
                      </div>
                    </button>
                    <button
                      type="button"
                      onClick={() => setFormData({ ...formData, role: 'host' })}
                      className={`p-4 border-2 rounded-lg transition ${
                        formData.role === 'host'
                          ? 'border-secondary-500 bg-secondary-50 dark:bg-secondary-900/20'
                          : 'border-primary-200 dark:border-primary-700'
                      }`}
                    >
                      <div className="text-center">
                        <div className="text-2xl mb-2">🏡</div>
                        <div className="font-semibold text-primary-900 dark:text-sand-100">Host Properties</div>
                        <div className="text-sm text-primary-600 dark:text-sand-300">I want to list my property</div>
                      </div>
                    </button>
                  </div>
                </div>
              </>
            )}

            {/* Navigation Buttons */}
            <div className="flex flex-col sm:flex-row gap-4 pt-4">
              {currentStep > 1 && (
                <Button
                  type="button"
                  onClick={handleBack}
                  variant="secondary"
                  size="lg"
                  className="flex-1"
                >
                  <ArrowLeft className="w-5 h-5" />
                  Back
                </Button>
              )}

              <Button
                type="submit"
                variant="primary"
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
              <AuthDivider text="or sign up with" />
              <SocialAuthButtons mode="signup" />
            </>
          )}

          {/* Sign In Link */}
          <div className="mt-6 text-center">
            <p className="text-sm text-primary-600 dark:text-sand-400">
              Already have an account?{' '}
              <Link href="/login" className="text-secondary-600 dark:text-secondary-400 hover:underline font-medium">
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
