'use client';

import { useState } from 'react';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/context/auth-context';
import Link from 'next/link';
import { Mail, Lock, Eye, EyeOff, Loader2, Home, User, Phone, MapPin, CheckCircle2, ArrowRight, ArrowLeft } from 'lucide-react';
import { toast } from 'react-hot-toast';

type Step = 1 | 2 | 3;

export function RegisterContent() {
  const router = useRouter();
  const { register } = useAuth();
  const [isLoading, setIsLoading] = useState(false);
  const [showPassword, setShowPassword] = useState(false);
  const [showConfirmPassword, setShowConfirmPassword] = useState(false);
  const [currentStep, setCurrentStep] = useState<Step>(1);
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

  const validateStep1 = () => {
    const newErrors: Record<string, string> = {};
    
    if (!formData.email) {
      newErrors.email = 'Email is required';
    } else if (!/\S+@\S+\.\S+/.test(formData.email)) {
      newErrors.email = 'Email is invalid';
    }
    
    if (!formData.password) {
      newErrors.password = 'Password is required';
    } else if (formData.password.length < 8) {
      newErrors.password = 'Password must be at least 8 characters';
    } else if (!/(?=.*[a-z])(?=.*[A-Z])(?=.*\d)/.test(formData.password)) {
      newErrors.password = 'Password must contain uppercase, lowercase, and number';
    }
    
    if (formData.password !== formData.confirmPassword) {
      newErrors.confirmPassword = 'Passwords do not match';
    }
    
    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const validateStep2 = () => {
    const newErrors: Record<string, string> = {};
    
    if (!formData.first_name) {
      newErrors.first_name = 'First name is required';
    }
    
    if (!formData.last_name) {
      newErrors.last_name = 'Last name is required';
    }
    
    if (!formData.phone_number) {
      newErrors.phone_number = 'Phone number is required';
    } else if (!/^\+?[\d\s-()]+$/.test(formData.phone_number)) {
      newErrors.phone_number = 'Invalid phone number format';
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

  const handleBack = () => {
    if (currentStep > 1) {
      setCurrentStep((currentStep - 1) as Step);
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
      router.push('/dashboard');
    } catch (error) {
      toast.error('Registration failed. Please try again.');
      console.error('Registration error:', error);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="min-h-screen flex items-center justify-center py-12 px-4 bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 dark:from-primary-900 dark:via-primary-800 dark:to-primary-900">
      <div className="w-full max-w-2xl">
        {/* Logo/Branding */}
        <div className="text-center mb-8">
          <Link href="/" className="inline-flex items-center space-x-2 text-primary-900 dark:text-sand-100">
            <Home className="w-8 h-8 text-secondary-500" />
            <span className="text-2xl font-bold">StayAfrica</span>
          </Link>
          <p className="text-primary-600 dark:text-sand-300 mt-2">
            Join us and discover amazing stays across Africa
          </p>
        </div>

        <div className="bg-white dark:bg-primary-800 rounded-2xl shadow-elevated p-8 border border-primary-100 dark:border-primary-700">
          {/* Progress Steps */}
          <div className="flex items-center justify-between mb-8">
            {[1, 2, 3].map((step) => (
              <div key={step} className="flex items-center flex-1">
                <div className={`flex items-center justify-center w-10 h-10 rounded-full font-semibold transition ${
                  step < currentStep 
                    ? 'bg-secondary-500 text-primary-900' 
                    : step === currentStep
                    ? 'bg-secondary-500 text-primary-900 ring-4 ring-secondary-200 dark:ring-secondary-900'
                    : 'bg-primary-200 dark:bg-primary-700 text-primary-500 dark:text-sand-400'
                }`}>
                  {step < currentStep ? <CheckCircle2 className="w-6 h-6" /> : step}
                </div>
                {step < 3 && (
                  <div className={`flex-1 h-1 mx-2 transition ${
                    step < currentStep 
                      ? 'bg-secondary-500' 
                      : 'bg-primary-200 dark:bg-primary-700'
                  }`} />
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

          <form onSubmit={currentStep === 3 ? handleSubmit : (e) => { e.preventDefault(); handleNext(); }} className="space-y-6">
            {/* Step 1: Credentials */}
            {currentStep === 1 && (
              <>
                <div>
                  <label htmlFor="email" className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">
                    Email Address
                  </label>
                  <div className="relative">
                    <Mail className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                    <input
                      id="email"
                      type="email"
                      value={formData.email}
                      onChange={(e) => setFormData({ ...formData, email: e.target.value })}
                      className={`w-full pl-10 pr-4 py-3 bg-sand-50 dark:bg-primary-700 border rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent transition ${
                        errors.email ? 'border-red-500' : 'border-primary-200 dark:border-primary-600'
                      } text-primary-900 dark:text-sand-100`}
                      placeholder="you@example.com"
                    />
                  </div>
                  {errors.email && <p className="mt-1 text-sm text-red-600 dark:text-red-400">{errors.email}</p>}
                </div>

                <div>
                  <label htmlFor="password" className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">
                    Password
                  </label>
                  <div className="relative">
                    <Lock className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                    <input
                      id="password"
                      type={showPassword ? 'text' : 'password'}
                      value={formData.password}
                      onChange={(e) => setFormData({ ...formData, password: e.target.value })}
                      className={`w-full pl-10 pr-12 py-3 bg-sand-50 dark:bg-primary-700 border rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent transition ${
                        errors.password ? 'border-red-500' : 'border-primary-200 dark:border-primary-600'
                      } text-primary-900 dark:text-sand-100`}
                      placeholder="••••••••"
                    />
                    <button
                      type="button"
                      onClick={() => setShowPassword(!showPassword)}
                      className="absolute right-3 top-1/2 -translate-y-1/2 text-primary-400 dark:text-sand-400"
                    >
                      {showPassword ? <EyeOff className="w-5 h-5" /> : <Eye className="w-5 h-5" />}
                    </button>
                  </div>
                  {errors.password && <p className="mt-1 text-sm text-red-600 dark:text-red-400">{errors.password}</p>}
                </div>

                <div>
                  <label htmlFor="confirmPassword" className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">
                    Confirm Password
                  </label>
                  <div className="relative">
                    <Lock className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                    <input
                      id="confirmPassword"
                      type={showConfirmPassword ? 'text' : 'password'}
                      value={formData.confirmPassword}
                      onChange={(e) => setFormData({ ...formData, confirmPassword: e.target.value })}
                      className={`w-full pl-10 pr-12 py-3 bg-sand-50 dark:bg-primary-700 border rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent transition ${
                        errors.confirmPassword ? 'border-red-500' : 'border-primary-200 dark:border-primary-600'
                      } text-primary-900 dark:text-sand-100`}
                      placeholder="••••••••"
                    />
                    <button
                      type="button"
                      onClick={() => setShowConfirmPassword(!showConfirmPassword)}
                      className="absolute right-3 top-1/2 -translate-y-1/2 text-primary-400 dark:text-sand-400"
                    >
                      {showConfirmPassword ? <EyeOff className="w-5 h-5" /> : <Eye className="w-5 h-5" />}
                    </button>
                  </div>
                  {errors.confirmPassword && <p className="mt-1 text-sm text-red-600 dark:text-red-400">{errors.confirmPassword}</p>}
                </div>
              </>
            )}

            {/* Step 2: Personal Info */}
            {currentStep === 2 && (
              <>
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <label htmlFor="first_name" className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">
                      First Name
                    </label>
                    <div className="relative">
                      <User className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                      <input
                        id="first_name"
                        type="text"
                        value={formData.first_name}
                        onChange={(e) => setFormData({ ...formData, first_name: e.target.value })}
                        className={`w-full pl-10 pr-4 py-3 bg-sand-50 dark:bg-primary-700 border rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent transition ${
                          errors.first_name ? 'border-red-500' : 'border-primary-200 dark:border-primary-600'
                        } text-primary-900 dark:text-sand-100`}
                        placeholder="John"
                      />
                    </div>
                    {errors.first_name && <p className="mt-1 text-sm text-red-600 dark:text-red-400">{errors.first_name}</p>}
                  </div>

                  <div>
                    <label htmlFor="last_name" className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">
                      Last Name
                    </label>
                    <div className="relative">
                      <User className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                      <input
                        id="last_name"
                        type="text"
                        value={formData.last_name}
                        onChange={(e) => setFormData({ ...formData, last_name: e.target.value })}
                        className={`w-full pl-10 pr-4 py-3 bg-sand-50 dark:bg-primary-700 border rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent transition ${
                          errors.last_name ? 'border-red-500' : 'border-primary-200 dark:border-primary-600'
                        } text-primary-900 dark:text-sand-100`}
                        placeholder="Doe"
                      />
                    </div>
                    {errors.last_name && <p className="mt-1 text-sm text-red-600 dark:text-red-400">{errors.last_name}</p>}
                  </div>
                </div>

                <div>
                  <label htmlFor="phone_number" className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">
                    Phone Number
                  </label>
                  <div className="relative">
                    <Phone className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                    <input
                      id="phone_number"
                      type="tel"
                      value={formData.phone_number}
                      onChange={(e) => setFormData({ ...formData, phone_number: e.target.value })}
                      className={`w-full pl-10 pr-4 py-3 bg-sand-50 dark:bg-primary-700 border rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent transition ${
                        errors.phone_number ? 'border-red-500' : 'border-primary-200 dark:border-primary-600'
                      } text-primary-900 dark:text-sand-100`}
                      placeholder="+1 (555) 123-4567"
                    />
                  </div>
                  {errors.phone_number && <p className="mt-1 text-sm text-red-600 dark:text-red-400">{errors.phone_number}</p>}
                </div>
              </>
            )}

            {/* Step 3: Additional Details */}
            {currentStep === 3 && (
              <>
                <div>
                  <label htmlFor="country_of_residence" className="block text-sm font-medium text-primary-900 dark:text-sand-100 mb-2">
                    Country of Residence
                  </label>
                  <div className="relative">
                    <MapPin className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                    <select
                      id="country_of_residence"
                      value={formData.country_of_residence}
                      onChange={(e) => setFormData({ ...formData, country_of_residence: e.target.value })}
                      className={`w-full pl-10 pr-4 py-3 bg-sand-50 dark:bg-primary-700 border rounded-lg focus:ring-2 focus:ring-secondary-500 focus:border-transparent transition ${
                        errors.country_of_residence ? 'border-red-500' : 'border-primary-200 dark:border-primary-600'
                      } text-primary-900 dark:text-sand-100`}
                    >
                      <option value="">Select your country</option>
                      <option value="Zimbabwe">Zimbabwe</option>
                      <option value="South Africa">South Africa</option>
                      <option value="Kenya">Kenya</option>
                      <option value="Nigeria">Nigeria</option>
                      <option value="Ghana">Ghana</option>
                      <option value="Tanzania">Tanzania</option>
                      <option value="Uganda">Uganda</option>
                      <option value="Botswana">Botswana</option>
                      <option value="Namibia">Namibia</option>
                      <option value="Zambia">Zambia</option>
                      <option value="Other">Other</option>
                    </select>
                  </div>
                  {errors.country_of_residence && <p className="mt-1 text-sm text-red-600 dark:text-red-400">{errors.country_of_residence}</p>}
                </div>

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
                        <div className="text-sm text-primary-600 dark:text-sand-300">I'm looking for places to stay</div>
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
            <div className="flex gap-4 pt-4">
              {currentStep > 1 && (
                <button
                  type="button"
                  onClick={handleBack}
                  className="flex-1 btn-secondary py-3 text-base font-semibold rounded-lg flex items-center justify-center gap-2"
                >
                  <ArrowLeft className="w-5 h-5" />
                  <span>Back</span>
                </button>
              )}
              
              <button
                type="submit"
                disabled={isLoading}
                className="flex-1 btn-primary py-3 text-base font-semibold rounded-lg flex items-center justify-center gap-2 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                {isLoading ? (
                  <>
                    <Loader2 className="w-5 h-5 animate-spin" />
                    <span>Creating Account...</span>
                  </>
                ) : currentStep === 3 ? (
                  <span>Create Account</span>
                ) : (
                  <>
                    <span>Continue</span>
                    <ArrowRight className="w-5 h-5" />
                  </>
                )}
              </button>
            </div>
          </form>

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

        {/* Footer Links */}
        <div className="mt-6 text-center text-sm text-primary-600 dark:text-sand-400">
          <p>
            By creating an account, you agree to our{' '}
            <Link href="/terms" className="text-secondary-600 dark:text-secondary-400 hover:underline">
              Terms of Service
            </Link>{' '}
            and{' '}
            <Link href="/privacy" className="text-secondary-600 dark:text-secondary-400 hover:underline">
              Privacy Policy
            </Link>
          </p>
        </div>
      </div>
    </div>
  );
}
