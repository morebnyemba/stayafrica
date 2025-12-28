'use client';

import { useState } from 'react';
import { useAuth } from '@/context/auth-context';
import Link from 'next/link';
import { Mail, Lock, Eye, EyeOff, Loader2 } from 'lucide-react';
import { FormField, Input } from '@/components/ui/form';
import { toast } from 'react-hot-toast';
import { validateEmail, validatePassword } from '@/lib/validation';

export function LoginContent() {
  const { login } = useAuth();
  const [isLoading, setIsLoading] = useState(false);
  const [showPassword, setShowPassword] = useState(false);
  const [formData, setFormData] = useState({
    email: '',
    password: '',
  });
  const [errors, setErrors] = useState<{ email?: string; password?: string }>({});

  const validateForm = () => {
    const newErrors: { email?: string; password?: string } = {};
    
    const emailValidation = validateEmail(formData.email);
    if (!emailValidation.isValid) {
      newErrors.email = emailValidation.error;
    }
    
    const passwordValidation = validatePassword(formData.password);
    if (!passwordValidation.isValid) {
      newErrors.password = passwordValidation.error;
    }
    
    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      return;
    }

    setIsLoading(true);
    try {
      await login(formData.email, formData.password);
      toast.success('Welcome back!');
      
      // Force a full-page navigation so middleware/SSR see the new cookie
      // This avoids needing a manual refresh after login
      window.location.replace('/dashboard');
    } catch (error) {
      toast.error('Invalid email or password');
      console.error('Login error:', error);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="min-h-screen flex items-center justify-center py-12 px-4 bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 dark:from-primary-900 dark:via-primary-800 dark:to-primary-900">
      <div className="w-full max-w-md">
        {/* Logo/Branding */}
        <div className="text-center mb-8">
          <Link href="/" className="inline-flex items-center justify-center">
            <img src="/logo.png" alt="StayAfrica" className="h-28 w-auto" />
          </Link>
          <p className="text-primary-600 dark:text-sand-300 mt-2">
            Welcome back! Sign in to continue
          </p>
        </div>

        <div className="bg-white dark:bg-primary-800 rounded-2xl shadow-elevated p-8 border border-primary-100 dark:border-primary-700">
          <h1 className="text-3xl font-bold mb-6 text-center text-primary-900 dark:text-sand-50">
            Sign In
          </h1>

          <form onSubmit={handleSubmit} className="space-y-6">
            {/* Email Field */}
            <FormField label="Email Address" error={errors.email}>
              <div className="relative">
                <Mail className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                <Input
                  id="email"
                  type="email"
                  value={formData.email}
                  onChange={(e) => setFormData({ ...formData, email: e.target.value })}
                  className="pl-10"
                  placeholder="you@example.com"
                  disabled={isLoading}
                  aria-invalid={!!errors.email}
                />
              </div>
            </FormField>

            {/* Password Field */}
            <FormField label="Password" error={errors.password}>
              <div className="relative">
                <Lock className="absolute left-3 top-1/2 -translate-y-1/2 w-5 h-5 text-primary-400 dark:text-sand-400" />
                <Input
                  id="password"
                  type={showPassword ? 'text' : 'password'}
                  value={formData.password}
                  onChange={(e) => setFormData({ ...formData, password: e.target.value })}
                  className="pl-10 pr-12"
                  placeholder="••••••••"
                  disabled={isLoading}
                  aria-invalid={!!errors.password}
                />
                <button
                  type="button"
                  onClick={() => setShowPassword(!showPassword)}
                  className="absolute right-3 top-1/2 -translate-y-1/2 text-primary-400 dark:text-sand-400 hover:text-primary-600 dark:hover:text-sand-200 transition"
                  aria-label={showPassword ? 'Hide password' : 'Show password'}
                >
                  {showPassword ? <EyeOff className="w-5 h-5" /> : <Eye className="w-5 h-5" />}
                </button>
              </div>
            </FormField>

            {/* Remember Me & Forgot Password */}
            <div className="flex items-center justify-between">
              <label className="flex items-center">
                <input
                  type="checkbox"
                  className="w-4 h-4 text-secondary-500 bg-sand-50 dark:bg-primary-700 border-primary-300 dark:border-primary-600 rounded focus:ring-secondary-500"
                />
                <span className="ml-2 text-sm text-primary-700 dark:text-sand-200">Remember me</span>
              </label>
              <Link 
                href="/forgot-password" 
                className="text-sm text-secondary-600 dark:text-secondary-400 hover:text-secondary-700 dark:hover:text-secondary-300 font-medium"
              >
                Forgot password?
              </Link>
            </div>

            {/* Submit Button */}
            <button
              type="submit"
              disabled={isLoading}
              className="w-full btn-primary py-3 text-base font-semibold rounded-lg flex items-center justify-center gap-2 disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {isLoading ? (
                <>
                  <Loader2 className="w-5 h-5 animate-spin" />
                  <span>Signing In...</span>
                </>
              ) : (
                <span>Sign In</span>
              )}
            </button>
          </form>

          {/* Divider */}
          <div className="relative my-6">
            <div className="absolute inset-0 flex items-center">
              <div className="w-full border-t border-primary-200 dark:border-primary-700"></div>
            </div>
            <div className="relative flex justify-center text-sm">
              <span className="px-4 bg-white dark:bg-primary-800 text-primary-500 dark:text-sand-400">
                New to StayAfrica?
              </span>
            </div>
          </div>

          {/* Sign Up Link */}
          <div className="text-center">
            <Link 
              href="/register" 
              className="btn-secondary w-full py-3 text-base font-semibold rounded-lg inline-block"
            >
              Create an Account
            </Link>
          </div>
        </div>

        {/* Footer Links */}
        <div className="mt-6 text-center text-sm text-primary-600 dark:text-sand-400">
          <p>
            By signing in, you agree to our{' '}
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
