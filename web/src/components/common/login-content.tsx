'use client';

import { useState } from 'react';
import { useAuth } from '@/store/auth-store';
import Link from 'next/link';
import { Mail, Lock } from 'lucide-react';
import { Input, Button } from '@/components/ui';
import { toast } from 'react-hot-toast';
import { validateEmail, validatePassword } from '@/lib/validation';
import SocialAuthButtons from '@/components/auth/SocialAuthButtons';
import TwoFactorVerify from '@/components/auth/TwoFactorVerify';

export function LoginContent() {
  const { login, setUser, twoFactorPending, clearTwoFactorPending } = useAuth();
  const [isLoading, setIsLoading] = useState(false);
  const [showTwoFactor, setShowTwoFactor] = useState(false);
  const [formData, setFormData] = useState({
    email: '',
    password: '',
  });
  const [errors, setErrors] = useState<{ email?: string; password?: string }>({});

  const validateForm = () => {
    const newErrors: { email?: string; password?: string } = {};
    
    if (!validateEmail(formData.email)) {
      newErrors.email = 'Please enter a valid email address';
    }
    
    if (!validatePassword(formData.password)) {
      newErrors.password = 'Password must be at least 8 characters';
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
    } catch (error: any) {
      if (error?.twoFactorRequired || error?.message === '2FA_REQUIRED') {
        setShowTwoFactor(true);
      } else {
        toast.error('Invalid email or password');
        console.error('Login error:', error);
      }
    } finally {
      setIsLoading(false);
    }
  };

  // Show 2FA verification screen when required
  if (showTwoFactor && twoFactorPending) {
    return (
      <div className="min-h-screen flex items-center justify-center py-12 px-4 bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 dark:from-primary-900 dark:via-primary-800 dark:to-primary-900">
        <div className="w-full max-w-md">
          <div className="text-center mb-8">
            <Link href="/" className="inline-flex items-center justify-center">
              <img src="/logo.png" alt="StayAfrica" className="h-28 w-auto" />
            </Link>
          </div>
          <TwoFactorVerify
            email={twoFactorPending.email}
            password={twoFactorPending.password}
            onSuccess={(data) => {
              // Store tokens and user from the 2FA response
              if (data.access) {
                localStorage.setItem('access_token', data.access);
                localStorage.setItem('refresh_token', data.refresh);
                // Set cookie for SSR
                fetch('/api/auth/set-cookie', {
                  method: 'POST',
                  headers: { 'Content-Type': 'application/json' },
                  body: JSON.stringify({ token: data.access, maxAge: 86400 }),
                }).catch(() => {
                  const isSecure = window.location.protocol === 'https:';
                  document.cookie = `access_token=${data.access}; path=/; max-age=86400; SameSite=Lax${isSecure ? '; Secure' : ''}`;
                });
              }
              if (data.user) {
                setUser(data.user);
              }
              clearTwoFactorPending();
              toast.success('Welcome back!');
              window.location.replace('/dashboard');
            }}
            onBack={() => {
              setShowTwoFactor(false);
              clearTwoFactorPending();
            }}
          />
        </div>
      </div>
    );
  }

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
            <Input
              label="Email Address"
              type="email"
              value={formData.email}
              onChange={(e) => setFormData({ ...formData, email: e.target.value })}
              placeholder="you@example.com"
              disabled={isLoading}
              error={errors.email}
              icon={<Mail className="w-5 h-5" />}
              required
            />

            {/* Password Field */}
            <Input
              label="Password"
              type="password"
              value={formData.password}
              onChange={(e) => setFormData({ ...formData, password: e.target.value })}
              placeholder="••••••••"
              disabled={isLoading}
              error={errors.password}
              icon={<Lock className="w-5 h-5" />}
              required
            />

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
            <Button
              type="submit"
              fullWidth
              size="lg"
              isLoading={isLoading}
              disabled={isLoading}
            >
              {isLoading ? 'Signing In...' : 'Sign In'}
            </Button>
          </form>

          {/* Divider */}
          <div className="relative my-6">
            <div className="absolute inset-0 flex items-center">
              <div className="w-full border-t border-primary-200 dark:border-primary-700"></div>
            </div>
            <div className="relative flex justify-center text-sm">
              <span className="px-4 bg-white dark:bg-primary-800 text-primary-500 dark:text-sand-400">
                or continue with
              </span>
            </div>
          </div>

          {/* Social Auth */}
          <SocialAuthButtons mode="signin" />

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
              className="inline-flex w-full items-center justify-center rounded-lg bg-secondary-500 px-6 py-3 text-lg font-medium text-neutral-900 hover:bg-secondary-600 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-offset-2 focus-visible:ring-secondary-500"
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
