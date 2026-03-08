'use client';

import { useState } from 'react';
import { useAuth } from '@/store/auth-store';
import { useSearchParams } from 'next/navigation';
import Link from 'next/link';
import { Mail, Lock, Building2, Plane } from 'lucide-react';
import { Input, Button } from '@/components/ui';
import { toast } from 'react-hot-toast';
import { validateEmail, validatePassword } from '@/lib/validation';
import SocialAuthButtons from '@/components/auth/SocialAuthButtons';
import TwoFactorVerify from '@/components/auth/TwoFactorVerify';
import { AuthDivider } from '@/components/auth/AuthDivider';
import { AuthFooter } from '@/components/auth/AuthFooter';

export type LoginMode = 'universal' | 'guest' | 'host';

interface LoginContentProps {
  loginMode?: LoginMode;
}

const LOGIN_CONFIG: Record<LoginMode, {
  heading: string;
  subtitle: string;
  defaultRedirect: string;
  icon?: React.ReactNode;
  registerHint: string;
  registerLabel: string;
}> = {
  universal: {
    heading: 'Sign In',
    subtitle: 'Welcome back to StayAfrica',
    defaultRedirect: '/dashboard',
    registerHint: 'New to StayAfrica?',
    registerLabel: 'Create an Account',
  },
  guest: {
    heading: 'Traveler Login',
    subtitle: 'Welcome back, Traveler! Find your next stay.',
    defaultRedirect: '/explore',
    icon: <Plane className="w-6 h-6 text-sky-400" />,
    registerHint: 'New to StayAfrica?',
    registerLabel: 'Create a Traveler Account',
  },
  host: {
    heading: 'Host Portal',
    subtitle: 'Welcome back, Host! Manage your properties.',
    defaultRedirect: '/host/dashboard',
    icon: <Building2 className="w-6 h-6 text-secondary-400" />,
    registerHint: 'Want to list your property?',
    registerLabel: 'Become a Host',
  },
};

function getRedirectUrl(searchParams: URLSearchParams, defaultRedirect: string): string {
  const redirect = searchParams.get('redirect');
  // Only allow relative paths to prevent open-redirect attacks
  if (redirect && redirect.startsWith('/')) {
    return redirect;
  }
  return defaultRedirect;
}

export function LoginContent({ loginMode = 'universal' }: LoginContentProps) {
  const { login, logout, setUser, twoFactorPending, clearTwoFactorPending } = useAuth();
  const searchParams = useSearchParams();
  const config = LOGIN_CONFIG[loginMode];
  const redirectUrl = getRedirectUrl(searchParams, config.defaultRedirect);
  const [isLoading, setIsLoading] = useState(false);
  const [showTwoFactor, setShowTwoFactor] = useState(false);
  const [formData, setFormData] = useState({
    email: '',
    password: '',
  });
  const [errors, setErrors] = useState<{ email?: string; password?: string }>({});

  const validateForm = () => {
    const newErrors: { email?: string; password?: string } = {};

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

    setErrors(newErrors);
    if (Object.keys(newErrors).length > 0) {
      toast.error(Object.values(newErrors)[0]!);
    }
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

      // Host login: check that the user actually has host/admin role
      if (loginMode === 'host') {
        // After login, the store has the user data. Access it from localStorage 
        // since the store may not have updated in this tick.
        const storedAuth = localStorage.getItem('auth-storage');
        let userRole = '';
        if (storedAuth) {
          try {
            const parsed = JSON.parse(storedAuth);
            userRole = parsed?.state?.user?.role || '';
          } catch { }
        }

        if (userRole !== 'host' && userRole !== 'admin') {
          // Not a host — log them out and show error
          logout();
          toast.error('This login is for hosts only. Please use the guest login or become a host first.');
          setIsLoading(false);
          return;
        }
      }

      toast.success('Welcome back!');

      // Force a full-page navigation so middleware/SSR see the new cookie
      window.location.replace(redirectUrl);
    } catch (error: any) {
      if (error?.twoFactorRequired || error?.message === '2FA_REQUIRED') {
        setShowTwoFactor(true);
      } else {
        toast.error(error?.message || 'Invalid email or password');
      }
    } finally {
      setIsLoading(false);
    }
  };

  // Determine the register URL based on login mode
  const registerUrl = loginMode === 'host'
    ? '/register?role=host'
    : '/register';

  // Show 2FA verification screen when required
  if (showTwoFactor && twoFactorPending) {
    return (
      <div className="min-h-screen flex items-center justify-center py-8 px-4 sm:py-12 bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 dark:from-primary-900 dark:via-primary-800 dark:to-primary-900">
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
              // For host login, check role before completing
              if (loginMode === 'host' && data.user) {
                const role = data.user.role;
                if (role !== 'host' && role !== 'admin') {
                  logout();
                  toast.error('This login is for hosts only. Please use the guest login or become a host first.');
                  setShowTwoFactor(false);
                  clearTwoFactorPending();
                  return;
                }
              }

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
              window.location.replace(redirectUrl);
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
    <div className="min-h-screen flex items-center justify-center py-8 px-4 sm:py-12 bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 dark:from-primary-900 dark:via-primary-800 dark:to-primary-900">
      <div className="w-full max-w-md">
        {/* Logo/Branding */}
        <div className="text-center mb-6 sm:mb-8">
          <Link href="/" className="inline-flex items-center justify-center">
            <img src="/logo.png" alt="StayAfrica" className="h-20 sm:h-28 w-auto" />
          </Link>
          <p className="text-primary-600 dark:text-sand-300 mt-2 text-sm sm:text-base">
            {config.subtitle}
          </p>
        </div>

        <div className="bg-white dark:bg-primary-800 rounded-2xl shadow-elevated p-6 sm:p-8 border border-primary-100 dark:border-primary-700">
          <div className="flex items-center justify-center gap-2 mb-6">
            {config.icon}
            <h1 className="text-2xl sm:text-3xl font-bold text-center text-primary-900 dark:text-sand-50">
              {config.heading}
            </h1>
          </div>

          {/* Host login notice */}
          {loginMode === 'host' && (
            <div className="mb-5 px-4 py-3 rounded-xl bg-secondary-50 dark:bg-secondary-900/20 border border-secondary-200 dark:border-secondary-700/40">
              <p className="text-xs text-secondary-700 dark:text-secondary-300 text-center">
                This portal is exclusively for registered hosts. Not a host?{' '}
                <Link href="/login/guest" className="font-semibold underline hover:text-secondary-900 dark:hover:text-secondary-200">
                  Log in as a traveler
                </Link>
              </p>
            </div>
          )}

          <form onSubmit={handleSubmit} className="space-y-5">
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
              variant="secondary"
              fullWidth
              size="lg"
              isLoading={isLoading}
              disabled={isLoading}
            >
              {isLoading ? 'Signing In...' : config.heading === 'Sign In' ? 'Sign In' : `Sign In as ${loginMode === 'host' ? 'Host' : 'Traveler'}`}
            </Button>
          </form>

          <AuthDivider text="or" />

          {/* Social Auth */}
          <SocialAuthButtons mode="signin" redirectUrl={redirectUrl} />

          {/* Sign Up Link */}
          <div className="mt-6 text-center">
            <p className="text-sm text-primary-600 dark:text-sand-400 mb-3">{config.registerHint}</p>
            <Link
              href={redirectUrl !== config.defaultRedirect ? `${registerUrl}${registerUrl.includes('?') ? '&' : '?'}redirect=${encodeURIComponent(redirectUrl)}` : registerUrl}
              className="inline-flex w-full items-center justify-center rounded-lg border-2 border-secondary-500 dark:border-secondary-600 px-6 py-2.5 text-base font-semibold text-secondary-700 dark:text-secondary-400 hover:bg-secondary-50 dark:hover:bg-secondary-900/20 transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-offset-2 focus-visible:ring-secondary-500"
            >
              {config.registerLabel}
            </Link>
          </div>

          {/* Link to other login modes */}
          {loginMode !== 'universal' && (
            <div className="mt-4 text-center">
              <Link
                href="/login"
                className="text-xs text-primary-400 dark:text-sand-500 hover:text-primary-600 dark:hover:text-sand-300 transition"
              >
                Use universal login instead
              </Link>
            </div>
          )}
        </div>

        <AuthFooter action="signin" />
      </div>
    </div>
  );
}
