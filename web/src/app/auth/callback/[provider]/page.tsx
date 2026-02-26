'use client';

import { useEffect, useState } from 'react';
import { useParams, useSearchParams } from 'next/navigation';
import { useAuth } from '@/store/auth-store';
import { Loader2, AlertCircle, CheckCircle2 } from 'lucide-react';
import Link from 'next/link';

const API_BASE = process.env.NEXT_PUBLIC_API_BASE_URL
  ? `${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1`
  : (typeof window !== 'undefined' && window.location.origin.includes('localhost')
      ? 'http://localhost:8000/api/v1'
      : 'https://api.zimlegend.online/api/v1');

export default function OAuthCallbackPage() {
  const params = useParams();
  const searchParams = useSearchParams();
  const { setUser } = useAuth();
  const [status, setStatus] = useState<'loading' | 'success' | 'error'>('loading');
  const [errorMessage, setErrorMessage] = useState('');

  const provider = params.provider as string;

  useEffect(() => {
    const handleCallback = async () => {
      const code = searchParams.get('code');
      const state = searchParams.get('state');
      const error = searchParams.get('error');

      if (error) {
        setStatus('error');
        setErrorMessage(
          searchParams.get('error_description') || 
          `Authentication with ${provider} was cancelled or failed.`
        );
        return;
      }

      if (!code) {
        setStatus('error');
        setErrorMessage('No authorization code received. Please try again.');
        return;
      }

      try {
        // Exchange authorization code for tokens via backend
        const response = await fetch(`${API_BASE}/auth/social/${provider}/login/`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            code,
            ...(state && { state }),
            redirect_uri: `${window.location.origin}/auth/callback/${provider}`,
          }),
        });

        if (response.ok) {
          const data = await response.json();
          const { access, refresh, user: userData } = data;

          // Store tokens
          localStorage.setItem('access_token', access);
          localStorage.setItem('refresh_token', refresh);

          // Set cookie for SSR/middleware
          try {
            await fetch('/api/auth/set-cookie', {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ token: access, maxAge: 60 * 60 * 24 }),
            });
          } catch {
            const isSecure = window.location.protocol === 'https:';
            document.cookie = `access_token=${access}; path=/; max-age=86400; SameSite=Lax${isSecure ? '; Secure' : ''}`;
          }

          setUser(userData);
          setStatus('success');

          // Redirect to the page the user was trying to access, or dashboard
          const savedRedirect = localStorage.getItem('auth_redirect');
          const destination = savedRedirect && savedRedirect.startsWith('/') ? savedRedirect : '/dashboard';
          localStorage.removeItem('auth_redirect');
          setTimeout(() => {
            window.location.replace(destination);
          }, 1500);
        } else {
          const errorData = await response.json();
          setStatus('error');
          setErrorMessage(
            errorData.detail || 
            errorData.non_field_errors?.[0] || 
            `Failed to authenticate with ${provider}. Please try again.`
          );
        }
      } catch (err) {
        console.error('OAuth callback error:', err);
        setStatus('error');
        setErrorMessage('An unexpected error occurred. Please try again.');
      }
    };

    handleCallback();
  }, [provider, searchParams, setUser]);

  const providerName = provider.charAt(0).toUpperCase() + provider.slice(1);

  return (
    <div className="min-h-screen flex items-center justify-center py-12 px-4 bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 dark:from-primary-900 dark:via-primary-800 dark:to-primary-900">
      <div className="w-full max-w-md">
        <div className="text-center mb-8">
          <Link href="/" className="inline-flex items-center justify-center">
            <img src="/logo.png" alt="StayAfrica" className="h-20 w-auto" />
          </Link>
        </div>

        <div className="bg-white dark:bg-primary-800 rounded-2xl shadow-elevated p-8 border border-primary-100 dark:border-primary-700 text-center">
          {status === 'loading' && (
            <>
              <Loader2 className="w-12 h-12 text-secondary-500 mx-auto mb-4 animate-spin" />
              <h2 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                Signing in with {providerName}
              </h2>
              <p className="text-primary-600 dark:text-sand-300">
                Please wait while we complete your authentication...
              </p>
            </>
          )}

          {status === 'success' && (
            <>
              <CheckCircle2 className="w-12 h-12 text-green-500 mx-auto mb-4" />
              <h2 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                Successfully signed in!
              </h2>
              <p className="text-primary-600 dark:text-sand-300">
                Redirecting to your dashboard...
              </p>
            </>
          )}

          {status === 'error' && (
            <>
              <AlertCircle className="w-12 h-12 text-red-500 mx-auto mb-4" />
              <h2 className="text-xl font-bold text-primary-900 dark:text-sand-50 mb-2">
                Authentication Failed
              </h2>
              <p className="text-primary-600 dark:text-sand-300 mb-6">
                {errorMessage}
              </p>
              <div className="space-y-3">
                <Link
                  href="/login"
                  className="inline-flex w-full items-center justify-center rounded-lg bg-secondary-500 px-6 py-3 text-base font-medium text-neutral-900 hover:bg-secondary-600"
                >
                  Back to Sign In
                </Link>
                <Link
                  href="/register"
                  className="inline-flex w-full items-center justify-center rounded-lg border border-primary-300 dark:border-primary-600 px-6 py-3 text-base font-medium text-primary-700 dark:text-sand-200 hover:bg-sand-50 dark:hover:bg-primary-700"
                >
                  Create Account
                </Link>
              </div>
            </>
          )}
        </div>
      </div>
    </div>
  );
}
