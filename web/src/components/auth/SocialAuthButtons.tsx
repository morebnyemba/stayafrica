'use client';

import { useEffect, useState } from 'react';
import { toast } from 'react-hot-toast';

const API_BASE = process.env.NEXT_PUBLIC_API_BASE_URL
  ? `${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1`
  : (typeof window !== 'undefined' && window.location.origin.includes('localhost')
      ? 'http://localhost:8000/api/v1'
      : 'https://api.zimlegend.online/api/v1');

interface ProviderConfig {
  client_id: string;
  name: string;
}

interface SocialAuthButtonsProps {
  mode?: 'signin' | 'signup';
  onSuccess?: (data: any) => void;
}

export default function SocialAuthButtons({ mode = 'signin', onSuccess: _onSuccess }: SocialAuthButtonsProps) {
  const [providers, setProviders] = useState<Record<string, ProviderConfig>>({});

  useEffect(() => {
    // Fetch OAuth client IDs from backend (DB-stored)
    fetch(`${API_BASE}/auth/social/config/`)
      .then(r => r.json())
      .then(data => setProviders(data.providers || {}))
      .catch(() => {}); // silent — buttons will show "not configured" on click
  }, []);

  const handleGoogleAuth = () => {
    const redirectUri = encodeURIComponent(`${window.location.origin}/auth/callback/google`);
    const clientId = providers.google?.client_id;

    if (!clientId) {
      toast.error('Google authentication is not configured');
      return;
    }

    window.location.href = `https://accounts.google.com/o/oauth2/v2/auth?client_id=${clientId}&redirect_uri=${redirectUri}&response_type=code&scope=email%20profile`;
  };

  const handleFacebookAuth = () => {
    const redirectUri = encodeURIComponent(`${window.location.origin}/auth/callback/facebook`);
    const appId = providers.facebook?.client_id;

    if (!appId) {
      toast.error('Facebook authentication is not configured');
      return;
    }

    window.location.href = `https://www.facebook.com/v13.0/dialog/oauth?client_id=${appId}&redirect_uri=${redirectUri}&response_type=code&scope=email,public_profile`;
  };

  const handleAppleAuth = () => {
    const redirectUri = encodeURIComponent(`${window.location.origin}/auth/callback/apple`);
    const clientId = providers.apple?.client_id;

    if (!clientId) {
      toast.error('Apple authentication is not configured');
      return;
    }

    window.location.href = `https://appleid.apple.com/auth/authorize?client_id=${clientId}&redirect_uri=${redirectUri}&response_type=code&scope=email%20name`;
  };

  return (
    <div className="space-y-3">
      <div className="relative">
        <div className="absolute inset-0 flex items-center">
          <div className="w-full border-t border-gray-300" />
        </div>
        <div className="relative flex justify-center text-sm">
          <span className="bg-white px-2 text-gray-500">
            Or {mode === 'signin' ? 'sign in' : 'sign up'} with
          </span>
        </div>
      </div>

      <div className="grid grid-cols-1 gap-3">
        {/* Google */}
        <button
          onClick={handleGoogleAuth}
          className="flex w-full items-center justify-center gap-3 rounded-md bg-white px-3 py-2 text-sm font-semibold text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50"
        >
          <svg className="h-5 w-5" viewBox="0 0 24 24">
            <path
              d="M22.56 12.25c0-.78-.07-1.53-.2-2.25H12v4.26h5.92c-.26 1.37-1.04 2.53-2.21 3.31v2.77h3.57c2.08-1.92 3.28-4.74 3.28-8.09z"
              fill="#4285F4"
            />
            <path
              d="M12 23c2.97 0 5.46-.98 7.28-2.66l-3.57-2.77c-.98.66-2.23 1.06-3.71 1.06-2.86 0-5.29-1.93-6.16-4.53H2.18v2.84C3.99 20.53 7.7 23 12 23z"
              fill="#34A853"
            />
            <path
              d="M5.84 14.09c-.22-.66-.35-1.36-.35-2.09s.13-1.43.35-2.09V7.07H2.18C1.43 8.55 1 10.22 1 12s.43 3.45 1.18 4.93l2.85-2.22.81-.62z"
              fill="#FBBC05"
            />
            <path
              d="M12 5.38c1.62 0 3.06.56 4.21 1.64l3.15-3.15C17.45 2.09 14.97 1 12 1 7.7 1 3.99 3.47 2.18 7.07l3.66 2.84c.87-2.6 3.3-4.53 6.16-4.53z"
              fill="#EA4335"
            />
          </svg>
          Continue with Google
        </button>

        {/* Facebook */}
        <button
          onClick={handleFacebookAuth}
          className="flex w-full items-center justify-center gap-3 rounded-md bg-[#1877F2] px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-[#166FE5]"
        >
          <svg className="h-5 w-5" fill="currentColor" viewBox="0 0 24 24">
            <path d="M24 12.073c0-6.627-5.373-12-12-12s-12 5.373-12 12c0 5.99 4.388 10.954 10.125 11.854v-8.385H7.078v-3.47h3.047V9.43c0-3.007 1.792-4.669 4.533-4.669 1.312 0 2.686.235 2.686.235v2.953H15.83c-1.491 0-1.956.925-1.956 1.874v2.25h3.328l-.532 3.47h-2.796v8.385C19.612 23.027 24 18.062 24 12.073z" />
          </svg>
          Continue with Facebook
        </button>

        {/* Apple */}
        <button
          onClick={handleAppleAuth}
          className="flex w-full items-center justify-center gap-3 rounded-md bg-black px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-gray-900"
        >
          <svg className="h-5 w-5" fill="currentColor" viewBox="0 0 24 24">
            <path d="M17.05 20.28c-.98.95-2.05.8-3.08.35-1.09-.46-2.09-.48-3.24 0-1.44.62-2.2.44-3.06-.35C2.79 15.25 3.51 7.59 9.05 7.31c1.35.07 2.29.74 3.08.8 1.18-.24 2.31-.93 3.57-.84 1.51.12 2.65.72 3.4 1.8-3.12 1.87-2.38 5.98.48 7.13-.57 1.5-1.31 2.99-2.54 4.09l.01-.01zM12.03 7.25c-.15-2.23 1.66-4.07 3.74-4.25.29 2.58-2.34 4.5-3.74 4.25z" />
          </svg>
          Continue with Apple
        </button>
      </div>
    </div>
  );
}
