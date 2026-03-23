'use client';

import { useState, useRef, useEffect } from 'react';
import Link from 'next/link';
import { useRouter, useSearchParams } from 'next/navigation';
import { Mail, ArrowLeft } from 'lucide-react';
import { NoticeModal } from '@/components/common/notice-modal';

export default function VerifyEmailPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const email = searchParams.get('email') || '';

  const [code, setCode] = useState(['', '', '', '', '', '']);
  const [loading, setLoading] = useState(false);
  const [resending, setResending] = useState(false);
  const [checkingVerification, setCheckingVerification] = useState(false);
  const [error, setError] = useState('');
  const [noticeMessage, setNoticeMessage] = useState('');
  const inputRefs = useRef<(HTMLInputElement | null)[]>([]);

  const persistSession = async (access: string, refresh: string) => {
    localStorage.setItem('access_token', access);
    localStorage.setItem('refresh_token', refresh);

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
  };

  const refreshVerifiedSession = async (accessToken: string) => {
    const response = await fetch(`${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1/users/refresh_session/`, {
      method: 'POST',
      headers: {
        Authorization: `Bearer ${accessToken}`,
      },
    });

    if (!response.ok) {
      throw new Error('Failed to refresh verified session.');
    }

    const data = await response.json();
    if (typeof data?.access === 'string' && typeof data?.refresh === 'string') {
      await persistSession(data.access, data.refresh);
    }

    return data;
  };

  const checkVerificationStatus = async () => {
    const accessToken = localStorage.getItem('access_token');
    if (!accessToken) {
      return;
    }

    setCheckingVerification(true);
    try {
      const response = await fetch(`${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1/users/profile/`, {
        headers: {
          Authorization: `Bearer ${accessToken}`,
        },
      });

      if (!response.ok) {
        return;
      }

      const profile = await response.json();
      if (profile?.is_verified) {
        await refreshVerifiedSession(accessToken);
        router.push('/dashboard');
      }
    } catch {
      // Ignore transient polling failures on this screen.
    } finally {
      setCheckingVerification(false);
    }
  };

  useEffect(() => {
    // Auto-focus first input on mount
    inputRefs.current[0]?.focus();
  }, []);

  useEffect(() => {
    const accessToken = localStorage.getItem('access_token');
    if (!accessToken) {
      return;
    }

    checkVerificationStatus();
    const interval = window.setInterval(checkVerificationStatus, 8000);
    return () => window.clearInterval(interval);
  }, []);

  const handleChange = (index: number, value: string) => {
    // Only allow numbers
    const numericValue = value.replace(/[^0-9]/g, '');

    if (numericValue.length > 1) {
      // Handle paste of multiple digits
      const digits = numericValue.split('').slice(0, 6);
      const newCode = [...code];
      digits.forEach((digit, i) => {
        if (index + i < 6) {
          newCode[index + i] = digit;
        }
      });
      setCode(newCode);

      // Focus on the next empty field or last field
      const nextIndex = Math.min(index + digits.length, 5);
      inputRefs.current[nextIndex]?.focus();
    } else {
      // Handle single digit input
      const newCode = [...code];
      newCode[index] = numericValue;
      setCode(newCode);

      // Auto-focus next input
      if (numericValue && index < 5) {
        inputRefs.current[index + 1]?.focus();
      }
    }
  };

  const handleKeyDown = (index: number, e: React.KeyboardEvent) => {
    if (e.key === 'Backspace' && !code[index] && index > 0) {
      inputRefs.current[index - 1]?.focus();
    }
  };

  const handleVerify = async (e: React.FormEvent) => {
    e.preventDefault();
    setError('');

    const verificationCode = code.join('');
    if (verificationCode.length !== 6) {
      setError('Please enter the complete 6-digit code');
      return;
    }

    setLoading(true);
    try {
      const response = await fetch(`${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1/auth/verify-email/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ email, code: verificationCode }),
      });

      if (!response.ok) {
        const data = await response.json();
        throw new Error(data.detail || data.code?.[0] || 'Invalid verification code');
      }

      const accessToken = localStorage.getItem('access_token');

      if (accessToken) {
        await refreshVerifiedSession(accessToken);
        router.push('/dashboard?verified=true');
      } else {
        router.push('/login?verified=true');
      }
    } catch (err: any) {
      setError(err.message || 'Invalid verification code. Please try again.');
      setCode(['', '', '', '', '', '']);
      inputRefs.current[0]?.focus();
    } finally {
      setLoading(false);
    }
  };

  const handleResend = async () => {
    if (!email) {
      setError('Email address is required');
      return;
    }

    setResending(true);
    setError('');
    try {
      const accessToken = localStorage.getItem('access_token');
      if (!accessToken) {
        throw new Error('Please log in first to resend verification email.');
      }

      const response = await fetch(`${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1/users/resend_verification/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Authorization: `Bearer ${accessToken}`,
        },
      });

      if (!response.ok) {
        throw new Error('Failed to resend code');
      }

      setCode(['', '', '', '', '', '']);
      inputRefs.current[0]?.focus();
      setNoticeMessage('Verification code has been resent to your email.');
    } catch (err: any) {
      setError(err.message || 'Failed to resend code. Please try again.');
    } finally {
      setResending(false);
    }
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 flex items-center justify-center p-4">
      <div className="max-w-md w-full bg-white rounded-2xl shadow-elevated border border-primary-100 p-6 sm:p-8">
        {/* Back Button */}
        <Link
          href="/register"
          className="inline-flex items-center text-primary-500 hover:text-primary-900 mb-6 transition-colors"
        >
          <ArrowLeft className="w-5 h-5 mr-2" />
          Back
        </Link>

        {/* Header */}
        <div className="mb-8">
          <div className="w-16 h-16 bg-secondary-100 rounded-full flex items-center justify-center mb-6">
            <Mail className="w-8 h-8 text-secondary-600" />
          </div>

          <h1 className="text-3xl font-bold text-primary-900 mb-3">
            Verify Your Email
          </h1>

          <p className="text-primary-500">
            We've sent a 6-digit verification code to{' '}
            <span className="font-semibold">{email || 'your email'}</span>. Please enter it below.
          </p>
        </div>

        {/* Form */}
        <form onSubmit={handleVerify} className="space-y-6">
          {error && (
            <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded-lg text-sm">
              {error}
            </div>
          )}

          <div className="bg-blue-50 border border-blue-200 rounded-lg px-4 py-3 text-sm">
            <div className="flex items-center justify-between gap-4">
              <div>
                <p className="font-semibold text-blue-900">Waiting for verification</p>
                <p className="text-blue-700">
                  This page checks automatically and will send you to your dashboard once your email is verified.
                </p>
              </div>
              <button
                type="button"
                onClick={checkVerificationStatus}
                disabled={checkingVerification}
                className="shrink-0 text-secondary-600 hover:text-secondary-700 font-semibold disabled:text-secondary-300"
              >
                {checkingVerification ? 'Checking...' : 'Check now'}
              </button>
            </div>
          </div>

          {/* Code Input */}
          <div>
            <div className="flex justify-between gap-2 mb-4">
              {code.map((digit, index) => (
                <input
                  key={index}
                  ref={(el) => { inputRefs.current[index] = el; }}
                  type="text"
                  inputMode="numeric"
                  maxLength={2}
                  value={digit}
                  onChange={(e) => handleChange(index, e.target.value)}
                  onKeyDown={(e) => handleKeyDown(index, e)}
                  className="w-11 h-14 sm:w-14 sm:h-16 text-center text-xl sm:text-2xl font-bold border-2 border-primary-300 rounded-xl focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-sand-50 text-primary-900"
                  disabled={loading || resending}
                />
              ))}
            </div>
            <p className="text-sm text-primary-400 text-center">
              Enter the 6-digit code sent to your email
            </p>
          </div>

          <button
            type="submit"
            disabled={loading || resending || code.join('').length !== 6}
            className="w-full bg-secondary-600 hover:bg-secondary-700 disabled:bg-secondary-300 text-white font-semibold py-3 px-6 rounded-xl transition-colors"
          >
            {loading ? 'Verifying...' : 'Verify Email'}
          </button>

          <div className="text-center space-y-3">
            <div>
              <span className="text-primary-500">Didn't receive the code? </span>
              <button
                type="button"
                onClick={handleResend}
                disabled={loading || resending}
                className="text-secondary-600 hover:text-secondary-700 font-semibold disabled:text-secondary-300"
              >
                {resending ? 'Sending...' : 'Resend'}
              </button>
            </div>

            <Link
              href="/register"
              className="block text-primary-500 hover:text-primary-900"
            >
              Wrong email address? Go back
            </Link>
          </div>
        </form>
      </div>
      <NoticeModal
        isOpen={Boolean(noticeMessage)}
        title="Verification Email Sent"
        message={noticeMessage}
        onClose={() => setNoticeMessage('')}
      />
    </div>
  );
}
