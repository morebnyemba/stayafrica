'use client';

import { useState } from 'react';
import Link from 'next/link';
import { useRouter } from 'next/navigation';
import { CheckCircle, XCircle } from 'lucide-react';

type VerifyLinkPageProps = {
  params: {
    user_hash: string;
    token: string;
  };
};

export default function VerifyLinkPage({ params }: VerifyLinkPageProps) {
  const router = useRouter();
  const [loading, setLoading] = useState(false);
  const [status, setStatus] = useState<'idle' | 'success' | 'error'>('idle');
  const [message, setMessage] = useState('');

  const verifyNow = async () => {
    setLoading(true);
    setMessage('');

    try {
      const response = await fetch(`${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1/auth/verify-email/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          user_hash: decodeURIComponent(params.user_hash),
          token: decodeURIComponent(params.token),
        }),
      });

      const data = await response.json().catch(() => ({}));

      if (!response.ok) {
        throw new Error(data.error || data.detail || 'Verification failed.');
      }

      setStatus('success');
      setMessage(data.message || 'Email verified successfully. You can now log in.');
      setTimeout(() => router.push('/login?verified=true'), 1600);
    } catch (err: any) {
      setStatus('error');
      setMessage(err.message || 'Verification failed. Please request a new verification email.');
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 flex items-center justify-center p-4">
      <div className="max-w-md w-full bg-white rounded-2xl shadow-elevated border border-primary-100 p-6 sm:p-8">
        <h1 className="text-2xl sm:text-3xl font-bold text-primary-900 mb-3 text-center">
          Verify Email
        </h1>

        <p className="text-primary-500 text-center mb-6">
          Confirm your email address to activate your StayAfrica account.
        </p>

        {status !== 'idle' && (
          <div className={`mb-6 rounded-lg border px-4 py-3 text-sm ${status === 'success' ? 'bg-green-50 border-green-200 text-green-700' : 'bg-red-50 border-red-200 text-red-700'}`}>
            <div className="flex items-center gap-2">
              {status === 'success' ? <CheckCircle className="h-4 w-4" /> : <XCircle className="h-4 w-4" />}
              <span>{message}</span>
            </div>
          </div>
        )}

        {status === 'idle' && (
          <button
            onClick={verifyNow}
            disabled={loading}
            className="w-full bg-secondary-600 hover:bg-secondary-700 disabled:bg-secondary-400 text-white font-semibold py-3 px-6 rounded-xl transition-colors"
          >
            {loading ? 'Verifying...' : 'Verify My Email'}
          </button>
        )}

        {status === 'error' && (
          <div className="mt-4 text-center">
            <Link href="/verify-email" className="text-secondary-600 hover:text-secondary-700 font-semibold">
              Request a new verification email
            </Link>
          </div>
        )}

        {status === 'success' && (
          <div className="mt-4 text-center text-primary-500 text-sm">
            Redirecting to login...
          </div>
        )}
      </div>
    </div>
  );
}
