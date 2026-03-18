'use client';

import { useMemo, useState } from 'react';
import { useParams } from 'next/navigation';
import Link from 'next/link';
import { ArrowLeft, CheckCircle, KeyRound, Lock } from 'lucide-react';

export default function ResetPasswordPage() {
  const params = useParams<{ token?: string | string[] }>();
  const rawToken = Array.isArray(params?.token) ? params.token[0] : (params?.token || '');
  const token = useMemo(() => decodeURIComponent(rawToken), [rawToken]);
  const [newPassword, setNewPassword] = useState('');
  const [confirmPassword, setConfirmPassword] = useState('');
  const [loading, setLoading] = useState(false);
  const [success, setSuccess] = useState(false);
  const [error, setError] = useState('');

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError('');

    if (!token) {
      setError('Invalid reset link. Please request a new password reset email.');
      return;
    }

    if (!newPassword || !confirmPassword) {
      setError('Please enter and confirm your new password.');
      return;
    }

    if (newPassword !== confirmPassword) {
      setError('Passwords do not match.');
      return;
    }

    setLoading(true);
    try {
      const response = await fetch(`${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1/auth/password-reset/confirm/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          token,
          new_password: newPassword,
          confirm_password: confirmPassword,
        }),
      });

      if (!response.ok) {
        const data = await response.json();
        throw new Error(
          data.error ||
            data.detail ||
            data.new_password?.[0] ||
            data.confirm_password?.[0] ||
            'Could not reset password. Please request a new reset link.'
        );
      }

      setSuccess(true);
    } catch (err: any) {
      setError(err.message || 'Could not reset password. Please request a new reset link.');
    } finally {
      setLoading(false);
    }
  };

  if (success) {
    return (
      <div className="min-h-screen bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 flex items-center justify-center p-4">
        <div className="max-w-md w-full bg-white rounded-2xl shadow-elevated border border-primary-100 p-6 sm:p-8">
          <div className="text-center">
            <div className="w-16 h-16 bg-green-100 rounded-full flex items-center justify-center mx-auto mb-6">
              <CheckCircle className="w-9 h-9 text-green-600" />
            </div>

            <h1 className="text-2xl sm:text-3xl font-bold text-primary-900 mb-3">
              Password Reset Successful
            </h1>

            <p className="text-primary-500 mb-8 text-sm sm:text-base">
              Your password has been updated. You can now sign in with your new password.
            </p>

            <Link
              href="/login"
              className="block w-full bg-secondary-600 hover:bg-secondary-700 text-white font-semibold py-3 px-6 rounded-xl transition-colors"
            >
              Continue to Login
            </Link>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 flex items-center justify-center p-4">
      <div className="max-w-md w-full bg-white rounded-2xl shadow-elevated border border-primary-100 p-6 sm:p-8">
        <Link
          href="/forgot-password"
          className="inline-flex items-center text-primary-500 hover:text-primary-900 mb-6 transition-colors"
        >
          <ArrowLeft className="w-5 h-5 mr-2" />
          Back
        </Link>

        <div className="mb-8">
          <div className="w-16 h-16 bg-secondary-100 rounded-full flex items-center justify-center mb-6">
            <KeyRound className="w-8 h-8 text-secondary-600" />
          </div>

          <h1 className="text-3xl font-bold text-primary-900 mb-3">
            Set New Password
          </h1>

          <p className="text-primary-500">
            Enter your new password below. For security, this link can only be used once.
          </p>
        </div>

        <form onSubmit={handleSubmit} className="space-y-5">
          {error && (
            <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded-lg text-sm">
              {error}
            </div>
          )}

          <div>
            <label htmlFor="new-password" className="block text-sm font-medium text-primary-700 mb-2">
              New Password
            </label>
            <div className="relative">
              <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                <Lock className="h-5 w-5 text-primary-300" />
              </div>
              <input
                id="new-password"
                type="password"
                value={newPassword}
                onChange={(e) => setNewPassword(e.target.value)}
                className="block w-full pl-10 pr-3 py-3 border border-primary-300 rounded-xl focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-sand-50 text-primary-900 placeholder:text-primary-400"
                placeholder="Enter new password"
                disabled={loading}
                required
              />
            </div>
          </div>

          <div>
            <label htmlFor="confirm-password" className="block text-sm font-medium text-primary-700 mb-2">
              Confirm New Password
            </label>
            <div className="relative">
              <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                <Lock className="h-5 w-5 text-primary-300" />
              </div>
              <input
                id="confirm-password"
                type="password"
                value={confirmPassword}
                onChange={(e) => setConfirmPassword(e.target.value)}
                className="block w-full pl-10 pr-3 py-3 border border-primary-300 rounded-xl focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-sand-50 text-primary-900 placeholder:text-primary-400"
                placeholder="Confirm new password"
                disabled={loading}
                required
              />
            </div>
          </div>

          <button
            type="submit"
            disabled={loading}
            className="w-full bg-secondary-600 hover:bg-secondary-700 disabled:bg-secondary-400 text-white font-semibold py-3 px-6 rounded-xl transition-colors"
          >
            {loading ? 'Resetting Password...' : 'Reset Password'}
          </button>
        </form>
      </div>
    </div>
  );
}
