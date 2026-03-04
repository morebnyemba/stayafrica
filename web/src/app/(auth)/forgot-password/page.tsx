'use client';

import { useState } from 'react';
import Link from 'next/link';
import { Mail, ArrowLeft, CheckCircle, Key } from 'lucide-react';

export default function ForgotPasswordPage() {
  const [email, setEmail] = useState('');
  const [loading, setLoading] = useState(false);
  const [submitted, setSubmitted] = useState(false);
  const [error, setError] = useState('');

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError('');

    if (!email.trim()) {
      setError('Please enter your email address');
      return;
    }

    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) {
      setError('Please enter a valid email address');
      return;
    }

    setLoading(true);
    try {
      const response = await fetch(`${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1/auth/password-reset/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ email: email.toLowerCase() }),
      });

      if (!response.ok) {
        const data = await response.json();
        throw new Error(data.detail || data.email?.[0] || 'Failed to send reset email');
      }

      setSubmitted(true);
    } catch (err: any) {
      setError(err.message || 'Failed to send reset email. Please try again.');
    } finally {
      setLoading(false);
    }
  };

  if (submitted) {
    return (
      <div className="min-h-screen bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 dark:from-primary-900 dark:via-primary-800 dark:to-primary-900 flex items-center justify-center p-4">
        <div className="max-w-md w-full bg-white dark:bg-primary-800 rounded-2xl shadow-elevated border border-primary-100 dark:border-primary-700 p-6 sm:p-8">
          <div className="text-center">
            <div className="w-16 h-16 bg-green-100 dark:bg-green-900/30 rounded-full flex items-center justify-center mx-auto mb-6">
              <CheckCircle className="w-9 h-9 text-green-600 dark:text-green-400" />
            </div>

            <h1 className="text-2xl sm:text-3xl font-bold text-primary-900 dark:text-sand-50 mb-3">
              Check Your Email
            </h1>

            <p className="text-primary-500 dark:text-sand-400 mb-4 text-sm sm:text-base">
              We&apos;ve sent password reset instructions to{' '}
              <span className="font-semibold text-primary-700 dark:text-sand-200">{email}</span>
            </p>

            <p className="text-sm text-primary-400 dark:text-sand-500 mb-8">
              Didn&apos;t receive the email? Check your spam folder or try again.
            </p>

            <Link
              href="/login"
              className="block w-full bg-secondary-600 hover:bg-secondary-700 text-white font-semibold py-3 px-6 rounded-xl transition-colors mb-4"
            >
              Back to Login
            </Link>

            <button
              onClick={() => setSubmitted(false)}
              className="text-secondary-600 dark:text-secondary-400 hover:text-secondary-700 font-medium text-sm"
            >
              Try Another Email
            </button>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-sand-100 via-secondary-50 to-primary-50 dark:from-primary-900 dark:via-primary-800 dark:to-primary-900 flex items-center justify-center p-4">
      <div className="max-w-md w-full bg-white dark:bg-primary-800 rounded-2xl shadow-elevated border border-primary-100 dark:border-primary-700 p-6 sm:p-8">
        {/* Back Button */}
        <Link
          href="/login"
          className="inline-flex items-center text-primary-500 dark:text-sand-400 hover:text-primary-900 dark:hover:text-sand-50 mb-6 transition-colors"
        >
          <ArrowLeft className="w-5 h-5 mr-2" />
          Back to Login
        </Link>

        {/* Header */}
        <div className="mb-8">
          <div className="w-16 h-16 bg-secondary-100 dark:bg-secondary-900/30 rounded-full flex items-center justify-center mb-6">
            <Key className="w-8 h-8 text-secondary-600 dark:text-secondary-400" />
          </div>

          <h1 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-3">
            Forgot Password?
          </h1>

          <p className="text-primary-500 dark:text-sand-400">
            No worries! Enter your email address and we'll send you instructions to reset your password.
          </p>
        </div>

        {/* Form */}
        <form onSubmit={handleSubmit} className="space-y-6">
          {error && (
            <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded-lg">
              {error}
            </div>
          )}

          <div>
            <label htmlFor="email" className="block text-sm font-medium text-primary-700 dark:text-sand-200 mb-2">
              Email Address
            </label>
            <div className="relative">
              <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                <Mail className="h-5 w-5 text-primary-300 dark:text-primary-500" />
              </div>
              <input
                id="email"
                type="email"
                value={email}
                onChange={(e) => setEmail(e.target.value)}
                className="block w-full pl-10 pr-3 py-3 border border-primary-300 dark:border-primary-600 rounded-xl focus:ring-2 focus:ring-secondary-500 focus:border-transparent bg-sand-50 dark:bg-primary-900 text-primary-900 dark:text-sand-50 placeholder:text-primary-400 dark:placeholder:text-primary-500"
                placeholder="Enter your email"
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
            {loading ? 'Sending...' : 'Send Reset Link'}
          </button>

          <div className="text-center">
            <span className="text-primary-500 dark:text-sand-400">Remember your password? </span>
            <Link href="/login" className="text-secondary-600 dark:text-secondary-400 hover:text-secondary-700 font-semibold">
              Back to Login
            </Link>
          </div>
        </form>
      </div>
    </div>
  );
}
