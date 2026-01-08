'use client';

import { useRouter, useSearchParams } from 'next/navigation';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { XCircle, Home, RefreshCcw, AlertCircle } from 'lucide-react';
import Link from 'next/link';
import { Button } from '@/components/ui/Button';

export default function BookingFailurePage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  
  const bookingId = searchParams.get('bookingId');
  const error = searchParams.get('error') || 'Payment failed';
  const provider = searchParams.get('provider');

  const handleRetry = () => {
    if (bookingId) {
      router.push(`/booking/payment?bookingId=${bookingId}`);
    } else {
      router.push('/explore');
    }
  };

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center py-12 px-4">
        <div className="max-w-2xl w-full">
          {/* Failure icon */}
          <div className="text-center mb-8">
            <div className="inline-flex items-center justify-center w-20 h-20 bg-red-100 dark:bg-red-900/30 rounded-full mb-6">
              <XCircle className="w-12 h-12 text-red-600 dark:text-red-400" />
            </div>
            <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-3">
              Payment Failed
            </h1>
            <p className="text-lg text-primary-600 dark:text-sand-300">
              We couldn't process your payment
            </p>
          </div>

          {/* Error details card */}
          <div className="card p-6 md:p-8 mb-6 border-l-4 border-red-500">
            <div className="flex items-start gap-4">
              <AlertCircle className="w-6 h-6 text-red-600 dark:text-red-400 flex-shrink-0 mt-0.5" />
              <div className="flex-1">
                <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-2">
                  What went wrong?
                </h2>
                <p className="text-primary-700 dark:text-sand-200 mb-4">
                  {error}
                </p>
                {bookingId && (
                  <div className="text-sm text-primary-600 dark:text-sand-400">
                    Booking Reference: <span className="font-mono font-medium">{bookingId}</span>
                  </div>
                )}
                {provider && (
                  <div className="text-sm text-primary-600 dark:text-sand-400">
                    Payment Method: <span className="font-medium capitalize">{provider.replace('_', ' ')}</span>
                  </div>
                )}
              </div>
            </div>
          </div>

          {/* Common reasons */}
          <div className="card p-6 md:p-8 mb-6">
            <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
              Common Reasons for Payment Failure
            </h2>
            <ul className="space-y-3 text-primary-700 dark:text-sand-200">
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-secondary-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>Insufficient funds in your account</span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-secondary-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>Incorrect card details or expired card</span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-secondary-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>Transaction declined by your bank</span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-secondary-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>Network connectivity issues</span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-secondary-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>Daily transaction limit exceeded</span>
              </li>
            </ul>
          </div>

          {/* Next steps */}
          <div className="card p-6 md:p-8 mb-6 bg-amber-50 dark:bg-amber-900/20 border-amber-200 dark:border-amber-800">
            <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
              What You Can Do
            </h2>
            <ul className="space-y-3 text-primary-700 dark:text-sand-200">
              <li className="flex items-start gap-3">
                <div className="w-6 h-6 bg-amber-100 dark:bg-amber-800 rounded-full flex items-center justify-center flex-shrink-0 mt-0.5">
                  <span className="text-xs font-bold text-amber-700 dark:text-amber-300">1</span>
                </div>
                <span>Check your card details and account balance</span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-6 h-6 bg-amber-100 dark:bg-amber-800 rounded-full flex items-center justify-center flex-shrink-0 mt-0.5">
                  <span className="text-xs font-bold text-amber-700 dark:text-amber-300">2</span>
                </div>
                <span>Contact your bank to ensure the transaction is not blocked</span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-6 h-6 bg-amber-100 dark:bg-amber-800 rounded-full flex items-center justify-center flex-shrink-0 mt-0.5">
                  <span className="text-xs font-bold text-amber-700 dark:text-amber-300">3</span>
                </div>
                <span>Try using a different payment method</span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-6 h-6 bg-amber-100 dark:bg-amber-800 rounded-full flex items-center justify-center flex-shrink-0 mt-0.5">
                  <span className="text-xs font-bold text-amber-700 dark:text-amber-300">4</span>
                </div>
                <span>Contact our support team if the issue persists</span>
              </li>
            </ul>
          </div>

          {/* Action buttons */}
          <div className="grid md:grid-cols-2 gap-4">
            <Button 
              onClick={handleRetry}
              variant="primary"
              size="lg"
              className="flex items-center justify-center gap-2"
            >
              <RefreshCcw className="w-5 h-5" />
              Try Again
            </Button>
            <Link href="/">
              <Button
                variant="secondary"
                size="lg"
                className="flex items-center justify-center gap-2"
              >
                <Home className="w-5 h-5" />
                Back to Home
              </Button>
            </Link>
          </div>

          {/* Support notice */}
          <div className="mt-6 text-center">
            <p className="text-sm text-primary-600 dark:text-sand-400">
              Need help?{' '}
              <Link href="/contact" className="text-secondary-600 dark:text-secondary-400 hover:underline font-medium">
                Contact Support
              </Link>
            </p>
          </div>
        </div>
      </div>
    </ProtectedRoute>
  );
}
