'use client';

import { useRouter, useSearchParams } from 'next/navigation';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import {
  XCircle, Home, RefreshCcw, AlertTriangle, CreditCard,
  Wifi, ShieldAlert, Headphones, ArrowRight,
} from 'lucide-react';
import Link from 'next/link';

export default function BookingFailurePage() {
  const router = useRouter();
  const searchParams = useSearchParams();

  const bookingId = searchParams.get('bookingId');
  const error = searchParams.get('error') || 'Payment could not be completed';
  const provider = searchParams.get('provider');

  const handleRetry = () => {
    if (bookingId) {
      router.push(`/booking/payment?bookingId=${bookingId}`);
    } else {
      router.push('/explore');
    }
  };

  const reasons = [
    { icon: CreditCard, label: 'Insufficient funds or expired card' },
    { icon: ShieldAlert, label: 'Transaction declined by your bank' },
    { icon: Wifi, label: 'Network or connectivity issue' },
    { icon: AlertTriangle, label: 'Daily transaction limit exceeded' },
  ];

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 py-12 px-4">
        <div className="max-w-lg mx-auto">

          {/* Error header */}
          <div className="text-center mb-8">
            <div className="w-20 h-20 bg-red-100 rounded-full flex items-center justify-center mx-auto mb-5">
              <XCircle className="w-12 h-12 text-red-600" />
            </div>
            <h1 className="text-3xl font-bold text-primary-900 mb-2">
              Payment didn&apos;t go through
            </h1>
            <p className="text-primary-600">
              Don&apos;t worry — your booking is still reserved. You can try again.
            </p>
          </div>

          {/* Error detail */}
          <div className="card p-5 mb-5 border-l-4 border-red-400">
            <div className="flex items-start gap-3">
              <AlertTriangle className="w-5 h-5 text-red-500 mt-0.5 flex-shrink-0" />
              <div>
                <p className="text-sm font-medium text-primary-900 mb-1">Error details</p>
                <p className="text-sm text-primary-700">{error}</p>
                {(bookingId || provider) && (
                  <div className="flex flex-wrap gap-x-4 gap-y-1 mt-2 text-xs text-primary-500">
                    {bookingId && <span>Ref: <span className="font-mono">{bookingId}</span></span>}
                    {provider && <span>Via: <span className="capitalize">{provider.replace(/_/g, ' ')}</span></span>}
                  </div>
                )}
              </div>
            </div>
          </div>

          {/* Common reasons */}
          <div className="card p-5 mb-5">
            <h2 className="text-sm font-semibold text-primary-900 uppercase tracking-wider mb-3">
              This can happen when
            </h2>
            <div className="space-y-2.5">
              {reasons.map((r, i) => (
                <div key={i} className="flex items-center gap-3">
                  <div className="w-8 h-8 rounded-full bg-sand-50 flex items-center justify-center flex-shrink-0">
                    <r.icon className="w-4 h-4 text-primary-500" />
                  </div>
                  <span className="text-sm text-primary-700">{r.label}</span>
                </div>
              ))}
            </div>
          </div>

          {/* What to do */}
          <div className="card p-5 mb-6">
            <h2 className="text-sm font-semibold text-primary-900 uppercase tracking-wider mb-3">
              What you can do
            </h2>
            <ol className="space-y-2">
              {[
                'Verify your account balance or card details',
                'Try a different payment method',
                'Contact your bank to allow the transaction',
                'Reach out to our support team if issues persist',
              ].map((step, i) => (
                <li key={i} className="flex items-start gap-2.5">
                  <span className="w-5 h-5 rounded-full bg-secondary-100 flex items-center justify-center flex-shrink-0 mt-0.5 text-[11px] font-bold text-secondary-700">
                    {i + 1}
                  </span>
                  <span className="text-sm text-primary-700">{step}</span>
                </li>
              ))}
            </ol>
          </div>

          {/* Primary CTA */}
          <button
            onClick={handleRetry}
            className="w-full flex items-center justify-center gap-2 bg-secondary-600 hover:bg-secondary-700 text-white font-semibold py-3.5 rounded-lg transition mb-3"
          >
            <RefreshCcw className="w-5 h-5" />
            {bookingId ? 'Try a different payment method' : 'Try again'}
          </button>

          {/* Secondary CTA */}
          <Link
            href="/"
            className="w-full flex items-center justify-center gap-2 border border-primary-300 text-primary-700 font-semibold py-3 rounded-lg hover:bg-sand-50 transition"
          >
            <Home className="w-5 h-5" />
            Back to home
          </Link>

          {/* Support footer */}
          <div className="mt-8 text-center">
            <div className="inline-flex items-center gap-2 text-sm text-primary-500">
              <Headphones className="w-4 h-4" />
              Need help?
              <Link href="/contact" className="text-secondary-600 hover:underline font-medium inline-flex items-center gap-0.5">
                Contact support <ArrowRight className="w-3.5 h-3.5" />
              </Link>
            </div>
          </div>
        </div>
      </div>
    </ProtectedRoute>
  );
}
