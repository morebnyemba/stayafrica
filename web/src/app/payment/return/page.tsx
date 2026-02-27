'use client';

import { useEffect, useState, useRef } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { CheckCircle, XCircle, Loader2, Home } from 'lucide-react';
import Link from 'next/link';
import { Button } from '@/components/ui';

interface PendingPayment {
  bookingId: string;
  provider: string;
  paymentId: string;
  gateway_ref: string;
  paypal_order_id?: string;
}

export default function PaymentReturnPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const [pendingPayment, setPendingPayment] = useState<PendingPayment | null>(null);
  const [captureStatus, setCaptureStatus] = useState<'idle' | 'capturing' | 'captured' | 'error'>('idle');
  const [captureError, setCaptureError] = useState<string | null>(null);
  const captureAttempted = useRef(false);

  // Read stored payment info on mount, with URL param fallback
  useEffect(() => {
    if (typeof window !== 'undefined') {
      const stored = localStorage.getItem('pending_payment');
      if (stored) {
        try {
          const parsed = JSON.parse(stored) as PendingPayment;
          // Merge provider from URL if available (PayPal redirect includes ?provider=paypal)
          const urlProvider = searchParams.get('provider');
          if (urlProvider && !parsed.provider) {
            parsed.provider = urlProvider;
          }
          setPendingPayment(parsed);
        } catch (e) {
          console.error('Failed to parse pending payment data:', e);
        }
      }

      // Fallback: if no localStorage data but we have URL params (e.g., cleared storage)
      if (!stored) {
        const urlGatewayRef = searchParams.get('gateway_ref');
        const urlProvider = searchParams.get('provider');
        if (urlGatewayRef) {
          setPendingPayment({
            bookingId: '',
            provider: urlProvider || '',
            paymentId: '',
            gateway_ref: urlGatewayRef,
            paypal_order_id: urlProvider === 'paypal' ? urlGatewayRef : undefined,
          });
        }
      }
    }
  }, [searchParams]);

  // For PayPal: capture the order before polling
  useEffect(() => {
    if (!pendingPayment) return;
    if (captureAttempted.current) return;

    const isPayPal = pendingPayment.provider === 'paypal';
    if (!isPayPal) {
      setCaptureStatus('captured'); // Non-PayPal: skip capture, go straight to polling
      return;
    }

    const orderId = pendingPayment.paypal_order_id || pendingPayment.gateway_ref;
    if (!orderId) {
      setCaptureStatus('error');
      setCaptureError('Missing PayPal order ID');
      return;
    }

    captureAttempted.current = true;
    setCaptureStatus('capturing');

    apiClient.capturePaypalOrder(orderId)
      .then((res) => {
        const data = res.data;
        if (data.status === 'captured' || data.status === 'already_captured') {
          setCaptureStatus('captured');
        } else {
          setCaptureStatus('error');
          setCaptureError(data.error || 'Capture failed');
        }
      })
      .catch((err) => {
        console.error('PayPal capture error:', err);
        setCaptureStatus('error');
        setCaptureError(err.response?.data?.error || 'Failed to capture payment');
      });
  }, [pendingPayment]);

  // Poll for payment status (only after capture completes or for non-PayPal)
  const { data: payment, isLoading, error } = useQuery({
    queryKey: ['payment-return-status', pendingPayment?.paymentId],
    queryFn: async () => {
      if (pendingPayment?.paymentId) {
        const response = await apiClient.getPaymentStatus(pendingPayment.paymentId);
        return response.data;
      }
      return null;
    },
    enabled: !!pendingPayment?.paymentId && captureStatus === 'captured',
    refetchInterval: (query) => {
      const data = query.state.data;
      if (data?.status === 'success' || data?.status === 'failed') {
        return false;
      }
      return 3000;
    },
  });

  // Redirect to success page on successful payment
  useEffect(() => {
    if ((payment?.status === 'success' || captureStatus === 'captured') && pendingPayment) {
      // For PayPal, capture success is enough even without polling
      const isSuccess = payment?.status === 'success' || (pendingPayment.provider === 'paypal' && captureStatus === 'captured');
      if (isSuccess) {
        localStorage.removeItem('pending_payment');
        const timer = setTimeout(() => {
          router.push(
            `/booking/success?bookingId=${pendingPayment.bookingId}&provider=${pendingPayment.provider}`
          );
        }, 2000);
        return () => clearTimeout(timer);
      }
    }
  }, [payment?.status, captureStatus, pendingPayment, router]);

  // Clean up localStorage on failed payment
  useEffect(() => {
    if (payment?.status === 'failed') {
      localStorage.removeItem('pending_payment');
    }
  }, [payment?.status]);

  const isSuccess = payment?.status === 'success' || (pendingPayment?.provider === 'paypal' && captureStatus === 'captured');
  const isFailed = payment?.status === 'failed' || captureStatus === 'error';
  const isProcessing = captureStatus === 'capturing' || (captureStatus === 'captured' && !isSuccess && !isFailed);

  if (!pendingPayment) {
    return (
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center py-12 px-4">
        <div className="max-w-md w-full text-center">
          <Loader2 className="w-16 h-16 text-secondary-600 dark:text-secondary-400 animate-spin mx-auto mb-6" />
          <h1 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-3">
            Processing Payment
          </h1>
          <p className="text-primary-600 dark:text-sand-300 mb-6">
            Please wait while we confirm your payment...
          </p>
          <Link href="/dashboard" className="inline-block">
            <Button variant="secondary">Go to Dashboard</Button>
          </Link>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center py-12 px-4">
      <div className="max-w-md w-full text-center">
        {isProcessing ? (
          <>
            <Loader2 className="w-16 h-16 text-secondary-600 dark:text-secondary-400 animate-spin mx-auto mb-6" />
            <h1 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-3">
              {captureStatus === 'capturing' ? 'Capturing Payment' : 'Processing Payment'}
            </h1>
            <p className="text-primary-600 dark:text-sand-300 mb-6">
              {captureStatus === 'capturing'
                ? 'Finalizing your payment with PayPal...'
                : 'Please wait while we confirm your payment...'}
            </p>
          </>
        ) : isSuccess ? (
          <>
            <div className="inline-flex items-center justify-center w-20 h-20 bg-green-100 dark:bg-green-900/30 rounded-full mb-6">
              <CheckCircle className="w-12 h-12 text-green-600 dark:text-green-400" />
            </div>
            <h1 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-3">
              Payment Successful!
            </h1>
            <p className="text-primary-600 dark:text-sand-300 mb-6">
              Redirecting to your booking confirmation...
            </p>
          </>
        ) : isFailed ? (
          <>
            <div className="inline-flex items-center justify-center w-20 h-20 bg-red-100 dark:bg-red-900/30 rounded-full mb-6">
              <XCircle className="w-12 h-12 text-red-600 dark:text-red-400" />
            </div>
            <h1 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-3">
              Payment Failed
            </h1>
            <p className="text-primary-600 dark:text-sand-300 mb-6">
              {captureError || 'Your payment could not be processed. Please try again.'}
            </p>
            <div className="space-y-3">
              <button
                onClick={() => {
                  if (pendingPayment?.bookingId) {
                    router.push(`/booking/payment?bookingId=${pendingPayment.bookingId}`);
                  } else {
                    router.back();
                  }
                }}
                className="w-full"
              >
                <Button variant="primary" className="w-full">
                  Try Again
                </Button>
              </button>
              <Link href="/" className="inline-block w-full">
                <Button variant="secondary" className="w-full flex items-center justify-center gap-2">
                  <Home className="w-5 h-5" />
                  Back to Home
                </Button>
              </Link>
            </div>
          </>
        ) : null}

        {error && (
          <div className="mt-6">
            <p className="text-red-600 dark:text-red-400 mb-4">
              Unable to check payment status. Please check your bookings.
            </p>
            <Link href="/dashboard" className="inline-block">
              <Button>Go to Dashboard</Button>
            </Link>
          </div>
        )}
      </div>
    </div>
  );
}
