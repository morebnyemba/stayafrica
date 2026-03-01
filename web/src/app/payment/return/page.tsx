'use client';

import { useEffect, useState, useRef, useCallback } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { apiClient } from '@/services/api-client';
import { CheckCircle, XCircle, Loader2, Home, Smartphone } from 'lucide-react';
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
  const [paymentData, setPaymentData] = useState<any>(null);
  const [pollError, setPollError] = useState(false);
  const captureAttempted = useRef(false);
  const pollRef = useRef<ReturnType<typeof setInterval> | null>(null);

  const source = searchParams.get('source') || 'web';
  const isMobileSource = source === 'mobile';

  // Read stored payment info on mount, with URL param + public lookup fallback
  useEffect(() => {
    if (typeof window === 'undefined') return;

    const stored = localStorage.getItem('pending_payment');
    if (stored) {
      try {
        const parsed = JSON.parse(stored) as PendingPayment;
        const urlProvider = searchParams.get('provider');
        if (urlProvider && !parsed.provider) parsed.provider = urlProvider;
        setPendingPayment(parsed);
        return;
      } catch (e) {
        console.error('Failed to parse pending payment data:', e);
      }
    }

    // Fallback: use URL params + public lookup
    const urlGatewayRef = searchParams.get('gateway_ref');
    const urlProvider = searchParams.get('provider');

    if (urlGatewayRef) {
      // Try public lookup to get payment details
      apiClient.lookupPaymentByRef(urlGatewayRef)
        .then((res) => {
          const data = res.data;
          setPendingPayment({
            bookingId: data.booking_id || '',
            provider: data.provider || urlProvider || '',
            paymentId: data.id || '',
            gateway_ref: urlGatewayRef,
            paypal_order_id: urlProvider === 'paypal' ? urlGatewayRef : undefined,
          });
          // If already resolved, set payment data directly
          if (data.status === 'success' || data.status === 'failed') {
            setPaymentData(data);
          }
        })
        .catch(() => {
          // Minimal fallback
          setPendingPayment({
            bookingId: '',
            provider: urlProvider || '',
            paymentId: '',
            gateway_ref: urlGatewayRef,
            paypal_order_id: urlProvider === 'paypal' ? urlGatewayRef : undefined,
          });
        });
    }
  }, [searchParams]);

  // For PayPal: capture the order before polling
  useEffect(() => {
    if (!pendingPayment) return;
    if (captureAttempted.current) return;

    const isPayPal = pendingPayment.provider === 'paypal';
    if (!isPayPal) {
      setCaptureStatus('captured');
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

  // Poll for payment status using public lookup (no auth needed)
  const pollPaymentStatus = useCallback(async () => {
    if (!pendingPayment?.gateway_ref) return;
    try {
      const res = await apiClient.lookupPaymentByRef(pendingPayment.gateway_ref);
      const data = res.data;
      setPaymentData(data);
      if (data.status === 'success' || data.status === 'failed') {
        // Stop polling
        if (pollRef.current) clearInterval(pollRef.current);
      }
    } catch {
      // Fallback: try authenticated endpoint if available
      if (pendingPayment.paymentId) {
        try {
          const res = await apiClient.getPaymentStatus(pendingPayment.paymentId);
          setPaymentData(res.data);
          if (res.data?.status === 'success' || res.data?.status === 'failed') {
            if (pollRef.current) clearInterval(pollRef.current);
          }
        } catch {
          setPollError(true);
        }
      }
    }
  }, [pendingPayment]);

  useEffect(() => {
    if (captureStatus !== 'captured' || !pendingPayment) return;
    // Already have final status from initial lookup
    if (paymentData?.status === 'success' || paymentData?.status === 'failed') return;

    pollPaymentStatus();
    pollRef.current = setInterval(pollPaymentStatus, 3000);
    return () => { if (pollRef.current) clearInterval(pollRef.current); };
  }, [captureStatus, pendingPayment, pollPaymentStatus, paymentData?.status]);

  // Handle redirect on success
  const handleRedirect = useCallback((bookingId: string, provider: string) => {
    if (isMobileSource) {
      // Redirect back to mobile app via deep link
      window.location.href = `stayafrica://booking/success?bookingId=${bookingId}`;
    } else {
      router.push(`/booking/success?bookingId=${bookingId}&provider=${provider}`);
    }
  }, [isMobileSource, router]);

  // Redirect on success
  useEffect(() => {
    if (!pendingPayment) return;
    const isPayPalCaptured = pendingPayment.provider === 'paypal' && captureStatus === 'captured';
    const isSuccess = paymentData?.status === 'success' || isPayPalCaptured;

    if (isSuccess) {
      localStorage.removeItem('pending_payment');
      const bookingId = pendingPayment.bookingId || paymentData?.booking_id || '';
      const timer = setTimeout(() => handleRedirect(bookingId, pendingPayment.provider), 2000);
      return () => clearTimeout(timer);
    }
  }, [paymentData?.status, captureStatus, pendingPayment, handleRedirect]);

  // Clean up localStorage on failed payment
  useEffect(() => {
    if (paymentData?.status === 'failed') {
      localStorage.removeItem('pending_payment');
    }
  }, [paymentData?.status]);

  const isPayPalCaptured = pendingPayment?.provider === 'paypal' && captureStatus === 'captured';
  const isSuccess = paymentData?.status === 'success' || isPayPalCaptured;
  const isFailed = paymentData?.status === 'failed' || captureStatus === 'error';
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
          {!isMobileSource && (
            <Link href="/dashboard" className="inline-block">
              <Button variant="secondary">Go to Dashboard</Button>
            </Link>
          )}
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
              {isMobileSource
                ? 'Redirecting you back to the app...'
                : 'Redirecting to your booking confirmation...'}
            </p>
            {isMobileSource && (
              <button
                onClick={() => {
                  const bookingId = pendingPayment.bookingId || paymentData?.booking_id || '';
                  window.location.href = `stayafrica://booking/success?bookingId=${bookingId}`;
                }}
                className="w-full"
              >
                <Button variant="primary" className="w-full flex items-center justify-center gap-2">
                  <Smartphone className="w-5 h-5" />
                  Open in App
                </Button>
              </button>
            )}
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
              {isMobileSource ? (
                <button
                  onClick={() => {
                    const bookingId = pendingPayment.bookingId || paymentData?.booking_id || '';
                    window.location.href = `stayafrica://booking/payment?bookingId=${bookingId}`;
                  }}
                  className="w-full"
                >
                  <Button variant="primary" className="w-full flex items-center justify-center gap-2">
                    <Smartphone className="w-5 h-5" />
                    Retry in App
                  </Button>
                </button>
              ) : (
                <>
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
                </>
              )}
            </div>
          </>
        ) : null}

        {pollError && (
          <div className="mt-6">
            <p className="text-red-600 dark:text-red-400 mb-4">
              Unable to check payment status. Please check your bookings.
            </p>
            {isMobileSource ? (
              <button onClick={() => { window.location.href = 'stayafrica://bookings'; }} className="inline-block">
                <Button>Open App</Button>
              </button>
            ) : (
              <Link href="/dashboard" className="inline-block">
                <Button>Go to Dashboard</Button>
              </Link>
            )}
          </div>
        )}
      </div>
    </div>
  );
}
