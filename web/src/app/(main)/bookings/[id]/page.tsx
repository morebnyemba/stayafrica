'use client';

import { useParams, useRouter } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/store/auth-store';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(
  () => import('@/components/auth/protected-route').then((m) => m.ProtectedRoute),
  { ssr: false }
);
import {
  ArrowLeft,
  Calendar,
  MapPin,
  Users,
  CreditCard,
  AlertCircle,
  CheckCircle,
  Clock,
  XCircle,
  Loader2,
  Home,
  RefreshCw,
} from 'lucide-react';
import { Button } from '@/components/ui';
import Link from 'next/link';

export default function BookingDetailPage() {
  const params = useParams();
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const bookingId = params.id as string;

  const {
    data: booking,
    isLoading,
    isError,
    error,
  } = useQuery({
    queryKey: ['booking', bookingId],
    queryFn: async () => {
      const response = await apiClient.getBookingById(bookingId);
      return response.data;
    },
    enabled: isAuthenticated && !!bookingId,
  });

  const {
    data: payment,
    isLoading: paymentLoading,
  } = useQuery({
    queryKey: ['booking-payment', bookingId],
    queryFn: async () => {
      try {
        // Fetch payments filtered by booking id
        const response = await apiClient.getPaymentHistory({ booking: bookingId });
        const payments = response.data?.results || response.data || [];
        if (Array.isArray(payments) && payments.length > 0) {
          return payments[0];
        }
        return null;
      } catch {
        return null;
      }
    },
    enabled: isAuthenticated && !!bookingId,
    refetchInterval: 15000, // Poll every 15s to catch webhook updates
  });

  const getStatusConfig = (status: string) => {
    const s = status?.toLowerCase();
    switch (s) {
      case 'confirmed':
        return {
          color: 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200',
          icon: CheckCircle,
          label: 'Confirmed',
        };
      case 'pending':
        return {
          color: 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200',
          icon: Clock,
          label: 'Pending',
        };
      case 'cancelled':
        return {
          color: 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200',
          icon: XCircle,
          label: 'Cancelled',
        };
      case 'completed':
        return {
          color: 'bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200',
          icon: CheckCircle,
          label: 'Completed',
        };
      default:
        return {
          color: 'bg-primary-100 text-primary-800 dark:bg-primary-700 dark:text-sand-200',
          icon: Clock,
          label: status,
        };
    }
  };

  // Determine if payment action is available
  const canPay =
    booking &&
    ['pending', 'confirmed'].includes(booking.status?.toLowerCase()) &&
    (!payment || ['failed', 'pending'].includes(payment.status?.toLowerCase()));

  const paymentProcessing =
    payment && payment.status?.toLowerCase() === 'initiated';

  const paymentSuccess =
    payment && payment.status?.toLowerCase() === 'success';

  const statusConfig = booking ? getStatusConfig(booking.status) : null;

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-50 dark:bg-primary-950">
        <div className="max-w-3xl mx-auto px-4 py-8">
          {/* Back navigation */}
          <button
            onClick={() => router.push('/bookings')}
            className="flex items-center gap-2 text-primary-600 dark:text-sand-300 hover:text-primary-900 dark:hover:text-sand-50 mb-6 transition-colors"
          >
            <ArrowLeft className="w-4 h-4" />
            <span>Back to Bookings</span>
          </button>

          {isLoading ? (
            <div className="card p-12 text-center">
              <Loader2 className="w-8 h-8 animate-spin text-primary-500 mx-auto mb-4" />
              <p className="text-primary-600 dark:text-sand-300">Loading booking details...</p>
            </div>
          ) : isError ? (
            <div className="card p-12 text-center">
              <AlertCircle className="w-12 h-12 text-red-500 mx-auto mb-4" />
              <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
                Booking Not Found
              </h3>
              <p className="text-primary-600 dark:text-sand-300 mb-6">
                {(error as any)?.response?.status === 404
                  ? 'This booking does not exist or you do not have access.'
                  : 'Failed to load booking details. Please try again.'}
              </p>
              <Button onClick={() => router.push('/bookings')}>Back to Bookings</Button>
            </div>
          ) : booking ? (
            <div className="space-y-6">
              {/* Header */}
              <div className="card p-6">
                <div className="flex flex-col md:flex-row md:items-start justify-between gap-4">
                  <div className="flex-1">
                    <div className="flex items-center gap-3 mb-2">
                      <span className="inline-flex items-center gap-1 text-xs font-medium text-primary-500 dark:text-sand-400 bg-primary-100 dark:bg-primary-800 px-2 py-0.5 rounded">
                        <Home className="w-3 h-3" /> Stay
                      </span>
                      {statusConfig && (
                        <span
                          className={`px-3 py-1 rounded-full text-xs font-semibold ${statusConfig.color}`}
                        >
                          {statusConfig.label}
                        </span>
                      )}
                    </div>
                    <h1 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-1">
                      {booking.property?.title || booking.property_title || 'Property'}
                    </h1>
                    <div className="flex items-center gap-2 text-primary-600 dark:text-sand-300 text-sm">
                      <MapPin className="w-4 h-4" />
                      <span>
                        {booking.property?.city || 'Unknown'},{' '}
                        {booking.property?.country || 'Unknown'}
                      </span>
                    </div>
                  </div>
                  <div className="text-right">
                    <div className="text-sm text-primary-500 dark:text-sand-400">
                      Booking Ref
                    </div>
                    <div className="text-lg font-mono font-bold text-primary-900 dark:text-sand-50">
                      {booking.booking_ref}
                    </div>
                  </div>
                </div>
              </div>

              {/* Dates & Guests */}
              <div className="card p-6">
                <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  Stay Details
                </h2>
                <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                  <div>
                    <div className="flex items-center gap-1 text-sm text-primary-500 dark:text-sand-400 mb-1">
                      <Calendar className="w-3.5 h-3.5" /> Check-in
                    </div>
                    <div className="font-semibold text-primary-900 dark:text-sand-50">
                      {new Date(booking.check_in).toLocaleDateString()}
                    </div>
                  </div>
                  <div>
                    <div className="flex items-center gap-1 text-sm text-primary-500 dark:text-sand-400 mb-1">
                      <Calendar className="w-3.5 h-3.5" /> Check-out
                    </div>
                    <div className="font-semibold text-primary-900 dark:text-sand-50">
                      {new Date(booking.check_out).toLocaleDateString()}
                    </div>
                  </div>
                  <div>
                    <div className="flex items-center gap-1 text-sm text-primary-500 dark:text-sand-400 mb-1">
                      <Clock className="w-3.5 h-3.5" /> Nights
                    </div>
                    <div className="font-semibold text-primary-900 dark:text-sand-50">
                      {booking.nights}
                    </div>
                  </div>
                  <div>
                    <div className="flex items-center gap-1 text-sm text-primary-500 dark:text-sand-400 mb-1">
                      <Users className="w-3.5 h-3.5" /> Guests
                    </div>
                    <div className="font-semibold text-primary-900 dark:text-sand-50">
                      {booking.number_of_guests}
                    </div>
                  </div>
                </div>
                {booking.special_requests && (
                  <div className="mt-4 pt-4 border-t border-primary-200 dark:border-primary-700">
                    <div className="text-sm text-primary-500 dark:text-sand-400 mb-1">
                      Special Requests
                    </div>
                    <p className="text-primary-900 dark:text-sand-50">
                      {booking.special_requests}
                    </p>
                  </div>
                )}
              </div>

              {/* Price Breakdown */}
              <div className="card p-6">
                <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  Price Breakdown
                </h2>
                <div className="space-y-3">
                  <div className="flex justify-between text-primary-700 dark:text-sand-200">
                    <span>
                      Nightly total ({booking.nights} night{booking.nights !== 1 ? 's' : ''})
                    </span>
                    <span>
                      {booking.currency} {Number(booking.nightly_total).toFixed(2)}
                    </span>
                  </div>
                  {Number(booking.cleaning_fee) > 0 && (
                    <div className="flex justify-between text-primary-700 dark:text-sand-200">
                      <span>Cleaning fee</span>
                      <span>
                        {booking.currency} {Number(booking.cleaning_fee).toFixed(2)}
                      </span>
                    </div>
                  )}
                  <div className="flex justify-between text-primary-700 dark:text-sand-200">
                    <span>Service fee</span>
                    <span>
                      {booking.currency} {Number(booking.service_fee).toFixed(2)}
                    </span>
                  </div>
                  <div className="border-t border-primary-200 dark:border-primary-700 pt-3 flex justify-between font-bold text-primary-900 dark:text-sand-50 text-lg">
                    <span>Total</span>
                    <span>
                      {booking.currency} {Number(booking.grand_total).toFixed(2)}
                    </span>
                  </div>
                </div>
              </div>

              {/* Payment Status */}
              <div className="card p-6">
                <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  Payment
                </h2>

                {paymentLoading ? (
                  <div className="flex items-center gap-2 text-primary-500 dark:text-sand-400">
                    <Loader2 className="w-4 h-4 animate-spin" />
                    <span>Checking payment status...</span>
                  </div>
                ) : paymentSuccess ? (
                  <div className="flex items-center gap-3 p-4 bg-green-50 dark:bg-green-900/20 rounded-lg">
                    <CheckCircle className="w-6 h-6 text-green-600 dark:text-green-400" />
                    <div>
                      <div className="font-semibold text-green-800 dark:text-green-200">
                        Payment Successful
                      </div>
                      <div className="text-sm text-green-600 dark:text-green-400">
                        Paid via {payment.provider} &mdash; Ref: {payment.gateway_ref}
                      </div>
                    </div>
                  </div>
                ) : paymentProcessing ? (
                  <div className="space-y-4">
                    <div className="flex items-center gap-3 p-4 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
                      <Loader2 className="w-6 h-6 text-blue-600 dark:text-blue-400 animate-spin" />
                      <div>
                        <div className="font-semibold text-blue-800 dark:text-blue-200">
                          Payment Processing
                        </div>
                        <div className="text-sm text-blue-600 dark:text-blue-400">
                          Your payment is being processed. This page will update when complete.
                        </div>
                      </div>
                    </div>
                  </div>
                ) : payment && payment.status?.toLowerCase() === 'failed' ? (
                  <div className="space-y-4">
                    <div className="flex items-center gap-3 p-4 bg-red-50 dark:bg-red-900/20 rounded-lg">
                      <XCircle className="w-6 h-6 text-red-600 dark:text-red-400" />
                      <div>
                        <div className="font-semibold text-red-800 dark:text-red-200">
                          Payment Failed
                        </div>
                        <div className="text-sm text-red-600 dark:text-red-400">
                          Your previous payment attempt was unsuccessful. You can try again below.
                        </div>
                      </div>
                    </div>
                    <Link href={`/booking/payment?bookingId=${booking.id}`}>
                      <Button className="w-full flex items-center justify-center gap-2">
                        <RefreshCw className="w-4 h-4" />
                        Retry Payment
                      </Button>
                    </Link>
                  </div>
                ) : canPay ? (
                  <div className="space-y-4">
                    <div className="flex items-center gap-3 p-4 bg-yellow-50 dark:bg-yellow-900/20 rounded-lg">
                      <AlertCircle className="w-6 h-6 text-yellow-600 dark:text-yellow-400" />
                      <div>
                        <div className="font-semibold text-yellow-800 dark:text-yellow-200">
                          Payment Required
                        </div>
                        <div className="text-sm text-yellow-600 dark:text-yellow-400">
                          Complete your payment to confirm this booking.
                        </div>
                      </div>
                    </div>
                    <Link href={`/booking/payment?bookingId=${booking.id}`}>
                      <Button className="w-full flex items-center justify-center gap-2">
                        <CreditCard className="w-4 h-4" />
                        Pay Now
                      </Button>
                    </Link>
                  </div>
                ) : (
                  <div className="text-primary-500 dark:text-sand-400 text-sm">
                    No payment information available.
                  </div>
                )}
              </div>

              {/* Actions */}
              <div className="card p-6">
                <div className="flex flex-wrap gap-3">
                  {booking.property?.id && (
                    <Link href={`/properties/${booking.property.id}`}>
                      <Button variant="secondary" size="sm">
                        View Property
                      </Button>
                    </Link>
                  )}
                  {(booking.status === 'confirmed' || booking.status === 'CONFIRMED') && (
                    <Link href={`/bookings/${booking.id}/directions`}>
                      <Button variant="outline" size="sm">
                        Get Directions
                      </Button>
                    </Link>
                  )}
                </div>
              </div>

              {/* Booking meta */}
              <div className="text-center text-xs text-primary-400 dark:text-sand-500">
                Booked on {new Date(booking.created_at).toLocaleDateString()} &mdash; Last updated{' '}
                {new Date(booking.updated_at).toLocaleDateString()}
              </div>
            </div>
          ) : null}
        </div>
      </div>
    </ProtectedRoute>
  );
}
