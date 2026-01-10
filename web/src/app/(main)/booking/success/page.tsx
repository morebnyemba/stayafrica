'use client';

import { useSearchParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { CheckCircle, Home, FileText, Loader2 } from 'lucide-react';
import Link from 'next/link';
import { Button } from '@/components/ui';

export default function BookingSuccessPage() {
  const searchParams = useSearchParams();
  
  const bookingId = searchParams.get('bookingId');
  const provider = searchParams.get('provider');

  // Fetch booking details
  const { data: booking, isLoading } = useQuery({
    queryKey: ['booking', bookingId],
    queryFn: async () => {
      const response = await apiClient.getBookingById(bookingId!);
      return response.data;
    },
    enabled: !!bookingId,
  });

  if (!bookingId) {
    return (
      <ProtectedRoute>
        <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
          <div className="card p-8 text-center max-w-md">
            <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-4">
              Invalid Request
            </h2>
            <Link href="/explore" className="inline-block">
              <Button>Browse Properties</Button>
            </Link>
          </div>
        </div>
      </ProtectedRoute>
    );
  }

  if (isLoading) {
    return (
      <ProtectedRoute>
        <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
          <div className="text-center">
            <Loader2 className="w-12 h-12 text-secondary-600 dark:text-secondary-400 animate-spin mx-auto mb-4" />
            <p className="text-primary-700 dark:text-sand-200">Loading booking details...</p>
          </div>
        </div>
      </ProtectedRoute>
    );
  }

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center py-12 px-4 pt-20 sm:pt-12">
        <div className="max-w-2xl w-full">
          {/* Success icon */}
          <div className="text-center mb-8">
            <div className="inline-flex items-center justify-center w-20 h-20 bg-green-100 dark:bg-green-900/30 rounded-full mb-6">
              <CheckCircle className="w-12 h-12 text-green-600 dark:text-green-400" />
            </div>
            <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-3">
              Booking Confirmed!
            </h1>
            <p className="text-lg text-primary-600 dark:text-sand-300">
              Your reservation has been successfully created
            </p>
          </div>

          {/* Booking details card */}
          <div className="card p-6 md:p-8 mb-6">
            <div className="space-y-4">
              <div className="flex justify-between items-center pb-4 border-b border-primary-200 dark:border-primary-700">
                <span className="text-primary-600 dark:text-sand-400">Booking Reference</span>
                <span className="font-bold text-lg text-secondary-600 dark:text-secondary-400">
                  {booking.booking_ref}
                </span>
              </div>

              <div className="grid md:grid-cols-2 gap-4 py-4 border-b border-primary-200 dark:border-primary-700">
                <div>
                  <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Property</div>
                  <div className="font-semibold text-primary-900 dark:text-sand-50">
                    {booking.rental_property.title}
                  </div>
                </div>
                <div>
                  <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Location</div>
                  <div className="font-semibold text-primary-900 dark:text-sand-50">
                    {booking.rental_property.city}, {booking.rental_property.country}
                  </div>
                </div>
              </div>

              <div className="grid md:grid-cols-2 gap-4 py-4 border-b border-primary-200 dark:border-primary-700">
                <div>
                  <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Check-in</div>
                  <div className="font-semibold text-primary-900 dark:text-sand-50">
                    {new Date(booking.check_in).toLocaleDateString('en-US', { 
                      weekday: 'short',
                      month: 'short', 
                      day: 'numeric', 
                      year: 'numeric' 
                    })}
                  </div>
                </div>
                <div>
                  <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Check-out</div>
                  <div className="font-semibold text-primary-900 dark:text-sand-50">
                    {new Date(booking.check_out).toLocaleDateString('en-US', { 
                      weekday: 'short',
                      month: 'short', 
                      day: 'numeric', 
                      year: 'numeric' 
                    })}
                  </div>
                </div>
              </div>

              <div className="grid md:grid-cols-2 gap-4 py-4 border-b border-primary-200 dark:border-primary-700">
                <div>
                  <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Guests</div>
                  <div className="font-semibold text-primary-900 dark:text-sand-50">
                    {booking.number_of_guests} {booking.number_of_guests === 1 ? 'guest' : 'guests'}
                  </div>
                </div>
                <div>
                  <div className="text-sm text-primary-600 dark:text-sand-400 mb-1">Payment Method</div>
                  <div className="font-semibold text-primary-900 dark:text-sand-50 capitalize">
                    {provider?.replace('_', ' ')}
                  </div>
                </div>
              </div>

              <div className="flex justify-between items-center pt-4">
                <span className="text-lg font-semibold text-primary-900 dark:text-sand-50">Total Paid</span>
                <span className="text-2xl font-bold text-secondary-600 dark:text-secondary-400">
                  {booking.currency} {booking.grand_total}
                </span>
              </div>
            </div>
          </div>

          {/* Next steps */}
          <div className="card p-6 md:p-8 mb-6 bg-secondary-50 dark:bg-secondary-900/20 border-secondary-200 dark:border-secondary-800">
            <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
              What's Next?
            </h2>
            <ul className="space-y-3 text-primary-700 dark:text-sand-200">
              <li className="flex items-start gap-3">
                <CheckCircle className="w-5 h-5 text-secondary-600 dark:text-secondary-400 mt-0.5 flex-shrink-0" />
                <span>A confirmation email has been sent to your email address</span>
              </li>
              <li className="flex items-start gap-3">
                <CheckCircle className="w-5 h-5 text-secondary-600 dark:text-secondary-400 mt-0.5 flex-shrink-0" />
                <span>The host will review your booking and may contact you</span>
              </li>
              <li className="flex items-start gap-3">
                <CheckCircle className="w-5 h-5 text-secondary-600 dark:text-secondary-400 mt-0.5 flex-shrink-0" />
                <span>You can view your booking details in your dashboard</span>
              </li>
              {provider === 'cash_on_arrival' && (
                <li className="flex items-start gap-3">
                  <CheckCircle className="w-5 h-5 text-secondary-600 dark:text-secondary-400 mt-0.5 flex-shrink-0" />
                  <span className="font-medium">Remember to bring cash payment on arrival</span>
                </li>
              )}
            </ul>
          </div>

          {/* Action buttons */}
          <div className="grid md:grid-cols-2 gap-4">
            <Link 
              href="/bookings"
              className="inline-block w-full"
            >
              <Button className="w-full flex items-center justify-center gap-2">
                <FileText className="w-5 h-5" />
                View My Bookings
              </Button>
            </Link>
            <Link 
              href="/"
              className="inline-block w-full"
            >
              <Button variant="secondary" className="w-full flex items-center justify-center gap-2">
                <Home className="w-5 h-5" />
                Back to Home
              </Button>
            </Link>
          </div>
        </div>
      </div>
    </ProtectedRoute>
  );
}
