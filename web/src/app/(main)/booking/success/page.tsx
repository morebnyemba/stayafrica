'use client';

import { useSearchParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import {
  CheckCircle, Home, FileText, Loader2, MapPin, Calendar,
  Users, CreditCard, Mail, MessageCircle, Clock, Copy,
} from 'lucide-react';
import Link from 'next/link';
import { toast } from 'react-hot-toast';

export default function BookingSuccessPage() {
  const searchParams = useSearchParams();

  const bookingId = searchParams.get('bookingId');
  const provider = searchParams.get('provider');
  const isPending = searchParams.get('pending') === 'true';

  const { data: booking, isLoading } = useQuery({
    queryKey: ['booking', bookingId],
    queryFn: async () => {
      const response = await apiClient.getBookingById(bookingId!);
      return response.data;
    },
    enabled: !!bookingId,
  });

  const formatDate = (d: string) =>
    new Date(d).toLocaleDateString('en-US', { weekday: 'short', month: 'short', day: 'numeric', year: 'numeric' });

  const copyRef = () => {
    if (booking?.booking_ref) {
      navigator.clipboard.writeText(booking.booking_ref);
      toast.success('Reference copied!');
    }
  };

  if (!bookingId) {
    return (
      <ProtectedRoute>
        <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center px-4">
          <div className="card p-8 text-center max-w-md">
            <div className="w-16 h-16 bg-green-100 dark:bg-green-900/30 rounded-full flex items-center justify-center mx-auto mb-4">
              <CheckCircle className="w-10 h-10 text-green-600 dark:text-green-400" />
            </div>
            <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-3">Payment Received</h2>
            <p className="text-primary-600 dark:text-sand-300 mb-6">Your payment has been processed. Check your bookings for details.</p>
            <div className="flex gap-3 justify-center">
              <Link href="/bookings" className="bg-secondary-600 hover:bg-secondary-700 text-white font-semibold py-2.5 px-6 rounded-lg transition">
                View My Bookings
              </Link>
              <Link href="/" className="border border-primary-300 dark:border-primary-600 text-primary-700 dark:text-sand-200 font-semibold py-2.5 px-6 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-800 transition">
                Home
              </Link>
            </div>
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

  const propertyTitle = booking?.property_title || booking?.property?.title || 'Property';
  const propertyCity = booking?.property?.city;
  const propertyCountry = booking?.property?.country;
  const propertyImage = booking?.property?.images?.[0]?.image || booking?.property?.images?.[0]?.image_url || booking?.property?.main_image_url;
  const propertyId = booking?.property?.id || booking?.rental_property;
  const nights = booking?.check_in && booking?.check_out
    ? Math.ceil((new Date(booking.check_out).getTime() - new Date(booking.check_in).getTime()) / (1000 * 60 * 60 * 24))
    : 0;

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 py-12 px-4">
        <div className="max-w-2xl mx-auto">

          {/* Animated success header */}
          <div className="text-center mb-10">
            <div className="relative inline-flex items-center justify-center mb-6">
              <div className="absolute w-24 h-24 bg-green-200 dark:bg-green-800/30 rounded-full animate-ping opacity-20" />
              <div className="w-20 h-20 bg-green-100 dark:bg-green-900/30 rounded-full flex items-center justify-center">
                <CheckCircle className="w-12 h-12 text-green-600 dark:text-green-400" />
              </div>
            </div>
            <h1 className="text-3xl sm:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-2">
              {isPending ? 'Booking submitted!' : 'Booking confirmed!'}
            </h1>
            <p className="text-primary-600 dark:text-sand-300 text-lg">
              {isPending
                ? 'Your payment is being verified — we\'ll notify you shortly'
                : 'Your reservation is all set. Get ready for your trip!'}
            </p>
          </div>

          {/* Property card with image */}
          <div className="card overflow-hidden mb-6">
            {propertyImage && (
              <div className="relative h-40 sm:h-48">
                <img src={propertyImage} alt={propertyTitle} className="w-full h-full object-cover" />
                <div className="absolute inset-0 bg-gradient-to-t from-black/50 to-transparent" />
                <div className="absolute bottom-4 left-5 right-5">
                  <h3 className="text-white text-lg font-semibold drop-shadow-sm">{propertyTitle}</h3>
                  {(propertyCity || propertyCountry) && (
                    <p className="text-white/80 text-sm flex items-center gap-1 mt-0.5">
                      <MapPin className="w-3.5 h-3.5" />
                      {[propertyCity, propertyCountry].filter(Boolean).join(', ')}
                    </p>
                  )}
                </div>
              </div>
            )}
            {!propertyImage && (
              <div className="p-5 pb-0">
                <h3 className="font-semibold text-primary-900 dark:text-sand-50">{propertyTitle}</h3>
                {(propertyCity || propertyCountry) && (
                  <p className="text-sm text-primary-500 dark:text-sand-400 flex items-center gap-1 mt-1">
                    <MapPin className="w-3.5 h-3.5" />
                    {[propertyCity, propertyCountry].filter(Boolean).join(', ')}
                  </p>
                )}
              </div>
            )}

            <div className="p-5">
              {/* Booking reference — prominent */}
              <div className="flex items-center justify-between p-3 rounded-lg bg-sand-50 dark:bg-primary-800 mb-4">
                <div>
                  <div className="text-[11px] uppercase tracking-wider font-semibold text-primary-500 dark:text-sand-400">
                    Booking reference
                  </div>
                  <div className="font-mono text-lg font-bold text-secondary-700 dark:text-secondary-400">
                    {booking?.booking_ref}
                  </div>
                </div>
                <button
                  onClick={copyRef}
                  className="p-2 rounded-lg hover:bg-primary-100 dark:hover:bg-primary-700 transition text-primary-500 dark:text-sand-400"
                  title="Copy reference"
                >
                  <Copy className="w-5 h-5" />
                </button>
              </div>

              {/* Trip details grid */}
              <div className="grid grid-cols-2 gap-4 mb-4">
                <div className="flex items-start gap-2.5">
                  <Calendar className="w-4 h-4 text-secondary-600 dark:text-secondary-400 mt-1 flex-shrink-0" />
                  <div>
                    <div className="text-[11px] uppercase tracking-wider font-semibold text-primary-500 dark:text-sand-400">Check-in</div>
                    <div className="text-sm font-medium text-primary-900 dark:text-sand-50">
                      {booking?.check_in ? formatDate(booking.check_in) : '—'}
                    </div>
                  </div>
                </div>
                <div className="flex items-start gap-2.5">
                  <Calendar className="w-4 h-4 text-secondary-600 dark:text-secondary-400 mt-1 flex-shrink-0" />
                  <div>
                    <div className="text-[11px] uppercase tracking-wider font-semibold text-primary-500 dark:text-sand-400">Check-out</div>
                    <div className="text-sm font-medium text-primary-900 dark:text-sand-50">
                      {booking?.check_out ? formatDate(booking.check_out) : '—'}
                    </div>
                  </div>
                </div>
                <div className="flex items-start gap-2.5">
                  <Clock className="w-4 h-4 text-secondary-600 dark:text-secondary-400 mt-1 flex-shrink-0" />
                  <div>
                    <div className="text-[11px] uppercase tracking-wider font-semibold text-primary-500 dark:text-sand-400">Duration</div>
                    <div className="text-sm font-medium text-primary-900 dark:text-sand-50">
                      {nights} night{nights !== 1 ? 's' : ''}
                    </div>
                  </div>
                </div>
                <div className="flex items-start gap-2.5">
                  <Users className="w-4 h-4 text-secondary-600 dark:text-secondary-400 mt-1 flex-shrink-0" />
                  <div>
                    <div className="text-[11px] uppercase tracking-wider font-semibold text-primary-500 dark:text-sand-400">Guests</div>
                    <div className="text-sm font-medium text-primary-900 dark:text-sand-50">
                      {booking?.number_of_guests || 1} guest{(booking?.number_of_guests || 1) !== 1 ? 's' : ''}
                    </div>
                  </div>
                </div>
              </div>

              {/* Payment summary */}
              <div className="flex items-center justify-between pt-4 border-t border-primary-100 dark:border-primary-800">
                <div>
                  <div className="text-sm text-primary-500 dark:text-sand-400 flex items-center gap-1.5">
                    <CreditCard className="w-3.5 h-3.5" />
                    Paid via {provider?.replace(/_/g, ' ') || 'card'}
                  </div>
                </div>
                <div className="text-right">
                  <div className="text-[11px] uppercase tracking-wider font-semibold text-primary-500 dark:text-sand-400">Total</div>
                  <div className="text-xl font-bold text-secondary-700 dark:text-secondary-400">
                    {booking?.currency} {booking?.grand_total}
                  </div>
                </div>
              </div>
            </div>
          </div>

          {/* What's next */}
          <div className="card p-6 mb-6">
            <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-4">
              What happens next
            </h2>
            <div className="space-y-3">
              {[
                { icon: Mail, text: 'A confirmation email has been sent to your inbox', done: true },
                { icon: MessageCircle, text: 'The host may contact you with check-in instructions', done: false },
                { icon: FileText, text: 'View full booking details in your dashboard anytime', done: false },
              ].map((item, i) => (
                <div key={i} className="flex items-start gap-3">
                  <div className={`w-8 h-8 rounded-full flex items-center justify-center flex-shrink-0 ${
                    item.done
                      ? 'bg-green-100 dark:bg-green-900/30'
                      : 'bg-primary-100 dark:bg-primary-800'
                  }`}>
                    <item.icon className={`w-4 h-4 ${
                      item.done
                        ? 'text-green-600 dark:text-green-400'
                        : 'text-primary-500 dark:text-sand-400'
                    }`} />
                  </div>
                  <p className="text-sm text-primary-700 dark:text-sand-200 pt-1.5">{item.text}</p>
                </div>
              ))}
              {provider === 'cash_on_arrival' && (
                <div className="flex items-start gap-3">
                  <div className="w-8 h-8 rounded-full flex items-center justify-center flex-shrink-0 bg-amber-100 dark:bg-amber-900/30">
                    <CreditCard className="w-4 h-4 text-amber-600 dark:text-amber-400" />
                  </div>
                  <p className="text-sm text-amber-700 dark:text-amber-300 pt-1.5 font-medium">
                    Remember to bring cash for the accommodation cost on arrival
                  </p>
                </div>
              )}
            </div>
          </div>

          {/* Action buttons */}
          <div className="grid sm:grid-cols-2 gap-3">
            <Link
              href="/bookings"
              className="flex items-center justify-center gap-2 bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white font-semibold py-3 px-4 rounded-lg transition"
            >
              <FileText className="w-5 h-5" />
              View my bookings
            </Link>
            <Link
              href={propertyId ? `/property/${propertyId}` : '/'}
              className="flex items-center justify-center gap-2 border border-primary-300 dark:border-primary-600 text-primary-700 dark:text-sand-200 font-semibold py-3 px-4 rounded-lg hover:bg-sand-50 dark:hover:bg-primary-800 transition"
            >
              <Home className="w-5 h-5" />
              Back to property
            </Link>
          </div>
        </div>
      </div>
    </ProtectedRoute>
  );
}
