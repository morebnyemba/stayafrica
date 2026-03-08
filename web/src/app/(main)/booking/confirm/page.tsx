'use client';

import { useState } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/store/auth-store';
import { useFeeConfiguration, calculateBookingCost } from '@/hooks/use-fees';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import {
  MapPin, Calendar, Users, ArrowLeft, Loader2,
  Star, Shield, MessageCircle, Clock, CheckCircle2,
  Bed, Bath, Home,
} from 'lucide-react';
import { toast } from 'react-hot-toast';
import Link from 'next/link';

export default function BookingConfirmPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { user } = useAuth();

  const propertyId = searchParams.get('propertyId');
  const checkIn = searchParams.get('checkIn');
  const checkOut = searchParams.get('checkOut');
  const guests = parseInt(searchParams.get('guests') || '1');

  const checkInDate = checkIn ? new Date(checkIn) : null;
  const checkOutDate = checkOut ? new Date(checkOut) : null;

  const formatDate = (value: string | null) => {
    if (!value) return '—';
    return new Date(value).toLocaleDateString('en-US', {
      weekday: 'short',
      month: 'short',
      day: 'numeric',
      year: 'numeric',
    });
  };

  const [agreedToTerms, setAgreedToTerms] = useState(false);
  const [isCreatingBooking, setIsCreatingBooking] = useState(false);

  const { data: feeConfig, isLoading: loadingFees } = useFeeConfiguration();

  const nights = checkInDate && checkOutDate
    ? Math.ceil((checkOutDate.getTime() - checkInDate.getTime()) / (1000 * 60 * 60 * 24))
    : 0;

  const { data: property, isLoading: loadingProperty } = useQuery({
    queryKey: ['property', propertyId],
    queryFn: async () => {
      if (!propertyId) return null;
      const response = await apiClient.getPropertyDetails(propertyId);
      return response.data;
    },
    enabled: !!propertyId,
  });

  let costs = { basePrice: 0, serviceFee: 0, commissionFee: 0, commissionRate: 0, cleaningFee: 0, total: 0 };
  if (property && feeConfig && nights > 0) {
    costs = calculateBookingCost(property.price_per_night, nights, feeConfig, property.cleaning_fee);
  }

  const [contactingHost, setContactingHost] = useState(false);
  const contactHost = async () => {
    const hostId = property?.host?.id || property?.host_id;
    if (!property || !hostId || !user?.id) {
      toast.error('Host or user information missing');
      return;
    }
    setContactingHost(true);
    try {
      await apiClient.createConversation({
        participants: [Number(hostId), Number(user.id)],
        property: Number(property.id),
        subject: `Inquiry about ${property.title}`,
      });
      toast.success('Conversation started!');
      router.push(`/messages`);
    } catch (error: any) {
      const errMsg = typeof error.response?.data?.error === 'string'
        ? error.response.data.error
        : typeof error.response?.data?.detail === 'string'
          ? error.response.data.detail
          : 'Failed to start conversation';
      toast.error(errMsg);
    } finally {
      setContactingHost(false);
    }
  };

  const handleConfirmBooking = async () => {
    if (!agreedToTerms) {
      toast.error('Please agree to the terms and conditions');
      return;
    }
    setIsCreatingBooking(true);
    try {
      const response = await apiClient.createBooking({
        rental_property: Number(propertyId),
        check_in: checkIn,
        check_out: checkOut,
        number_of_guests: guests,
        cleaning_fee: costs.cleaningFee || 0,
      });
      const booking = response.data;
      toast.success('Booking created! Redirecting to payment...');
      router.push(`/booking/payment?bookingId=${booking.id}`);
    } catch (error: any) {
      const rawMsg = error.response?.data?.detail
        || error.response?.data?.non_field_errors?.[0]
        || error.response?.data?.error;
      const msg = typeof rawMsg === 'string' ? rawMsg : 'Failed to create booking. Please try again.';
      toast.error(msg);
    } finally {
      setIsCreatingBooking(false);
    }
  };

  if (loadingProperty || loadingFees) {
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

  const mainImage = property?.images?.[0]?.image || property?.images?.[0]?.image_url || property?.main_image_url;

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-5xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Back button */}
          <Link
            href={`/property/${propertyId}`}
            className="inline-flex items-center gap-2 text-primary-700 dark:text-sand-300 hover:text-secondary-600 dark:hover:text-secondary-400 mb-6 transition"
          >
            <ArrowLeft className="w-4 h-4" />
            Back to property
          </Link>

          {/* Progress stepper */}
          <div className="flex items-center gap-3 mb-8">
            {['Details', 'Payment', 'Confirmation'].map((step, i) => (
              <div key={step} className="flex items-center gap-3">
                <div className="flex items-center gap-2">
                  <div className={`w-8 h-8 rounded-full flex items-center justify-center text-sm font-semibold ${
                    i === 0
                      ? 'bg-secondary-600 text-white'
                      : 'bg-primary-200 dark:bg-primary-700 text-primary-500 dark:text-sand-400'
                  }`}>
                    {i + 1}
                  </div>
                  <span className={`text-sm font-medium ${
                    i === 0
                      ? 'text-primary-900 dark:text-sand-50'
                      : 'text-primary-400 dark:text-sand-500'
                  }`}>
                    {step}
                  </span>
                </div>
                {i < 2 && (
                  <div className="w-8 sm:w-16 h-px bg-primary-200 dark:bg-primary-700" />
                )}
              </div>
            ))}
          </div>

          <h1 className="text-2xl sm:text-3xl font-bold text-primary-900 dark:text-sand-50 mb-8">
            Confirm and pay
          </h1>

          <div className="grid lg:grid-cols-5 gap-8">
            {/* Left column */}
            <div className="lg:col-span-3 space-y-6">

              {/* Property hero card */}
              <section className="card overflow-hidden">
                {mainImage && (
                  <div className="relative h-48 sm:h-56">
                    <img
                      src={mainImage}
                      alt={property?.title}
                      className="w-full h-full object-cover"
                    />
                    <div className="absolute inset-0 bg-gradient-to-t from-black/50 to-transparent" />
                    <div className="absolute bottom-4 left-5 right-5">
                      <h3 className="text-white text-lg font-semibold drop-shadow-sm">
                        {property?.title}
                      </h3>
                      <div className="flex items-center gap-3 mt-1">
                        <span className="flex items-center gap-1 text-white/90 text-sm">
                          <MapPin className="w-3.5 h-3.5" />
                          {property?.city}, {property?.country}
                        </span>
                        {property?.average_rating > 0 && (
                          <span className="flex items-center gap-1 text-white/90 text-sm">
                            <Star className="w-3.5 h-3.5 fill-secondary-400 text-secondary-400" />
                            {property.average_rating.toFixed(1)}
                          </span>
                        )}
                      </div>
                    </div>
                  </div>
                )}
                <div className="p-5">
                  <div className="flex flex-wrap items-center gap-4 text-sm text-primary-600 dark:text-sand-300">
                    {property?.property_type && (
                      <span className="flex items-center gap-1.5">
                        <Home className="w-4 h-4 text-secondary-600 dark:text-secondary-400" />
                        {property.property_type}
                      </span>
                    )}
                    {property?.bedrooms > 0 && (
                      <span className="flex items-center gap-1.5">
                        <Bed className="w-4 h-4 text-secondary-600 dark:text-secondary-400" />
                        {property.bedrooms} bedroom{property.bedrooms !== 1 ? 's' : ''}
                      </span>
                    )}
                    {property?.bathrooms > 0 && (
                      <span className="flex items-center gap-1.5">
                        <Bath className="w-4 h-4 text-secondary-600 dark:text-secondary-400" />
                        {property.bathrooms} bathroom{property.bathrooms !== 1 ? 's' : ''}
                      </span>
                    )}
                    {property?.max_guests > 0 && (
                      <span className="flex items-center gap-1.5">
                        <Users className="w-4 h-4 text-secondary-600 dark:text-secondary-400" />
                        Up to {property.max_guests} guest{property.max_guests !== 1 ? 's' : ''}
                      </span>
                    )}
                  </div>
                  {/* Host info row */}
                  {property?.host && (
                    <div className="flex items-center justify-between mt-4 pt-4 border-t border-primary-200 dark:border-primary-700">
                      <div className="flex items-center gap-3">
                        {property.host.profile_picture ? (
                          <img
                            src={property.host.profile_picture}
                            alt={property.host.first_name}
                            className="w-10 h-10 rounded-full object-cover"
                          />
                        ) : (
                          <div className="w-10 h-10 rounded-full bg-primary-700 dark:bg-primary-600 flex items-center justify-center">
                            <span className="text-white text-sm font-semibold">
                              {property.host.first_name?.[0] || 'H'}
                            </span>
                          </div>
                        )}
                        <div>
                          <p className="text-sm font-medium text-primary-900 dark:text-sand-50">
                            Hosted by {property.host.first_name}
                          </p>
                          {property.host.is_verified && (
                            <p className="text-xs text-primary-500 dark:text-sand-400 flex items-center gap-1">
                              <CheckCircle2 className="w-3 h-3 text-primary-500" />
                              Verified host
                            </p>
                          )}
                        </div>
                      </div>
                      <button
                        onClick={contactHost}
                        disabled={contactingHost}
                        className="flex items-center gap-2 px-4 py-2 text-sm font-medium rounded-lg border border-primary-300 dark:border-primary-600 text-primary-700 dark:text-sand-200 hover:bg-sand-50 dark:hover:bg-primary-800 transition disabled:opacity-50"
                      >
                        {contactingHost ? (
                          <Loader2 className="w-4 h-4 animate-spin" />
                        ) : (
                          <MessageCircle className="w-4 h-4" />
                        )}
                        Message host
                      </button>
                    </div>
                  )}
                </div>
              </section>

              {/* Trip details */}
              <section className="card p-6">
                <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-5">
                  Your trip
                </h2>
                <div className="grid sm:grid-cols-2 gap-5">
                  <div className="flex items-start gap-3 p-4 rounded-xl bg-sand-50 dark:bg-primary-800">
                    <Calendar className="w-5 h-5 text-secondary-600 dark:text-secondary-400 mt-0.5 flex-shrink-0" />
                    <div>
                      <div className="text-xs uppercase tracking-wider font-semibold text-primary-500 dark:text-sand-400 mb-1">Check-in</div>
                      <div className="font-medium text-primary-900 dark:text-sand-50 text-sm">
                        {formatDate(checkIn)}
                      </div>
                      <div className="text-xs text-primary-500 dark:text-sand-400 mt-0.5">From 2:00 PM</div>
                    </div>
                  </div>
                  <div className="flex items-start gap-3 p-4 rounded-xl bg-sand-50 dark:bg-primary-800">
                    <Calendar className="w-5 h-5 text-secondary-600 dark:text-secondary-400 mt-0.5 flex-shrink-0" />
                    <div>
                      <div className="text-xs uppercase tracking-wider font-semibold text-primary-500 dark:text-sand-400 mb-1">Check-out</div>
                      <div className="font-medium text-primary-900 dark:text-sand-50 text-sm">
                        {formatDate(checkOut)}
                      </div>
                      <div className="text-xs text-primary-500 dark:text-sand-400 mt-0.5">Before 11:00 AM</div>
                    </div>
                  </div>
                </div>
                <div className="flex items-center gap-6 mt-4 pt-4 border-t border-primary-100 dark:border-primary-700">
                  <div className="flex items-center gap-2 text-sm text-primary-700 dark:text-sand-200">
                    <Clock className="w-4 h-4 text-secondary-600 dark:text-secondary-400" />
                    <span className="font-medium">{nights} night{nights !== 1 ? 's' : ''}</span>
                  </div>
                  <div className="flex items-center gap-2 text-sm text-primary-700 dark:text-sand-200">
                    <Users className="w-4 h-4 text-secondary-600 dark:text-secondary-400" />
                    <span className="font-medium">{guests} guest{guests !== 1 ? 's' : ''}</span>
                  </div>
                </div>
              </section>

              {/* Guest info */}
              <section className="card p-6">
                <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  Guest information
                </h2>
                <div className="space-y-3 text-sm">
                  <div className="flex justify-between py-2 border-b border-primary-100 dark:border-primary-800">
                    <span className="text-primary-500 dark:text-sand-400">Name</span>
                    <span className="font-medium text-primary-900 dark:text-sand-50">
                      {user?.first_name} {user?.last_name}
                    </span>
                  </div>
                  <div className="flex justify-between py-2 border-b border-primary-100 dark:border-primary-800">
                    <span className="text-primary-500 dark:text-sand-400">Email</span>
                    <span className="font-medium text-primary-900 dark:text-sand-50">
                      {user?.email}
                    </span>
                  </div>
                  {user?.phone_number && (
                    <div className="flex justify-between py-2">
                      <span className="text-primary-500 dark:text-sand-400">Phone</span>
                      <span className="font-medium text-primary-900 dark:text-sand-50">
                        {user.phone_number}
                      </span>
                    </div>
                  )}
                </div>
              </section>

              {/* Terms */}
              <section className="card p-6">
                <label className="flex items-start gap-3 cursor-pointer group">
                  <input
                    type="checkbox"
                    checked={agreedToTerms}
                    onChange={(e) => setAgreedToTerms(e.target.checked)}
                    className="mt-0.5 w-5 h-5 rounded border-primary-300 dark:border-primary-600 text-secondary-600 focus:ring-secondary-500"
                  />
                  <span className="text-sm text-primary-700 dark:text-sand-200 group-hover:text-primary-900 dark:group-hover:text-sand-50 transition leading-relaxed">
                    I agree to the{' '}
                    <Link href="/terms" className="text-secondary-600 dark:text-secondary-400 hover:underline font-medium">
                      Terms and Conditions
                    </Link>{' '}
                    and{' '}
                    <Link href="/cancellation-policy" className="text-secondary-600 dark:text-secondary-400 hover:underline font-medium">
                      Cancellation Policy
                    </Link>
                  </span>
                </label>
              </section>
            </div>

            {/* Right column — Price sidebar */}
            <div className="lg:col-span-2">
              <div className="card p-6 sticky top-24">
                {/* Mini property recap */}
                <div className="flex items-center gap-3 pb-5 mb-5 border-b border-primary-200 dark:border-primary-700">
                  {mainImage && (
                    <img src={mainImage} alt="" className="w-16 h-16 rounded-lg object-cover flex-shrink-0" />
                  )}
                  <div className="min-w-0">
                    <p className="text-sm font-semibold text-primary-900 dark:text-sand-50 truncate">
                      {property?.title}
                    </p>
                    <p className="text-xs text-primary-500 dark:text-sand-400">
                      {property?.city}, {property?.country}
                    </p>
                    {property?.average_rating > 0 && (
                      <p className="text-xs text-primary-600 dark:text-sand-300 flex items-center gap-1 mt-0.5">
                        <Star className="w-3 h-3 fill-secondary-500 text-secondary-500" />
                        {property.average_rating.toFixed(1)}
                        {property.review_count > 0 && (
                          <span className="text-primary-400 dark:text-sand-500">
                            ({property.review_count} review{property.review_count !== 1 ? 's' : ''})
                          </span>
                        )}
                      </p>
                    )}
                  </div>
                </div>

                <h2 className="text-lg font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  Price details
                </h2>

                <div className="space-y-3 text-sm">
                  <div className="flex justify-between">
                    <span className="text-primary-600 dark:text-sand-300">
                      {property?.currency} {property?.price_per_night} × {nights} night{nights !== 1 ? 's' : ''}
                    </span>
                    <span className="font-medium text-primary-900 dark:text-sand-50">
                      {property?.currency} {costs.basePrice.toFixed(2)}
                    </span>
                  </div>
                  {costs.serviceFee > 0 && (
                  <div className="flex justify-between">
                    <span className="text-primary-600 dark:text-sand-300">Service fee</span>
                    <span className="font-medium text-primary-900 dark:text-sand-50">
                      {property?.currency} {costs.serviceFee.toFixed(2)}
                    </span>
                  </div>
                  )}
                  {costs.commissionFee > 0 && (
                    <div className="flex justify-between">
                      <span className="text-primary-600 dark:text-sand-300">
                        Commission ({(costs.commissionRate * 100).toFixed(1)}%)
                      </span>
                      <span className="font-medium text-primary-900 dark:text-sand-50">
                        {property?.currency} {costs.commissionFee.toFixed(2)}
                      </span>
                    </div>
                  )}
                  {costs.cleaningFee > 0 && (
                    <div className="flex justify-between">
                      <span className="text-primary-600 dark:text-sand-300">Cleaning fee</span>
                      <span className="font-medium text-primary-900 dark:text-sand-50">
                        {property?.currency} {costs.cleaningFee.toFixed(2)}
                      </span>
                    </div>
                  )}
                </div>

                <div className="flex justify-between text-lg font-bold mt-5 pt-5 border-t border-primary-200 dark:border-primary-700">
                  <span className="text-primary-900 dark:text-sand-50">Total</span>
                  <span className="text-secondary-700 dark:text-secondary-400">
                    {property?.currency} {costs.total.toFixed(2)}
                  </span>
                </div>

                <button
                  onClick={handleConfirmBooking}
                  disabled={!agreedToTerms || isCreatingBooking}
                  className="w-full mt-6 bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white font-semibold py-3.5 px-4 rounded-lg transition disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center gap-2 text-base"
                >
                  {isCreatingBooking ? (
                    <><Loader2 className="w-5 h-5 animate-spin" /> Creating booking...</>
                  ) : (
                    'Confirm booking'
                  )}
                </button>

                <p className="text-xs text-center text-primary-500 dark:text-sand-400 mt-3">
                  You won&apos;t be charged yet. Payment is on the next step.
                </p>

                {/* Trust signals */}
                <div className="mt-5 pt-5 border-t border-primary-200 dark:border-primary-700 space-y-2.5">
                  <div className="flex items-center gap-2 text-xs text-primary-600 dark:text-sand-300">
                    <Shield className="w-4 h-4 text-primary-500 dark:text-sand-400 flex-shrink-0" />
                    <span>Secure checkout — your data is encrypted</span>
                  </div>
                  <div className="flex items-center gap-2 text-xs text-primary-600 dark:text-sand-300">
                    <CheckCircle2 className="w-4 h-4 text-primary-500 dark:text-sand-400 flex-shrink-0" />
                    <span>Free cancellation within policy window</span>
                  </div>
                  <div className="flex items-center gap-2 text-xs text-primary-600 dark:text-sand-300">
                    <Star className="w-4 h-4 text-primary-500 dark:text-sand-400 flex-shrink-0" />
                    <span>24/7 support from StayAfrica</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Mobile sticky CTA */}
        <div className="lg:hidden fixed bottom-0 left-0 right-0 p-4 bg-white dark:bg-primary-900 border-t border-primary-200 dark:border-primary-700 shadow-[0_-4px_20px_rgba(0,0,0,0.1)] z-40">
          <div className="flex items-center justify-between mb-2">
            <span className="text-sm font-bold text-primary-900 dark:text-sand-50">
              {property?.currency} {costs.total.toFixed(2)}
            </span>
            <span className="text-xs text-primary-500 dark:text-sand-400">
              {nights} night{nights !== 1 ? 's' : ''}
            </span>
          </div>
          <button
            onClick={handleConfirmBooking}
            disabled={!agreedToTerms || isCreatingBooking}
            className="w-full bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white font-semibold py-3 rounded-lg transition disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center gap-2"
          >
            {isCreatingBooking ? (
              <><Loader2 className="w-5 h-5 animate-spin" /> Creating...</>
            ) : (
              'Confirm booking'
            )}
          </button>
        </div>
      </div>
    </ProtectedRoute>
  );
}
