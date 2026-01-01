'use client';

import { useState } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery, useMutation } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/store/auth-store';
import { useFeeConfiguration, calculateBookingCost } from '@/hooks/use-fees';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { MapPin, Calendar, Users, CreditCard, ArrowLeft } from 'lucide-react';
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

  const [agreedToTerms, setAgreedToTerms] = useState(false);

  // Fetch property details
  const { data: property, isLoading: loadingProperty } = useQuery({
    queryKey: ['property', propertyId],
    queryFn: async () => {
      const response = await apiClient.getPropertyById(propertyId!);
      return response.data;
    },
    enabled: !!propertyId,
  });

  // Fetch fee configuration
  const { data: feeConfig, isLoading: loadingFees } = useFeeConfiguration();

  // Calculate costs
  const nights = checkIn && checkOut 
    ? Math.ceil((new Date(checkOut).getTime() - new Date(checkIn).getTime()) / (1000 * 60 * 60 * 24)) 
    : 0;
  
  const costs = property && feeConfig && nights > 0
    ? calculateBookingCost(property.price_per_night, nights, feeConfig, property.cleaning_fee)
    : { basePrice: 0, serviceFee: 0, commissionFee: 0, commissionRate: 0, cleaningFee: 0, total: 0 };

  // Create booking mutation
  const createBookingMutation = useMutation({
    mutationFn: async () => {
      const response = await apiClient.createBooking({
        rental_property: propertyId,
        check_in: checkIn,
        check_out: checkOut,
        number_of_guests: guests,
      });
      return response.data;
    },
    onSuccess: (data) => {
      toast.success('Booking created successfully!');
      // Redirect to payment page
      router.push(`/booking/payment?bookingId=${data.id}`);
    },
    onError: (error: any) => {
      toast.error(error.response?.data?.error || 'Failed to create booking');
    },
  });

  const handleConfirmBooking = () => {
    if (!agreedToTerms) {
      toast.error('Please agree to the terms and conditions');
      return;
    }
    createBookingMutation.mutate();
  };

  if (!propertyId || !checkIn || !checkOut) {
    return (
      <ProtectedRoute>
        <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
          <div className="card p-8 text-center max-w-md">
            <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-4">
              Invalid Booking Details
            </h2>
            <p className="text-primary-600 dark:text-sand-300 mb-6">
              Please select dates and property to continue.
            </p>
            <Link href="/explore" className="btn-primary px-6 py-3">
              Browse Properties
            </Link>
          </div>
        </div>
      </ProtectedRoute>
    );
  }

  if (loadingProperty || loadingFees) {
    return (
      <ProtectedRoute>
        <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
          <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
            <div className="animate-pulse space-y-6">
              <div className="h-8 bg-primary-200 dark:bg-primary-700 rounded w-1/3"></div>
              <div className="h-64 bg-primary-200 dark:bg-primary-700 rounded"></div>
              <div className="h-48 bg-primary-200 dark:bg-primary-700 rounded"></div>
            </div>
          </div>
        </div>
      </ProtectedRoute>
    );
  }

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Back button */}
          <Link 
            href={`/properties/${propertyId}`}
            className="inline-flex items-center gap-2 text-primary-700 dark:text-sand-300 hover:text-secondary-600 dark:hover:text-secondary-400 mb-6 transition"
          >
            <ArrowLeft className="w-4 h-4" />
            Back to property
          </Link>

          <h1 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-8">
            Confirm Your Booking
          </h1>

          <div className="grid lg:grid-cols-3 gap-8">
            {/* Left column - Booking details */}
            <div className="lg:col-span-2 space-y-6">
              {/* Property card */}
              <section className="card p-6">
                <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  Property Details
                </h2>
                <div className="flex gap-4">
                  {property.images?.[0] && (
                    <img
                      src={property.images[0].image}
                      alt={property.title}
                      className="w-24 h-24 object-cover rounded-lg"
                    />
                  )}
                  <div className="flex-1">
                    <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-2">
                      {property.title}
                    </h3>
                    <div className="flex items-center gap-2 text-primary-600 dark:text-sand-300 text-sm mb-1">
                      <MapPin className="w-4 h-4" />
                      <span>{property.city}, {property.country}</span>
                    </div>
                    <div className="text-sm text-primary-600 dark:text-sand-300">
                      {property.property_type}
                    </div>
                  </div>
                </div>
              </section>

              {/* Trip details */}
              <section className="card p-6">
                <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  Your Trip
                </h2>
                <div className="space-y-4">
                  <div className="flex items-start gap-3">
                    <Calendar className="w-5 h-5 text-secondary-600 dark:text-secondary-400 mt-0.5" />
                    <div>
                      <div className="font-medium text-primary-900 dark:text-sand-50">Dates</div>
                      <div className="text-sm text-primary-600 dark:text-sand-300">
                        {new Date(checkIn).toLocaleDateString('en-US', { 
                          month: 'short', 
                          day: 'numeric', 
                          year: 'numeric' 
                        })} - {new Date(checkOut).toLocaleDateString('en-US', { 
                          month: 'short', 
                          day: 'numeric', 
                          year: 'numeric' 
                        })}
                      </div>
                      <div className="text-sm text-primary-600 dark:text-sand-300">
                        {nights} {nights === 1 ? 'night' : 'nights'}
                      </div>
                    </div>
                  </div>
                  <div className="flex items-start gap-3">
                    <Users className="w-5 h-5 text-secondary-600 dark:text-secondary-400 mt-0.5" />
                    <div>
                      <div className="font-medium text-primary-900 dark:text-sand-50">Guests</div>
                      <div className="text-sm text-primary-600 dark:text-sand-300">
                        {guests} {guests === 1 ? 'guest' : 'guests'}
                      </div>
                    </div>
                  </div>
                </div>
              </section>

              {/* Guest details */}
              <section className="card p-6">
                <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  Guest Information
                </h2>
                <div className="space-y-2 text-sm">
                  <div className="flex justify-between">
                    <span className="text-primary-600 dark:text-sand-300">Name:</span>
                    <span className="font-medium text-primary-900 dark:text-sand-50">
                      {user?.first_name} {user?.last_name}
                    </span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-primary-600 dark:text-sand-300">Email:</span>
                    <span className="font-medium text-primary-900 dark:text-sand-50">
                      {user?.email}
                    </span>
                  </div>
                  {user?.phone_number && (
                    <div className="flex justify-between">
                      <span className="text-primary-600 dark:text-sand-300">Phone:</span>
                      <span className="font-medium text-primary-900 dark:text-sand-50">
                        {user.phone_number}
                      </span>
                    </div>
                  )}
                </div>
              </section>

              {/* Terms and conditions */}
              <section className="card p-6">
                <label className="flex items-start gap-3 cursor-pointer group">
                  <input
                    type="checkbox"
                    checked={agreedToTerms}
                    onChange={(e) => setAgreedToTerms(e.target.checked)}
                    className="mt-1 w-5 h-5 rounded border-primary-300 dark:border-primary-600 text-secondary-600 focus:ring-secondary-500"
                  />
                  <span className="text-sm text-primary-700 dark:text-sand-200 group-hover:text-primary-900 dark:group-hover:text-sand-50 transition">
                    I agree to the{' '}
                    <Link href="/terms" className="text-secondary-600 dark:text-secondary-400 hover:underline">
                      Terms and Conditions
                    </Link>{' '}
                    and{' '}
                    <Link href="/cancellation-policy" className="text-secondary-600 dark:text-secondary-400 hover:underline">
                      Cancellation Policy
                    </Link>
                  </span>
                </label>
              </section>
            </div>

            {/* Right column - Price breakdown */}
            <div className="lg:col-span-1">
              <div className="card p-6 sticky top-24">
                <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  Price Details
                </h2>
                
                <div className="space-y-3 mb-6 pb-6 border-b border-primary-200 dark:border-primary-700">
                  <div className="flex justify-between text-sm">
                    <span className="text-primary-700 dark:text-sand-200">
                      {property.currency} {property.price_per_night} × {nights} {nights === 1 ? 'night' : 'nights'}
                    </span>
                    <span className="font-medium text-primary-900 dark:text-sand-50">
                      {property.currency} {costs.basePrice.toFixed(2)}
                    </span>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span className="text-primary-700 dark:text-sand-200">Service fee</span>
                    <span className="font-medium text-primary-900 dark:text-sand-50">
                      {property.currency} {costs.serviceFee.toFixed(2)}
                    </span>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span className="text-primary-700 dark:text-sand-200">Commission fee ({(costs.commissionRate * 100).toFixed(1)}%)</span>
                    <span className="font-medium text-primary-900 dark:text-sand-50">
                      {property.currency} {costs.commissionFee.toFixed(2)}
                    </span>
                  </div>
                  {costs.cleaningFee > 0 && (
                    <div className="flex justify-between text-sm">
                      <span className="text-primary-700 dark:text-sand-200">Cleaning fee</span>
                      <span className="font-medium text-primary-900 dark:text-sand-50">
                        {property.currency} {costs.cleaningFee.toFixed(2)}
                      </span>
                    </div>
                  )}
                </div>

                <div className="flex justify-between text-lg font-semibold mb-6">
                  <span className="text-primary-900 dark:text-sand-50">Total</span>
                  <span className="text-primary-900 dark:text-sand-50">
                    {property.currency} {costs.total.toFixed(2)}
                  </span>
                </div>

                <button
                  onClick={handleConfirmBooking}
                  disabled={!agreedToTerms || createBookingMutation.isPending}
                  className="w-full bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white font-medium py-3 px-4 rounded-lg transition disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center gap-2"
                >
                  {createBookingMutation.isPending ? (
                    <>
                      <div className="w-5 h-5 border-2 border-white border-t-transparent rounded-full animate-spin"></div>
                      Processing...
                    </>
                  ) : (
                    <>
                      <CreditCard className="w-5 h-5" />
                      Confirm and Pay
                    </>
                  )}
                </button>

                <p className="text-xs text-center text-primary-600 dark:text-sand-400 mt-4">
                  You won't be charged yet. Payment will be processed on the next page.
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </ProtectedRoute>
  );
}
