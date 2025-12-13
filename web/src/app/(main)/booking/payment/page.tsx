'use client';

import { useState } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery, useMutation } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/context/auth-context';
import { ProtectedRoute } from '@/components/auth/protected-route';
import { CreditCard, Wallet, DollarSign, CheckCircle, ArrowLeft, AlertCircle, Loader2 } from 'lucide-react';
import { toast } from 'react-hot-toast';
import Link from 'next/link';

interface PaymentProvider {
  id: string;
  name: string;
  description: string;
  icon: any;
  available: boolean;
}

export default function BookingPaymentPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { user } = useAuth();
  
  const bookingId = searchParams.get('bookingId');
  const [selectedProvider, setSelectedProvider] = useState<string>('');

  // Fetch booking details
  const { data: booking, isLoading: loadingBooking } = useQuery({
    queryKey: ['booking', bookingId],
    queryFn: async () => {
      const response = await apiClient.getBookingById(bookingId!);
      return response.data;
    },
    enabled: !!bookingId,
  });

  // Fetch available payment providers
  const { data: providersData, isLoading: loadingProviders } = useQuery({
    queryKey: ['providers', user?.country_of_residence],
    queryFn: async () => {
      // Mock providers based on country - in reality this would come from backend
      const country = user?.country_of_residence || 'International';
      const allProviders: PaymentProvider[] = [
        {
          id: 'paynow',
          name: 'Paynow',
          description: 'Pay with Ecocash, Visa, or Mastercard',
          icon: Wallet,
          available: country === 'Zimbabwe',
        },
        {
          id: 'payfast',
          name: 'PayFast',
          description: 'Secure payment gateway for South Africa',
          icon: CreditCard,
          available: country === 'South Africa',
        },
        {
          id: 'ozow',
          name: 'Ozow',
          description: 'Instant EFT payments',
          icon: DollarSign,
          available: country === 'South Africa',
        },
        {
          id: 'stripe',
          name: 'Stripe',
          description: 'Pay with credit or debit card',
          icon: CreditCard,
          available: !['Zimbabwe', 'South Africa'].includes(country),
        },
        {
          id: 'cash_on_arrival',
          name: 'Cash on Arrival',
          description: 'Pay in cash when you arrive',
          icon: DollarSign,
          available: ['Zimbabwe', 'South Africa'].includes(country),
        },
      ];
      return allProviders.filter(p => p.available);
    },
    enabled: !!user,
  });

  // Initiate payment mutation
  const initiatePaymentMutation = useMutation({
    mutationFn: async (provider: string) => {
      const response = await apiClient.initiatePayment(bookingId!, provider);
      return response.data;
    },
    onSuccess: () => {
      // For now, redirect to success page
      // In production, this would redirect to payment gateway
      if (selectedProvider === 'cash_on_arrival') {
        toast.success('Booking confirmed! You can pay cash on arrival.');
        router.push(`/booking/success?bookingId=${bookingId}&provider=${selectedProvider}`);
      } else {
        toast.success('Payment initiated. Redirecting to payment gateway...');
        // Simulate redirect to payment gateway
        setTimeout(() => {
          router.push(`/booking/success?bookingId=${bookingId}&provider=${selectedProvider}`);
        }, 2000);
      }
    },
    onError: (error: any) => {
      toast.error(error.response?.data?.error || 'Failed to initiate payment');
    },
  });

  const handlePayment = () => {
    if (!selectedProvider) {
      toast.error('Please select a payment method');
      return;
    }
    initiatePaymentMutation.mutate(selectedProvider);
  };

  if (!bookingId) {
    return (
      <ProtectedRoute>
        <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
          <div className="card p-8 text-center max-w-md">
            <AlertCircle className="w-16 h-16 text-amber-500 mx-auto mb-4" />
            <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-4">
              No Booking Found
            </h2>
            <p className="text-primary-600 dark:text-sand-300 mb-6">
              Please create a booking first.
            </p>
            <Link href="/explore" className="btn-primary px-6 py-3">
              Browse Properties
            </Link>
          </div>
        </div>
      </ProtectedRoute>
    );
  }

  if (loadingBooking || loadingProviders) {
    return (
      <ProtectedRoute>
        <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
          <div className="text-center">
            <Loader2 className="w-12 h-12 text-secondary-600 dark:text-secondary-400 animate-spin mx-auto mb-4" />
            <p className="text-primary-700 dark:text-sand-200">Loading payment options...</p>
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
            href={`/booking/confirm?propertyId=${booking.rental_property.id}&checkIn=${booking.check_in}&checkOut=${booking.check_out}&guests=${booking.number_of_guests}`}
            className="inline-flex items-center gap-2 text-primary-700 dark:text-sand-300 hover:text-secondary-600 dark:hover:text-secondary-400 mb-6 transition"
          >
            <ArrowLeft className="w-4 h-4" />
            Back to booking details
          </Link>

          <h1 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-8">
            Choose Payment Method
          </h1>

          <div className="grid lg:grid-cols-3 gap-8">
            {/* Left column - Payment methods */}
            <div className="lg:col-span-2 space-y-4">
              {providersData && providersData.length > 0 ? (
                providersData.map((provider) => {
                  const IconComponent = provider.icon;
                  return (
                    <button
                      key={provider.id}
                      onClick={() => setSelectedProvider(provider.id)}
                      className={`w-full card p-6 text-left transition hover:shadow-lg ${
                        selectedProvider === provider.id
                          ? 'ring-2 ring-secondary-500 dark:ring-secondary-400 bg-secondary-50 dark:bg-secondary-900/20'
                          : ''
                      }`}
                    >
                      <div className="flex items-start gap-4">
                        <div className={`p-3 rounded-lg ${
                          selectedProvider === provider.id
                            ? 'bg-secondary-100 dark:bg-secondary-800'
                            : 'bg-primary-100 dark:bg-primary-700'
                        }`}>
                          <IconComponent className={`w-6 h-6 ${
                            selectedProvider === provider.id
                              ? 'text-secondary-600 dark:text-secondary-400'
                              : 'text-primary-600 dark:text-sand-400'
                          }`} />
                        </div>
                        <div className="flex-1">
                          <div className="flex items-center justify-between mb-2">
                            <h3 className="font-semibold text-lg text-primary-900 dark:text-sand-50">
                              {provider.name}
                            </h3>
                            {selectedProvider === provider.id && (
                              <CheckCircle className="w-5 h-5 text-secondary-600 dark:text-secondary-400" />
                            )}
                          </div>
                          <p className="text-sm text-primary-600 dark:text-sand-300">
                            {provider.description}
                          </p>
                        </div>
                      </div>
                    </button>
                  );
                })
              ) : (
                <div className="card p-8 text-center">
                  <AlertCircle className="w-12 h-12 text-amber-500 mx-auto mb-4" />
                  <p className="text-primary-700 dark:text-sand-200">
                    No payment methods available for your region.
                  </p>
                </div>
              )}

              {/* Security notice */}
              <div className="card p-6 bg-secondary-50 dark:bg-secondary-900/20 border-secondary-200 dark:border-secondary-800">
                <div className="flex items-start gap-3">
                  <CheckCircle className="w-5 h-5 text-secondary-600 dark:text-secondary-400 mt-0.5 flex-shrink-0" />
                  <div>
                    <h3 className="font-semibold text-primary-900 dark:text-sand-50 mb-2">
                      Secure Payment
                    </h3>
                    <p className="text-sm text-primary-700 dark:text-sand-200">
                      Your payment information is encrypted and secure. We never store your full card details.
                    </p>
                  </div>
                </div>
              </div>
            </div>

            {/* Right column - Booking summary */}
            <div className="lg:col-span-1">
              <div className="card p-6 sticky top-24">
                <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
                  Booking Summary
                </h2>
                
                <div className="space-y-3 mb-6 pb-6 border-b border-primary-200 dark:border-primary-700 text-sm">
                  <div className="flex justify-between">
                    <span className="text-primary-700 dark:text-sand-200">Booking Ref:</span>
                    <span className="font-medium text-primary-900 dark:text-sand-50">
                      {booking.booking_ref}
                    </span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-primary-700 dark:text-sand-200">Property:</span>
                    <span className="font-medium text-primary-900 dark:text-sand-50 text-right truncate ml-2">
                      {booking.rental_property.title}
                    </span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-primary-700 dark:text-sand-200">Check-in:</span>
                    <span className="font-medium text-primary-900 dark:text-sand-50">
                      {new Date(booking.check_in).toLocaleDateString()}
                    </span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-primary-700 dark:text-sand-200">Check-out:</span>
                    <span className="font-medium text-primary-900 dark:text-sand-50">
                      {new Date(booking.check_out).toLocaleDateString()}
                    </span>
                  </div>
                </div>

                <div className="flex justify-between text-lg font-semibold mb-6">
                  <span className="text-primary-900 dark:text-sand-50">Total Amount</span>
                  <span className="text-primary-900 dark:text-sand-50">
                    {booking.currency} {booking.grand_total}
                  </span>
                </div>

                <button
                  onClick={handlePayment}
                  disabled={!selectedProvider || initiatePaymentMutation.isPending}
                  className="w-full bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white font-medium py-3 px-4 rounded-lg transition disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center gap-2"
                >
                  {initiatePaymentMutation.isPending ? (
                    <>
                      <Loader2 className="w-5 h-5 animate-spin" />
                      Processing...
                    </>
                  ) : (
                    <>
                      <CreditCard className="w-5 h-5" />
                      Pay {booking.currency} {booking.grand_total}
                    </>
                  )}
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </ProtectedRoute>
  );
}
