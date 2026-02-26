'use client';

import { useState } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery, useMutation } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/store/auth-store';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { CreditCard, Wallet, DollarSign, CheckCircle, ArrowLeft, AlertCircle, Loader2, Globe } from 'lucide-react';
import { toast } from 'react-hot-toast';
import Link from 'next/link';

interface PaymentProvider {
  id: string;
  name: string;
  description: string;
  icon: any;
  category: 'regional' | 'international';
  available: boolean;
}

export default function BookingPaymentPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { user } = useAuth();
  
  const bookingId = searchParams.get('bookingId');
  const providerFromConfirm = searchParams.get('provider');
  const [selectedProvider, setSelectedProvider] = useState<string>(providerFromConfirm || '');

  // Fetch booking details
  const { data: booking, isLoading: loadingBooking, isError: bookingError } = useQuery({
    queryKey: ['booking', bookingId],
    queryFn: async () => {
      const response = await apiClient.getBookingById(bookingId!);
      return response.data;
    },
    enabled: !!bookingId,
  });

  // Fetch available payment providers
  const { data: providersData, isLoading: loadingProviders, isError: providersError } = useQuery({
    queryKey: ['providers', user?.country_of_residence],
    queryFn: async () => {
      const response = await apiClient.getAvailableProviders(user?.country_of_residence || 'International');
      const providerList: { id: string; name: string; category?: string }[] = response.data?.providers || [];
      const iconMap: Record<string, any> = {
        paynow: Wallet,
        payfast: CreditCard,
        ozow: DollarSign,
        stripe: CreditCard,
        flutterwave: CreditCard,
        paystack: CreditCard,
        paypal: CreditCard,
        cash_on_arrival: DollarSign,
      };
      const descriptionMap: Record<string, string> = {
        paynow: 'Pay with Ecocash, Visa, or Mastercard',
        payfast: 'Secure payment gateway for South Africa',
        ozow: 'Instant EFT payments',
        stripe: 'Pay with credit or debit card',
        flutterwave: 'Pay with cards and mobile money',
        paystack: 'Pay with cards and bank transfer',
        paypal: 'Pay with your PayPal account',
        cash_on_arrival: 'Pay in cash when you arrive',
      };
      return providerList.map((p) => ({
        id: p.id,
        name: p.name,
        description: descriptionMap[p.id] || `Pay with ${p.name}`,
        icon: iconMap[p.id] || CreditCard,
        category: (p.category === 'international' ? 'international' : 'regional') as 'regional' | 'international',
        available: true,
      })) as PaymentProvider[];
    },
    enabled: !!user,
    retry: 2,
  });

  // Initiate payment mutation
  const initiatePaymentMutation = useMutation({
    mutationFn: async (provider: string) => {
      const response = await apiClient.initiatePayment(bookingId!, provider);
      return response.data;
    },
    onSuccess: (data) => {
      if (selectedProvider === 'cash_on_arrival') {
        toast.success('Booking confirmed! You can pay cash on arrival.');
        router.push(`/booking/success?bookingId=${bookingId}&provider=${selectedProvider}`);
      } else if (data?.checkout_url || data?.redirect_url || data?.payment_link) {
        // Store payment info for the return page before redirecting
        if (typeof window !== 'undefined') {
          localStorage.setItem('pending_payment', JSON.stringify({
            bookingId,
            provider: selectedProvider,
            paymentId: data.id,
            gateway_ref: data.gateway_ref,
          }));
        }
        toast.success('Redirecting to payment gateway...');
        const redirectUrl = data.checkout_url || data.redirect_url || data.payment_link;
        window.location.href = redirectUrl;
      } else if (data?.status === 'success' || data?.status === 'completed') {
        toast.success('Payment completed!');
        router.push(`/booking/success?bookingId=${bookingId}&provider=${selectedProvider}`);
      } else {
        // Payment initiated but pending webhook confirmation
        toast.success('Payment initiated. You will be notified once confirmed.');
        router.push(`/booking/success?bookingId=${bookingId}&provider=${selectedProvider}&pending=true`);
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
            <Link href="/explore">
              <button className="bg-secondary-600 hover:bg-secondary-700 text-white font-medium py-2 px-6 rounded-lg transition">
                Browse Properties
              </button>
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

  if (bookingError || !booking) {
    return (
      <ProtectedRoute>
        <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
          <div className="card p-8 text-center max-w-md">
            <AlertCircle className="w-16 h-16 text-amber-500 mx-auto mb-4" />
            <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-4">
              Booking Not Found
            </h2>
            <p className="text-primary-600 dark:text-sand-300 mb-6">
              We couldn&apos;t load the booking details. Please go back and try again.
            </p>
            <Link href="/explore">
              <button className="bg-secondary-600 hover:bg-secondary-700 text-white font-medium py-2 px-6 rounded-lg transition">
                Browse Properties
              </button>
            </Link>
          </div>
        </div>
      </ProtectedRoute>
    );
  }

  // Safely extract property info from booking response
  // The serializer returns: rental_property (FK integer), property (nested object), property_title (string)
  const propertyId = booking.property?.id || booking.rental_property;
  const propertyTitle = booking.property_title || booking.property?.title || 'Property';

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
        <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Back button */}
          <Link 
            href={`/booking/confirm?propertyId=${propertyId}&checkIn=${booking.check_in}&checkOut=${booking.check_out}&guests=${booking.number_of_guests || 1}`}
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
            <div className="lg:col-span-2 space-y-6">
              {providersData && providersData.length > 0 ? (
                <>
                  {/* Regional payment methods */}
                  {providersData.filter(p => p.category === 'regional').length > 0 && (
                    <div className="space-y-3">
                      <div className="flex items-center gap-2">
                        <Wallet className="w-4 h-4 text-primary-500 dark:text-sand-400" />
                        <h2 className="text-sm font-semibold uppercase tracking-wider text-primary-500 dark:text-sand-400">
                          Local Payment Methods
                        </h2>
                      </div>
                      {providersData.filter(p => p.category === 'regional').map((provider) => {
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
                      })}
                    </div>
                  )}

                  {/* International payment methods */}
                  {providersData.filter(p => p.category === 'international').length > 0 && (
                    <div className="space-y-3">
                      <div className="flex items-center gap-2">
                        <Globe className="w-4 h-4 text-primary-500 dark:text-sand-400" />
                        <h2 className="text-sm font-semibold uppercase tracking-wider text-primary-500 dark:text-sand-400">
                          International Payment Methods
                        </h2>
                      </div>
                      {providersData.filter(p => p.category === 'international').map((provider) => {
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
                      })}
                    </div>
                  )}
                </>
              ) : (
                <div className="card p-8 text-center">
                  <AlertCircle className="w-12 h-12 text-amber-500 mx-auto mb-4" />
                  <p className="text-primary-700 dark:text-sand-200">
                    {providersError
                      ? 'Failed to load payment methods. Please try again later.'
                      : 'No payment methods available for your region.'}
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
                      {propertyTitle}
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
