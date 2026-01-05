'use client';

import { useState } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/store/auth-store';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import { MapPin, Calendar, Users, CreditCard, ArrowLeft, Loader2, AlertCircle, CheckCircle } from 'lucide-react';
import { toast } from 'react-hot-toast';
import Link from 'next/link';

interface PaymentProvider {
  id: string;
  name: string;
  description: string;
  icon?: any;
}

export default function BookingConfirmPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { user } = useAuth();

  const propertyId = searchParams.get('propertyId');
  const checkIn = searchParams.get('checkIn');
  const checkOut = searchParams.get('checkOut');
  const guests = parseInt(searchParams.get('guests') || '1');

  const [agreedToTerms, setAgreedToTerms] = useState(false);
  const [selectedProvider, setSelectedProvider] = useState<string>('');

  // Fetch available payment providers based on user's country
  const userCountry = user ? user.country_of_residence : undefined;
  const { data: providersData, isLoading: loadingProviders } = useQuery({
    queryKey: ['providers', userCountry],
    queryFn: async () => {
      if (!userCountry) return [];
      const response = await apiClient.getAvailableProviders(userCountry);
      return response.data;
    },
    enabled: !!userCountry,
  });

  // Contact Host logic
  const [contactingHost, setContactingHost] = useState(false);
  const contactHost = async () => {
    if (!property?.host_id || !user?.id) {
      toast.error('Host or user information missing');
      return;
    }
    setContactingHost(true);
    try {
      // Create a new conversation with the host
      const response = await apiClient.createConversation({
        participants: [property.host_id, user.id],
        property: property.id,
        subject: `Inquiry about ${property.title}`,
      });
      const conversationId = response.data.id;
      toast.success('Conversation started!');
      // Redirect to messaging panel for this conversation
      router.push(`/messaging/conversations/${conversationId}`);
    } catch (error: any) {
      toast.error(error.response?.data?.error || 'Failed to start conversation');
    } finally {
      setContactingHost(false);
    }
  };


  if (loadingProperty || loadingFees || loadingProviders) {
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
                            {/* Payment method selection */}
                            <section className="card p-6">
                              <h2 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-4">
                                Payment Method
                              </h2>
                              {providersData && providersData.length > 0 ? (
                                <div className="space-y-3">
                                  {providersData.map((provider: PaymentProvider) => (
                                    <button
                                      key={provider.id}
                                      type="button"
                                      onClick={() => setSelectedProvider(provider.id)}
                                      className={`w-full flex items-center gap-4 p-4 rounded-lg border transition focus:outline-none ${
                                        selectedProvider === provider.id
                                          ? 'border-secondary-500 bg-secondary-50 dark:bg-secondary-900/20'
                                          : 'border-primary-200 dark:border-primary-700'
                                      }`}
                                    >
                                      <div className="flex-1 text-left">
                                        <div className="font-semibold text-primary-900 dark:text-sand-50">{provider.name}</div>
                                        <div className="text-sm text-primary-600 dark:text-sand-300">{provider.description}</div>
                                      </div>
                                      {selectedProvider === provider.id && (
                                        <CheckCircle className="w-5 h-5 text-secondary-600 dark:text-secondary-400" />
                                      )}
                                    </button>
                                  ))}
                                </div>
                              ) : (
                                <div className="flex items-center gap-2 text-amber-600 dark:text-amber-400">
                                  <AlertCircle className="w-5 h-5" />
                                  <span>No payment methods available for your region.</span>
                                </div>
                              )}
                            </section>
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

              {/* Contact Host Button */}
              <section className="card p-6">
                <button
                  onClick={contactHost}
                  disabled={contactingHost}
                  className="w-full bg-primary-700 hover:bg-primary-800 dark:bg-primary-800 dark:hover:bg-primary-700 text-white font-medium py-3 px-4 rounded-lg transition disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center gap-2"
                >
                  {contactingHost ? (
                    <>
                      <Loader2 className="w-5 h-5 animate-spin" />
                      Contacting Host...
                    </>
                  ) : (
                    <>
                      <Users className="w-5 h-5" />
                      Contact Host
                    </>
                  )}
                </button>
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
