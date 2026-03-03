'use client';

import { useState } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery, useMutation } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { useAuth } from '@/store/auth-store';
import dynamic from 'next/dynamic';
const ProtectedRoute = dynamic(() => import('@/components/auth/protected-route').then(m => m.ProtectedRoute), { ssr: false });
import {
  CreditCard, Wallet, CheckCircle, ArrowLeft, AlertCircle, Loader2, Globe,
  Shield, Lock, Smartphone, Banknote, MapPin,
  ExternalLink, Info,
} from 'lucide-react';
import { toast } from 'react-hot-toast';
import Link from 'next/link';

/* ── Payment method tags per provider ── */
type MethodIcon = 'card' | 'mobile' | 'eft' | 'wallet' | 'cash' | 'ussd' | 'qr';

const PROVIDER_META: Record<string, {
  description: string;
  methods: { label: string; icon: MethodIcon }[];
  poweredBy: string;
  cardOnly?: boolean;
}> = {
  stripe: {
    description: 'All major credit & debit cards, processed securely by Stripe',
    methods: [
      { label: 'Visa', icon: 'card' },
      { label: 'Mastercard', icon: 'card' },
      { label: 'Amex', icon: 'card' },
      { label: 'Discover', icon: 'card' },
      { label: 'Diners Club', icon: 'card' },
      { label: 'JCB', icon: 'card' },
      { label: 'Apple Pay', icon: 'wallet' },
      { label: 'Google Pay', icon: 'wallet' },
    ],
    poweredBy: 'Stripe',
    cardOnly: true,
  },
  paypal: {
    description: 'Pay with your PayPal account, linked cards or bank',
    methods: [
      { label: 'PayPal Balance', icon: 'wallet' },
      { label: 'Visa', icon: 'card' },
      { label: 'Mastercard', icon: 'card' },
      { label: 'Amex', icon: 'card' },
      { label: 'Linked Bank', icon: 'eft' },
      { label: 'Pay Later', icon: 'wallet' },
    ],
    poweredBy: 'PayPal',
    cardOnly: true,
  },
  paynow: {
    description: 'Zimbabwe\'s leading gateway — mobile money & wallets',
    methods: [
      { label: 'EcoCash', icon: 'mobile' },
      { label: 'OneMoney', icon: 'mobile' },
      { label: 'Telecash', icon: 'mobile' },
      { label: 'InnBucks', icon: 'mobile' },
      { label: 'Omari', icon: 'mobile' },
      { label: 'ZimSwitch', icon: 'eft' },
    ],
    poweredBy: 'Paynow',
  },
  flutterwave: {
    description: 'Africa-wide payments — mobile money, bank transfer & USSD',
    methods: [
      { label: 'M-Pesa', icon: 'mobile' },
      { label: 'MTN MoMo', icon: 'mobile' },
      { label: 'Airtel Money', icon: 'mobile' },
      { label: 'Vodafone Cash', icon: 'mobile' },
      { label: 'Tigo', icon: 'mobile' },
      { label: 'Bank Transfer', icon: 'eft' },
      { label: 'USSD', icon: 'ussd' },
    ],
    poweredBy: 'Flutterwave',
  },
  paystack: {
    description: 'Secure payments for Nigeria, Ghana & South Africa',
    methods: [
      { label: 'Bank Transfer', icon: 'eft' },
      { label: 'Mobile Money', icon: 'mobile' },
      { label: 'USSD', icon: 'ussd' },
      { label: 'QR Code', icon: 'qr' },
    ],
    poweredBy: 'Paystack',
  },
  ozow: {
    description: 'Instant EFT — pay directly from your South African bank',
    methods: [
      { label: 'ABSA', icon: 'eft' },
      { label: 'FNB', icon: 'eft' },
      { label: 'Nedbank', icon: 'eft' },
      { label: 'Standard Bank', icon: 'eft' },
      { label: 'Capitec', icon: 'eft' },
      { label: 'Investec', icon: 'eft' },
    ],
    poweredBy: 'Ozow',
  },
  mpesa: {
    description: 'Kenya\'s most popular mobile money service',
    methods: [
      { label: 'M-Pesa', icon: 'mobile' },
    ],
    poweredBy: 'Safaricom M-Pesa',
  },
  cash_on_arrival: {
    description: 'Pay cash directly at the property on check-in',
    methods: [
      { label: 'Cash', icon: 'cash' },
    ],
    poweredBy: '',
  },
};

function MethodIcon({ type }: { type: string }) {
  switch (type) {
    case 'card': return <CreditCard className="w-3.5 h-3.5" />;
    case 'mobile': return <Smartphone className="w-3.5 h-3.5" />;
    case 'eft': return <Banknote className="w-3.5 h-3.5" />;
    case 'wallet': return <Wallet className="w-3.5 h-3.5" />;
    case 'cash': return <Banknote className="w-3.5 h-3.5" />;
    case 'ussd': return <Smartphone className="w-3.5 h-3.5" />;
    case 'qr': return <Globe className="w-3.5 h-3.5" />;
    default: return <CreditCard className="w-3.5 h-3.5" />;
  }
}

function ProviderIcon({ id }: { id: string }) {
  const base = "w-7 h-7";
  switch (id) {
    case 'stripe': return <CreditCard className={base} />;
    case 'paypal': return <Wallet className={base} />;
    case 'flutterwave': return <Globe className={base} />;
    case 'paystack': return <Shield className={base} />;
    case 'paynow': return <Smartphone className={base} />;
    case 'ozow': return <Banknote className={base} />;
    case 'mpesa': return <Smartphone className={base} />;
    case 'cash_on_arrival': return <Banknote className={base} />;
    default: return <CreditCard className={base} />;
  }
}

export default function BookingPaymentPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { user } = useAuth();

  const bookingId = searchParams.get('bookingId');
  const [selectedProvider, setSelectedProvider] = useState<string>('');

  const { data: booking, isLoading: loadingBooking, isError: bookingError } = useQuery({
    queryKey: ['booking', bookingId],
    queryFn: async () => {
      const response = await apiClient.getBookingById(bookingId!);
      return response.data;
    },
    enabled: !!bookingId,
  });

  const { data: providersData, isLoading: loadingProviders, isError: providersError } = useQuery({
    queryKey: ['providers', user?.country_of_residence],
    queryFn: async () => {
      const response = await apiClient.getAvailableProviders(user?.country_of_residence || 'International');
      const providerList: { id: string; name: string; category?: string }[] = response.data?.providers || [];
      return providerList.map((p) => ({
        id: p.id,
        name: p.name,
        category: (p.category === 'international' ? 'international' : 'regional') as 'regional' | 'international',
      }));
    },
    enabled: !!user,
    retry: 2,
  });

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
        if (typeof window !== 'undefined') {
          localStorage.setItem('pending_payment', JSON.stringify({
            bookingId,
            provider: selectedProvider,
            paymentId: data.id,
            gateway_ref: data.gateway_ref,
            paypal_order_id: data.paypal_order_id || null,
          }));
        }
        toast.success('Redirecting to payment gateway...');
        const redirectUrl = data.checkout_url || data.redirect_url || data.payment_link;
        window.location.href = redirectUrl;
      } else if (data?.status === 'success' || data?.status === 'completed') {
        toast.success('Payment completed!');
        router.push(`/booking/success?bookingId=${bookingId}&provider=${selectedProvider}`);
      } else {
        toast.success('Payment initiated. You will be notified once confirmed.');
        router.push(`/booking/success?bookingId=${bookingId}&provider=${selectedProvider}&pending=true`);
      }
    },
    onError: (error: any) => {
      const errData = error.response?.data;
      const msg = typeof errData?.error === 'string'
        ? errData.error
        : typeof errData?.detail === 'string'
          ? errData.detail
          : 'Failed to initiate payment. Please try again.';
      toast.error(msg);
    },
  });

  const handlePayment = () => {
    if (!selectedProvider) {
      toast.error('Please select a payment method');
      return;
    }
    initiatePaymentMutation.mutate(selectedProvider);
  };

  /* ── Error / empty states ── */
  if (!bookingId) {
    return (
      <ProtectedRoute>
        <div className="min-h-screen bg-sand-100 dark:bg-primary-900 flex items-center justify-center">
          <div className="card p-8 text-center max-w-md">
            <AlertCircle className="w-16 h-16 text-amber-500 mx-auto mb-4" />
            <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-4">No Booking Found</h2>
            <p className="text-primary-600 dark:text-sand-300 mb-6">Please create a booking first.</p>
            <Link href="/explore">
              <button className="bg-secondary-600 hover:bg-secondary-700 text-white font-semibold py-2.5 px-6 rounded-lg transition">
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
            <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-50 mb-4">Booking Not Found</h2>
            <p className="text-primary-600 dark:text-sand-300 mb-6">
              We couldn&apos;t load the booking details. Please go back and try again.
            </p>
            <Link href="/explore">
              <button className="bg-secondary-600 hover:bg-secondary-700 text-white font-semibold py-2.5 px-6 rounded-lg transition">
                Browse Properties
              </button>
            </Link>
          </div>
        </div>
      </ProtectedRoute>
    );
  }

  const propertyId = booking.property?.id || booking.rental_property;
  const propertyTitle = booking.property_title || booking.property?.title || 'Property';
  const propertyImage = booking.property?.images?.[0]?.image || booking.property?.images?.[0]?.image_url || booking.property?.main_image_url;
  const propertyCity = booking.property?.city;
  const propertyCountry = booking.property?.country;

  const isCashOnArrival = selectedProvider === 'cash_on_arrival';
  const chargesOnly = booking
    ? (parseFloat(booking.service_fee || '0') + parseFloat(booking.commission_fee || '0') + parseFloat(booking.cleaning_fee || '0') + parseFloat(booking.taxes || '0')).toFixed(2)
    : '0.00';
  const displayAmount = isCashOnArrival ? chargesOnly : booking?.grand_total;

  const nights = booking.check_in && booking.check_out
    ? Math.ceil((new Date(booking.check_out).getTime() - new Date(booking.check_in).getTime()) / (1000 * 60 * 60 * 24))
    : 0;

  const formatDate = (d: string) => new Date(d).toLocaleDateString('en-US', { weekday: 'short', month: 'short', day: 'numeric' });

  const regionalProviders = providersData?.filter(p => p.category === 'regional') || [];
  const internationalProviders = providersData?.filter(p => p.category === 'international') || [];

  const selectedMeta = selectedProvider ? PROVIDER_META[selectedProvider] : null;

  return (
    <ProtectedRoute>
      <div className="min-h-screen bg-sand-100 dark:bg-primary-900 pb-28 lg:pb-8">
        <div className="max-w-5xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Back button */}
          <Link
            href={`/booking/confirm?propertyId=${propertyId}&checkIn=${booking.check_in}&checkOut=${booking.check_out}&guests=${booking.number_of_guests || 1}`}
            className="inline-flex items-center gap-2 text-primary-700 dark:text-sand-300 hover:text-secondary-600 dark:hover:text-secondary-400 mb-6 transition"
          >
            <ArrowLeft className="w-4 h-4" />
            Back to booking details
          </Link>

          {/* Progress stepper */}
          <div className="flex items-center gap-3 mb-8">
            {['Details', 'Payment', 'Confirmation'].map((step, i) => (
              <div key={step} className="flex items-center gap-3">
                <div className="flex items-center gap-2">
                  <div className={`w-8 h-8 rounded-full flex items-center justify-center text-sm font-semibold ${
                    i <= 1
                      ? 'bg-secondary-600 text-white'
                      : 'bg-primary-200 dark:bg-primary-700 text-primary-500 dark:text-sand-400'
                  }`}>
                    {i === 0 ? <CheckCircle className="w-4 h-4" /> : i + 1}
                  </div>
                  <span className={`text-sm font-medium ${
                    i <= 1
                      ? 'text-primary-900 dark:text-sand-50'
                      : 'text-primary-400 dark:text-sand-500'
                  }`}>
                    {step}
                  </span>
                </div>
                {i < 2 && (
                  <div className={`w-8 sm:w-16 h-px ${i === 0 ? 'bg-secondary-500' : 'bg-primary-200 dark:bg-primary-700'}`} />
                )}
              </div>
            ))}
          </div>

          <h1 className="text-2xl sm:text-3xl font-bold text-primary-900 dark:text-sand-50 mb-2">
            Choose how to pay
          </h1>
          <p className="text-sm text-primary-600 dark:text-sand-300 mb-8">
            All payments are processed securely by trusted third-party providers. StayAfrica never stores your payment details.
          </p>

          <div className="grid lg:grid-cols-5 gap-8">
            {/* Left column — Payment methods */}
            <div className="lg:col-span-3 space-y-8">

              {/* Regional providers */}
              {regionalProviders.length > 0 && (
                <section>
                  <div className="flex items-center gap-2 mb-4">
                    <MapPin className="w-4 h-4 text-secondary-600 dark:text-secondary-400" />
                    <h2 className="text-sm font-semibold uppercase tracking-wider text-primary-500 dark:text-sand-400">
                      Local payment methods
                    </h2>
                  </div>

                  <div className="space-y-3">
                    {regionalProviders.map((provider) => {
                      const meta = PROVIDER_META[provider.id];
                      const isSelected = selectedProvider === provider.id;
                      return (
                        <button
                          key={provider.id}
                          onClick={() => setSelectedProvider(provider.id)}
                          className={`w-full card p-5 text-left transition hover:shadow-lg ${
                            isSelected
                              ? 'ring-2 ring-secondary-500 dark:ring-secondary-400 bg-secondary-50/50 dark:bg-secondary-900/20'
                              : 'hover:border-primary-300 dark:hover:border-primary-600'
                          }`}
                        >
                          <div className="flex items-start gap-4">
                            <div className={`p-2.5 rounded-xl ${
                              isSelected
                                ? 'bg-secondary-100 dark:bg-secondary-800 text-secondary-600 dark:text-secondary-400'
                                : 'bg-primary-100 dark:bg-primary-700 text-primary-600 dark:text-sand-400'
                            }`}>
                              <ProviderIcon id={provider.id} />
                            </div>
                            <div className="flex-1 min-w-0">
                              <div className="flex items-center justify-between mb-1">
                                <h3 className="font-semibold text-primary-900 dark:text-sand-50">
                                  {provider.name}
                                </h3>
                                {isSelected && (
                                  <CheckCircle className="w-5 h-5 text-secondary-600 dark:text-secondary-400 flex-shrink-0" />
                                )}
                              </div>
                              <p className="text-xs text-primary-500 dark:text-sand-400 mb-2.5">
                                {meta?.description || `Pay with ${provider.name}`}
                              </p>
                              {/* Method chips */}
                              {meta?.methods && (
                                <div className="flex flex-wrap gap-1.5">
                                  {meta.methods.map((m) => (
                                    <span key={m.label} className="inline-flex items-center gap-1 px-2 py-0.5 rounded-full text-[11px] font-medium bg-primary-100 dark:bg-primary-700 text-primary-600 dark:text-sand-300">
                                      <MethodIcon type={m.icon} />
                                      {m.label}
                                    </span>
                                  ))}
                                </div>
                              )}
                              {meta?.poweredBy && (
                                <p className="text-[10px] text-primary-400 dark:text-sand-500 mt-2 flex items-center gap-1">
                                  <Lock className="w-3 h-3" />
                                  Processed by {meta.poweredBy}
                                </p>
                              )}
                            </div>
                          </div>
                        </button>
                      );
                    })}
                  </div>

                  {/* Regional availability note */}
                  <div className="mt-3 flex items-start gap-2 px-1">
                    <Info className="w-4 h-4 text-primary-400 dark:text-sand-500 mt-0.5 flex-shrink-0" />
                    <p className="text-xs text-primary-500 dark:text-sand-400 leading-relaxed">
                      Local payment availability depends on your region. We&apos;re actively working to add more localized providers across Africa.
                    </p>
                  </div>
                </section>
              )}

              {/* International providers */}
              {internationalProviders.length > 0 && (
                <section>
                  <div className="flex items-center gap-2 mb-4">
                    <Globe className="w-4 h-4 text-secondary-600 dark:text-secondary-400" />
                    <h2 className="text-sm font-semibold uppercase tracking-wider text-primary-500 dark:text-sand-400">
                      International payment methods
                    </h2>
                  </div>

                  <div className="space-y-3">
                    {internationalProviders.map((provider) => {
                      const meta = PROVIDER_META[provider.id];
                      const isSelected = selectedProvider === provider.id;
                      return (
                        <button
                          key={provider.id}
                          onClick={() => setSelectedProvider(provider.id)}
                          className={`w-full card p-5 text-left transition hover:shadow-lg ${
                            isSelected
                              ? 'ring-2 ring-secondary-500 dark:ring-secondary-400 bg-secondary-50/50 dark:bg-secondary-900/20'
                              : 'hover:border-primary-300 dark:hover:border-primary-600'
                          }`}
                        >
                          <div className="flex items-start gap-4">
                            <div className={`p-2.5 rounded-xl ${
                              isSelected
                                ? 'bg-secondary-100 dark:bg-secondary-800 text-secondary-600 dark:text-secondary-400'
                                : 'bg-primary-100 dark:bg-primary-700 text-primary-600 dark:text-sand-400'
                            }`}>
                              <ProviderIcon id={provider.id} />
                            </div>
                            <div className="flex-1 min-w-0">
                              <div className="flex items-center justify-between mb-1">
                                <div className="flex items-center gap-2">
                                  <h3 className="font-semibold text-primary-900 dark:text-sand-50">
                                    {provider.name}
                                  </h3>
                                  {meta?.cardOnly && (
                                    <span className="text-[10px] font-medium px-1.5 py-0.5 rounded bg-blue-100 dark:bg-blue-900/40 text-blue-700 dark:text-blue-300">
                                      Cards only
                                    </span>
                                  )}
                                </div>
                                {isSelected && (
                                  <CheckCircle className="w-5 h-5 text-secondary-600 dark:text-secondary-400 flex-shrink-0" />
                                )}
                              </div>
                              <p className="text-xs text-primary-500 dark:text-sand-400 mb-2.5">
                                {meta?.description || `Pay with ${provider.name}`}
                              </p>
                              {meta?.methods && (
                                <div className="flex flex-wrap gap-1.5">
                                  {meta.methods.map((m) => (
                                    <span key={m.label} className="inline-flex items-center gap-1 px-2 py-0.5 rounded-full text-[11px] font-medium bg-primary-100 dark:bg-primary-700 text-primary-600 dark:text-sand-300">
                                      <MethodIcon type={m.icon} />
                                      {m.label}
                                    </span>
                                  ))}
                                </div>
                              )}
                              {meta?.poweredBy && (
                                <p className="text-[10px] text-primary-400 dark:text-sand-500 mt-2 flex items-center gap-1">
                                  <Lock className="w-3 h-3" />
                                  Processed by {meta.poweredBy}
                                  <ExternalLink className="w-2.5 h-2.5" />
                                </p>
                              )}
                            </div>
                          </div>
                        </button>
                      );
                    })}
                  </div>
                </section>
              )}

              {/* No providers state */}
              {(!providersData || providersData.length === 0) && (
                <div className="card p-8 text-center">
                  <AlertCircle className="w-12 h-12 text-amber-500 mx-auto mb-4" />
                  <p className="text-primary-700 dark:text-sand-200">
                    {providersError
                      ? 'Failed to load payment methods. Please try again later.'
                      : 'No payment methods available for your region yet. We\'re working on adding more.'}
                  </p>
                </div>
              )}

              {/* Security & trust footer */}
              <div className="card p-5">
                <div className="flex items-start gap-3 mb-4">
                  <Shield className="w-5 h-5 text-primary-500 dark:text-sand-400 mt-0.5 flex-shrink-0" />
                  <div>
                    <h3 className="font-semibold text-sm text-primary-900 dark:text-sand-50 mb-1">
                      Your payment is secure
                    </h3>
                    <p className="text-xs text-primary-600 dark:text-sand-300 leading-relaxed">
                      StayAfrica partners with industry-leading payment providers. Your card details and financial information are handled entirely by the payment provider — we never see or store them.
                    </p>
                  </div>
                </div>
                <div className="flex flex-wrap items-center gap-3 pt-3 border-t border-primary-100 dark:border-primary-800">
                  <span className="flex items-center gap-1 text-[11px] text-primary-500 dark:text-sand-400">
                    <Lock className="w-3 h-3" /> SSL Encrypted
                  </span>
                  <span className="flex items-center gap-1 text-[11px] text-primary-500 dark:text-sand-400">
                    <Shield className="w-3 h-3" /> PCI Compliant
                  </span>
                  <span className="flex items-center gap-1 text-[11px] text-primary-500 dark:text-sand-400">
                    <CheckCircle className="w-3 h-3" /> Verified Providers
                  </span>
                </div>
              </div>
            </div>

            {/* Right column — Booking summary sidebar */}
            <div className="lg:col-span-2">
              <div className="card sticky top-24 overflow-hidden">
                {/* Mini property banner */}
                {propertyImage && (
                  <div className="relative h-32">
                    <img src={propertyImage} alt="" className="w-full h-full object-cover" />
                    <div className="absolute inset-0 bg-gradient-to-t from-black/60 to-transparent" />
                    <div className="absolute bottom-3 left-4 right-4">
                      <p className="text-white text-sm font-semibold truncate drop-shadow-sm">{propertyTitle}</p>
                      {(propertyCity || propertyCountry) && (
                        <p className="text-white/80 text-xs flex items-center gap-1 mt-0.5">
                          <MapPin className="w-3 h-3" />
                          {[propertyCity, propertyCountry].filter(Boolean).join(', ')}
                        </p>
                      )}
                    </div>
                  </div>
                )}

                <div className="p-5">
                  <h2 className="text-sm font-semibold uppercase tracking-wider text-primary-500 dark:text-sand-400 mb-4">
                    Booking summary
                  </h2>

                  <div className="space-y-2.5 text-sm pb-4 border-b border-primary-100 dark:border-primary-800">
                    <div className="flex justify-between">
                      <span className="text-primary-500 dark:text-sand-400">Reference</span>
                      <span className="font-mono text-xs font-medium text-primary-900 dark:text-sand-50">{booking.booking_ref}</span>
                    </div>
                    <div className="flex justify-between">
                      <span className="text-primary-500 dark:text-sand-400">Dates</span>
                      <span className="text-primary-900 dark:text-sand-50 text-xs font-medium">
                        {formatDate(booking.check_in)} → {formatDate(booking.check_out)}
                      </span>
                    </div>
                    <div className="flex justify-between">
                      <span className="text-primary-500 dark:text-sand-400">Duration</span>
                      <span className="text-primary-900 dark:text-sand-50 font-medium">
                        {nights} night{nights !== 1 ? 's' : ''}
                      </span>
                    </div>
                    <div className="flex justify-between">
                      <span className="text-primary-500 dark:text-sand-400">Guests</span>
                      <span className="text-primary-900 dark:text-sand-50 font-medium">
                        {booking.number_of_guests || 1}
                      </span>
                    </div>
                  </div>

                  {/* Price breakdown */}
                  <div className="space-y-2 text-sm py-4 border-b border-primary-100 dark:border-primary-800">
                    <div className="flex justify-between">
                      <span className="text-primary-600 dark:text-sand-300">Nightly total</span>
                      <span className="text-primary-900 dark:text-sand-50">{booking.currency} {booking.nightly_total}</span>
                    </div>
                    {parseFloat(booking.service_fee || '0') > 0 && (
                      <div className="flex justify-between">
                        <span className="text-primary-600 dark:text-sand-300">Service fee</span>
                        <span className="text-primary-900 dark:text-sand-50">{booking.currency} {booking.service_fee}</span>
                      </div>
                    )}
                    {parseFloat(booking.commission_fee || '0') > 0 && (
                      <div className="flex justify-between">
                        <span className="text-primary-600 dark:text-sand-300">Commission</span>
                        <span className="text-primary-900 dark:text-sand-50">{booking.currency} {booking.commission_fee}</span>
                      </div>
                    )}
                    {parseFloat(booking.cleaning_fee || '0') > 0 && (
                      <div className="flex justify-between">
                        <span className="text-primary-600 dark:text-sand-300">Cleaning fee</span>
                        <span className="text-primary-900 dark:text-sand-50">{booking.currency} {booking.cleaning_fee}</span>
                      </div>
                    )}
                    {parseFloat(booking.taxes || '0') > 0 && (
                      <div className="flex justify-between">
                        <span className="text-primary-600 dark:text-sand-300">Taxes</span>
                        <span className="text-primary-900 dark:text-sand-50">{booking.currency} {booking.taxes}</span>
                      </div>
                    )}
                  </div>

                  {/* Total */}
                  <div className="flex justify-between items-baseline pt-4 mb-1">
                    <span className="font-bold text-primary-900 dark:text-sand-50">
                      {isCashOnArrival ? 'Pay now' : 'Total'}
                    </span>
                    <span className="text-xl font-bold text-secondary-700 dark:text-secondary-400">
                      {booking.currency} {displayAmount}
                    </span>
                  </div>

                  {isCashOnArrival && (
                    <div className="mb-4 p-3 rounded-lg bg-amber-50 dark:bg-amber-900/20 border border-amber-200 dark:border-amber-800">
                      <div className="flex justify-between text-sm font-medium text-amber-800 dark:text-amber-300 mb-1">
                        <span>Pay on arrival</span>
                        <span>{booking.currency} {booking.nightly_total}</span>
                      </div>
                      <p className="text-[11px] text-amber-700 dark:text-amber-400 leading-relaxed">
                        Accommodation cost is paid in cash at the property on check-in. Only platform charges are processed now.
                      </p>
                    </div>
                  )}

                  {/* Redirect notice */}
                  {selectedMeta && selectedProvider !== 'cash_on_arrival' && (
                    <div className="flex items-start gap-2 mb-4 p-3 rounded-lg bg-sand-50 dark:bg-primary-800">
                      <ExternalLink className="w-4 h-4 text-primary-500 dark:text-sand-400 mt-0.5 flex-shrink-0" />
                      <p className="text-xs text-primary-600 dark:text-sand-300 leading-relaxed">
                        You&apos;ll be redirected to <strong className="text-primary-900 dark:text-sand-50">{selectedMeta.poweredBy}</strong> to complete your payment securely, then brought back to StayAfrica.
                      </p>
                    </div>
                  )}

                  {/* CTA */}
                  <button
                    onClick={handlePayment}
                    disabled={!selectedProvider || initiatePaymentMutation.isPending}
                    className="w-full bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white font-semibold py-3.5 px-4 rounded-lg transition disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center gap-2 text-base mt-4"
                  >
                    {initiatePaymentMutation.isPending ? (
                      <><Loader2 className="w-5 h-5 animate-spin" /> Processing...</>
                    ) : !selectedProvider ? (
                      'Select a payment method'
                    ) : isCashOnArrival ? (
                      <>
                        <Banknote className="w-5 h-5" />
                        Pay charges {booking.currency} {chargesOnly}
                      </>
                    ) : (
                      <>
                        <Lock className="w-5 h-5" />
                        Pay {booking.currency} {booking.grand_total}
                      </>
                    )}
                  </button>

                  <p className="text-[11px] text-center text-primary-400 dark:text-sand-500 mt-3 leading-relaxed">
                    By paying, you agree to StayAfrica&apos;s{' '}
                    <Link href="/terms" className="underline hover:text-secondary-600">Terms</Link> and{' '}
                    <Link href="/cancellation-policy" className="underline hover:text-secondary-600">Cancellation Policy</Link>
                  </p>
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Mobile sticky CTA */}
        <div className="lg:hidden fixed bottom-0 left-0 right-0 p-4 bg-white dark:bg-primary-900 border-t border-primary-200 dark:border-primary-700 shadow-[0_-4px_20px_rgba(0,0,0,0.1)] z-40">
          <div className="flex items-center justify-between mb-2">
            <span className="text-sm font-bold text-primary-900 dark:text-sand-50">
              {booking.currency} {displayAmount}
            </span>
            {selectedMeta && (
              <span className="text-xs text-primary-500 dark:text-sand-400 flex items-center gap-1">
                <Lock className="w-3 h-3" /> via {selectedMeta.poweredBy || 'Cash'}
              </span>
            )}
          </div>
          <button
            onClick={handlePayment}
            disabled={!selectedProvider || initiatePaymentMutation.isPending}
            className="w-full bg-secondary-600 hover:bg-secondary-700 dark:bg-secondary-700 dark:hover:bg-secondary-600 text-white font-semibold py-3 rounded-lg transition disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center gap-2"
          >
            {initiatePaymentMutation.isPending ? (
              <><Loader2 className="w-5 h-5 animate-spin" /> Processing...</>
            ) : !selectedProvider ? (
              'Select a payment method'
            ) : (
              <>
                <Lock className="w-4 h-4" />
                {isCashOnArrival ? `Pay charges ${booking.currency} ${chargesOnly}` : `Pay ${booking.currency} ${booking.grand_total}`}
              </>
            )}
          </button>
        </div>
      </div>
    </ProtectedRoute>
  );
}
