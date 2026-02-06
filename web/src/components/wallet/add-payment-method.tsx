'use client';

import { useState } from 'react';
import { useRouter } from 'next/navigation';
import { apiClient } from '@/services/api-client';
import { Button } from '@/components/ui';
import { CreditCard, Smartphone, Building, CheckCircle, ArrowRight, ArrowLeft } from 'lucide-react';
import toast from 'react-hot-toast';

type PaymentProvider = 'stripe' | 'paynow' | 'flutterwave' | 'paystack';
type PaymentMethod = 'card' | 'mobile' | 'bank' | 'ussd';
type Step = 'provider' | 'method' | 'details' | 'confirmation';

interface PaymentData {
  provider: PaymentProvider | null;
  method: PaymentMethod | null;
  name: string;
  cardNumber: string;
  expiryMonth: string;
  expiryYear: string;
  cvv: string;
  phoneNumber: string;
  accountNumber: string;
  bankName: string;
  ussdCode: string;
}

const PROVIDERS = [
  { id: 'stripe', name: 'Stripe', description: 'Global payments - Cards accepted worldwide', icon: CreditCard },
  { id: 'paynow', name: 'Paynow', description: 'Zimbabwe - Ecocash, Visa, Mastercard', icon: Smartphone },
  { id: 'flutterwave', name: 'Flutterwave', description: 'Africa-wide - Cards, Mobile Money, USSD', icon: Smartphone },
  { id: 'paystack', name: 'Paystack', description: 'Nigeria, Ghana, South Africa - All payment methods', icon: CreditCard },
];

const PAYMENT_METHODS: Record<PaymentProvider, Array<{ id: PaymentMethod; name: string; icon: any }>> = {
  stripe: [{ id: 'card', name: 'Credit/Debit Card', icon: CreditCard }],
  paynow: [
    { id: 'mobile', name: 'Ecocash', icon: Smartphone },
    { id: 'card', name: 'Visa/Mastercard', icon: CreditCard },
  ],
  flutterwave: [
    { id: 'card', name: 'Credit/Debit Card', icon: CreditCard },
    { id: 'mobile', name: 'Mobile Money', icon: Smartphone },
    { id: 'bank', name: 'Bank Transfer', icon: Building },
    { id: 'ussd', name: 'USSD', icon: Smartphone },
  ],
  paystack: [
    { id: 'card', name: 'Credit/Debit Card', icon: CreditCard },
    { id: 'bank', name: 'Bank Transfer', icon: Building },
  ],
};

export function AddPaymentMethodWizard() {
  const router = useRouter();
  const [currentStep, setCurrentStep] = useState<Step>('provider');
  const [loading, setLoading] = useState(false);
  const [paymentData, setPaymentData] = useState<PaymentData>({
    provider: null,
    method: null,
    name: '',
    cardNumber: '',
    expiryMonth: '',
    expiryYear: '',
    cvv: '',
    phoneNumber: '',
    accountNumber: '',
    bankName: '',
    ussdCode: '',
  });

  const handleProviderSelect = (provider: PaymentProvider) => {
    setPaymentData({ ...paymentData, provider, method: null });
    setCurrentStep('method');
  };

  const handleMethodSelect = (method: PaymentMethod) => {
    setPaymentData({ ...paymentData, method });
    setCurrentStep('details');
  };

  const handleAddPaymentMethod = async () => {
    if (!paymentData.provider || !paymentData.method) {
      toast.error('Please complete all steps');
      return;
    }

    // Validate fields based on method type
    if (paymentData.method === 'card') {
      if (!paymentData.cardNumber || !paymentData.expiryMonth || !paymentData.expiryYear || !paymentData.cvv) {
        toast.error('Please fill in all card details');
        return;
      }
    } else if (paymentData.method === 'mobile') {
      if (!paymentData.phoneNumber) {
        toast.error('Please enter phone number');
        return;
      }
    } else if (paymentData.method === 'bank') {
      if (!paymentData.accountNumber || !paymentData.bankName) {
        toast.error('Please fill in all bank details');
        return;
      }
    } else if (paymentData.method === 'ussd') {
      if (!paymentData.phoneNumber) {
        toast.error('Please enter phone number');
        return;
      }
    }

    setLoading(true);
    try {
      await apiClient.post('/payments/payment-methods/', {
        provider: paymentData.provider,
        method_type: paymentData.method,
        name: paymentData.provider === 'stripe' ? 'Credit Card' :
              paymentData.provider === 'paynow' ? 'Paynow Account' :
              paymentData.provider === 'flutterwave' ? 'Flutterwave Account' :
              'Paystack Account',
        token: 'placeholder-token', // In production, this would be tokenized
        last_four: paymentData.cardNumber?.slice(-4) || '',
        expiry_month: paymentData.expiryMonth ? parseInt(paymentData.expiryMonth) : undefined,
        expiry_year: paymentData.expiryYear ? parseInt(paymentData.expiryYear) : undefined,
        phone_number: paymentData.phoneNumber || '',
        is_default: false,
      });

      toast.success('Payment method added successfully!');
      router.push('/wallet');
    } catch (error: any) {
      console.error('Error adding payment method:', error);
      toast.error(error.response?.data?.error || 'Failed to add payment method');
    } finally {
      setLoading(false);
    }
  };

  const renderProgressBar = () => {
    const steps: Step[] = ['provider', 'method', 'details', 'confirmation'];
    const currentIndex = steps.indexOf(currentStep);

    return (
      <div className="flex gap-2 mb-8">
        {steps.map((step, index) => (
          <div
            key={step}
            className={`flex-1 h-2 rounded-full transition ${
              index <= currentIndex ? 'bg-secondary-500' : 'bg-primary-200 dark:bg-primary-700'
            }`}
          />
        ))}
      </div>
    );
  };

  // Provider Selection Step
  const renderProviderStep = () => (
    <div>
      <h2 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-2">Select Payment Provider</h2>
      <p className="text-primary-600 dark:text-sand-300 mb-8">Choose your preferred payment method provider</p>

      <div className="grid gap-4">
        {PROVIDERS.map((provider) => {
          const IconComponent = provider.icon;
          return (
            <button
              key={provider.id}
              onClick={() => handleProviderSelect(provider.id as PaymentProvider)}
              className="card p-6 text-left hover:shadow-lg transition group"
            >
              <div className="flex items-start gap-4">
                <div className="p-3 bg-secondary-100 dark:bg-secondary-900/30 rounded-xl group-hover:bg-secondary-200 dark:group-hover:bg-secondary-800/50 transition">
                  <IconComponent className="w-6 h-6 text-secondary-600 dark:text-secondary-400" />
                </div>
                <div className="flex-1">
                  <h3 className="font-bold text-primary-900 dark:text-sand-50 mb-1">{provider.name}</h3>
                  <p className="text-sm text-primary-600 dark:text-sand-300">{provider.description}</p>
                </div>
                <ArrowRight className="w-5 h-5 text-primary-400 dark:text-sand-500 group-hover:text-secondary-600 dark:group-hover:text-secondary-400 transition" />
              </div>
            </button>
          );
        })}
      </div>
    </div>
  );

  // Payment Method Selection Step
  const renderMethodStep = () => {
    if (!paymentData.provider) return null;

    const methods = PAYMENT_METHODS[paymentData.provider];

    return (
      <div>
        <button
          onClick={() => setCurrentStep('provider')}
          className="flex items-center gap-2 text-primary-600 dark:text-sand-300 hover:text-secondary-600 dark:hover:text-secondary-400 mb-6 transition"
        >
          <ArrowLeft className="w-4 h-4" />
          Back to providers
        </button>

        <h2 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-2">Select Payment Method</h2>
        <p className="text-primary-600 dark:text-sand-300 mb-8">
          Choose how you want to pay with {PROVIDERS.find(p => p.id === paymentData.provider)?.name}
        </p>

        <div className="grid gap-4">
          {methods.map((method) => {
            const IconComponent = method.icon;
            return (
              <button
                key={method.id}
                onClick={() => handleMethodSelect(method.id)}
                className="card p-6 text-left hover:shadow-lg transition group"
              >
                <div className="flex items-center gap-4">
                  <div className="p-3 bg-secondary-100 dark:bg-secondary-900/30 rounded-xl group-hover:bg-secondary-200 dark:group-hover:bg-secondary-800/50 transition">
                    <IconComponent className="w-6 h-6 text-secondary-600 dark:text-secondary-400" />
                  </div>
                  <div className="flex-1">
                    <h3 className="font-bold text-primary-900 dark:text-sand-50">{method.name}</h3>
                  </div>
                  <ArrowRight className="w-5 h-5 text-primary-400 dark:text-sand-500 group-hover:text-secondary-600 dark:group-hover:text-secondary-400 transition" />
                </div>
              </button>
            );
          })}
        </div>
      </div>
    );
  };

  // Payment Details Step
  const renderDetailsStep = () => {
    if (!paymentData.method) return null;

    return (
      <div>
        <button
          onClick={() => setCurrentStep('method')}
          className="flex items-center gap-2 text-primary-600 dark:text-sand-300 hover:text-secondary-600 dark:hover:text-secondary-400 mb-6 transition"
        >
          <ArrowLeft className="w-4 h-4" />
          Back to methods
        </button>

        <h2 className="text-3xl font-bold text-primary-900 dark:text-sand-50 mb-2">Enter Payment Details</h2>
        <p className="text-primary-600 dark:text-sand-300 mb-8">Provide your payment information securely</p>

        <div className="card p-8 space-y-6">
          {/* Card Payment */}
          {paymentData.method === 'card' && (
            <>
              <div>
                <label className="block text-sm font-semibold text-primary-700 dark:text-sand-200 mb-2">
                  Card Number
                </label>
                <input
                  type="text"
                  placeholder="1234 5678 9012 3456"
                  maxLength={19}
                  value={paymentData.cardNumber}
                  onChange={(e) => setPaymentData({ ...paymentData, cardNumber: e.target.value })}
                  className="w-full px-4 py-3 border border-primary-200 dark:border-primary-700 rounded-xl bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-secondary-500 dark:focus:ring-secondary-400 focus:border-transparent transition"
                />
              </div>

              <div className="grid grid-cols-3 gap-4">
                <div>
                  <label className="block text-sm font-semibold text-primary-700 dark:text-sand-200 mb-2">
                    Month
                  </label>
                  <input
                    type="text"
                    placeholder="MM"
                    maxLength={2}
                    value={paymentData.expiryMonth}
                    onChange={(e) => setPaymentData({ ...paymentData, expiryMonth: e.target.value })}
                    className="w-full px-4 py-3 border border-primary-200 dark:border-primary-700 rounded-xl bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-secondary-500 dark:focus:ring-secondary-400 focus:border-transparent transition"
                  />
                </div>

                <div>
                  <label className="block text-sm font-semibold text-primary-700 dark:text-sand-200 mb-2">
                    Year
                  </label>
                  <input
                    type="text"
                    placeholder="YYYY"
                    maxLength={4}
                    value={paymentData.expiryYear}
                    onChange={(e) => setPaymentData({ ...paymentData, expiryYear: e.target.value })}
                    className="w-full px-4 py-3 border border-primary-200 dark:border-primary-700 rounded-xl bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-secondary-500 dark:focus:ring-secondary-400 focus:border-transparent transition"
                  />
                </div>

                <div>
                  <label className="block text-sm font-semibold text-primary-700 dark:text-sand-200 mb-2">
                    CVV
                  </label>
                  <input
                    type="text"
                    placeholder="123"
                    maxLength={4}
                    value={paymentData.cvv}
                    onChange={(e) => setPaymentData({ ...paymentData, cvv: e.target.value })}
                    className="w-full px-4 py-3 border border-primary-200 dark:border-primary-700 rounded-xl bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-secondary-500 dark:focus:ring-secondary-400 focus:border-transparent transition"
                  />
                </div>
              </div>
            </>
          )}

          {/* Mobile Money */}
          {paymentData.method === 'mobile' && (
            <div>
              <label className="block text-sm font-semibold text-primary-700 dark:text-sand-200 mb-2">
                Phone Number
              </label>
              <input
                type="tel"
                placeholder="+263 77 123 4567"
                value={paymentData.phoneNumber}
                onChange={(e) => setPaymentData({ ...paymentData, phoneNumber: e.target.value })}
                className="w-full px-4 py-3 border border-primary-200 dark:border-primary-700 rounded-xl bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-secondary-500 dark:focus:ring-secondary-400 focus:border-transparent transition"
              />
            </div>
          )}

          {/* Bank Transfer */}
          {paymentData.method === 'bank' && (
            <>
              <div>
                <label className="block text-sm font-semibold text-primary-700 dark:text-sand-200 mb-2">
                  Bank Name
                </label>
                <input
                  type="text"
                  placeholder="Enter bank name"
                  value={paymentData.bankName}
                  onChange={(e) => setPaymentData({ ...paymentData, bankName: e.target.value })}
                  className="w-full px-4 py-3 border border-primary-200 dark:border-primary-700 rounded-xl bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-secondary-500 dark:focus:ring-secondary-400 focus:border-transparent transition"
                />
              </div>

              <div>
                <label className="block text-sm font-semibold text-primary-700 dark:text-sand-200 mb-2">
                  Account Number
                </label>
                <input
                  type="text"
                  placeholder="Enter account number"
                  value={paymentData.accountNumber}
                  onChange={(e) => setPaymentData({ ...paymentData, accountNumber: e.target.value })}
                  className="w-full px-4 py-3 border border-primary-200 dark:border-primary-700 rounded-xl bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-secondary-500 dark:focus:ring-secondary-400 focus:border-transparent transition"
                />
              </div>
            </>
          )}

          {/* USSD */}
          {paymentData.method === 'ussd' && (
            <div>
              <label className="block text-sm font-semibold text-primary-700 dark:text-sand-200 mb-2">
                Phone Number
              </label>
              <input
                type="tel"
                placeholder="+234 80 1234 5678"
                value={paymentData.phoneNumber}
                onChange={(e) => setPaymentData({ ...paymentData, phoneNumber: e.target.value })}
                className="w-full px-4 py-3 border border-primary-200 dark:border-primary-700 rounded-xl bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50 focus:ring-2 focus:ring-secondary-500 dark:focus:ring-secondary-400 focus:border-transparent transition"
              />
            </div>
          )}

          {/* Security Note */}
          <div className="bg-blue-50 dark:bg-blue-900/20 p-4 rounded-xl flex items-start gap-3">
            <CheckCircle className="w-5 h-5 text-blue-600 dark:text-blue-400 flex-shrink-0 mt-0.5" />
            <div>
              <p className="text-sm font-semibold text-blue-900 dark:text-blue-200 mb-1">Secure Payment</p>
              <p className="text-xs text-blue-800 dark:text-blue-300">
                Your payment information is encrypted and secure. We never store full card details.
              </p>
            </div>
          </div>

          <Button
            onClick={handleAddPaymentMethod}
            disabled={loading}
            variant="primary"
            size="lg"
            className="w-full"
          >
            {loading ? 'Adding Payment Method...' : 'Add Payment Method'}
          </Button>
        </div>
      </div>
    );
  };

  return (
    <div className="min-h-screen bg-sand-100 dark:bg-primary-900">
      <div className="max-w-3xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        {renderProgressBar()}

        {currentStep === 'provider' && renderProviderStep()}
        {currentStep === 'method' && renderMethodStep()}
        {currentStep === 'details' && renderDetailsStep()}
      </div>
    </div>
  );
}
