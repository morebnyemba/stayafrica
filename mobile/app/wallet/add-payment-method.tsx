import { View, Text, ScrollView, TouchableOpacity, Platform, Alert, ActivityIndicator, Modal } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { apiClient } from '@/services/api-client';
import Input from '@/components/common/Input';

type PaymentStep = 'provider' | 'method' | 'details' | 'confirmation';
type PaymentProvider = 'stripe' | 'paynow' | 'flutterwave' | 'paystack';
type PaymentMethod = 'card' | 'mobile_money' | 'bank_transfer' | 'ussd';

interface PaymentMethodData {
  provider: PaymentProvider;
  method: PaymentMethod;
  cardNumber?: string;
  cardHolder?: string;
  expiryMonth?: string;
  expiryYear?: string;
  cvv?: string;
  phoneNumber?: string;
  accountNumber?: string;
  accountHolder?: string;
  bankCode?: string;
}

const PROVIDERS = [
  { id: 'stripe', name: 'Stripe', icon: 'card', regions: 'Global', description: 'Credit/Debit Cards' },
  { id: 'paynow', name: 'Paynow', icon: 'phone-portrait', regions: 'Zimbabwe', description: 'Ecocash, Visa' },
  { id: 'flutterwave', name: 'Flutterwave', icon: 'globe', regions: 'Africa', description: 'Cards, Mobile Money, USSD' },
  { id: 'paystack', name: 'Paystack', icon: 'phone-portrait', regions: 'Nigeria, Ghana, SA', description: 'Cards, Bank Transfers' },
];

const PAYMENT_METHODS: Record<PaymentProvider, Array<{ id: PaymentMethod; name: string; icon: string }>> = {
  stripe: [
    { id: 'card', name: 'Credit/Debit Card', icon: 'card' },
  ],
  paynow: [
    { id: 'mobile_money', name: 'Ecocash', icon: 'phone-portrait' },
  ],
  flutterwave: [
    { id: 'card', name: 'Debit/Credit Card', icon: 'card' },
    { id: 'mobile_money', name: 'Mobile Money', icon: 'phone-portrait' },
    { id: 'ussd', name: 'USSD', icon: 'dial' },
  ],
  paystack: [
    { id: 'card', name: 'Debit/Credit Card', icon: 'card' },
    { id: 'bank_transfer', name: 'Bank Transfer', icon: 'business' },
  ],
};

export default function AddPaymentMethodScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [currentStep, setCurrentStep] = useState<PaymentStep>('provider');
  const [loading, setLoading] = useState(false);
  const [paymentData, setPaymentData] = useState<PaymentMethodData>({
    provider: 'stripe',
    method: 'card',
  });

  const handleProviderSelect = (provider: PaymentProvider) => {
    setPaymentData(prev => ({
      ...prev,
      provider,
      method: PAYMENT_METHODS[provider][0].id as PaymentMethod,
    }));
    setCurrentStep('method');
  };

  const handleMethodSelect = (method: PaymentMethod) => {
    setPaymentData(prev => ({ ...prev, method }));
    setCurrentStep('details');
  };

  const handleAddPaymentMethod = async () => {
    // Validate input
    if (!validatePaymentData()) {
      return;
    }

    setLoading(true);
    try {
      // Call backend API to add payment method
      const response = await apiClient.post('/payments/payment-methods/', {
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

      Alert.alert('Success', 'Payment method added successfully!', [
        { text: 'OK', onPress: () => router.replace('/wallet/payment-methods') }
      ]);
    } catch (error) {
      console.error('Error adding payment method:', error);
      Alert.alert('Error', 'Failed to add payment method. Please try again.');
    } finally {
      setLoading(false);
    }
  };

  const validatePaymentData = (): boolean => {
    if (paymentData.method === 'card') {
      if (!paymentData.cardNumber || paymentData.cardNumber.length < 13) {
        Alert.alert('Error', 'Please enter a valid card number');
        return false;
      }
      if (!paymentData.cardHolder || paymentData.cardHolder.trim().length === 0) {
        Alert.alert('Error', 'Please enter the cardholder name');
        return false;
      }
      if (!paymentData.expiryMonth || !paymentData.expiryYear) {
        Alert.alert('Error', 'Please enter the expiry date');
        return false;
      }
      if (!paymentData.cvv || paymentData.cvv.length < 3) {
        Alert.alert('Error', 'Please enter a valid CVV');
        return false;
      }
    } else if (paymentData.method === 'mobile_money') {
      if (!paymentData.phoneNumber || paymentData.phoneNumber.length < 10) {
        Alert.alert('Error', 'Please enter a valid phone number');
        return false;
      }
    }
    return true;
  };

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
      >
        <View className="flex-row items-center">
          <TouchableOpacity
            onPress={() => {
              if (currentStep === 'provider') {
                router.back();
              } else {
                setCurrentStep(
                  currentStep === 'method' ? 'provider' :
                  currentStep === 'details' ? 'method' :
                  'details'
                );
              }
            }}
            className="w-10 h-10 rounded-xl items-center justify-center mr-3"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-xl font-bold text-white">Add Payment Method</Text>
        </View>

        {/* Progress Indicator */}
        <View className="flex-row gap-2 mt-4 mx-10">
          {(['provider', 'method', 'details', 'confirmation'] as const).map((step, index) => (
            <View
              key={step}
              className="flex-1 h-1 rounded-full"
              style={{
                backgroundColor:
                  (['provider', 'method', 'details', 'confirmation'].indexOf(currentStep) >= index)
                    ? '#D9B168'
                    : 'rgba(255, 255, 255, 0.3)',
              }}
            />
          ))}
        </View>
      </LinearGradient>

      <ScrollView
        className="flex-1"
        showsVerticalScrollIndicator={false}
        keyboardShouldPersistTaps="handled"
      >
        {/* Provider Selection Step */}
        {currentStep === 'provider' && (
          <View className="px-4 py-6">
            <Text className="text-2xl font-bold text-forest mb-2">Select Payment Provider</Text>
            <Text className="text-moss mb-6">Choose your preferred payment method provider</Text>

            {PROVIDERS.map((provider) => (
              <TouchableOpacity
                key={provider.id}
                onPress={() => handleProviderSelect(provider.id as PaymentProvider)}
                className="mb-3"
              >
                <View className="bg-white rounded-2xl p-4 flex-row items-center"
                  style={{
                    shadowColor: '#122F26',
                    shadowOffset: { width: 0, height: 2 },
                    shadowOpacity: 0.05,
                    shadowRadius: 4,
                    elevation: 2,
                  }}>
                  <View className="w-14 h-14 rounded-xl bg-gold/20 items-center justify-center mr-4">
                    <Ionicons name={provider.icon as any} size={24} color="#D9B168" />
                  </View>
                  <View className="flex-1">
                    <Text className="text-base font-bold text-forest">{provider.name}</Text>
                    <Text className="text-xs text-moss mt-1">{provider.regions}</Text>
                    <Text className="text-sm text-gold mt-1 font-semibold">{provider.description}</Text>
                  </View>
                  <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
                </View>
              </TouchableOpacity>
            ))}
          </View>
        )}

        {/* Payment Method Selection Step */}
        {currentStep === 'method' && (
          <View className="px-4 py-6">
            <Text className="text-2xl font-bold text-forest mb-2">Payment Method</Text>
            <Text className="text-moss mb-6">Available payment methods for {PROVIDERS.find(p => p.id === paymentData.provider)?.name}</Text>

            {PAYMENT_METHODS[paymentData.provider].map((method) => (
              <TouchableOpacity
                key={method.id}
                onPress={() => handleMethodSelect(method.id as PaymentMethod)}
                className={`mb-3 rounded-2xl p-4 flex-row items-center ${
                  paymentData.method === method.id ? 'bg-gold/20 border-2 border-gold' : 'bg-white'
                }`}
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 2 },
                  shadowOpacity: 0.05,
                  shadowRadius: 4,
                  elevation: 2,
                }}>
                <View className={`w-14 h-14 rounded-xl ${paymentData.method === method.id ? 'bg-gold' : 'bg-sand-100'} items-center justify-center mr-4`}>
                  <Ionicons name={method.icon as any} size={24} color={paymentData.method === method.id ? '#122F26' : '#3A5C50'} />
                </View>
                <View className="flex-1">
                  <Text className={`text-base font-bold ${paymentData.method === method.id ? 'text-gold' : 'text-forest'}`}>{method.name}</Text>
                </View>
                {paymentData.method === method.id && (
                  <Ionicons name="checkmark-circle" size={24} color="#D9B168" />
                )}
              </TouchableOpacity>
            ))}
          </View>
        )}

        {/* Payment Details Step */}
        {currentStep === 'details' && (
          <View className="px-4 py-6">
            <Text className="text-2xl font-bold text-forest mb-6">Enter Payment Details</Text>

            {paymentData.method === 'card' && (
              <>
                <Text className="text-base font-semibold text-forest mb-2">Card Number *</Text>
                <TextInput
                  className="bg-white rounded-2xl p-4 text-base text-forest mb-4 border border-sand-200"
                  placeholder="1234 5678 9012 3456"
                  placeholderTextColor="#94a3b8"
                  maxLength={19}
                  value={paymentData.cardNumber}
                  onChangeText={(text) => {
                    const formatted = text.replace(/\s/g, '').replace(/(\d{4})/g, '$1 ').trim();
                    setPaymentData(prev => ({ ...prev, cardNumber: formatted }));
                  }}
                />

                <Text className="text-base font-semibold text-forest mb-2">Cardholder Name *</Text>
                <TextInput
                  className="bg-white rounded-2xl p-4 text-base text-forest mb-4 border border-sand-200"
                  placeholder="John Doe"
                  placeholderTextColor="#94a3b8"
                  value={paymentData.cardHolder}
                  onChangeText={(text) => setPaymentData(prev => ({ ...prev, cardHolder: text }))}
                />

                <View className="flex-row gap-3 mb-4">
                  <View className="flex-1">
                    <Text className="text-base font-semibold text-forest mb-2">Month *</Text>
                    <TextInput
                      className="bg-white rounded-2xl p-4 text-base text-forest border border-sand-200"
                      placeholder="MM"
                      placeholderTextColor="#94a3b8"
                      maxLength={2}
                      keyboardType="number-pad"
                      value={paymentData.expiryMonth}
                      onChangeText={(text) => setPaymentData(prev => ({ ...prev, expiryMonth: text }))}
                    />
                  </View>
                  <View className="flex-1">
                    <Text className="text-base font-semibold text-forest mb-2">Year *</Text>
                    <TextInput
                      className="bg-white rounded-2xl p-4 text-base text-forest border border-sand-200"
                      placeholder="YY"
                      placeholderTextColor="#94a3b8"
                      maxLength={2}
                      keyboardType="number-pad"
                      value={paymentData.expiryYear}
                      onChangeText={(text) => setPaymentData(prev => ({ ...prev, expiryYear: text }))}
                    />
                  </View>
                  <View className="flex-1">
                    <Text className="text-base font-semibold text-forest mb-2">CVV *</Text>
                    <TextInput
                      className="bg-white rounded-2xl p-4 text-base text-forest border border-sand-200"
                      placeholder="123"
                      placeholderTextColor="#94a3b8"
                      maxLength={4}
                      keyboardType="number-pad"
                      secureTextEntry
                      value={paymentData.cvv}
                      onChangeText={(text) => setPaymentData(prev => ({ ...prev, cvv: text }))}
                    />
                  </View>
                </View>
              </>
            )}

            {paymentData.method === 'mobile_money' && (
              <>
                <Text className="text-base font-semibold text-forest mb-2">Phone Number *</Text>
                <TextInput
                  className="bg-white rounded-2xl p-4 text-base text-forest mb-4 border border-sand-200"
                  placeholder="+1 (555) 000-0000"
                  placeholderTextColor="#94a3b8"
                  keyboardType="phone-pad"
                  value={paymentData.phoneNumber}
                  onChangeText={(text) => setPaymentData(prev => ({ ...prev, phoneNumber: text }))}
                />
              </>
            )}

            {paymentData.method === 'bank_transfer' && (
              <>
                <Text className="text-base font-semibold text-forest mb-2">Account Number *</Text>
                <TextInput
                  className="bg-white rounded-2xl p-4 text-base text-forest mb-4 border border-sand-200"
                  placeholder="Enter account number"
                  placeholderTextColor="#94a3b8"
                  value={paymentData.accountNumber}
                  onChangeText={(text) => setPaymentData(prev => ({ ...prev, accountNumber: text }))}
                />

                <Text className="text-base font-semibold text-forest mb-2">Account Holder *</Text>
                <TextInput
                  className="bg-white rounded-2xl p-4 text-base text-forest mb-4 border border-sand-200"
                  placeholder="Account holder name"
                  placeholderTextColor="#94a3b8"
                  value={paymentData.accountHolder}
                  onChangeText={(text) => setPaymentData(prev => ({ ...prev, accountHolder: text }))}
                />
              </>
            )}

            {/* Security Note */}
            <View className="bg-blue-50 rounded-2xl p-4 mt-6 flex-row items-start">
              <Ionicons name="shield-checkmark" size={20} color="#3B82F6" />
              <Text className="text-sm text-blue-800 ml-3 flex-1">
                Your payment information is encrypted and secure. We never store full card details.
              </Text>
            </View>
          </View>
        )}

        {/* Confirmation Step */}
        {currentStep === 'confirmation' && (
          <View className="px-4 py-6">
            <View className="items-center">
              <View className="w-20 h-20 rounded-full bg-green-100 items-center justify-center mb-4">
                <Ionicons name="checkmark" size={40} color="#10B981" />
              </View>
              <Text className="text-2xl font-bold text-forest mb-2">Payment Method Added!</Text>
              <Text className="text-moss text-center mb-6">Your payment method has been successfully added</Text>

              <View className="bg-white rounded-2xl p-6 w-full mb-6"
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 2 },
                  shadowOpacity: 0.05,
                  shadowRadius: 4,
                  elevation: 2,
                }}>
                <View className="flex-row items-center mb-4">
                  <View className="w-12 h-12 rounded-xl bg-gold/20 items-center justify-center mr-3">
                    <Ionicons name="card" size={20} color="#D9B168" />
                  </View>
                  <View>
                    <Text className="text-sm text-moss">Payment Provider</Text>
                    <Text className="text-base font-bold text-forest">{paymentData.provider.toUpperCase()}</Text>
                  </View>
                </View>
                <View className="h-px bg-sand-100 mb-4" />
                <View className="flex-row items-center">
                  <Ionicons name="checkmark-circle" size={20} color="#10B981" />
                  <Text className="text-moss ml-2">This method is now active</Text>
                </View>
              </View>
            </View>
          </View>
        )}
      </ScrollView>

      {/* Action Buttons */}
      {currentStep !== 'confirmation' && (
        <View className="px-4 pb-6 pt-4">
          <TouchableOpacity
            onPress={handleAddPaymentMethod}
            disabled={loading}
          >
            <LinearGradient
              colors={loading ? ['#94a3b8', '#94a3b8'] : ['#D9B168', '#bea04f']}
              className="py-4 rounded-2xl items-center"
            >
              {loading ? (
                <ActivityIndicator color="#122F26" />
              ) : (
                <Text className="text-forest font-bold text-base">
                  {currentStep === 'provider' ? 'Continue' :
                   currentStep === 'method' ? 'Next' :
                   'Add Payment Method'}
                </Text>
              )}
            </LinearGradient>
          </TouchableOpacity>
        </View>
      )}

      {currentStep === 'confirmation' && (
        <View className="px-4 pb-6 pt-4">
          <TouchableOpacity
            onPress={() => router.replace('/wallet/payment-methods')}
          >
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="py-4 rounded-2xl items-center"
            >
              <Text className="text-forest font-bold text-base">Done</Text>
            </LinearGradient>
          </TouchableOpacity>
        </View>
      )}
    </SafeAreaView>
  );
}

AddPaymentMethodScreen.displayName = 'AddPaymentMethodScreen';
