import { View, Text, ScrollView, TouchableOpacity, TextInput, Platform, Alert, KeyboardAvoidingView, ActivityIndicator } from 'react-native';
import { useState, useEffect } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { useAuth } from '@/context/auth-context';
import { apiClient } from '@/services/api-client';
import { logApiError } from '@/utils/logger';

export default function WithdrawScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { user } = useAuth();
  const [amount, setAmount] = useState('');
  const [selectedMethod, setSelectedMethod] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const [balance, setBalance] = useState<number>(0);
  const [loadingBalance, setLoadingBalance] = useState(true);

  useEffect(() => {
    fetchBalance();
  }, []);

  const fetchBalance = async () => {
    try {
      setLoadingBalance(true);
      const wallet = await apiClient.getMyWallet();
      setBalance(wallet.balance || 0);
    } catch (error: any) {
      logApiError('/wallets/my_wallet/', error, { action: 'fetch balance' });
      // Don't show error alert, just keep balance at 0
    } finally {
      setLoadingBalance(false);
    }
  };

  const withdrawalMethods = [
    { id: 'bank', name: 'Bank Transfer', icon: 'business', description: '2-3 business days' },
    { id: 'mobile', name: 'Mobile Money', icon: 'phone-portrait', description: 'Instant' },
    { id: 'paypal', name: 'PayPal', icon: 'logo-paypal', description: '1-2 business days' },
  ];

  const handleWithdraw = async () => {
    if (!amount || parseFloat(amount) <= 0) {
      Alert.alert('Error', 'Please enter a valid amount');
      return;
    }
    if (!selectedMethod) {
      Alert.alert('Error', 'Please select a withdrawal method');
      return;
    }
    if (parseFloat(amount) > balance) {
      Alert.alert('Error', 'Insufficient balance');
      return;
    }

    setLoading(true);
    try {
      await apiClient.initiateWithdrawal({
        amount: parseFloat(amount),
        method: selectedMethod,
      });
      
      Alert.alert(
        'Success', 
        'Withdrawal request submitted successfully. You will receive your funds within the specified timeframe.', 
        [{ text: 'OK', onPress: () => router.back() }]
      );
    } catch (error: any) {
      const errorMessage = error?.response?.data?.detail || 
                          error?.response?.data?.message || 
                          'Failed to process withdrawal. Please try again.';
      Alert.alert('Error', errorMessage);
    } finally {
      setLoading(false);
    }
  };

  return (
    <KeyboardAvoidingView 
      behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
      className="flex-1"
    >
      <ScrollView 
        className="flex-1 bg-sand-100" 
        showsVerticalScrollIndicator={false}
        contentContainerStyle={{ paddingBottom: insets.bottom + 24 }}
        keyboardShouldPersistTaps="handled"
      >
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <View className="flex-row items-center mb-4">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center mr-3"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-xl font-bold text-white">Withdraw Funds</Text>
        </View>

        {/* Balance Card - Shows actual balance from API */}
        <View className="bg-white/10 rounded-2xl p-4">
          <Text className="text-sand-200 text-sm">Available Balance</Text>
          {loadingBalance ? (
            <ActivityIndicator color="#fff" size="small" style={{ marginVertical: 8 }} />
          ) : (
            <Text className="text-3xl font-black text-white mt-1">${balance.toFixed(2)}</Text>
          )}
        </View>
      </LinearGradient>

      <View className="px-4 py-6">
        {/* Amount Input */}
        <Text className="text-lg font-bold text-forest mb-3">Amount to Withdraw</Text>
        <View className="bg-white rounded-2xl p-4 mb-6" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}>
          <View className="flex-row items-center">
            <Text className="text-2xl font-bold text-forest mr-2">$</Text>
            <TextInput
              className="flex-1 text-2xl font-bold text-forest"
              placeholder="0.00"
              placeholderTextColor="#94a3b8"
              keyboardType="decimal-pad"
              value={amount}
              onChangeText={setAmount}
            />
          </View>
        </View>

        {/* Withdrawal Methods */}
        <Text className="text-lg font-bold text-forest mb-3">Withdrawal Method</Text>
        {withdrawalMethods.map((method) => (
          <TouchableOpacity
            key={method.id}
            className={`bg-white rounded-2xl p-4 mb-3 flex-row items-center ${
              selectedMethod === method.id ? 'border-2 border-gold' : ''
            }`}
            onPress={() => setSelectedMethod(method.id)}
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}
          >
            <View className={`w-12 h-12 rounded-xl items-center justify-center ${
              selectedMethod === method.id ? 'bg-gold' : 'bg-sand-100'
            }`}>
              <Ionicons 
                name={method.icon as keyof typeof Ionicons.glyphMap} 
                size={24} 
                color={selectedMethod === method.id ? '#122F26' : '#3A5C50'} 
              />
            </View>
            <View className="flex-1 ml-4">
              <Text className="text-base font-semibold text-forest">{method.name}</Text>
              <Text className="text-sm text-moss">{method.description}</Text>
            </View>
            {selectedMethod === method.id && (
              <Ionicons name="checkmark-circle" size={24} color="#D9B168" />
            )}
          </TouchableOpacity>
        ))}

        {/* Withdraw Button */}
        <TouchableOpacity
          onPress={handleWithdraw}
          disabled={loading}
          className="mt-6"
        >
          <LinearGradient
            colors={loading ? ['#94a3b8', '#94a3b8'] : ['#D9B168', '#bea04f']}
            className="py-4 rounded-2xl items-center"
          >
            <Text className="text-forest font-bold text-base">
              {loading ? 'Processing...' : 'Withdraw Funds'}
            </Text>
          </LinearGradient>
        </TouchableOpacity>
      </View>
      </ScrollView>
    </KeyboardAvoidingView>
  );
}
