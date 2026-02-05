import { View, Text, ScrollView, TouchableOpacity, Platform, Alert } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { GlassmorphicView } from '@/components/common/GlassmorphicView';

interface PaymentMethod {
  id: string;
  type: 'card' | 'mobile' | 'bank';
  name: string;
  lastFour?: string;
  isDefault: boolean;
}

export default function PaymentMethodsScreen() {
  const router = useRouter();
  const [paymentMethods, setPaymentMethods] = useState<PaymentMethod[]>([
    // Sample data - replace with actual API data
  ]);

  const getIcon = (type: string): keyof typeof Ionicons.glyphMap => {
    switch (type) {
      case 'card': return 'card';
      case 'mobile': return 'phone-portrait';
      case 'bank': return 'business';
      default: return 'wallet';
    }
  };

  const handleSetDefault = (id: string) => {
    setPaymentMethods(methods => 
      methods.map(m => ({ ...m, isDefault: m.id === id }))
    );
  };

  const handleDelete = (id: string) => {
    Alert.alert(
      'Delete Payment Method',
      'Are you sure you want to delete this payment method?',
      [
        { text: 'Cancel', style: 'cancel' },
        { 
          text: 'Delete', 
          style: 'destructive',
          onPress: () => setPaymentMethods(methods => methods.filter(m => m.id !== id))
        },
      ]
    );
  };

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <View className="flex-row items-center">
          <GlassmorphicView
            intensity={40}
            tint="dark"
            borderRadius={12}
            style={{ width: 40, height: 40 }}
          >
            <TouchableOpacity
              onPress={() => router.back()}
              className="flex-1 items-center justify-center"
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
          </GlassmorphicView>
          <Text className="text-xl font-bold text-white ml-3">Payment Methods</Text>
        </View>
      </LinearGradient>

      <View className="px-4 py-6">
        {paymentMethods.length > 0 ? (
          <>
            {paymentMethods.map((method) => (
              <View
                key={method.id}
                className="bg-white rounded-2xl p-4 mb-3"
                style={{
                  shadowColor: '#122F26',
                  shadowOffset: { width: 0, height: 2 },
                  shadowOpacity: 0.05,
                  shadowRadius: 4,
                  elevation: 2,
                }}
              >
                <View className="flex-row items-center">
                  <View className="w-12 h-12 rounded-xl bg-sand-100 items-center justify-center">
                    <Ionicons name={getIcon(method.type)} size={24} color="#3A5C50" />
                  </View>
                  <View className="flex-1 ml-4">
                    <Text className="text-base font-semibold text-forest">{method.name}</Text>
                    {method.lastFour && (
                      <Text className="text-sm text-moss">•••• {method.lastFour}</Text>
                    )}
                  </View>
                  {method.isDefault && (
                    <View className="bg-gold/20 px-3 py-1 rounded-full">
                      <Text className="text-gold text-xs font-semibold">Default</Text>
                    </View>
                  )}
                </View>
                <View className="flex-row mt-4 pt-3 border-t border-sand-100">
                  {!method.isDefault && (
                    <TouchableOpacity 
                      className="flex-1 items-center"
                      onPress={() => handleSetDefault(method.id)}
                    >
                      <Text className="text-gold font-semibold text-sm">Set as Default</Text>
                    </TouchableOpacity>
                  )}
                  <TouchableOpacity 
                    className="flex-1 items-center"
                    onPress={() => handleDelete(method.id)}
                  >
                    <Text className="text-red-500 font-semibold text-sm">Remove</Text>
                  </TouchableOpacity>
                </View>
              </View>
            ))}
          </>
        ) : (
          <View className="items-center py-12">
            <View className="bg-sand-200 rounded-full p-6 mb-4">
              <Ionicons name="card-outline" size={48} color="#94a3b8" />
            </View>
            <Text className="text-xl font-bold text-forest mb-2">No Payment Methods</Text>
            <Text className="text-moss text-center mb-6 px-8">
              Add a payment method to make bookings and transactions easier
            </Text>
          </View>
        )}

        {/* Add New Method Button */}
        <TouchableOpacity 
          className="mt-4"
          onPress={() => Alert.alert('Coming Soon', 'Adding payment methods will be available soon.')}
        >
          <LinearGradient
            colors={['#D9B168', '#bea04f']}
            className="py-4 rounded-2xl flex-row items-center justify-center"
          >
            <Ionicons name="add" size={24} color="#122F26" />
            <Text className="text-forest font-bold text-base ml-2">Add Payment Method</Text>
          </LinearGradient>
        </TouchableOpacity>

        {/* Info Section */}
        <View className="mt-6 bg-blue-50 rounded-2xl p-4">
          <View className="flex-row items-center mb-2">
            <Ionicons name="shield-checkmark" size={20} color="#3B82F6" />
            <Text className="text-blue-800 font-semibold ml-2">Secure Payments</Text>
          </View>
          <Text className="text-blue-700 text-sm">
            Your payment information is encrypted and securely stored. We never share your financial data.
          </Text>
        </View>
      </View>
    </ScrollView>
  );
}
