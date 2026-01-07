import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { useAuth } from '@/context/auth-context';

export default function WalletScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [balance, setBalance] = useState(0);
  const [transactions, setTransactions] = useState<any[]>([]);
  const [isLoading, setIsLoading] = useState(false);

  if (!isAuthenticated) {
    return (
      <View className="flex-1 items-center justify-center bg-white px-6">
        <Ionicons name="wallet-outline" size={64} color="#ddd" />
        <Text className="text-xl font-bold text-gray-800 mt-4">Wallet Access Required</Text>
        <Text className="text-gray-600 text-center mt-2 mb-6">
          Sign in to view your wallet and transactions
        </Text>
        <TouchableOpacity
          className="bg-primary-600 px-8 py-3 rounded-lg"
          onPress={() => router.push('/(auth)/login')}
        >
          <Text className="text-white font-semibold">Sign In</Text>
        </TouchableOpacity>
      </View>
    );
  }

  const TransactionItem = ({ icon, title, amount, date, type }: any) => (
    <View className="bg-white rounded-xl p-4 mb-3 shadow-sm border border-gray-100 flex-row items-center">
      <View className={`w-12 h-12 rounded-full items-center justify-center ${
        type === 'credit' ? 'bg-green-100' : 'bg-red-100'
      }`}>
        <Ionicons name={icon} size={24} color={type === 'credit' ? '#10B981' : '#EF4444'} />
      </View>
      <View className="flex-1 ml-4">
        <Text className="text-base font-semibold text-gray-900">{title}</Text>
        <Text className="text-sm text-gray-600 mt-1">{date}</Text>
      </View>
      <Text className={`text-lg font-bold ${
        type === 'credit' ? 'text-green-600' : 'text-red-600'
      }`}>
        {type === 'credit' ? '+' : '-'}${amount}
      </Text>
    </View>
  );

  return (
    <ScrollView className="flex-1 bg-gray-50" showsVerticalScrollIndicator={false}>
      {/* Header */}
      <View className="bg-primary-600 px-4 pt-12 pb-8">
        <Text className="text-3xl font-extrabold text-white mb-2 tracking-tight">
          My Wallet
        </Text>
        <Text className="text-base text-white/80">
          Track your earnings and expenses
        </Text>
      </View>

      {/* Balance Card */}
      <View className="mx-4 -mt-6 bg-white rounded-2xl p-6 shadow-lg border border-gray-100">
        <Text className="text-sm text-gray-600 mb-2">Available Balance</Text>
        <Text className="text-4xl font-extrabold text-primary-900 mb-4">${balance.toFixed(2)}</Text>
        
        <View className="flex-row gap-3">
          <TouchableOpacity
            className="flex-1 bg-primary-600 py-3 rounded-lg flex-row items-center justify-center"
            onPress={() => router.push('/wallet/withdraw')}
          >
            <Ionicons name="arrow-down" size={18} color="white" />
            <Text className="text-white font-semibold ml-2">Withdraw</Text>
          </TouchableOpacity>
          
          <TouchableOpacity
            className="flex-1 bg-secondary-600 py-3 rounded-lg flex-row items-center justify-center"
            onPress={() => router.push('/wallet/add-funds')}
          >
            <Ionicons name="arrow-up" size={18} color="white" />
            <Text className="text-white font-semibold ml-2">Add Funds</Text>
          </TouchableOpacity>
        </View>
      </View>

      {/* Quick Stats */}
      <View className="flex-row px-4 mt-4 gap-3">
        <View className="flex-1 bg-white rounded-xl p-4 shadow-sm border border-gray-100">
          <Text className="text-sm text-gray-600 mb-1">This Month</Text>
          <Text className="text-2xl font-bold text-green-600">$0</Text>
          <Text className="text-xs text-gray-500 mt-1">Earned</Text>
        </View>
        
        <View className="flex-1 bg-white rounded-xl p-4 shadow-sm border border-gray-100">
          <Text className="text-sm text-gray-600 mb-1">This Month</Text>
          <Text className="text-2xl font-bold text-red-600">$0</Text>
          <Text className="text-xs text-gray-500 mt-1">Spent</Text>
        </View>
      </View>

      {/* Transactions */}
      <View className="px-4 mt-6">
        <View className="flex-row justify-between items-center mb-3">
          <Text className="text-lg font-bold text-gray-900">Recent Transactions</Text>
          <TouchableOpacity>
            <Text className="text-primary-600 font-semibold">See All</Text>
          </TouchableOpacity>
        </View>
        
        {isLoading ? (
          <View className="py-12">
            <ActivityIndicator size="large" color="#3A5C50" />
          </View>
        ) : transactions.length === 0 ? (
          <View className="bg-white rounded-xl p-8 items-center">
            <Ionicons name="receipt-outline" size={48} color="#ddd" />
            <Text className="text-gray-600 mt-4">No transactions yet</Text>
            <Text className="text-gray-500 text-sm mt-2 text-center">
              Your transaction history will appear here
            </Text>
          </View>
        ) : (
          <View>
            {transactions.map((transaction, index) => (
              <TransactionItem key={index} {...transaction} />
            ))}
          </View>
        )}
      </View>

      {/* Payment Methods */}
      <View className="px-4 mt-6 mb-6">
        <Text className="text-lg font-bold text-gray-900 mb-3">Payment Methods</Text>
        
        <TouchableOpacity
          className="bg-white rounded-xl p-4 shadow-sm border border-gray-100 flex-row items-center"
          onPress={() => router.push('/wallet/payment-methods')}
        >
          <View className="w-12 h-12 rounded-full bg-primary-100 items-center justify-center">
            <Ionicons name="card" size={24} color="#3A5C50" />
          </View>
          <View className="flex-1 ml-4">
            <Text className="text-base font-semibold text-gray-900">Manage Cards</Text>
            <Text className="text-sm text-gray-600 mt-1">Add or remove payment methods</Text>
          </View>
          <Ionicons name="chevron-forward" size={20} color="#999" />
        </TouchableOpacity>
      </View>
    </ScrollView>
  );
}
