import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator } from 'react-native';
import { useRouter } from 'expo-router';
import { useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';
import { Skeleton } from '@/components/common/Skeletons';
import type { Transaction } from '@/types';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { Sidebar } from '@/components/common/Sidebar';

interface TransactionItemProps {
  icon: keyof typeof Ionicons.glyphMap;
  title: string;
  amount: number;
  date: string;
  type: 'credit' | 'debit';
}

export default function WalletScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const { isAuthenticated } = useAuth();
  const [balance, setBalance] = useState(0);
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [isLoading, setIsLoading] = useState(false);
  const [sidebarVisible, setSidebarVisible] = useState(false);

  if (!isAuthenticated) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        {/* Sidebar */}
        <Sidebar
          isVisible={sidebarVisible}
          onClose={() => setSidebarVisible(false)}
        />
        
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          {/* Top Navigation Bar with Menu */}
          <View className="flex-row items-center justify-between mb-4">
            {/* Hamburger Menu */}
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
          </View>
          
          <Text className="text-3xl font-black text-white tracking-tight">
            My Wallet
          </Text>
          <Text className="text-sand-200 text-sm mt-1">
            Manage your finances
          </Text>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="wallet-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Wallet Access Required</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Sign in to view your wallet and manage transactions
            </Text>
            <TouchableOpacity
              onPress={() => router.push('/(auth)/login')}
            >
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                className="px-8 py-4 rounded-2xl"
                style={{
                  shadowColor: '#D9B168',
                  shadowOffset: { width: 0, height: 4 },
                  shadowOpacity: 0.3,
                  shadowRadius: 8,
                  elevation: 5,
                }}
              >
                <Text className="text-forest font-bold text-base">Sign In Now</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
      </SafeAreaView>
    );
  }

  const TransactionItem = ({ icon, title, amount, date, type }: TransactionItemProps) => (
    <View
      className="p-4 bg-white rounded-2xl mb-3 flex-row items-center"
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}
    >
      <LinearGradient
        colors={type === 'credit' ? ['#10B981', '#059669'] : ['#EF4444', '#DC2626']}
        className="w-12 h-12 rounded-full items-center justify-center"
      >
        <Ionicons name={icon} size={24} color="white" />
      </LinearGradient>
      <View className="flex-1 ml-4">
        <Text className="text-base font-semibold text-forest">{title}</Text>
        <Text className="text-sm text-moss mt-1">{date}</Text>
      </View>
      <Text className={`text-lg font-bold ${
        type === 'credit' ? 'text-green-600' : 'text-red-600'
      }`}>
        {type === 'credit' ? '+' : '-'}${amount}
      </Text>
    </View>
  );

  const TransactionSkeleton = () => (
    <View className="bg-white rounded-2xl p-4 bg-white rounded-2xl mb-3">
      <View className="flex-row items-center">
        <Skeleton height={48} width={48} borderRadius={24} className="mr-4" />
        <View className="flex-1">
          <Skeleton height={16} width="60%" className="mb-2" />
          <Skeleton height={14} width="40%" />
        </View>
        <Skeleton height={20} width={60} />
      </View>
    </View>
  );

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      {/* Sidebar */}
      <Sidebar
        isVisible={sidebarVisible}
        onClose={() => setSidebarVisible(false)}
      />
      
      <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
        {/* Modern Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f', '#2d4a40']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="px-4 pb-8"
          style={{ paddingTop: insets.top + 12 }}
        >
          {/* Top Navigation Bar with Menu */}
          <View className="flex-row items-center justify-between mb-4">
            {/* Hamburger Menu */}
            <TouchableOpacity
              onPress={() => setSidebarVisible(true)}
              className="w-10 h-10 rounded-xl items-center justify-center"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="menu" size={24} color="#fff" />
            </TouchableOpacity>
          </View>
          
          <Text className="text-3xl font-black text-white tracking-tight mb-2">
            My Wallet
          </Text>
          <View className="flex-row items-center">
            <Ionicons name="wallet" size={16} color="#D9B168" />
            <Text className="text-sand-100 ml-2">
              Track your earnings and expenses
            </Text>
          </View>
        </LinearGradient>

      {/* Balance Card */}
      <View className="mx-4 -mt-6 mb-4">
        <LinearGradient
          colors={['#D9B168', '#bea04f']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="rounded-3xl p-6"
          style={{
            shadowColor: '#D9B168',
            shadowOffset: { width: 0, height: 8 },
            shadowOpacity: 0.3,
            shadowRadius: 16,
            elevation: 8,
          }}
        >
          <Text className="text-sm text-forest/80 mb-2 font-semibold">Available Balance</Text>
          <Text className="text-5xl font-black text-forest mb-6">${balance.toFixed(2)}</Text>
          
          <View className="flex-row gap-3">
            <View
              borderRadius={12}
              className="flex-1"
            >
              <TouchableOpacity
                className="py-3 flex-row items-center justify-center"
                onPress={() => router.push('/wallet/withdraw')}
              >
                <Ionicons name="arrow-down" size={18} color="#fff" />
                <Text className="text-white font-semibold ml-2">Withdraw</Text>
              </TouchableOpacity>
            </View>
            
            <View
              borderRadius={12}
              className="flex-1"
            >
              <TouchableOpacity
                className="py-3 flex-row items-center justify-center"
                onPress={() => router.push('/wallet/add-funds')}
              >
                <Ionicons name="arrow-up" size={18} color="#122F26" />
                <Text className="text-forest font-semibold ml-2">Add Funds</Text>
              </TouchableOpacity>
            </View>
          </View>
        </LinearGradient>
      </View>

      {/* Quick Stats */}
      <View className="flex-row px-4 mt-4 gap-3">
        <View className="flex-1 bg-white rounded-2xl p-4 bg-white rounded-2xl" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}>
          <View className="flex-row items-center mb-2">
            <View className="bg-green-100 rounded-full p-2 mr-2">
              <Ionicons name="trending-up" size={16} color="#10B981" />
            </View>
            <Text className="text-xs text-moss font-semibold">This Month</Text>
          </View>
          <Text className="text-2xl font-bold text-green-600">$0</Text>
          <Text className="text-xs text-moss mt-1">Earned</Text>
        </View>
        
        <View className="flex-1 bg-white rounded-2xl p-4 bg-white rounded-2xl" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}>
          <View className="flex-row items-center mb-2">
            <View className="bg-red-100 rounded-full p-2 mr-2">
              <Ionicons name="trending-down" size={16} color="#EF4444" />
            </View>
            <Text className="text-xs text-moss font-semibold">This Month</Text>
          </View>
          <Text className="text-2xl font-bold text-red-600">$0</Text>
          <Text className="text-xs text-moss mt-1">Spent</Text>
        </View>
      </View>

      {/* Transactions */}
      <View className="px-4 mt-6">
        <View className="flex-row justify-between items-center mb-4">
          <Text className="text-lg font-bold text-forest">Recent Transactions</Text>
          <TouchableOpacity>
            <Text className="text-gold font-semibold">See All</Text>
          </TouchableOpacity>
        </View>
        
        {isLoading ? (
          <View>
            {[1, 2, 3, 4].map((i) => (
              <TransactionSkeleton key={i} />
            ))}
          </View>
        ) : transactions.length === 0 ? (
          <View className="bg-white rounded-2xl p-8 items-center" style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.08,
            shadowRadius: 8,
            elevation: 4,
          }}>
            <View className="bg-sand-200 rounded-full p-6 mb-4">
              <Ionicons name="receipt-outline" size={48} color="#3A5C50" />
            </View>
            <Text className="text-forest font-bold text-lg mb-2">No transactions yet</Text>
            <Text className="text-moss text-sm text-center">
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
        <Text className="text-lg font-bold text-forest mb-4">Payment Methods</Text>
        
        <TouchableOpacity
          className="bg-white rounded-2xl p-4 bg-white rounded-2xl flex-row items-center"
          onPress={() => router.push('/wallet/payment-methods')}
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 2 },
            shadowOpacity: 0.05,
            shadowRadius: 4,
            elevation: 2,
          }}
        >
          <View className="w-12 h-12 rounded-full bg-sand-200 items-center justify-center">
            <Ionicons name="card" size={24} color="#3A5C50" />
          </View>
          <View className="flex-1 ml-4">
            <Text className="text-base font-semibold text-forest">Manage Cards</Text>
            <Text className="text-sm text-moss mt-1">Add or remove payment methods</Text>
          </View>
          <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
        </TouchableOpacity>
      </View>
      </ScrollView>
    </SafeAreaView>
  );
}
