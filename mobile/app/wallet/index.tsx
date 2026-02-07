import { View, Text, ScrollView, TouchableOpacity, ActivityIndicator, RefreshControl } from 'react-native';
import { useState, useCallback } from 'react';
import { useRouter, useFocusEffect } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { apiClient } from '@/services/api-client';
import { logError, logApiError } from '@/utils/logger';

interface WalletData {
  id: string;
  balance: string;
  currency: string;
  status: string;
}

interface Transaction {
  id: string;
  txn_type: 'credit' | 'debit' | 'refund' | 'adjustment';
  amount: string;
  currency: string;
  status: 'pending' | 'completed' | 'failed';
  reference: string;
  created_at: string;
  description?: string;
}

export default function WalletDashboardScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [loading, setLoading] = useState(true);
  const [refreshing, setRefreshing] = useState(false);
  const [wallet, setWallet] = useState<WalletData | null>(null);
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [activeTab, setActiveTab] = useState<'transactions' | 'withdrawals'>('transactions');
  const [error, setError] = useState<string | null>(null);

  useFocusEffect(
    useCallback(() => {
      fetchWalletData();
    }, [])
  );

  const fetchWalletData = async () => {
    try {
      setLoading(true);
      setError(null);
      
      // Fetch wallet balance
      const walletResponse = await apiClient.get('/wallets/my_wallet/');
      setWallet(walletResponse.data);

      // Fetch recent transactions
      const transactionsResponse = await apiClient.get('/wallets/' + walletResponse.data.id + '/transactions/', {
        params: { page_size: 20, ordering: '-created_at' }
      });
      setTransactions(transactionsResponse.data.results || []);
    } catch (error: any) {
      logApiError('/wallets/my_wallet/', error, { action: 'fetch wallet data' });
      const errorMessage = error?.response?.data?.detail || 
                          error?.response?.data?.message || 
                          'Failed to load wallet data. Please try again.';
      setError(errorMessage);
    } finally {
      setLoading(false);
      setRefreshing(false);
    }
  };

  const onRefresh = () => {
    setRefreshing(true);
    fetchWalletData();
  };

  const getTransactionIcon = (type: string) => {
    switch (type) {
      case 'credit':
      case 'refund':
        return 'arrow-down-circle';
      case 'debit':
        return 'arrow-up-circle';
      case 'adjustment':
        return 'swap-horizontal';
      default:
        return 'help-circle';
    }
  };

  const getTransactionColor = (type: string) => {
    switch (type) {
      case 'credit':
      case 'refund':
        return '#10B981';
      case 'debit':
        return '#EF4444';
      case 'adjustment':
        return '#F59E0B';
      default:
        return '#6B7280';
    }
  };

  if (loading) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <View className="flex-1 items-center justify-center">
          <ActivityIndicator size="large" color="#D9B168" />
          <Text className="text-moss font-semibold mt-4">Loading wallet...</Text>
        </View>
      </SafeAreaView>
    );
  }

  if (error && !wallet) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: insets.top + 12 }}
        >
          <View className="flex-row items-center">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center mr-3"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-xl font-bold text-white">My Wallet</Text>
          </View>
        </LinearGradient>
        
        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-red-50 rounded-2xl p-6 items-center">
            <Ionicons name="alert-circle" size={48} color="#EF4444" />
            <Text className="text-red-800 font-bold text-lg mt-4 text-center">
              Failed to Load Wallet
            </Text>
            <Text className="text-red-600 text-center mt-2">{error}</Text>
            <TouchableOpacity
              onPress={() => {
                setError(null);
                fetchWalletData();
              }}
              className="mt-6"
            >
              <LinearGradient
                colors={['#D9B168', '#bea04f']}
                className="px-8 py-4 rounded-2xl"
              >
                <Text className="text-forest font-bold">Try Again</Text>
              </LinearGradient>
            </TouchableOpacity>
          </View>
        </View>
      </SafeAreaView>
    );
  }

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      {/* Header with Balance */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2a4c3d']}
        className="px-4 pb-8"
        style={{ paddingTop: insets.top + 12 }}
      >
        <View className="flex-row items-center justify-between mb-6">
          <View className="flex-row items-center flex-1">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center mr-3 -ml-2"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-2xl font-bold text-white">My Wallet</Text>
          </View>
        </View>

        {/* Balance Card */}
        <View className="bg-white/10 backdrop-blur rounded-3xl p-6 border border-white/20">
          <Text className="text-sand-200 text-sm font-semibold mb-2">AVAILABLE BALANCE</Text>
          <Text className="text-5xl font-black text-white mb-4">
            {wallet?.currency || 'USD'} {parseFloat(wallet?.balance || '0').toFixed(2)}
          </Text>
          
          <View className="flex-row gap-3 mt-4">
            <TouchableOpacity
              onPress={() => router.push('/wallet/add-funds')}
              className="flex-1 bg-gold rounded-xl py-3 items-center"
            >
              <View className="flex-row items-center">
                <Ionicons name="add-circle" size={20} color="#122F26" />
                <Text className="text-forest font-bold ml-2">Add Funds</Text>
              </View>
            </TouchableOpacity>
            
            <TouchableOpacity
              onPress={() => router.push('/wallet/withdraw')}
              className="flex-1 bg-white/20 backdrop-blur rounded-xl py-3 items-center border border-white/30"
            >
              <View className="flex-row items-center">
                <Ionicons name="arrow-up-circle" size={20} color="#fff" />
                <Text className="text-white font-bold ml-2">Withdraw</Text>
              </View>
            </TouchableOpacity>
          </View>
        </View>
      </LinearGradient>

      {/* Quick Actions */}
      <View className="px-4 mt-4 mb-4">
        <View className="flex-row gap-3">
          <TouchableOpacity
            onPress={() => router.push('/wallet/payment-methods')}
            className="flex-1 bg-white rounded-2xl p-4 items-center"
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}
          >
            <View className="w-12 h-12 bg-gold/20 rounded-full items-center justify-center mb-2">
              <Ionicons name="card" size={24} color="#D9B168" />
            </View>
            <Text className="text-forest font-semibold text-xs">Payment Methods</Text>
          </TouchableOpacity>

          <TouchableOpacity
            onPress={() => router.push('/wallet/bank-accounts')}
            className="flex-1 bg-white rounded-2xl p-4 items-center"
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}
          >
            <View className="w-12 h-12 bg-gold/20 rounded-full items-center justify-center mb-2">
              <Ionicons name="business" size={24} color="#D9B168" />
            </View>
            <Text className="text-forest font-semibold text-xs">Bank Accounts</Text>
          </TouchableOpacity>

          <TouchableOpacity
            onPress={() => router.push('/(tabs)/payments')}
            className="flex-1 bg-white rounded-2xl p-4 items-center"
            style={{
              shadowColor: '#122F26',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.05,
              shadowRadius: 4,
              elevation: 2,
            }}
          >
            <View className="w-12 h-12 bg-gold/20 rounded-full items-center justify-center mb-2">
              <Ionicons name="receipt" size={24} color="#D9B168" />
            </View>
            <Text className="text-forest font-semibold text-xs">Payment History</Text>
          </TouchableOpacity>
        </View>
      </View>

      {/* Tabs */}
      <View className="px-4 flex-row border-b border-sand-200 mb-4">
        <TouchableOpacity
          onPress={() => setActiveTab('transactions')}
          className={`flex-1 pb-3 items-center ${
            activeTab === 'transactions' ? 'border-b-2 border-gold' : ''
          }`}
        >
          <Text
            className={`font-semibold ${
              activeTab === 'transactions' ? 'text-gold' : 'text-moss'
            }`}
          >
            Transactions
          </Text>
        </TouchableOpacity>
        <TouchableOpacity
          onPress={() => setActiveTab('withdrawals')}
          className={`flex-1 pb-3 items-center ${
            activeTab === 'withdrawals' ? 'border-b-2 border-gold' : ''
          }`}
        >
          <Text
            className={`font-semibold ${
              activeTab === 'withdrawals' ? 'text-gold' : 'text-moss'
            }`}
          >
            Withdrawals
          </Text>
        </TouchableOpacity>
      </View>

      {/* Transaction List */}
      <ScrollView
        className="flex-1 px-4"
        showsVerticalScrollIndicator={false}
        refreshControl={
          <RefreshControl refreshing={refreshing} onRefresh={onRefresh} colors={['#D9B168']} />
        }
      >
        {activeTab === 'transactions' && (
          <>
            {transactions.length > 0 ? (
              transactions.map((txn) => (
                <View
                  key={txn.id}
                  className="bg-white rounded-2xl p-4 mb-3 flex-row items-center"
                  style={{
                    shadowColor: '#122F26',
                    shadowOffset: { width: 0, height: 2 },
                    shadowOpacity: 0.05,
                    shadowRadius: 4,
                    elevation: 2,
                  }}
                >
                  <View
                    className="w-12 h-12 rounded-full items-center justify-center mr-3"
                    style={{ backgroundColor: `${getTransactionColor(txn.txn_type)}20` }}
                  >
                    <Ionicons
                      name={getTransactionIcon(txn.txn_type)}
                      size={24}
                      color={getTransactionColor(txn.txn_type)}
                    />
                  </View>

                  <View className="flex-1">
                    <Text className="text-forest font-semibold capitalize">{txn.txn_type}</Text>
                    <Text className="text-moss text-xs mt-1">{txn.reference}</Text>
                    <Text className="text-moss text-xs">
                      {new Date(txn.created_at).toLocaleDateString()} {new Date(txn.created_at).toLocaleTimeString()}
                    </Text>
                  </View>

                  <View className="items-end">
                    <Text
                      className="text-lg font-bold"
                      style={{ color: getTransactionColor(txn.txn_type) }}
                    >
                      {txn.txn_type === 'credit' || txn.txn_type === 'refund' ? '+' : '-'}
                      {txn.currency} {parseFloat(txn.amount).toFixed(2)}
                    </Text>
                    <View className="flex-row items-center mt-1">
                      {txn.status === 'completed' ? (
                        <Ionicons name="checkmark-circle" size={14} color="#10B981" />
                      ) : txn.status === 'pending' ? (
                        <Ionicons name="time" size={14} color="#F59E0B" />
                      ) : (
                        <Ionicons name="close-circle" size={14} color="#EF4444" />
                      )}
                      <Text className="text-xs text-moss ml-1 capitalize">{txn.status}</Text>
                    </View>
                  </View>
                </View>
              ))
            ) : (
              <View className="items-center py-16">
                <View className="bg-sand-200 rounded-full p-6 mb-4">
                  <Ionicons name="receipt-outline" size={48} color="#94a3b8" />
                </View>
                <Text className="text-xl font-bold text-forest mb-2">No Transactions</Text>
                <Text className="text-moss text-center px-8">
                  Your transaction history will appear here
                </Text>
              </View>
            )}
          </>
        )}

        {activeTab === 'withdrawals' && (
          <View className="items-center py-16">
            <View className="bg-sand-200 rounded-full p-6 mb-4">
              <Ionicons name="cash-outline" size={48} color="#94a3b8" />
            </View>
            <Text className="text-xl font-bold text-forest mb-2">No Withdrawals</Text>
            <Text className="text-moss text-center px-8 mb-6">
              Withdraw funds to your bank account
            </Text>
            <TouchableOpacity
              onPress={() => router.push('/wallet/withdraw')}
              className="bg-gold rounded-xl px-6 py-3"
            >
              <Text className="text-forest font-bold">Request Withdrawal</Text>
            </TouchableOpacity>
          </View>
        )}

        <View className="h-8" />
      </ScrollView>
    </SafeAreaView>
  );
}
