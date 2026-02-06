import { View, Text, ScrollView, TouchableOpacity, Platform, Alert, ActivityIndicator, Modal, TextInput } from 'react-native';
import { useState, useCallback } from 'react';
import { useRouter, useFocusEffect } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { apiClient } from '@/services/api-client';

interface BankAccount {
  id: string;
  bank_name: string;
  account_name: string;
  account_number: string;
  branch_code?: string;
  country?: string;
  is_primary: boolean;
  created_at: string;
}

export default function BankAccountsScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const [bankAccounts, setBankAccounts] = useState<BankAccount[]>([]);
  const [loading, setLoading] = useState(true);
  const [showModal, setShowModal] = useState(false);
  const [formData, setFormData] = useState({
    bank_name: '',
    account_name: '',
    account_number: '',
    branch_code: '',
    country: 'Zimbabwe',
  });

  useFocusEffect(
    useCallback(() => {
      fetchBankAccounts();
    }, [])
  );

  const fetchBankAccounts = async () => {
    try {
      setLoading(true);
      const response = await apiClient.get('/payments/bank-accounts/');
      setBankAccounts(response.data.results || response.data || []);
    } catch (error) {
      console.error('Error fetching bank accounts:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleSetPrimary = async (id: string) => {
    try {
      await apiClient.post(`/payments/bank-accounts/${id}/set_primary/`);
      setBankAccounts(accounts =>
        accounts.map(a => ({ ...a, is_primary: a.id === id }))
      );
      Alert.alert('Success', 'Primary bank account updated');
    } catch (error) {
      console.error('Error setting primary:', error);
      Alert.alert('Error', 'Failed to update primary bank account');
    }
  };

  const handleDelete = (id: string) => {
    Alert.alert(
      'Delete Bank Account',
      'Are you sure you want to delete this bank account?',
      [
        { text: 'Cancel', style: 'cancel' },
        {
          text: 'Delete',
          style: 'destructive',
          onPress: async () => {
            try {
              await apiClient.delete(`/payments/bank-accounts/${id}/`);
              setBankAccounts(accounts => accounts.filter(a => a.id !== id));
              Alert.alert('Success', 'Bank account deleted');
            } catch (error) {
              console.error('Error deleting:', error);
              Alert.alert('Error', 'Failed to delete bank account');
            }
          }
        },
      ]
    );
  };

  const handleAddAccount = async () => {
    if (!formData.bank_name || !formData.account_name || !formData.account_number) {
      Alert.alert('Error', 'Please fill in all required fields');
      return;
    }

    try {
      const response = await apiClient.post('/payments/bank-accounts/', {
        ...formData,
        is_primary: bankAccounts.length === 0, // Auto-set as primary if first account
      });
      
      setBankAccounts([...bankAccounts, response.data]);
      setShowModal(false);
      setFormData({
        bank_name: '',
        account_name: '',
        account_number: '',
        branch_code: '',
        country: 'Zimbabwe',
      });
      Alert.alert('Success', 'Bank account added successfully!');
    } catch (error: any) {
      console.error('Error adding account:', error);
      Alert.alert('Error', error.response?.data?.error || 'Failed to add bank account');
    }
  };

  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f']}
        className="px-4 pb-6"
        style={{ paddingTop: insets.top + 12 }}
      >
        <View className="flex-row items-center justify-between">
          <View className="flex-row items-center flex-1">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center mr-3 -ml-2"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </TouchableOpacity>
            <View>
              <Text className="text-xl font-bold text-white">Bank Accounts</Text>
              <Text className="text-xs text-sand-300 mt-1">
                {bankAccounts.length} account{bankAccounts.length !== 1 ? 's' : ''}
              </Text>
            </View>
          </View>
          <TouchableOpacity
            onPress={() => setShowModal(true)}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="add" size={24} color="#fff" />
          </TouchableOpacity>
        </View>
      </LinearGradient>

      {loading ? (
        <View className="flex-1 items-center justify-center">
          <ActivityIndicator size="large" color="#D9B168" />
        </View>
      ) : (
        <ScrollView className="flex-1" showsVerticalScrollIndicator={false}>
          <View className="px-4 py-6">
            {bankAccounts.length > 0 ? (
              <>
                {bankAccounts.map((account) => (
                  <View
                    key={account.id}
                    className="bg-white rounded-2xl p-4 mb-3"
                    style={{
                      shadowColor: '#122F26',
                      shadowOffset: { width: 0, height: 2 },
                      shadowOpacity: 0.05,
                      shadowRadius: 4,
                      elevation: 2,
                    }}
                  >
                    <View className="flex-row items-center mb-3">
                      <View className="w-12 h-12 bg-gold/20 rounded-xl items-center justify-center mr-3">
                        <Ionicons name="business" size={24} color="#D9B168" />
                      </View>
                      <View className="flex-1">
                        <Text className="text-base font-semibold text-forest">{account.bank_name}</Text>
                        <Text className="text-sm text-moss">{account.account_name}</Text>
                        <Text className="text-sm text-moss">•••• {account.account_number.slice(-4)}</Text>
                      </View>
                      {account.is_primary && (
                        <View className="bg-gold/20 px-3 py-1 rounded-full">
                          <Text className="text-gold text-xs font-semibold">Primary</Text>
                        </View>
                      )}
                    </View>
                    <View className="flex-row pt-3 border-t border-sand-100">
                      {!account.is_primary && (
                        <TouchableOpacity
                          className="flex-1 items-center py-2"
                          onPress={() => handleSetPrimary(account.id)}
                        >
                          <Text className="text-gold font-semibold text-sm">Set as Primary</Text>
                        </TouchableOpacity>
                      )}
                      <TouchableOpacity
                        className={`flex-1 items-center py-2 ${!account.is_primary ? 'border-l border-sand-100' : ''}`}
                        onPress={() => handleDelete(account.id)}
                      >
                        <Text className="text-red-500 font-semibold text-sm">Remove</Text>
                      </TouchableOpacity>
                    </View>
                  </View>
                ))}
              </>
            ) : (
              <View className="items-center py-16">
                <View className="bg-sand-200 rounded-full p-6 mb-4">
                  <Ionicons name="business-outline" size={48} color="#94a3b8" />
                </View>
                <Text className="text-xl font-bold text-forest mb-2">No Bank Accounts</Text>
                <Text className="text-moss text-center mb-8 px-8 leading-5">
                  Add a bank account to receive payouts from your bookings
                </Text>
                <TouchableOpacity
                  onPress={() => setShowModal(true)}
                  className="bg-gold rounded-xl px-6 py-3"
                >
                  <Text className="text-forest font-bold">Add Bank Account</Text>
                </TouchableOpacity>
              </View>
            )}
          </View>
        </ScrollView>
      )}

      {/* Add Bank Account Modal */}
      <Modal
        visible={showModal}
        animationType="slide"
        transparent={true}
        onRequestClose={() => setShowModal(false)}
      >
        <View className="flex-1 bg-black/50 justify-end">
          <View className="bg-white rounded-t-3xl pt-6 pb-8" style={{ paddingBottom: insets.bottom + 32 }}>
            <View className="px-6 pb-4 border-b border-sand-100 flex-row items-center justify-between">
              <Text className="text-2xl font-bold text-forest">Add Bank Account</Text>
              <TouchableOpacity onPress={() => setShowModal(false)}>
                <Ionicons name="close" size={28} color="#3A5C50" />
              </TouchableOpacity>
            </View>

            <ScrollView
              className="px-6 pt-6"
              showsVerticalScrollIndicator={false}
              keyboardShouldPersistTaps="handled"
            >
              <View className="mb-4">
                <Text className="text-forest font-semibold mb-2">Bank Name *</Text>
                <TextInput
                  className="bg-sand-50 rounded-xl px-4 py-3 text-forest"
                  placeholder="e.g., CBZ Bank"
                  value={formData.bank_name}
                  onChangeText={(text) => setFormData({ ...formData, bank_name: text })}
                />
              </View>

              <View className="mb-4">
                <Text className="text-forest font-semibold mb-2">Account Holder Name *</Text>
                <TextInput
                  className="bg-sand-50 rounded-xl px-4 py-3 text-forest"
                  placeholder="e.g., John Doe"
                  value={formData.account_name}
                  onChangeText={(text) => setFormData({ ...formData, account_name: text })}
                />
              </View>

              <View className="mb-4">
                <Text className="text-forest font-semibold mb-2">Account Number *</Text>
                <TextInput
                  className="bg-sand-50 rounded-xl px-4 py-3 text-forest"
                  placeholder="e.g., 1234567890"
                  value={formData.account_number}
                  onChangeText={(text) => setFormData({ ...formData, account_number: text })}
                  keyboardType="numeric"
                />
              </View>

              <View className="mb-4">
                <Text className="text-forest font-semibold mb-2">Branch Code (Optional)</Text>
                <TextInput
                  className="bg-sand-50 rounded-xl px-4 py-3 text-forest"
                  placeholder="e.g., 0101"
                  value={formData.branch_code}
                  onChangeText={(text) => setFormData({ ...formData, branch_code: text })}
                />
              </View>

              <View className="mb-6">
                <Text className="text-forest font-semibold mb-2">Country</Text>
                <TextInput
                  className="bg-sand-50 rounded-xl px-4 py-3 text-forest"
                  placeholder="e.g., Zimbabwe"
                  value={formData.country}
                  onChangeText={(text) => setFormData({ ...formData, country: text })}
                />
              </View>

              <TouchableOpacity
                onPress={handleAddAccount}
                className="bg-gold rounded-xl py-4 items-center mb-4"
              >
                <Text className="text-forest font-bold text-lg">Add Account</Text>
              </TouchableOpacity>
            </ScrollView>
          </View>
        </View>
      </Modal>
    </SafeAreaView>
  );
}
