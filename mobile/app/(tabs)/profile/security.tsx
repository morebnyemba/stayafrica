import { useState, useEffect, useCallback } from 'react';
import {
  View,
  Text,
  ScrollView,
  TouchableOpacity,
  Alert,
  ActivityIndicator,
  Image,
  TextInput,
  Share,
} from 'react-native';
import { SafeAreaView } from 'react-native-safe-area-context';
import { Ionicons } from '@expo/vector-icons';
import { useRouter } from 'expo-router';
import * as Clipboard from 'expo-clipboard';
import { apiClient } from '@/services/api-client';

type ScreenState = 'loading' | 'status' | 'setup' | 'verify' | 'backup-codes' | 'disable';

interface TwoFAStatus {
  two_factor_enabled: boolean;
  has_backup_codes: boolean;
  backup_codes_remaining: number;
}

interface SetupData {
  secret: string;
  qr_code: string;
  backup_codes: string[];
}

export default function SecurityScreen() {
  const router = useRouter();

  // State
  const [screen, setScreen] = useState<ScreenState>('loading');
  const [status, setStatus] = useState<TwoFAStatus | null>(null);
  const [setupData, setSetupData] = useState<SetupData | null>(null);
  const [backupCodes, setBackupCodes] = useState<string[]>([]);
  const [verifyToken, setVerifyToken] = useState('');
  const [password, setPassword] = useState('');
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState('');

  // Load 2FA status on mount
  const loadStatus = useCallback(async () => {
    try {
      setScreen('loading');
      const data = await apiClient.get2FAStatus();
      setStatus(data);
      setScreen('status');
    } catch (err: any) {
      setError('Failed to load security settings');
      setScreen('status');
    }
  }, []);

  useEffect(() => {
    loadStatus();
  }, [loadStatus]);

  // Start 2FA setup
  const handleStartSetup = async () => {
    try {
      setBusy(true);
      setError('');
      const data = await apiClient.setup2FA();
      setSetupData(data);
      setBackupCodes(data.backup_codes);
      setScreen('setup');
    } catch (err: any) {
      setError(err?.response?.data?.error || 'Failed to start 2FA setup');
    } finally {
      setBusy(false);
    }
  };

  // Verify and enable 2FA
  const handleEnable = async () => {
    if (verifyToken.length !== 6) {
      setError('Please enter a 6-digit code');
      return;
    }
    try {
      setBusy(true);
      setError('');
      await apiClient.enable2FA(verifyToken);
      setVerifyToken('');
      setScreen('backup-codes');
    } catch (err: any) {
      setError(err?.response?.data?.error || 'Invalid verification code');
    } finally {
      setBusy(false);
    }
  };

  // Disable 2FA
  const handleDisable = async () => {
    if (!password) {
      setError('Please enter your password');
      return;
    }
    try {
      setBusy(true);
      setError('');
      await apiClient.disable2FA(password);
      setPassword('');
      Alert.alert('Success', '2FA has been disabled');
      loadStatus();
    } catch (err: any) {
      setError(err?.response?.data?.error || 'Invalid password');
    } finally {
      setBusy(false);
    }
  };

  // Regenerate backup codes
  const handleRegenerateCodes = async () => {
    if (!password) {
      setError('Please enter your password');
      return;
    }
    try {
      setBusy(true);
      setError('');
      const data = await apiClient.regenerateBackupCodes(password);
      setBackupCodes(data.backup_codes);
      setPassword('');
      setScreen('backup-codes');
    } catch (err: any) {
      setError(err?.response?.data?.error || 'Failed to regenerate codes');
    } finally {
      setBusy(false);
    }
  };

  // Copy backup codes
  const handleCopyBackupCodes = async () => {
    const text = backupCodes.join('\n');
    await Clipboard.setStringAsync(text);
    Alert.alert('Copied', 'Backup codes copied to clipboard');
  };

  // Share backup codes
  const handleShareBackupCodes = async () => {
    const text = `StayAfrica 2FA Backup Codes:\n\n${backupCodes.join('\n')}\n\nKeep these codes safe. Each code can only be used once.`;
    try {
      await Share.share({ message: text });
    } catch {}
  };

  // Confirm disable
  const confirmDisable = () => {
    Alert.alert(
      'Disable 2FA',
      'This will make your account less secure. Are you sure?',
      [
        { text: 'Cancel', style: 'cancel' },
        { text: 'Disable', style: 'destructive', onPress: () => setScreen('disable') },
      ]
    );
  };

  // Header
  const renderHeader = () => (
    <View className="flex-row items-center px-4 py-3">
      <TouchableOpacity
        onPress={() => {
          if (screen === 'setup' || screen === 'verify' || screen === 'disable') {
            setScreen('status');
            setError('');
            setVerifyToken('');
            setPassword('');
          } else if (screen === 'backup-codes') {
            loadStatus();
          } else {
            router.back();
          }
        }}
        className="w-10 h-10 items-center justify-center rounded-full bg-white"
        style={{ elevation: 2 }}
      >
        <Ionicons name="arrow-back" size={22} color="#1B4332" />
      </TouchableOpacity>
      <Text className="text-xl font-bold text-forest ml-3">Security</Text>
    </View>
  );

  // Loading
  if (screen === 'loading') {
    return (
      <SafeAreaView className="flex-1 bg-sage-50">
        {renderHeader()}
        <View className="flex-1 items-center justify-center">
          <ActivityIndicator size="large" color="#D4A574" />
          <Text className="mt-4 text-gray-500">Loading security settings…</Text>
        </View>
      </SafeAreaView>
    );
  }

  // Status overview
  if (screen === 'status') {
    return (
      <SafeAreaView className="flex-1 bg-sage-50">
        {renderHeader()}
        <ScrollView className="flex-1 px-4" showsVerticalScrollIndicator={false}>
          {error ? (
            <View className="bg-red-50 rounded-xl p-3 mb-4">
              <Text className="text-red-600 text-sm">{error}</Text>
            </View>
          ) : null}

          {/* 2FA Status Card */}
          <View
            className="bg-white rounded-2xl p-5 mb-4"
            style={{ elevation: 3, shadowColor: '#000', shadowOpacity: 0.08, shadowRadius: 8, shadowOffset: { width: 0, height: 2 } }}
          >
            <View className="flex-row items-center mb-4">
              <View className={`w-12 h-12 rounded-full items-center justify-center ${status?.two_factor_enabled ? 'bg-green-100' : 'bg-orange-100'}`}>
                <Ionicons
                  name={status?.two_factor_enabled ? 'shield-checkmark' : 'shield-outline'}
                  size={26}
                  color={status?.two_factor_enabled ? '#16A34A' : '#F97316'}
                />
              </View>
              <View className="ml-4 flex-1">
                <Text className="text-lg font-bold text-forest">Two-Factor Authentication</Text>
                <Text className={`text-sm font-medium ${status?.two_factor_enabled ? 'text-green-600' : 'text-orange-500'}`}>
                  {status?.two_factor_enabled ? 'Enabled' : 'Disabled'}
                </Text>
              </View>
            </View>

            <Text className="text-gray-500 text-sm leading-5 mb-5">
              {status?.two_factor_enabled
                ? 'Your account is protected with two-factor authentication. You\'ll need your authenticator app to sign in.'
                : 'Add an extra layer of security to your account by requiring a verification code from your authenticator app when signing in.'}
            </Text>

            {status?.two_factor_enabled ? (
              <View>
                {/* Backup codes info */}
                <View className="flex-row items-center bg-blue-50 rounded-xl p-3 mb-3">
                  <Ionicons name="key-outline" size={20} color="#3B82F6" />
                  <Text className="text-blue-700 text-sm ml-2 flex-1">
                    {status.backup_codes_remaining} backup {status.backup_codes_remaining === 1 ? 'code' : 'codes'} remaining
                  </Text>
                </View>

                {/* Actions */}
                <TouchableOpacity
                  className="bg-blue-500 rounded-xl py-3 items-center mb-3"
                  onPress={() => {
                    setError('');
                    setPassword('');
                    setScreen('disable'); // reuse disable screen for password entry, then regenerate
                    // Actually let's make a separate flow
                  }}
                >
                  <Text className="text-white font-semibold">Regenerate Backup Codes</Text>
                </TouchableOpacity>

                <TouchableOpacity
                  className="bg-red-50 rounded-xl py-3 items-center"
                  onPress={confirmDisable}
                >
                  <Text className="text-red-600 font-semibold">Disable 2FA</Text>
                </TouchableOpacity>
              </View>
            ) : (
              <TouchableOpacity
                className="bg-forest rounded-xl py-3 items-center"
                onPress={handleStartSetup}
                disabled={busy}
              >
                {busy ? (
                  <ActivityIndicator color="white" />
                ) : (
                  <Text className="text-white font-semibold">Enable 2FA</Text>
                )}
              </TouchableOpacity>
            )}
          </View>

          {/* Security tips */}
          <View
            className="bg-white rounded-2xl p-5 mb-8"
            style={{ elevation: 2, shadowColor: '#000', shadowOpacity: 0.05, shadowRadius: 6, shadowOffset: { width: 0, height: 1 } }}
          >
            <Text className="text-base font-bold text-forest mb-3">Security Tips</Text>
            {[
              { icon: 'phone-portrait-outline', text: 'Use an authenticator app like Google Authenticator or Authy' },
              { icon: 'save-outline', text: 'Save your backup codes in a safe place' },
              { icon: 'key-outline', text: 'Never share your verification codes with anyone' },
              { icon: 'refresh-outline', text: 'Regenerate backup codes periodically' },
            ].map((tip, i) => (
              <View key={i} className="flex-row items-start mb-3">
                <Ionicons name={tip.icon as any} size={18} color="#6B7280" className="mt-0.5" />
                <Text className="text-gray-600 text-sm ml-3 flex-1">{tip.text}</Text>
              </View>
            ))}
          </View>
        </ScrollView>
      </SafeAreaView>
    );
  }

  // Setup screen — show QR code
  if (screen === 'setup') {
    return (
      <SafeAreaView className="flex-1 bg-sage-50">
        {renderHeader()}
        <ScrollView className="flex-1 px-4" showsVerticalScrollIndicator={false}>
          {error ? (
            <View className="bg-red-50 rounded-xl p-3 mb-4">
              <Text className="text-red-600 text-sm">{error}</Text>
            </View>
          ) : null}

          <View
            className="bg-white rounded-2xl p-5 mb-4"
            style={{ elevation: 3, shadowColor: '#000', shadowOpacity: 0.08, shadowRadius: 8, shadowOffset: { width: 0, height: 2 } }}
          >
            <Text className="text-lg font-bold text-forest mb-2">Step 1: Scan QR Code</Text>
            <Text className="text-gray-500 text-sm mb-4">
              Open your authenticator app and scan this QR code to add your account.
            </Text>

            {/* QR Code */}
            {setupData?.qr_code ? (
              <View className="items-center bg-white p-4 rounded-xl mb-4">
                <Image
                  source={{ uri: `data:image/png;base64,${setupData.qr_code}` }}
                  style={{ width: 200, height: 200 }}
                  resizeMode="contain"
                />
              </View>
            ) : null}

            {/* Manual entry */}
            <View className="bg-gray-50 rounded-xl p-3 mb-4">
              <Text className="text-xs text-gray-400 mb-1">Manual entry key:</Text>
              <TouchableOpacity
                onPress={async () => {
                  if (setupData?.secret) {
                    await Clipboard.setStringAsync(setupData.secret);
                    Alert.alert('Copied', 'Secret key copied to clipboard');
                  }
                }}
              >
                <Text className="text-forest font-mono text-sm" selectable>
                  {setupData?.secret}
                </Text>
              </TouchableOpacity>
            </View>

            <Text className="text-lg font-bold text-forest mb-2">Step 2: Enter Verification Code</Text>
            <Text className="text-gray-500 text-sm mb-4">
              Enter the 6-digit code from your authenticator app to verify setup.
            </Text>

            <TextInput
              className="bg-gray-50 border border-gray-200 rounded-xl px-4 py-3 text-center text-2xl font-bold text-forest tracking-widest"
              placeholder="000000"
              placeholderTextColor="#9CA3AF"
              value={verifyToken}
              onChangeText={(text) => setVerifyToken(text.replace(/\D/g, '').slice(0, 6))}
              keyboardType="number-pad"
              maxLength={6}
              autoFocus
            />

            <TouchableOpacity
              className="bg-forest rounded-xl py-3 items-center mt-4"
              onPress={handleEnable}
              disabled={busy || verifyToken.length !== 6}
              style={{ opacity: busy || verifyToken.length !== 6 ? 0.5 : 1 }}
            >
              {busy ? (
                <ActivityIndicator color="white" />
              ) : (
                <Text className="text-white font-semibold">Verify & Enable 2FA</Text>
              )}
            </TouchableOpacity>
          </View>
        </ScrollView>
      </SafeAreaView>
    );
  }

  // Backup codes screen
  if (screen === 'backup-codes') {
    return (
      <SafeAreaView className="flex-1 bg-sage-50">
        {renderHeader()}
        <ScrollView className="flex-1 px-4" showsVerticalScrollIndicator={false}>
          <View
            className="bg-white rounded-2xl p-5 mb-4"
            style={{ elevation: 3, shadowColor: '#000', shadowOpacity: 0.08, shadowRadius: 8, shadowOffset: { width: 0, height: 2 } }}
          >
            <View className="flex-row items-center mb-4">
              <View className="w-10 h-10 rounded-full bg-green-100 items-center justify-center">
                <Ionicons name="checkmark-circle" size={24} color="#16A34A" />
              </View>
              <Text className="text-lg font-bold text-forest ml-3">2FA Enabled!</Text>
            </View>

            <Text className="text-gray-500 text-sm mb-4">
              Save these backup codes in a safe place. Each code can only be used once to sign in if you lose access to your authenticator app.
            </Text>

            {/* Backup codes grid */}
            <View className="bg-gray-50 rounded-xl p-4 mb-4">
              <View className="flex-row flex-wrap">
                {backupCodes.map((code, i) => (
                  <View key={i} className="w-1/2 py-1.5 px-2">
                    <Text className="text-forest font-mono text-sm text-center bg-white rounded-lg py-2" selectable>
                      {code}
                    </Text>
                  </View>
                ))}
              </View>
            </View>

            {/* Action buttons */}
            <View className="flex-row gap-3 mb-2">
              <TouchableOpacity
                className="flex-1 bg-forest rounded-xl py-3 flex-row items-center justify-center"
                onPress={handleCopyBackupCodes}
              >
                <Ionicons name="copy-outline" size={18} color="white" />
                <Text className="text-white font-semibold ml-2">Copy</Text>
              </TouchableOpacity>

              <TouchableOpacity
                className="flex-1 bg-blue-500 rounded-xl py-3 flex-row items-center justify-center"
                onPress={handleShareBackupCodes}
              >
                <Ionicons name="share-outline" size={18} color="white" />
                <Text className="text-white font-semibold ml-2">Share</Text>
              </TouchableOpacity>
            </View>

            <TouchableOpacity
              className="bg-gray-100 rounded-xl py-3 items-center mt-2"
              onPress={loadStatus}
            >
              <Text className="text-forest font-semibold">Done</Text>
            </TouchableOpacity>
          </View>
        </ScrollView>
      </SafeAreaView>
    );
  }

  // Disable / Regenerate screen (password entry)
  if (screen === 'disable') {
    return (
      <SafeAreaView className="flex-1 bg-sage-50">
        {renderHeader()}
        <ScrollView className="flex-1 px-4" showsVerticalScrollIndicator={false}>
          {error ? (
            <View className="bg-red-50 rounded-xl p-3 mb-4">
              <Text className="text-red-600 text-sm">{error}</Text>
            </View>
          ) : null}

          <View
            className="bg-white rounded-2xl p-5 mb-4"
            style={{ elevation: 3, shadowColor: '#000', shadowOpacity: 0.08, shadowRadius: 8, shadowOffset: { width: 0, height: 2 } }}
          >
            <View className="flex-row items-center mb-4">
              <View className="w-10 h-10 rounded-full bg-red-100 items-center justify-center">
                <Ionicons name="warning" size={24} color="#EF4444" />
              </View>
              <Text className="text-lg font-bold text-forest ml-3">Confirm with Password</Text>
            </View>

            <Text className="text-gray-500 text-sm mb-4">
              Enter your account password to continue.
            </Text>

            <TextInput
              className="bg-gray-50 border border-gray-200 rounded-xl px-4 py-3 text-forest"
              placeholder="Your password"
              placeholderTextColor="#9CA3AF"
              secureTextEntry
              value={password}
              onChangeText={setPassword}
              autoFocus
            />

            <View className="flex-row gap-3 mt-4">
              <TouchableOpacity
                className="flex-1 bg-gray-100 rounded-xl py-3 items-center"
                onPress={() => {
                  setScreen('status');
                  setPassword('');
                  setError('');
                }}
              >
                <Text className="text-forest font-semibold">Cancel</Text>
              </TouchableOpacity>

              <TouchableOpacity
                className="flex-1 bg-red-500 rounded-xl py-3 items-center"
                onPress={handleDisable}
                disabled={busy || !password}
                style={{ opacity: busy || !password ? 0.5 : 1 }}
              >
                {busy ? (
                  <ActivityIndicator color="white" />
                ) : (
                  <Text className="text-white font-semibold">Disable 2FA</Text>
                )}
              </TouchableOpacity>
            </View>

            {/* Also offer regenerate option */}
            <View className="mt-4 pt-4 border-t border-gray-100">
              <Text className="text-gray-400 text-xs text-center mb-2">Or regenerate backup codes instead</Text>
              <TouchableOpacity
                className="bg-blue-50 rounded-xl py-3 items-center"
                onPress={handleRegenerateCodes}
                disabled={busy || !password}
                style={{ opacity: busy || !password ? 0.5 : 1 }}
              >
                {busy ? (
                  <ActivityIndicator color="#3B82F6" />
                ) : (
                  <Text className="text-blue-600 font-semibold">Regenerate Backup Codes</Text>
                )}
              </TouchableOpacity>
            </View>
          </View>
        </ScrollView>
      </SafeAreaView>
    );
  }

  return null;
}
