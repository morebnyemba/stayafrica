import { Text, View, ScrollView, TextInput, TouchableOpacity } from 'react-native';
import { useAuth } from '@/context/auth-context';
import { useState } from 'react';
import { useRouter } from 'expo-router';

export default function LoginScreen() {
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const [loading, setLoading] = useState(false);
  const { login } = useAuth();
  const router = useRouter();

  const handleLogin = async () => {
    try {
      setLoading(true);
      await login(email, password);
      router.replace('/(tabs)/explore');
    } catch (error) {
      console.error('Login failed:', error);
    } finally {
      setLoading(false);
    }
  };

  return (
    <ScrollView className="flex-1 bg-white">
      <View className="flex-1 justify-center px-6 py-12">
        <Text className="text-4xl font-bold text-primary-600 mb-8 text-center">
          StayAfrica
        </Text>

        <TextInput
          className="w-full px-4 py-3 border border-gray-300 rounded-lg mb-4"
          placeholder="Email"
          placeholderTextColor="#999"
          value={email}
          onChangeText={setEmail}
          editable={!loading}
          keyboardType="email-address"
        />

        <TextInput
          className="w-full px-4 py-3 border border-gray-300 rounded-lg mb-6"
          placeholder="Password"
          placeholderTextColor="#999"
          value={password}
          onChangeText={setPassword}
          secureTextEntry
          editable={!loading}
        />

        <TouchableOpacity
          className="w-full bg-primary-600 py-3 rounded-lg mb-4 disabled:opacity-50"
          onPress={handleLogin}
          disabled={loading}
        >
          <Text className="text-white font-semibold text-center">
            {loading ? 'Logging in...' : 'Login'}
          </Text>
        </TouchableOpacity>

        <TouchableOpacity onPress={() => router.push('/(auth)/register')}>
          <Text className="text-center text-gray-600">
            Don't have an account? <Text className="text-primary-600 font-semibold">Sign Up</Text>
          </Text>
        </TouchableOpacity>
      </View>
    </ScrollView>
  );
}
