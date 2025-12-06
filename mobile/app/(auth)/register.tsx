import { Text, View, ScrollView, TextInput, TouchableOpacity } from 'react-native';
import { useState } from 'react';
import { useRouter } from 'expo-router';
import { useAuth } from '@/context/auth-context';

export default function RegisterScreen() {
  const [formData, setFormData] = useState({
    email: '',
    password: '',
    firstName: '',
    lastName: '',
    phone: '',
    country: 'South Africa',
  });
  const [loading, setLoading] = useState(false);
  const { register } = useAuth();
  const router = useRouter();

  const handleRegister = async () => {
    try {
      setLoading(true);
      await register({
        email: formData.email,
        password: formData.password,
        first_name: formData.firstName,
        last_name: formData.lastName,
        phone_number: formData.phone,
        country_of_residence: formData.country,
      });
      router.replace('/(tabs)/explore');
    } catch (error) {
      console.error('Registration failed:', error);
    } finally {
      setLoading(false);
    }
  };

  return (
    <ScrollView className="flex-1 bg-white">
      <View className="flex-1 justify-center px-6 py-12">
        <Text className="text-3xl font-bold text-primary-600 mb-8 text-center">
          Create Account
        </Text>

        <TextInput
          className="w-full px-4 py-3 border border-gray-300 rounded-lg mb-4"
          placeholder="First Name"
          placeholderTextColor="#999"
          value={formData.firstName}
          onChangeText={(value) => setFormData({ ...formData, firstName: value })}
          editable={!loading}
        />

        <TextInput
          className="w-full px-4 py-3 border border-gray-300 rounded-lg mb-4"
          placeholder="Last Name"
          placeholderTextColor="#999"
          value={formData.lastName}
          onChangeText={(value) => setFormData({ ...formData, lastName: value })}
          editable={!loading}
        />

        <TextInput
          className="w-full px-4 py-3 border border-gray-300 rounded-lg mb-4"
          placeholder="Email"
          placeholderTextColor="#999"
          value={formData.email}
          onChangeText={(value) => setFormData({ ...formData, email: value })}
          keyboardType="email-address"
          editable={!loading}
        />

        <TextInput
          className="w-full px-4 py-3 border border-gray-300 rounded-lg mb-4"
          placeholder="Phone"
          placeholderTextColor="#999"
          value={formData.phone}
          onChangeText={(value) => setFormData({ ...formData, phone: value })}
          keyboardType="phone-pad"
          editable={!loading}
        />

        <TextInput
          className="w-full px-4 py-3 border border-gray-300 rounded-lg mb-6"
          placeholder="Password"
          placeholderTextColor="#999"
          value={formData.password}
          onChangeText={(value) => setFormData({ ...formData, password: value })}
          secureTextEntry
          editable={!loading}
        />

        <TouchableOpacity
          className="w-full bg-primary-600 py-3 rounded-lg mb-4"
          onPress={handleRegister}
          disabled={loading}
        >
          <Text className="text-white font-semibold text-center">
            {loading ? 'Creating Account...' : 'Sign Up'}
          </Text>
        </TouchableOpacity>

        <TouchableOpacity onPress={() => router.back()}>
          <Text className="text-center text-gray-600">
            Already have an account? <Text className="text-primary-600 font-semibold">Login</Text>
          </Text>
        </TouchableOpacity>
      </View>
    </ScrollView>
  );
}
