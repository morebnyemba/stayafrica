import { View, Text, ScrollView, TouchableOpacity, Platform } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';

export default function BookingFailureScreen() {
  const router = useRouter();

  const handleTryAgain = () => {
    router.back();
  };

  const handleContactSupport = () => {
    // Navigate to messages or support
    router.push('/(tabs)/messages');
  };

  const handleBackToHome = () => {
    router.replace('/(tabs)/explore');
  };

  return (
    <ScrollView 
      className="flex-1 bg-sand-100" 
      showsVerticalScrollIndicator={false}
      contentContainerStyle={{ paddingBottom: 40 }}
    >
      <LinearGradient
        colors={['#EF4444', '#DC2626']}
        className="px-4 pb-12"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <Text className="text-3xl font-black text-white tracking-tight text-center mt-8">
          Payment Failed
        </Text>
        <Text className="text-white/80 text-center mt-2">
          We couldn't process your payment
        </Text>
      </LinearGradient>

      <View className="px-4 -mt-8">
        {/* Error Card */}
        <View className="bg-white rounded-2xl p-8 mb-4 items-center" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 8 },
          shadowOpacity: 0.1,
          shadowRadius: 16,
          elevation: 8,
        }}>
          <View className="bg-red-100 rounded-full p-6 mb-6">
            <Ionicons name="close-circle" size={80} color="#EF4444" />
          </View>

          <Text className="text-2xl font-bold text-forest mb-2 text-center">
            Payment Unsuccessful
          </Text>
          <Text className="text-moss text-center mb-6">
            Your payment could not be processed. Please try again or use a different payment method.
          </Text>

          {/* Common Reasons */}
          <View className="w-full bg-amber-50 rounded-xl p-4 mb-6 border border-amber-200">
            <Text className="font-semibold text-amber-900 mb-3">Common Reasons:</Text>
            <View className="space-y-2">
              <View className="flex-row items-start">
                <Text className="text-amber-800 mr-2">•</Text>
                <Text className="flex-1 text-sm text-amber-800">Insufficient funds in your account</Text>
              </View>
              <View className="flex-row items-start">
                <Text className="text-amber-800 mr-2">•</Text>
                <Text className="flex-1 text-sm text-amber-800">Incorrect payment information</Text>
              </View>
              <View className="flex-row items-start">
                <Text className="text-amber-800 mr-2">•</Text>
                <Text className="flex-1 text-sm text-amber-800">Card declined by bank</Text>
              </View>
              <View className="flex-row items-start">
                <Text className="text-amber-800 mr-2">•</Text>
                <Text className="flex-1 text-sm text-amber-800">Network connection issues</Text>
              </View>
            </View>
          </View>

          {/* Action Buttons */}
          <TouchableOpacity
            onPress={handleTryAgain}
            className="w-full rounded-2xl overflow-hidden mb-3"
          >
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="py-4 items-center"
              style={{
                shadowColor: '#D9B168',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.3,
                shadowRadius: 8,
                elevation: 5,
              }}
            >
              <View className="flex-row items-center">
                <Ionicons name="refresh" size={20} color="#122F26" />
                <Text className="text-forest font-bold text-base ml-2">Try Again</Text>
              </View>
            </LinearGradient>
          </TouchableOpacity>

          <TouchableOpacity
            onPress={handleContactSupport}
            className="w-full bg-sand-200 rounded-2xl py-4 items-center mb-3"
          >
            <View className="flex-row items-center">
              <Ionicons name="chatbubble-ellipses" size={20} color="#3A5C50" />
              <Text className="text-forest font-semibold ml-2">Contact Support</Text>
            </View>
          </TouchableOpacity>

          <TouchableOpacity
            onPress={handleBackToHome}
            className="w-full"
          >
            <View className="py-3 items-center">
              <Text className="text-moss font-semibold">Back to Home</Text>
            </View>
          </TouchableOpacity>
        </View>

        {/* Help Card */}
        <View className="bg-white rounded-2xl p-6" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 4 },
          shadowOpacity: 0.08,
          shadowRadius: 8,
          elevation: 4,
        }}>
          <Text className="text-lg font-bold text-forest mb-3">Need Help?</Text>
          <Text className="text-sm text-moss mb-4">
            If you continue to experience issues, please contact your bank or try a different payment method.
          </Text>
          <View className="flex-row items-center">
            <Ionicons name="help-circle" size={20} color="#D9B168" />
            <Text className="text-moss text-sm ml-2">
              Our support team is available 24/7
            </Text>
          </View>
        </View>
      </View>
    </ScrollView>
  );
}
