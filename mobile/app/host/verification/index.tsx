import { View, Text, ScrollView, TouchableOpacity, Platform } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

export default function HostVerificationScreen() {
  const router = useRouter();
  const { user, isAuthenticated } = useAuth();

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pb-6"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
        >
          <TouchableOpacity onPress={() => router.back()} className="mb-4">
            <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
              <Ionicons name="arrow-back" size={24} color="#fff" />
            </View>
          </TouchableOpacity>
          <Text className="text-3xl font-black text-white tracking-tight">
            Verification
          </Text>
        </LinearGradient>
        <View className="flex-1 items-center justify-center px-6">
          <Text className="text-forest text-lg">Please sign in to continue</Text>
        </View>
      </View>
    );
  }

  const VerificationStep = ({ icon, title, description, status, onPress }: any) => (
    <TouchableOpacity
      onPress={onPress}
      className="bg-white rounded-2xl p-4 mb-3 flex-row items-center"
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}
    >
      <View className={`w-12 h-12 rounded-full items-center justify-center ${
        status === 'verified' ? 'bg-green-100' : status === 'pending' ? 'bg-yellow-100' : 'bg-sand-200'
      }`}>
        <Ionicons 
          name={icon} 
          size={24} 
          color={status === 'verified' ? '#10B981' : status === 'pending' ? '#F59E0B' : '#3A5C50'} 
        />
      </View>
      <View className="flex-1 ml-4">
        <Text className="text-base font-semibold text-forest">{title}</Text>
        <Text className="text-sm text-moss mt-1">{description}</Text>
      </View>
      {status === 'verified' ? (
        <View className="bg-green-100 rounded-full p-1">
          <Ionicons name="checkmark" size={20} color="#10B981" />
        </View>
      ) : status === 'pending' ? (
        <View className="bg-yellow-100 rounded-full px-3 py-1">
          <Text className="text-yellow-700 text-xs font-semibold">Pending</Text>
        </View>
      ) : (
        <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
      )}
    </TouchableOpacity>
  );

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
      {/* Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pb-8"
        style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
      >
        <TouchableOpacity onPress={() => router.back()} className="mb-4">
          <View className="w-10 h-10 rounded-xl items-center justify-center" style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}>
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </View>
        </TouchableOpacity>
        <Text className="text-3xl font-black text-white tracking-tight mb-2">
          Identity Verification
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="shield-checkmark" size={16} color="#D9B168" />
          <Text className="text-sand-100 ml-2">
            Verify your identity to build trust
          </Text>
        </View>
      </LinearGradient>

      {/* Status Card */}
      <View className="px-4 -mt-4">
        <LinearGradient
          colors={user?.is_verified ? ['#10B981', '#059669'] : ['#F59E0B', '#D97706']}
          className="rounded-2xl p-6 mb-4"
          style={{
            shadowColor: user?.is_verified ? '#10B981' : '#F59E0B',
            shadowOffset: { width: 0, height: 8 },
            shadowOpacity: 0.25,
            shadowRadius: 12,
            elevation: 8,
          }}
        >
          <View className="flex-row items-center">
            <View className="bg-white/20 rounded-full p-3">
              <Ionicons 
                name={user?.is_verified ? "shield-checkmark" : "alert-circle"} 
                size={32} 
                color="#fff" 
              />
            </View>
            <View className="flex-1 ml-4">
              <Text className="text-white text-lg font-bold">
                {user?.is_verified ? 'Verified Account' : 'Verification Required'}
              </Text>
              <Text className="text-white/80 text-sm mt-1">
                {user?.is_verified 
                  ? 'Your identity has been verified' 
                  : 'Complete verification to list properties'}
              </Text>
            </View>
          </View>
        </LinearGradient>

        {/* Verification Steps */}
        <Text className="text-lg font-bold text-forest mb-3 mt-4">Verification Steps</Text>

        <VerificationStep
          icon="mail-outline"
          title="Email Verification"
          description="Verify your email address"
          status={user?.is_verified ? 'verified' : 'not_started'}
          onPress={() => {}}
        />

        <VerificationStep
          icon="call-outline"
          title="Phone Verification"
          description="Verify your phone number"
          status="not_started"
          onPress={() => {}}
        />

        <VerificationStep
          icon="card-outline"
          title="ID Verification"
          description="Upload a valid government ID"
          status="not_started"
          onPress={() => {}}
        />

        <VerificationStep
          icon="camera-outline"
          title="Selfie Verification"
          description="Take a selfie for facial verification"
          status="not_started"
          onPress={() => {}}
        />

        {/* Benefits Section */}
        <Text className="text-lg font-bold text-forest mb-3 mt-6">Benefits of Verification</Text>

        <View className="bg-white rounded-2xl p-4 mb-8" style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}>
          <View className="flex-row items-center mb-3">
            <View className="bg-green-100 rounded-full p-2 mr-3">
              <Ionicons name="checkmark" size={16} color="#10B981" />
            </View>
            <Text className="text-forest flex-1">Increased trust from guests</Text>
          </View>
          <View className="flex-row items-center mb-3">
            <View className="bg-green-100 rounded-full p-2 mr-3">
              <Ionicons name="checkmark" size={16} color="#10B981" />
            </View>
            <Text className="text-forest flex-1">Higher booking rates</Text>
          </View>
          <View className="flex-row items-center mb-3">
            <View className="bg-green-100 rounded-full p-2 mr-3">
              <Ionicons name="checkmark" size={16} color="#10B981" />
            </View>
            <Text className="text-forest flex-1">Access to premium features</Text>
          </View>
          <View className="flex-row items-center">
            <View className="bg-green-100 rounded-full p-2 mr-3">
              <Ionicons name="checkmark" size={16} color="#10B981" />
            </View>
            <Text className="text-forest flex-1">Priority customer support</Text>
          </View>
        </View>
      </View>
    </ScrollView>
  );
}
