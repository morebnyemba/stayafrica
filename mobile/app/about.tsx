import { View, Text, ScrollView, TouchableOpacity } from 'react-native';
import { LinearGradient } from 'expo-linear-gradient';
import { Ionicons } from '@expo/vector-icons';
import { useRouter } from 'expo-router';
import { useSafeAreaInsets } from 'react-native-safe-area-context';

export default function AboutScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();

  const ValueCard = ({ icon, title, description }: any) => (
    <View
      className="bg-white rounded-2xl p-6 mb-4"
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.08,
        shadowRadius: 8,
        elevation: 4,
      }}
    >
      <View className="items-center mb-4">
        <View className="bg-sand-200 rounded-full p-4 mb-3">
          <Ionicons name={icon} size={32} color="#3A5C50" />
        </View>
        <Text className="text-lg font-bold text-forest text-center mb-2">
          {title}
        </Text>
        <Text className="text-moss text-center leading-6">
          {description}
        </Text>
      </View>
    </View>
  );

  const StatCard = ({ value, label }: any) => (
    <View className="flex-1 items-center">
      <Text className="text-4xl font-black text-gold mb-1">{value}</Text>
      <Text className="text-sm text-sand-200 text-center">{label}</Text>
    </View>
  );

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
      {/* Hero Section */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="pb-8"
        style={{ paddingTop: insets.top + 12 }}
      >
        {/* Header */}
        <View className="flex-row items-center justify-between px-4 mb-6">
          <TouchableOpacity
            onPress={() => router.back()}
            className="w-10 h-10 rounded-xl items-center justify-center"
            style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
          >
            <Ionicons name="arrow-back" size={24} color="#fff" />
          </TouchableOpacity>
          <Text className="text-white font-bold text-lg">About StayAfrica</Text>
          <View className="w-10" />
        </View>

        {/* Title and Description */}
        <View className="px-6">
          <Text className="text-4xl font-black text-white mb-4 text-center">
            About StayAfrica
          </Text>
          <Text className="text-sand-200 text-base text-center leading-6">
            Connecting travelers with authentic African experiences through unique accommodations and local hospitality.
          </Text>
        </View>

        {/* Stats */}
        <View className="flex-row px-6 mt-8">
          <StatCard value="500+" label="Properties" />
          <StatCard value="15" label="Countries" />
          <StatCard value="10K+" label="Happy Guests" />
          <StatCard value="4.8★" label="Avg Rating" />
        </View>
      </LinearGradient>

      {/* Mission Section */}
      <View className="px-4 mt-6">
        <View
          className="bg-white rounded-3xl p-6"
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 4 },
            shadowOpacity: 0.08,
            shadowRadius: 12,
            elevation: 6,
          }}
        >
          <Text className="text-2xl font-black text-forest mb-4">
            Our Mission
          </Text>
          <Text className="text-moss text-base leading-7 mb-4">
            StayAfrica is dedicated to making travel across Africa accessible, authentic, and unforgettable. 
            We believe in the power of hospitality to bridge cultures and create lasting memories.
          </Text>
          <Text className="text-moss text-base leading-7">
            Whether you're seeking a luxury safari lodge, a coastal villa, or a city apartment, 
            we connect you with verified hosts who are passionate about sharing their corner of Africa with the world.
          </Text>
        </View>
      </View>

      {/* Values Section */}
      <View className="px-4 mt-6">
        <Text className="text-2xl font-black text-forest mb-4 px-2">
          Our Values
        </Text>

        <ValueCard
          icon="globe-outline"
          title="Authentic Experiences"
          description="We showcase the real Africa through genuine local connections and unique stays."
        />

        <ValueCard
          icon="shield-checkmark-outline"
          title="Trust & Safety"
          description="All properties are verified, and secure payment processing ensures peace of mind."
        />

        <ValueCard
          icon="heart-outline"
          title="Community First"
          description="We support local economies by empowering hosts and promoting sustainable tourism."
        />

        <ValueCard
          icon="trophy-outline"
          title="Excellence"
          description="We maintain high standards for all listings and provide exceptional customer service."
        />
      </View>

      {/* CTA Section */}
      <View className="px-4 mt-6 mb-8">
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="rounded-3xl p-8"
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          style={{
            shadowColor: '#122F26',
            shadowOffset: { width: 0, height: 8 },
            shadowOpacity: 0.2,
            shadowRadius: 16,
            elevation: 8,
          }}
        >
          <Text className="text-2xl font-black text-white mb-3 text-center">
            Ready to Start Your African Adventure?
          </Text>
          <Text className="text-sand-200 text-base text-center mb-6 leading-6">
            Join thousands of travelers who have discovered the magic of Africa through StayAfrica.
          </Text>
          
          <TouchableOpacity
            onPress={() => router.push('/(tabs)/explore')}
            className="mb-3"
          >
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="py-4 rounded-2xl"
              style={{
                shadowColor: '#D9B168',
                shadowOffset: { width: 0, height: 4 },
                shadowOpacity: 0.3,
                shadowRadius: 8,
                elevation: 5,
              }}
            >
              <Text className="text-forest font-bold text-base text-center">
                Explore Properties
              </Text>
            </LinearGradient>
          </TouchableOpacity>

          <TouchableOpacity
            onPress={() => router.push('/(tabs)/host')}
            className="border-2 border-sand-100 py-4 rounded-2xl"
          >
            <Text className="text-sand-100 font-bold text-base text-center">
              Become a Host
            </Text>
          </TouchableOpacity>
        </LinearGradient>
      </View>
    </ScrollView>
  );
}
