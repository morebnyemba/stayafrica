import { View, Text, ScrollView, TouchableOpacity } from 'react-native';
import { useRouter } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { useAuth } from '@/context/auth-context';

interface StatCardProps {
  icon: keyof typeof Ionicons.glyphMap;
  label: string;
  value: string;
  color: string;
}

interface MenuItemProps {
  icon: keyof typeof Ionicons.glyphMap;
  title: string;
  description: string;
  onPress: () => void;
  color?: string;
}

export default function HostScreen() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();

  if (!isAuthenticated) {
    return (
      <View className="flex-1 bg-sand-100">
        {/* Header */}
        <LinearGradient
          colors={['#122F26', '#1d392f']}
          className="px-4 pt-12 pb-6"
        >
          <Text className="text-3xl font-black text-white tracking-tight">
            Become a Host
          </Text>
          <Text className="text-sand-200 text-sm mt-1">
            Share your property and earn
          </Text>
        </LinearGradient>

        <View className="flex-1 items-center justify-center px-6">
          <View className="bg-white rounded-3xl p-8 items-center" style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 8 }, shadowOpacity: 0.1, shadowRadius: 16, elevation: 8 }}>
            <View className="bg-sand-200 rounded-full p-8 mb-6">
              <Ionicons name="home-outline" size={72} color="#D9B168" />
            </View>
            <Text className="text-2xl font-bold text-forest mb-3">Become a Host</Text>
            <Text className="text-moss text-center mb-8 px-4 leading-6">
              Sign in to start hosting your properties and earn income from your spaces
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
      </View>
    );
  }

  const StatCard = ({ icon, label, value, color }: StatCardProps) => (
    <View className="flex-1 bg-white rounded-2xl p-4 m-2" style={{
      shadowColor: '#122F26',
      shadowOffset: { width: 0, height: 4 },
      shadowOpacity: 0.08,
      shadowRadius: 8,
      elevation: 4,
    }}>
      <LinearGradient
        colors={[`${color}20`, `${color}10`]}
        className="w-12 h-12 rounded-full items-center justify-center mb-3"
      >
        <Ionicons name={icon} size={24} color={color} />
      </LinearGradient>
      <Text className="text-2xl font-bold text-forest">{value}</Text>
      <Text className="text-sm text-moss mt-1">{label}</Text>
    </View>
  );

  const MenuItem = ({ icon, title, description, onPress, color = '#3A5C50' }: MenuItemProps) => (
    <TouchableOpacity
      className="bg-white rounded-2xl p-4 mb-3 flex-row items-center"
      onPress={onPress}
      style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.05,
        shadowRadius: 4,
        elevation: 2,
      }}
    >
      <LinearGradient
        colors={[`${color}20`, `${color}10`]}
        className="w-12 h-12 rounded-full items-center justify-center"
      >
        <Ionicons name={icon} size={24} color={color} />
      </LinearGradient>
      <View className="flex-1 ml-4">
        <Text className="text-base font-semibold text-forest">{title}</Text>
        <Text className="text-sm text-moss mt-1">{description}</Text>
      </View>
      <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
    </TouchableOpacity>
  );

  return (
    <ScrollView className="flex-1 bg-sand-100" showsVerticalScrollIndicator={false}>
      {/* Modern Header */}
      <LinearGradient
        colors={['#122F26', '#1d392f', '#2d4a40']}
        start={{ x: 0, y: 0 }}
        end={{ x: 1, y: 1 }}
        className="px-4 pt-12 pb-8"
      >
        <Text className="text-3xl font-black text-white tracking-tight mb-2">
          Host Dashboard
        </Text>
        <View className="flex-row items-center">
          <Ionicons name="home" size={16} color="#D9B168" />
          <Text className="text-sand-100 ml-2">
            Manage your properties and bookings
          </Text>
        </View>
      </LinearGradient>

      {/* Stats Cards */}
      <View className="flex-row px-2 -mt-6">
        <StatCard
          icon="home"
          label="Properties"
          value="0"
          color="#3A5C50"
        />
        <StatCard
          icon="calendar"
          label="Bookings"
          value="0"
          color="#D9B168"
        />
      </View>

      <View className="flex-row px-2">
        <StatCard
          icon="cash"
          label="Earnings"
          value="$0"
          color="#10B981"
        />
        <StatCard
          icon="star"
          label="Rating"
          value="N/A"
          color="#F59E0B"
        />
      </View>

      {/* Menu Items */}
      <View className="px-4 mt-4">
        <Text className="text-lg font-bold text-gray-900 mb-3">Quick Actions</Text>
        
        <MenuItem
          icon="add-circle"
          title="List New Property"
          description="Add a new property to your listings"
          onPress={() => router.push('/host/properties/new')}
          color="#3A5C50"
        />
        
        <MenuItem
          icon="home"
          title="My Properties"
          description="View and manage your properties"
          onPress={() => router.push('/host/properties')}
          color="#D9B168"
        />
        
        <MenuItem
          icon="calendar"
          title="Bookings"
          description="View property bookings and reservations"
          onPress={() => router.push('/host/bookings')}
          color="#6366F1"
        />
        
        <MenuItem
          icon="cash"
          title="Earnings & Payouts"
          description="Track your income and payments"
          onPress={() => router.push('/host/earnings')}
          color="#10B981"
        />
        
        <MenuItem
          icon="star"
          title="Reviews"
          description="View guest reviews and ratings"
          onPress={() => router.push('/host/reviews')}
          color="#F59E0B"
        />
        
        <MenuItem
          icon="settings"
          title="Host Settings"
          description="Manage your hosting preferences"
          onPress={() => router.push('/host/settings')}
          color="#6B7280"
        />
      </View>

      {/* Tips Section */}
      <View className="mx-4 mt-4 mb-6 rounded-2xl overflow-hidden" style={{
        shadowColor: '#3B82F6',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.15,
        shadowRadius: 8,
        elevation: 4,
      }}>
        <LinearGradient
          colors={['#DBEAFE', '#BFDBFE']}
          className="p-4"
        >
          <View className="flex-row items-center mb-2">
            <Ionicons name="bulb" size={20} color="#3B82F6" />
            <Text className="text-base font-semibold text-blue-900 ml-2">Hosting Tip</Text>
          </View>
          <Text className="text-sm text-blue-800">
            High-quality photos and detailed descriptions help attract more guests. 
            Complete your property profile to increase bookings!
          </Text>
        </LinearGradient>
      </View>
    </ScrollView>
  );
}
