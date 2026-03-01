import { Tabs } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { Platform } from 'react-native';
import { useSafeAreaInsets } from 'react-native-safe-area-context';
import { AnimatedCompassIcon } from '@/components/common/AnimatedCompassIcon';

export default function TabsLayout() {
  const insets = useSafeAreaInsets();
  
  // Calculate proper bottom padding to stay above navigation controls
  const bottomPadding = Platform.OS === 'ios' 
    ? Math.max(insets.bottom, 20) 
    : Math.max(insets.bottom, 12);

  return (
    <Tabs
      screenOptions={({ route }) => ({
        headerShown: false,
        tabBarIcon: ({ color, focused }) => {
          let iconName: any;
          let size = focused ? 26 : 22;

          // Use different icons for focused/unfocused states
          if (route.name === 'dashboard') {
            iconName = focused ? 'home' : 'home-outline';
            return <Ionicons name={iconName} size={size} color={color} />;
          } else if (route.name === 'explore') {
            size = focused ? 34 : 30; // Reduce size to avoid clipping
            return <AnimatedCompassIcon size={size} color={color} filled={focused} />;
          } else if (route.name === 'profile') {
            iconName = focused ? 'person-circle' : 'person-circle-outline';
            return <Ionicons name={iconName} size={size} color={color} />;
          }

          return <Ionicons name="help-outline" size={size} color={color} />;
        },
        tabBarActiveTintColor: '#D9B168', // Safari Gold
        tabBarInactiveTintColor: '#94a3b8',
        tabBarStyle: {
          backgroundColor: '#122F26', // Deep Forest
          borderTopWidth: 0,
          elevation: 0,
          height: 90 + bottomPadding,
          paddingBottom: bottomPadding,
          paddingTop: 12,
          shadowColor: '#000',
          shadowOffset: { width: 0, height: -4 },
          shadowOpacity: 0.3,
          shadowRadius: 12,
        },
        tabBarLabelStyle: {
          fontSize: 10,
          fontWeight: '600',
          marginTop: 2,
        },
        tabBarItemStyle: {
          paddingVertical: 4,
          alignItems: 'center',
          justifyContent: 'center',
          overflow: 'visible',
        },
      })}
    >
      {/* Hidden index route for redirect */}
      <Tabs.Screen
        name="index"
        options={{
          href: null, // Hide from tab bar
        }}
      />
      <Tabs.Screen
        name="dashboard"
        options={{
          title: 'Home',
          tabBarLabel: 'Home',
        }}
      />
      <Tabs.Screen
        name="explore"
        options={{
          title: 'Explore',
          tabBarLabel: 'Explore',
        }}
      />
      <Tabs.Screen
        name="profile"
        options={{
          title: 'Profile',
          tabBarLabel: 'Profile',
        }}
      />
      {/* Hidden tabs - accessible via navigation but not in bottom bar */}
      <Tabs.Screen
        name="bookings"
        options={{
          href: null,
          title: 'Bookings',
        }}
      />
      <Tabs.Screen
        name="wishlist"
        options={{
          href: null,
          title: 'Wishlist',
        }}
      />
      <Tabs.Screen
        name="messages"
        options={{
          href: null,
          title: 'Messages',
        }}
      />
      <Tabs.Screen
        name="host"
        options={{
          href: null,
          title: 'Host',
        }}
      />
      <Tabs.Screen
        name="wallet"
        options={{
          href: null,
          title: 'Wallet',
        }}
      />
      <Tabs.Screen
        name="payments"
        options={{
          href: null,
          title: 'Payments',
        }}
      />
      <Tabs.Screen
        name="about"
        options={{
          href: null,
          title: 'About',
        }}
      />
    </Tabs>
  );
}
