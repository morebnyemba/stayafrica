import { Tabs } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';

export default function TabsLayout() {
  return (
    <Tabs
      screenOptions={({ route }) => ({
        headerShown: false,
        tabBarIcon: ({ color, size }) => {
          let iconName: any;

          if (route.name === 'explore') {
            iconName = 'search';
          } else if (route.name === 'bookings') {
            iconName = 'calendar';
          } else if (route.name === 'wishlist') {
            iconName = 'heart';
          } else if (route.name === 'host') {
            iconName = 'home';
          } else if (route.name === 'wallet') {
            iconName = 'wallet';
          } else if (route.name === 'profile') {
            iconName = 'person';
          }

          return <Ionicons name={iconName} size={size} color={color} />;
        },
        tabBarActiveTintColor: '#3A5C50', // Moss Green
        tabBarInactiveTintColor: '#999',
      })}
    >
      <Tabs.Screen
        name="explore"
        options={{
          title: 'Explore',
          tabBarLabel: 'Explore',
        }}
      />
      <Tabs.Screen
        name="wishlist"
        options={{
          title: 'Wishlist',
          tabBarLabel: 'Wishlist',
        }}
      />
      <Tabs.Screen
        name="bookings"
        options={{
          title: 'Bookings',
          tabBarLabel: 'Bookings',
        }}
      />
      <Tabs.Screen
        name="host"
        options={{
          title: 'Host',
          tabBarLabel: 'Host',
        }}
      />
      <Tabs.Screen
        name="wallet"
        options={{
          title: 'Wallet',
          tabBarLabel: 'Wallet',
        }}
      />
      <Tabs.Screen
        name="profile"
        options={{
          title: 'Profile',
          tabBarLabel: 'Profile',
        }}
      />
    </Tabs>
  );
}
