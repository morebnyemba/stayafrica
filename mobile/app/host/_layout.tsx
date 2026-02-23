import { Stack, Redirect } from 'expo-router';
import { useAuth } from '@/context/auth-context';

export default function HostLayout() {
  const { user, isAuthenticated, isLoading } = useAuth();

  if (isLoading) {
    return null;
  }

  if (!isAuthenticated) {
    return <Redirect href="/(auth)/login" />;
  }

  if (user?.role !== 'host') {
    return <Redirect href="/(tabs)/dashboard" />;
  }

  return (
    <Stack
      screenOptions={{
        headerShown: false,
        animation: 'fade',
      }}
    >
      <Stack.Screen name="bookings/index" />
      <Stack.Screen name="bookings/[id]" />
      <Stack.Screen name="properties/index" />
      <Stack.Screen name="properties/[id]" />
      <Stack.Screen name="properties/new" />
      <Stack.Screen name="earnings/index" />
      <Stack.Screen name="verification/index" />
      <Stack.Screen name="reviews/index" />
      <Stack.Screen name="settings/index" />
      <Stack.Screen name="pricing/index" />
      <Stack.Screen name="tax-reports/index" />
      <Stack.Screen name="experiences/index" />
      <Stack.Screen name="experiences/new" />
      <Stack.Screen name="experiences/bookings" />
      <Stack.Screen name="analytics/index" />
      <Stack.Screen name="calendar/index" />
      <Stack.Screen name="properties/[id]/calendar" />
    </Stack>
  );
}
