import { Stack } from 'expo-router';

export default function HostLayout() {
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
    </Stack>
  );
}
