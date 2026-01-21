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
      <Stack.Screen name="properties/index" />
      <Stack.Screen name="earnings/index" />
    </Stack>
  );
}
