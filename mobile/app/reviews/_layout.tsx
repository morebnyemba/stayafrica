import { Stack } from 'expo-router';

export default function ReviewsLayout() {
  return (
    <Stack
      screenOptions={{
        headerShown: false,
        animationEnabled: true,
      }}
    >
      <Stack.Screen name="index" />
    </Stack>
  );
}
