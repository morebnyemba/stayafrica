import { Stack } from 'expo-router';

export default function ReviewsLayout() {
  return (
    <Stack
      screenOptions={{
        headerShown: false,
        animation: 'fade',
      }}
    >
      <Stack.Screen name="index" />
      <Stack.Screen name="my-reviews" />
    </Stack>
  );
}

ReviewsLayout.displayName = 'ReviewsLayout';
