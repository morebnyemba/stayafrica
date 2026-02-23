import { Stack } from 'expo-router';

export default function BookingLayout() {
    return (
        <Stack
            screenOptions={{
                headerShown: false,
                animation: 'slide_from_right',
            }}
        >
            <Stack.Screen name="confirm" />
            <Stack.Screen name="payment" />
            <Stack.Screen name="success" />
            <Stack.Screen name="failure" />
        </Stack>
    );
}
