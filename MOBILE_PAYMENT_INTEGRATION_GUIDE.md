# Mobile Payment Processing - Integration Guide

## Overview

This document outlines how to integrate the payment wizard and payment processing screens into the complete booking flow on the mobile app.

---

## Booking Flow Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                     BOOKING CREATION FLOW                     │
├──────────────────────────────────────────────────────────────┤
│                                                                │
│  Step 1: Property Details                                     │
│  ├─ Select Property                                           │
│  ├─ View Details & Reviews                                    │
│  └─ Check Availability                                        │
│         ↓                                                       │
│  Step 2: Select Dates & Guests                                │
│  ├─ Pick Check-in Date                                        │
│  ├─ Pick Check-out Date                                       │
│  ├─ Select Number of Guests                                   │
│  └─ Validate Date Range                                       │
│         ↓                                                       │
│  Step 3: Review & Confirm                                     │
│  ├─ Show Booking Summary                                      │
│  ├─ Calculate Total Price (nights × rate + fees)             │
│  └─ Display Cancellation Policy                               │
│         ↓                                                       │
│  Step 4: PAYMENT PROCESSING ← NEW INTEGRATION                │
│  ├─ Check Payment Methods Available                           │
│  ├─ Show Payment Method Selection                             │
│  ├─ Allow Add New Method                                      │
│  └─ Process Payment                                           │
│         ↓                                                       │
│  Step 5: Success/Pending                                      │
│  ├─ Show Confirmation                                         │
│  ├─ Display Booking Details                                   │
│  └─ Offer Next Steps                                          │
│         ↓                                                       │
│  Step 6: Host Notification                                    │
│         ↓                                                       │
│  Booking Confirmed & Listed in Dashboard                      │
│                                                                │
└──────────────────────────────────────────────────────────────┘
```

---

## File Integration Points

### 1. Booking Creation Screen (explore/[id]/booking.tsx)

This is where the booking flow starts. Update this to include a new "Confirm & Pay" button:

```typescript
// app/(tabs)/explore/[id]/booking.tsx

import { useRouter } from 'expo-router';

export default function BookingScreen() {
  // ... existing code ...
  
  const handleConfirmBooking = async () => {
    setLoading(true);
    try {
      // Step 1: Create booking in draft state
      const response = await apiClient.post('/bookings/', {
        property_id: propertyId,
        check_in: checkInDate,
        check_out: checkOutDate,
        number_of_guests: numberOfGuests,
        total_price: calculatePrice(),
        status: 'pending', // Waiting for payment
      });
      
      const bookingId = response.data.id;
      
      // Step 2: Navigate to payment screen with booking details
      router.push({
        pathname: '/booking/[id]/payment',
        params: {
          bookingId,
          propertyName: property.name,
          checkIn: checkInDate,
          checkOut: checkOutDate,
          guests: numberOfGuests.toString(),
          total: calculatePrice().toString(),
        }
      });
    } catch (error) {
      Alert.alert('Error', 'Failed to create booking');
    } finally {
      setLoading(false);
    }
  };
  
  return (
    <View>
      {/* Booking Summary */}
      <View className="p-4">
        <TouchableOpacity
          onPress={handleConfirmBooking}
          disabled={loading}
        >
          <LinearGradient
            colors={['#D9B168', '#bea04f']}
            className="py-4 rounded-2xl"
          >
            <Text className="text-forest font-bold text-center">
              {loading ? 'Creating Booking...' : 'Confirm & Pay'}
            </Text>
          </LinearGradient>
        </TouchableOpacity>
      </View>
    </View>
  );
}
```

---

### 2. Booking Payment Screen (booking/[id]/payment.tsx) - Enhanced

The payment screen now handles:
- Fetching available payment methods
- Processing payment through the API
- Handling success/failure cases

**Key Features**:
- Lists saved payment methods
- Allows adding new methods on-the-fly
- Shows booking summary
- Displays security info
- Handles errors gracefully

---

### 3. Booking Success Screen (booking/[id]/success.tsx)

Create a new screen for successful bookings:

```typescript
// app/booking/[id]/success.tsx

import { View, Text, TouchableOpacity, ScrollView } from 'react-native';
import { useRouter, useLocalSearchParams } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { useEffect, useState } from 'react';
import { apiClient } from '@/services/api-client';

interface BookingDetails {
  id: string;
  booking_id: string;
  property_name: string;
  check_in: string;
  check_out: string;
  number_of_guests: number;
  total_price: string;
  status: string;
}

export default function BookingSuccessScreen() {
  const router = useRouter();
  const insets = useSafeAreaInsets();
  const params = useLocalSearchParams();
  const [booking, setBooking] = useState<BookingDetails | null>(null);
  const [loading, setLoading] = useState(true);
  
  const bookingId = params.id as string;
  
  useEffect(() => {
    fetchBookingDetails();
  }, [bookingId]);
  
  const fetchBookingDetails = async () => {
    try {
      const response = await apiClient.get(`/bookings/${bookingId}/`);
      setBooking(response.data);
    } catch (error) {
      console.error('Error fetching booking:', error);
    } finally {
      setLoading(false);
    }
  };
  
  if (loading) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100">
        <ActivityIndicator size="large" color="#D9B168" />
      </SafeAreaView>
    );
  }
  
  if (!booking) {
    return (
      <SafeAreaView className="flex-1 bg-sand-100 items-center justify-center">
        <Text className="text-forest font-bold">Booking not found</Text>
      </SafeAreaView>
    );
  }
  
  return (
    <SafeAreaView className="flex-1 bg-sand-100">
      <ScrollView className="flex-1">
        {/* Success Animation */}
        <View className="items-center py-12">
          <View className="w-20 h-20 bg-green-100 rounded-full items-center justify-center mb-4">
            <Ionicons name="checkmark" size={40} color="#10B981" />
          </View>
          <Text className="text-3xl font-bold text-forest">Booking Confirmed!</Text>
          <Text className="text-moss mt-2">Your reservation is confirmed</Text>
        </View>
        
        {/* Booking Number */}
        <View className="px-4 mb-6">
          <View className="bg-white rounded-2xl p-6">
            <Text className="text-moss text-sm font-semibold mb-2">BOOKING REFERENCE</Text>
            <Text className="text-2xl font-black text-forest">{booking.booking_id}</Text>
            <Text className="text-gold text-xs mt-3">Save this for your records</Text>
          </View>
        </View>
        
        {/* Booking Details */}
        <View className="px-4 mb-6">
          <Text className="text-lg font-bold text-forest mb-3">Booking Details</Text>
          <View className="bg-white rounded-2xl p-6">
            <DetailRow label="Property" value={booking.property_name} />
            <DetailRow label="Check-in" value={booking.check_in} />
            <DetailRow label="Check-out" value={booking.check_out} />
            <DetailRow label="Guests" value={`${booking.number_of_guests} Guest(s)`} />
            <View className="border-t border-sand-200 pt-4 mt-4">
              <View className="flex-row justify-between items-center">
                <Text className="text-moss font-semibold">Total Paid</Text>
                <Text className="text-2xl font-bold text-forest">${booking.total_price}</Text>
              </View>
            </View>
          </View>
        </View>
        
        {/* Next Steps */}
        <View className="px-4 mb-6">
          <Text className="text-lg font-bold text-forest mb-3">What's Next?</Text>
          <StepCard 
            icon="mail"
            title="Confirmation Email Sent"
            description="Check your email for booking confirmation and property details"
          />
          <StepCard 
            icon="call"
            title="Host Will Contact You"
            description="The host may reach out with check-in instructions"
          />
          <StepCard 
            icon="home"
            title="Check-in on Your Date"
            description={`Check-in is available after 3:00 PM on ${booking.check_in}`}
          />
        </View>
        
        {/* Action Buttons */}
        <View className="px-4 pb-8 flex-row gap-3">
          <TouchableOpacity
            onPress={() => router.replace('/(tabs)/dashboard')}
            className="flex-1"
          >
            <LinearGradient
              colors={['#D9B168', '#bea04f']}
              className="py-4 rounded-2xl"
            >
              <Text className="text-forest font-bold text-center">View Booking</Text>
            </LinearGradient>
          </TouchableOpacity>
          
          <TouchableOpacity
            onPress={() => router.replace('/(tabs)/explore')}
            className="flex-1"
          >
            <View className="border-2 border-gold py-4 rounded-2xl">
              <Text className="text-gold font-bold text-center">Browse More</Text>
            </View>
          </TouchableOpacity>
        </View>
      </ScrollView>
    </SafeAreaView>
  );
}

function DetailRow({ label, value }: { label: string; value: string }) {
  return (
    <View className="flex-row justify-between mb-3">
      <Text className="text-moss font-semibold">{label}</Text>
      <Text className="text-forest font-bold">{value}</Text>
    </View>
  );
}

function StepCard({ icon, title, description }: any) {
  return (
    <View className="flex-row items-start mb-3 p-4 bg-sand-50 rounded-xl">
      <View className="w-10 h-10 rounded-full bg-gold/20 items-center justify-center mr-3">
        <Ionicons name={icon as any} size={20} color="#D9B168" />
      </View>
      <View className="flex-1">
        <Text className="font-bold text-forest mb-1">{title}</Text>
        <Text className="text-moss text-sm">{description}</Text>
      </View>
    </View>
  );
}
```

---

### 4. Booking Pending Screen (booking/[id]/pending.tsx)

For payments that require webhook confirmation:

```typescript
// app/booking/[id]/pending.tsx

export default function BookingPendingScreen() {
  const [status, setStatus] = useState('processing');
  const bookingId = params.id as string;
  
  useEffect(() => {
    // Poll for payment confirmation
    const pollInterval = setInterval(async () => {
      try {
        const response = await apiClient.get(`/bookings/${bookingId}/`);
        if (response.data.status === 'confirmed') {
          clearInterval(pollInterval);
          setStatus('confirmed');
          // Redirect to success
          setTimeout(() => {
            router.replace({
              pathname: '/booking/[id]/success',
              params: { id: bookingId }
            });
          }, 1000);
        }
      } catch (error) {
        console.error('Error checking status:', error);
      }
    }, 3000); // Check every 3 seconds
    
    return () => clearInterval(pollInterval);
  }, [bookingId]);
  
  return (
    <SafeAreaView className="flex-1 bg-sand-100 items-center justify-center">
      <View className="items-center">
        <ActivityIndicator size="large" color="#D9B168" />
        <Text className="text-forest font-bold mt-4">Processing Payment...</Text>
        <Text className="text-moss text-sm mt-2">
          This may take a few moments
        </Text>
      </View>
    </SafeAreaView>
  );
}
```

---

### 5. Dashboard Integration

Update the dashboard to show bookings with their payment status:

```typescript
// app/(tabs)/dashboard/index.tsx

// In the booking list, show payment status badge:

function BookingCard({ booking }: { booking: Booking }) {
  const getStatusColor = (status: string) => {
    switch (status) {
      case 'confirmed': return 'bg-green-100 text-green-800';
      case 'pending': return 'bg-yellow-100 text-yellow-800';
      case 'cancelled': return 'bg-red-100 text-red-800';
      default: return 'bg-sand-100 text-moss';
    }
  };
  
  return (
    <View className="bg-white rounded-2xl p-4 mb-3">
      <View className="flex-row justify-between items-start mb-3">
        <Text className="text-forest font-bold flex-1">{booking.property_name}</Text>
        <View className={`px-3 py-1 rounded-full ${getStatusColor(booking.status)}`}>
          <Text className="text-xs font-bold capitalize">{booking.status}</Text>
        </View>
      </View>
      
      {/* Booking details */}
      <Text className="text-moss text-sm mb-2">
        {booking.check_in} - {booking.check_out}
      </Text>
      
      {/* Action button based on status */}
      {booking.status === 'pending' && (
        <TouchableOpacity
          onPress={() => router.push({
            pathname: '/booking/[id]/payment',
            params: { 
              bookingId: booking.id,
              propertyName: booking.property_name,
              checkIn: booking.check_in,
              checkOut: booking.check_out,
              guests: booking.number_of_guests.toString(),
              total: booking.total_price.toString(),
            }
          })}
        >
          <View className="bg-gold/20 py-2 rounded-lg mt-3">
            <Text className="text-gold font-bold text-center text-sm">Complete Payment</Text>
          </View>
        </TouchableOpacity>
      )}
    </View>
  );
}
```

---

## Navigation Structure Update

Update the router configuration to include new payment screens:

```typescript
// app/booking/_layout.tsx (Create if doesn't exist)

import { Stack } from 'expo-router';

export default function BookingLayout() {
  return (
    <Stack
      screenOptions={{
        headerShown: false,
      }}
    >
      <Stack.Screen name="[id]/payment" />
      <Stack.Screen name="[id]/success" />
      <Stack.Screen name="[id]/pending" />
    </Stack>
  );
}
```

---

## API Integration Flow

### Payment Processing Sequence

```
Client                          Backend                         Provider
  │                               │                               │
  ├─POST /bookings/               │                               │
  │ (create draft booking)         │                               │
  │─────────────────────────────→  │                               │
  │                           ✓201 │                               │
  │←─────────────────────────────  │                               │
  │                                │                               │
  │ (navigate to payment screen)    │                               │
  │ (fetch payment methods)         │                               │
  │─GET /wallet/payment-methods/?──→                              │
  │←─────────────────────────────200│                               │
  │                                │                               │
  │ (user selects method)           │                               │
  │ (user taps Pay)                 │                               │
  │─POST /bookings/{id}/pay/        │                               │
  │ {payment_method_id}             │                               │
  │─────────────────────────────→  │                               │
  │                                 │─POST /charges (tokenized)────→│
  │                                 │                               │
  │                                 │←──Webhook (payment.succeeded)─│
  │                                 │                               │
  │                            ✓200 │                               │
  │←─────────────────────────────  │                               │
  │ (status: completed)             │                               │
  │                                 │                               │
  │ (navigate to success screen)     │                               │
  │                                 │                               │
```

---

## Error Handling

### Common Payment Errors

```typescript
interface PaymentError {
  code: string;
  message: string;
  recoverable: boolean;
  nextStep?: string;
}

const PAYMENT_ERRORS: Record<string, PaymentError> = {
  'insufficient_funds': {
    code: 'INSUFFICIENT_FUNDS',
    message: 'Your payment method has insufficient funds',
    recoverable: true,
    nextStep: 'Try a different payment method'
  },
  'declined': {
    code: 'CARD_DECLINED',
    message: 'Your card was declined',
    recoverable: true,
    nextStep: 'Check card details and try again'
  },
  'invalid_method': {
    code: 'INVALID_METHOD',
    message: 'This payment method is no longer valid',
    recoverable: true,
    nextStep: 'Add a new payment method'
  },
  'timeout': {
    code: 'PAYMENT_TIMEOUT',
    message: 'Payment processing timed out',
    recoverable: true,
    nextStep: 'Check booking status and try again'
  }
};

function handlePaymentError(error: any) {
  const errorCode = error.code || 'unknown';
  const errorInfo = PAYMENT_ERRORS[errorCode];
  
  if (errorInfo?.recoverable) {
    Alert.alert(
      'Payment Failed',
      errorInfo.message,
      [
        {
          text: errorInfo.nextStep || 'Try Again',
          onPress: () => {
            if (errorCode === 'invalid_method') {
              router.push('/wallet/add-payment-method');
            } else {
              // Retry payment
            }
          }
        }
      ]
    );
  } else {
    Alert.alert(
      'Payment Error',
      'An unexpected error occurred. Please contact support.'
    );
  }
}
```

---

## Testing Checklist

### Unit Tests

- [ ] Payment method list fetching
- [ ] Payment method selection
- [ ] Booking creation with payment
- [ ] Payment processing initiation
- [ ] Error handling and recovery

### Integration Tests

- [ ] Complete booking flow (create → payment → success)
- [ ] Payment method addition during checkout
- [ ] Multiple payment method switching
- [ ] Webhook confirmation polling
- [ ] Dashboard booking updates

### End-to-End Tests (with test keys)

- [ ] Complete payment with Stripe test card
- [ ] Complete payment with Paynow test
- [ ] Complete payment with Flutterwave test
- [ ] Payment failure and retry
- [ ] Booking cancellation after payment

---

## Performance Optimization

### Network Optimization

```typescript
// Prefetch payment methods when navigating to booking screen
useFocusEffect(
  React.useCallback(() => {
    // Prefetch payment methods
    queryClient.prefetchQuery(['paymentMethods'], () =>
      apiClient.get('/wallet/payment-methods/')
    );
  }, [])
);
```

### Caching Strategy

```typescript
// Cache payment methods for 5 minutes
const paymentMethodsQuery = useQuery('paymentMethods', {
  staleTime: 5 * 60 * 1000, // 5 minutes
  cacheTime: 10 * 60 * 1000, // 10 minutes
});
```

---

## Security Best Practices

✅ **Do's**:
- Use HTTPS for all API calls
- Validate payment method before processing
- Show clear error messages
- Implement timeout handling
- Log payment events (without sensitive data)

❌ **Don'ts**:
- Don't store card data on device
- Don't expose payment tokens in logs
- Don't retry payments indefinitely
- Don't store passwords or PINs
- Don't send payment data in unencrypted requests

---

## References

- Payment Wizard: `/mobile/app/wallet/add-payment-method.tsx`
- Payment Methods Manager: `/mobile/app/wallet/payment-methods.tsx`
- Payment Processing: `/mobile/app/booking/payment.tsx`
- API Integration: `/mobile/API_PAYMENT_INTEGRATION.md`
- Backend Guide: `/PAYMENT_PROCESSING_INTEGRATION_GUIDE.md`

