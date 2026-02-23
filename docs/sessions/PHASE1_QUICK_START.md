# Quick Start Guide - Phase 1 Components

Quick reference for using the new Phase 1 frontend components.

---

## 🎨 Dynamic Pricing

### Basic Usage
```tsx
import { DynamicPricingDisplay } from '@/components/pricing';

<DynamicPricingDisplay
  propertyId="property-123"
  checkIn="2026-02-01"
  checkOut="2026-02-07"
  basePrice={100}
  guests={2}
/>
```

### In PropertyCard
```tsx
import { DynamicPricingDisplay } from '@/components/pricing';

<DynamicPricingDisplay
  propertyId={property.id}
  checkIn={searchDates.checkIn}
  checkOut={searchDates.checkOut}
  basePrice={property.base_price}
  className="mt-4"
/>
```

---

## 🗓️ Flexible Date Search

### In Search Page
```tsx
import { FlexibilityToggle, FlexibleDateSearchPanel, FlexibleDateResults } from '@/components/search';
import { useFlexibleSearch } from '@/hooks/useFlexibleSearch';

function SearchPage() {
  const [isFlexible, setIsFlexible] = useState(false);
  const [flexibility, setFlexibility] = useState<FlexibilityType>('exact');
  
  const { data, isLoading } = useFlexibleSearch({
    checkIn: '2026-02-01',
    checkOut: '2026-02-07',
    flexibility,
    days: 3,
  });

  return (
    <>
      <FlexibilityToggle isFlexible={isFlexible} onChange={setIsFlexible} />
      
      {isFlexible && (
        <FlexibleDateSearchPanel
          checkIn={new Date('2026-02-01')}
          checkOut={new Date('2026-02-07')}
          onSearch={(flex, days) => setFlexibility(flex)}
        />
      )}
      
      {flexibility !== 'exact' && (
        <FlexibleDateResults results={data?.date_options || []} isLoading={isLoading} />
      )}
    </>
  );
}
```

---

## ⚡ Instant Booking

### On Property Page
```tsx
import { InstantBookButton } from '@/components/booking/InstantBookButton';

<InstantBookButton
  propertyId={property.id}
  checkIn="2026-02-01"
  checkOut="2026-02-07"
  guests={2}
  onSuccess={(booking) => {
    router.push(`/bookings/${booking.id}`);
  }}
/>
```

### Host Settings Page
```tsx
import { InstantBookingSettings } from '@/components/host/InstantBookingSettings';

<InstantBookingSettings propertyId={propertyId} />
```

---

## 📍 Points of Interest

### On Property Details Page
```tsx
import { POIList } from '@/components/poi';

<POIList propertyId={propertyId} radiusKm={2} />
```

### With Map Display
```tsx
import { POIMapDisplay, POIList } from '@/components/poi';
import { useQuery } from '@tanstack/react-query';
import poiApi from '@/services/poi-api';

function PropertyPOIs({ propertyId }) {
  const { data } = useQuery({
    queryKey: ['pois', propertyId],
    queryFn: () => poiApi.getNearbyPOIs(propertyId, { radiusKm: 2 }),
  });

  return (
    <div>
      <POIMapDisplay
        propertyId={propertyId}
        propertyLocation={{ latitude: 40.7128, longitude: -74.0060 }}
        pois={data?.pois_by_category?.restaurant || []}
      />
      <POIList propertyId={propertyId} radiusKm={2} />
    </div>
  );
}
```

---

## ❤️ Enhanced Wishlists

### Wishlist Page
```tsx
import { CollaborativeWishlist } from '@/components/wishlist';

function WishlistPage({ wishlistId }) {
  const { user } = useAuth();
  const { data: wishlist } = useQuery({
    queryKey: ['wishlist', wishlistId],
    queryFn: () => wishlistApi.getWishlist(wishlistId),
  });

  return (
    <CollaborativeWishlist
      wishlistId={wishlistId}
      isOwner={wishlist?.owner.id === user?.id}
      canEdit={true}
    />
  );
}
```

### Add to Wishlist Button
```tsx
import { useMutation, useQueryClient } from '@tanstack/react-query';
import wishlistApi from '@/services/wishlist-api';

function AddToWishlistButton({ propertyId }) {
  const queryClient = useQueryClient();
  
  const addMutation = useMutation({
    mutationFn: (wishlistId: string) =>
      wishlistApi.addProperty(wishlistId, { property_id: propertyId }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['wishlist'] });
      toast.success('Added to wishlist!');
    },
  });

  return (
    <button onClick={() => addMutation.mutate(selectedWishlistId)}>
      Add to Wishlist
    </button>
  );
}
```

---

## 🎣 Custom Hooks

### useFlexibleSearch
```tsx
import { useFlexibleSearch } from '@/hooks/useFlexibleSearch';

const { data, isLoading, error } = useFlexibleSearch({
  location: 'Cape Town',
  checkIn: '2026-02-01',
  checkOut: '2026-02-07',
  flexibility: 'flexible_days',
  days: 3,
  guests: 2,
});
```

### useInstantBooking
```tsx
import { useInstantBooking } from '@/hooks/useInstantBooking';

const {
  isQualified,
  qualificationDetails,
  bookInstantly,
  isBooking,
  bookingError,
} = useInstantBooking(propertyId);

// Check if user can book instantly
if (isQualified) {
  await bookInstantly({
    check_in: '2026-02-01',
    check_out: '2026-02-07',
    number_of_guests: 2,
  });
}
```

---

## 📋 Common Patterns

### Loading States
```tsx
if (isLoading) {
  return <div className="animate-pulse">
    <div className="h-8 bg-gray-200 rounded w-32"></div>
  </div>;
}
```

### Error States
```tsx
if (error) {
  return <div className="bg-red-50 border border-red-200 rounded-lg p-4">
    <p className="text-red-800">Failed to load data</p>
  </div>;
}
```

### Empty States
```tsx
if (!data || data.length === 0) {
  return <div className="text-center py-12">
    <Icon className="w-16 h-16 text-gray-400 mx-auto mb-4" />
    <h3 className="text-xl font-semibold text-gray-900 mb-2">No results</h3>
    <p className="text-gray-600">Try adjusting your search</p>
  </div>;
}
```

---

## 🎨 Styling

### Color Classes
```tsx
// Discounts/Success
className="bg-green-50 text-green-700 border-green-200"

// Surcharges/Warning
className="bg-orange-50 text-orange-700 border-orange-200"

// Info
className="bg-blue-50 text-blue-700 border-blue-200"

// Error
className="bg-red-50 text-red-700 border-red-200"
```

### Responsive Grid
```tsx
className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4"
```

### Card Shadow
```tsx
className="bg-white rounded-xl shadow hover:shadow-lg transition"
```

---

## 🔧 TypeScript Types

### Import Types
```tsx
import type { DynamicPricing, PricingRule } from '@/types/pricing-types';
import type { POIType, PropertyPOI } from '@/types/poi-types';
import type { Wishlist, WishlistItem } from '@/types/wishlist-types';
```

### Component Props
```tsx
interface MyComponentProps {
  propertyId: string;
  onSuccess?: (data: any) => void;
  className?: string;
}
```

---

## 🚨 Error Handling

### API Errors
```tsx
try {
  const result = await apiCall();
} catch (error) {
  const apiError = error as { response?: { data?: { message?: string } } };
  toast.error(apiError.response?.data?.message || 'Something went wrong');
}
```

### Mutation Errors
```tsx
const mutation = useMutation({
  mutationFn: apiCall,
  onError: (error: unknown) => {
    const apiError = error as { response?: { data?: { detail?: string } } };
    toast.error(apiError.response?.data?.detail || 'Failed to update');
  },
});
```

---

## 📱 Mobile Considerations

### Touch Targets
```tsx
// Minimum 44x44px touch target
className="p-3 min-h-[44px] min-w-[44px]"
```

### Text Size
```tsx
// Minimum 16px to prevent zoom on iOS
className="text-base" // 16px
```

### Responsive Typography
```tsx
className="text-sm sm:text-base md:text-lg"
```

---

## ⚡ Performance Tips

### Lazy Loading
```tsx
import dynamic from 'next/dynamic';

const HeavyComponent = dynamic(() => import('./HeavyComponent'), {
  loading: () => <LoadingSkeleton />,
});
```

### Image Optimization
```tsx
import Image from 'next/image';

<Image
  src={imageUrl}
  alt={alt}
  width={400}
  height={300}
  loading="lazy"
  placeholder="blur"
/>
```

### Query Caching
```tsx
const { data } = useQuery({
  queryKey: ['key'],
  queryFn: fetchData,
  staleTime: 5 * 60 * 1000, // 5 minutes
  cacheTime: 10 * 60 * 1000, // 10 minutes
});
```

---

## 🧪 Testing Examples

### Component Test
```tsx
import { render, screen } from '@testing-library/react';
import { DynamicPricingDisplay } from '@/components/pricing';

test('displays dynamic price', () => {
  render(<DynamicPricingDisplay propertyId="123" basePrice={100} />);
  expect(screen.getByText('$100')).toBeInTheDocument();
});
```

### Hook Test
```tsx
import { renderHook } from '@testing-library/react';
import { useFlexibleSearch } from '@/hooks/useFlexibleSearch';

test('flexible search hook', () => {
  const { result } = renderHook(() => useFlexibleSearch({
    checkIn: '2026-02-01',
    checkOut: '2026-02-07',
    flexibility: 'exact',
  }));
  
  expect(result.current.isLoading).toBe(true);
});
```

---

**Last Updated**: January 16, 2026  
**Version**: Phase 1.0  
**Status**: Production Ready
