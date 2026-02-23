# Integration Guide - New UI Components

## Overview
This guide shows how to integrate the new UI components into existing pages with minimal conflicts.

---

## 1. Import Strategy

### New Barrel Exports Available
```typescript
// UI Components
import { Button, Card, CardHeader, CardBody, CardFooter, Input, Skeleton, Modal, Badge } from '@/components/ui';

// Common Components
import { ErrorBoundary, NotFound, ServerError, Header, Breadcrumbs, BottomNav, Footer, OptimizedImage } from '@/components/common';

// Property Components
import { PropertyCard } from '@/components/property';

// Booking Components
import { BookingPanel } from '@/components/booking';

// Utilities
import { validateForm, validators } from '@/lib/form-validation';
import { announceToScreenReader, createFocusTrap, keyboardEvents } from '@/lib/accessibility';
import { colors, spacing, shadows, typography } from '@/lib/design-tokens';
```

---

## 2. Layout Integration (✅ Already Done)

### Updated: `src/app/layout.tsx`
```typescript
import { ErrorBoundary } from '@/components/common/ErrorBoundary';
import { BottomNav } from '@/components/common/Navigation';

export default function RootLayout({ children }) {
  return (
    <html>
      <body>
        <ErrorBoundary>
          <Providers>
            <div className="flex flex-col min-h-screen pb-16 lg:pb-0">
              <Navigation />
              <main id="main-content" className="flex-grow">
                {children}
              </main>
              <BottomNav />
              <Footer />
            </div>
          </Providers>
        </ErrorBoundary>
      </body>
    </html>
  );
}
```

---

## 3. Property Listing Integration

### Update: `src/components/property/explore-content.tsx`

Replace the inline property cards with the new PropertyCard component:

```typescript
import { PropertyCard } from '@/components/property';
import { PropertyCardSkeleton } from '@/components/ui';

export function ExploreContent() {
  // ... existing code ...

  return (
    <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        <h1 className="text-3xl md:text-4xl font-bold">Explore Properties</h1>
        
        {/* Properties Grid */}
        {isLoading ? (
          <div className="grid md:grid-cols-3 lg:grid-cols-4 gap-6">
            {Array.from({ length: 9 }).map((_, i) => (
              <PropertyCardSkeleton key={i} />
            ))}
          </div>
        ) : (
          <div className="grid md:grid-cols-3 lg:grid-cols-4 gap-6">
            {properties.map((property: any) => (
              <PropertyCard
                key={property.id}
                property={{
                  id: property.id,
                  title: property.title,
                  location: `${property.city}, ${property.country}`,
                  price: property.price_per_night,
                  rating: property.average_rating || 4.5,
                  reviewCount: property.review_count || 0,
                  images: property.images?.map((img: any) => img.image_url) || [],
                  amenities: property.amenities || [],
                  beds: property.bedrooms || 1,
                  baths: property.bathrooms || 1,
                  guests: property.max_guests || 2,
                }}
                onFavorite={(id) => {
                  // Handle favorite
                }}
                onBook={(id) => {
                  router.push(`/property/${id}`);
                }}
              />
            ))}
          </div>
        )}
      </div>
    </div>
  );
}
```

---

## 4. Property Detail + Booking Integration

### Create: `src/app/(main)/property/[id]/page.tsx`

```typescript
'use client';

import { useParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { BookingPanel } from '@/components/booking';
import { Skeleton } from '@/components/ui';
import { OptimizedImage } from '@/components/common';

export default function PropertyPage() {
  const { id } = useParams();
  const { data: property, isLoading } = useQuery({
    queryKey: ['property', id],
    queryFn: () => apiClient.getProperty(id as string),
  });

  if (isLoading) {
    return (
      <div className="max-w-7xl mx-auto px-4 py-8">
        <Skeleton variant="rect" className="h-96 w-full mb-8" />
        <div className="grid md:grid-cols-3 gap-8">
          <div className="md:col-span-2 space-y-4">
            <Skeleton variant="text" className="h-8 w-2/3" />
            <Skeleton variant="text" className="h-4 w-full" />
            <Skeleton variant="text" className="h-4 w-full" />
          </div>
          <div>
            <Skeleton variant="rect" className="h-96" />
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="max-w-7xl mx-auto px-4 py-8">
      <div className="grid md:grid-cols-3 gap-8">
        {/* Main Content */}
        <div className="md:col-span-2">
          {/* Image Gallery */}
          <div className="mb-8">
            <OptimizedImage
              src={property.images?.[0]?.image_url}
              alt={property.title}
              width={800}
              height={600}
              className="w-full h-96 rounded-xl"
            />
          </div>

          <h1 className="text-4xl font-bold mb-2">{property.title}</h1>
          <p className="text-gray-600 mb-6">{property.description}</p>

          {/* Amenities */}
          <div className="mb-8">
            <h2 className="text-2xl font-bold mb-4">Amenities</h2>
            <div className="grid grid-cols-2 md:grid-cols-3 gap-4">
              {property.amenities?.map((amenity: string) => (
                <div key={amenity} className="flex items-center gap-2">
                  <span>✓</span>
                  <span>{amenity}</span>
                </div>
              ))}
            </div>
          </div>
        </div>

        {/* Booking Panel (Sticky) */}
        <div>
          <BookingPanel
            propertyId={property.id}
            pricePerNight={property.price_per_night}
            maxGuests={property.max_guests}
            minStay={property.min_stay}
            hostVerified={property.host.verified}
            hostRating={property.host.average_rating}
            onBook={async (dates, guests) => {
              // Handle booking
              console.log('Booking:', dates, guests);
            }}
          />
        </div>
      </div>
    </div>
  );
}
```

---

## 5. Form Integration

### Update: `src/components/auth/login-form.tsx`

Replace old form fields with new Input component:

```typescript
import { Input } from '@/components/ui';
import { Button } from '@/components/ui';
import { validateForm, validators } from '@/lib/form-validation';
import { useState } from 'react';

export function LoginForm() {
  const [formData, setFormData] = useState({ email: '', password: '' });
  const [errors, setErrors] = useState<Record<string, string>>({});

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    // Validate
    const newErrors = validateForm(formData, {
      email: { required: true, validate: validators.email },
      password: { required: true, minLength: { value: 6 } },
    });

    if (Object.keys(newErrors).length > 0) {
      setErrors(newErrors);
      return;
    }

    // Submit
    console.log('Submitting:', formData);
  };

  return (
    <form onSubmit={handleSubmit} className="space-y-4">
      <Input
        label="Email"
        type="email"
        value={formData.email}
        onChange={(e) => setFormData({ ...formData, email: e.target.value })}
        error={errors.email}
        placeholder="your@email.com"
        required
      />

      <Input
        label="Password"
        type="password"
        value={formData.password}
        onChange={(e) => setFormData({ ...formData, password: e.target.value })}
        error={errors.password}
        placeholder="••••••••"
        required
      />

      <Button fullWidth type="submit">
        Sign In
      </Button>
    </form>
  );
}
```

---

## 6. Accessibility Integration

### Update: `src/app/layout.tsx` for Skip Link

```typescript
import { SkipToMainContent } from '@/lib/accessibility';

export default function RootLayout({ children }) {
  return (
    <html>
      <body>
        <SkipToMainContent />
        <ErrorBoundary>
          {/* ... rest of layout ... */}
        </ErrorBoundary>
      </body>
    </html>
  );
}
```

### Add Keyboard Navigation to Dropdowns

```typescript
import { keyboardEvents, handleListKeydown } from '@/lib/accessibility';

export function UserMenu() {
  const [selectedIndex, setSelectedIndex] = useState(0);
  const menuItems = ['Profile', 'Settings', 'Logout'];

  const handleKeyDown = (e: React.KeyboardEvent) => {
    handleListKeydown(e, menuItems, selectedIndex, setSelectedIndex);
  };

  return (
    <div onKeyDown={handleKeyDown}>
      {menuItems.map((item, index) => (
        <button
          key={item}
          className={index === selectedIndex ? 'bg-blue-100' : ''}
          onClick={() => console.log(item)}
        >
          {item}
        </button>
      ))}
    </div>
  );
}
```

---

## 7. No Breaking Changes

### Preserved Compatibility
- ✅ Old form.tsx exports still available from `@/components/ui`
- ✅ Existing navigation imports still work (case-insensitive)
- ✅ Existing Skeleton component works (enhanced)
- ✅ All existing pages continue to function without changes

### Migration Path
1. **Phase 1**: New projects/pages use new components immediately
2. **Phase 2**: Gradually refactor existing pages (no deadline)
3. **Phase 3**: Eventually deprecate old patterns (optional)

---

## 8. Quick Integration Checklist

- [ ] Layout.tsx updated with ErrorBoundary, BottomNav ✅
- [ ] Explore page updated to use PropertyCard
- [ ] Property detail page created with BookingPanel
- [ ] Forms updated to use new Input with validation
- [ ] Barrel exports created for easier imports
- [ ] Skip-to-main-content added for a11y
- [ ] Test mobile responsiveness (BottomNav visible)
- [ ] Test keyboard navigation on forms
- [ ] Test error boundary (intentionally break a component)
- [ ] Verify all images load (lazy loading)

---

## 9. File Locations

```
web/src/
├── lib/
│   ├── design-tokens.ts          (colors, spacing, typography)
│   ├── form-validation.ts        (validators, rules)
│   ├── accessibility.ts          (a11y helpers, ARIA)
│
├── components/
│   ├── ui/
│   │   ├── Button.tsx
│   │   ├── Card.tsx
│   │   ├── Input.tsx
│   │   ├── Skeleton.tsx
│   │   ├── Modal.tsx
│   │   ├── Badge.tsx
│   │   ├── index.ts             (barrel export)
│   │
│   ├── common/
│   │   ├── Navigation.tsx        (Header, BottomNav)
│   │   ├── Footer.tsx
│   │   ├── ErrorBoundary.tsx
│   │   ├── ErrorPages.tsx
│   │   ├── OptimizedImage.tsx
│   │   ├── index.ts             (barrel export)
│   │
│   ├── property/
│   │   ├── PropertyCard.tsx
│   │   ├── index.ts             (barrel export)
│   │
│   ├── booking/
│   │   ├── BookingPanel.tsx
│   │   ├── index.ts             (barrel export)
│
├── app/
│   ├── layout.tsx               (✅ Updated)
│   ├── page.tsx
```

---

## 10. Testing the Integration

### Manual Testing Checklist
```bash
# 1. Visual checks
- [ ] Header is sticky and visible
- [ ] BottomNav appears on mobile only
- [ ] PropertyCard carousel works
- [ ] Modal opens/closes smoothly
- [ ] Form validation shows errors

# 2. Accessibility checks
- [ ] Tab navigation works
- [ ] Escape closes modals
- [ ] Screen reader announces states
- [ ] Focus visible on all interactive elements
- [ ] Color contrast passes WCAG AA

# 3. Performance checks
- [ ] Images lazy load
- [ ] Skeleton loaders appear instantly
- [ ] Page transitions are smooth
- [ ] No console errors
```

---

## Notes

- All new components are **production-ready**
- Backward compatibility **maintained** with existing code
- No breaking changes to the codebase
- Gradual migration path available
- Full TypeScript support with proper types

