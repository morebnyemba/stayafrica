# Mobile App - Quick Start Guide

## Development Setup

### 1. Install Dependencies
```bash
cd mobile
npm install
```

### 2. Start Development Server
```bash
npx expo start
```

Or use the convenience script:
```bash
npm start
```

### 3. Clear Cache (if needed)
```bash
npx expo start -c
```

## Testing on Device

### Expo Go (Development)
1. Install Expo Go app on your phone
2. Start dev server: `npx expo start`
3. Scan QR code with:
   - **iOS**: Camera app
   - **Android**: Expo Go app

### Development Build
```bash
# Android
npx expo run:android

# iOS
npx expo run:ios
```

## Building for Production

### Preview Build (Test on Device)
```bash
# Android
eas build --profile preview --platform android

# iOS
eas build --profile preview --platform ios
```

### Production Build
```bash
# Android
eas build --profile production --platform android

# iOS
eas build --profile production --platform ios
```

## Common Issues & Fixes

### "Route Unmatched" Error
✅ **FIXED** - All routes are now properly registered in `_layout.tsx` files

### Metro Bundler Cache Issues
```bash
npx expo start -c
# or
rm -rf .expo node_modules
npm install
```

### Expo Go Not Loading
1. Make sure phone and computer are on same network
2. Try tunnel mode: `npx expo start --tunnel`
3. Clear Expo Go cache on device

### TypeScript Errors
```bash
npm run type-check
```

### Build Failures
```bash
# Clear EAS build cache
eas build --clear-cache

# Check configuration
eas config
```

## Key Directories

```
mobile/
├── app/                    # Routes (Expo Router)
│   ├── _layout.tsx        # Root layout
│   ├── (auth)/            # Auth screens
│   ├── (tabs)/            # Main tab navigation
│   ├── host/              # Host-specific routes
│   └── reviews/           # Reviews routes
├── src/
│   ├── components/        # Reusable components
│   ├── context/           # React Context providers
│   ├── hooks/             # Custom hooks
│   ├── services/          # API services
│   ├── types/             # TypeScript types
│   ├── utils/             # Utility functions
│   └── constants/         # App constants
├── assets/                # Images, fonts, etc.
└── app.json              # Expo configuration
```

## Useful Commands

### Check Routes
```bash
npx expo customize expo-router
```

### View Bundle Size
```bash
npx expo-router info
```

### Update Expo
```bash
npx expo upgrade
```

### Login to Expo
```bash
npx expo login
```

## Brand Colors Reference

Use these in your screens:

```tsx
// Deep Forest (Primary backgrounds, headers)
className="bg-forest"        // #122F26
colors={['#122F26', '#1d392f']}

// Safari Gold (Accents, CTAs)
className="bg-gold"          // #D9B168
colors={['#D9B168', '#bea04f']}

// Ivory Sand (App background)
className="bg-sand-100"      // #F4F1EA

// Moss Green (Secondary)
className="text-moss"        // #3A5C50
```

## Icon Usage (Ionicons)

```tsx
import { Ionicons } from '@expo/vector-icons';

<Ionicons name="home" size={24} color="#D9B168" />
<Ionicons name="heart-outline" size={24} color="#D9B168" />
<Ionicons name="person" size={24} color="#122F26" />
```

## Skeleton Loading

```tsx
import { PropertyCardSkeleton, BookingCardSkeleton } from '@/components/common/Skeletons';

{isLoading ? (
  <PropertyCardSkeleton />
) : (
  <PropertyCard property={property} />
)}
```

## Navigation Examples

```tsx
import { useRouter } from 'expo-router';

const router = useRouter();

// Navigate to tab
router.push('/(tabs)/explore');

// Navigate with params
router.push(`/(tabs)/explore/${propertyId}`);

// Replace (no back button)
router.replace('/(auth)/login');

// Go back
router.back();
```

## Environment Variables

Create `.env` file:
```
EXPO_PUBLIC_API_URL=http://localhost:8000
```

Access in code:
```tsx
import Constants from 'expo-constants';

const apiUrl = Constants.expoConfig?.extra?.apiUrl;
```

## Help & Documentation

- [Expo Documentation](https://docs.expo.dev/)
- [Expo Router Docs](https://docs.expo.dev/router/introduction/)
- [NativeWind Docs](https://www.nativewind.dev/)
- [React Native Docs](https://reactnative.dev/)

## Need Help?

Check the detailed documentation:
- `MOBILE_FIX_SUMMARY.md` - Complete fix summary
- `README.md` - General mobile app info
- `TESTING_GUIDE.md` - Testing instructions
