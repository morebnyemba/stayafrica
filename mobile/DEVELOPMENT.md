# Mobile App Development Guide

## Getting Started

### Prerequisites
- Node.js 18+ and npm/yarn
- Expo CLI: `npm install -g expo-cli`
- iOS Simulator (Mac) or Android Emulator
- Expo Go app on physical device (optional)

### Installation

```bash
cd mobile
npm install
```

### Environment Setup

Create a `.env` file from `.env.example`:

```bash
cp .env.example .env
```

Fill in the required environment variables:
- `EXPO_PUBLIC_API_BASE_URL`: Backend API URL
- `EXPO_PUBLIC_MAPBOX_TOKEN`: Mapbox API token
- `EXPO_PUBLIC_STRIPE_KEY`: Stripe public key

### Development

**Start Expo development server:**
```bash
npm run start
```

**Run on iOS simulator:**
```bash
npm run ios
```

**Run on Android emulator:**
```bash
npm run android
```

**Run on physical device:**
- Scan QR code with Expo Go app
- Or use `npm run web` for browser testing

## Project Structure

```
mobile/
├── app/                          # Expo Router navigation
│   ├── (auth)/                  # Authentication stack
│   │   ├── login.tsx
│   │   ├── register.tsx
│   │   └── forgot-password.tsx
│   ├── (tabs)/                  # Main tabbed navigation
│   │   ├── explore/             # Property discovery
│   │   ├── bookings/            # User bookings
│   │   ├── messages/            # Messaging interface
│   │   └── profile/             # User profile
│   └── _layout.tsx              # Root layout
├── src/
│   ├── components/              # Reusable React Native components
│   │   ├── common/              # Generic components (Header, Loading, etc.)
│   │   ├── property/            # Property-specific components
│   │   └── booking/             # Booking-specific components
│   ├── context/                 # React context (Auth, Providers)
│   ├── hooks/                   # Custom hooks (useAuth, useLocation, etc.)
│   ├── services/                # API client and external services
│   ├── types/                   # TypeScript type definitions
│   ├── utils/                   # Helper functions
│   └── constants/               # App constants and configuration
├── assets/                      # Images and static files
├── package.json
├── app.json                     # Expo configuration
├── tsconfig.json               # TypeScript configuration
├── metro.config.js             # Metro bundler config
└── jest.config.js              # Jest test configuration
```

## Core Features

### 1. Authentication
- **Location:** `src/context/auth-context.tsx`
- **Features:** Login, Register, JWT token management
- **Hook:** `useAuth()`

```typescript
const { user, isAuthenticated, login, logout } = useAuth();
```

### 2. API Integration
- **Client:** `src/services/api-client.ts`
- **Hooks:** `src/hooks/api-hooks.ts`
- **Features:** Automatic token refresh, request/response interceptors

```typescript
const { data: properties } = useNearbyProperties(latitude, longitude, radius);
```

### 3. Navigation
- **Router:** Expo Router (file-based routing)
- **Structure:** Auth stack → Main tabs (Explore, Bookings, Messages, Profile)

### 4. Type Safety
- Full TypeScript support with strict mode enabled
- Shared types with backend API (`src/types/index.ts`)

## API Endpoints

All endpoints defined in `src/constants/index.ts`

| Method | Endpoint | Purpose |
|--------|----------|---------|
| GET | `/properties/` | List nearby properties |
| GET | `/properties/{id}/` | Get property details |
| GET | `/bookings/` | User bookings |
| POST | `/bookings/` | Create booking |
| GET | `/users/profile/` | User profile |
| GET | `/messaging/conversations/` | User conversations |
| POST | `/messaging/conversations/{id}/messages/` | Send message |
| POST | `/reviews/` | Submit review |

## Component Development

### Creating a New Component

**Property Card Component:**
```typescript
// src/components/property/PropertyCard.tsx
import { View, Text, TouchableOpacity, Image } from 'react-native';
import { Property } from '@/types';

interface PropertyCardProps {
  property: Property;
  onPress: (id: string) => void;
}

export function PropertyCard({ property, onPress }: PropertyCardProps) {
  return (
    <TouchableOpacity onPress={() => onPress(property.id)}>
      <Image source={{ uri: property.image_urls[0] }} style={{ height: 200 }} />
      <Text className="font-bold">{property.title}</Text>
      <Text>${property.price_per_night}/night</Text>
    </TouchableOpacity>
  );
}
```

### Using API Hooks

```typescript
// src/hooks/api-hooks.ts
export function useNearbyProperties(latitude: number, longitude: number, radius: number) {
  return useQuery({
    queryKey: ['properties', 'nearby', latitude, longitude, radius],
    queryFn: () => apiClient.getNearbyProperties(latitude, longitude, radius),
  });
}
```

## Styling

Uses **NativeWind** (Tailwind CSS for React Native):

```typescript
<View className="flex-1 px-4 py-6">
  <Text className="text-2xl font-bold text-primary-600">Title</Text>
</View>
```

**Custom Colors:** Defined in `tailwind.config.ts`
- Primary: `#0ea5e9` (Cyan)
- Secondary: `#a855f7` (Purple)
- Accent: `#f97316` (Orange)

## Testing

```bash
# Run tests
npm test

# Run with coverage
npm test -- --coverage
```

## Building for Production

### iOS Build
```bash
eas build --platform ios
```

### Android Build
```bash
eas build --platform android
```

### Web Export
```bash
npm run web
```

## Debugging

### React Native Debugger
```bash
npm start -- --dev-client
```

### Logs
```bash
npm start -- --clear
```

## Performance Optimization

1. **React Query:** Automatic caching and stale data handling
2. **Image Optimization:** Lazy loading with `Image` component
3. **Code Splitting:** File-based routing automatically splits code
4. **State Management:** Zustand (can be integrated for complex state)

## Common Issues & Solutions

### Issue: `Module not found` errors
**Solution:** Check path aliases in `tsconfig.json` match `metro.config.js`

### Issue: Authentication token expired
**Solution:** `apiClient` automatically refreshes tokens via interceptors

### Issue: Slow builds
**Solution:** 
- Clear cache: `npm start -- --clear`
- Use `--no-dev-client` for faster builds

### Issue: Permissions (Location, Camera, etc.)
**Solution:** 
- Permissions requested at runtime
- Check `app.json` for required plugins
- iOS: Update `Info.plist` (handled by Expo)

## Best Practices

1. **Always use hooks** for auth and API calls
2. **Implement error boundaries** for better UX
3. **Use TypeScript** for type safety
4. **Follow naming conventions** (PascalCase components, camelCase functions)
5. **Keep components small** and focused
6. **Use constants** for magic strings
7. **Test on physical devices** before release

## Resources

- [Expo Documentation](https://docs.expo.dev/)
- [React Native Docs](https://reactnative.dev/)
- [NativeWind](https://www.nativewind.dev/)
- [React Query](https://tanstack.com/query/latest)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/)

## Contributing

1. Create feature branch: `git checkout -b feature/your-feature`
2. Commit changes: `git commit -m "Add your feature"`
3. Push to branch: `git push origin feature/your-feature`
4. Open pull request

## License

MIT
