# StayAfrica Mobile App

A modern React Native/Expo mobile application for the StayAfrica property rental platform. Discover, book, and manage properties on the go.

## 📱 Features

### Core Features
- 🔐 **Secure Authentication** - JWT-based login/register with token refresh
- 🏠 **Property Discovery** - Browse nearby properties with Mapbox integration
- 📍 **Geolocation Search** - Find properties within custom radius
- 💳 **Seamless Bookings** - Easy checkout flow and booking management
- 💬 **Real-time Messaging** - Chat with hosts and guests
- ⭐ **Ratings & Reviews** - Leave and view property reviews
- 👤 **User Profiles** - Manage account information and preferences
- 🔄 **Offline Support** - AsyncStorage for data persistence

### Technical Features
- 📘 **Full TypeScript** - Strict mode for type safety
- 🚀 **Expo Router** - File-based routing with no config
- 🗺️ **Maps Integration** - Mapbox GL for interactive maps
- 💳 **Payment Ready** - Stripe React Native configured
- 📍 **Location Services** - Built-in geolocation
- 📸 **Image Handling** - Image picker and gallery support
- 🔔 **Push Notifications** - Expo Notifications configured
- 💾 **Secure Storage** - expo-secure-store for tokens

## 🚀 Quick Start

### Prerequisites
```
Node.js 18+
Expo CLI: npm install -g expo-cli
iOS Simulator (Mac) or Android Emulator
```

### Installation

```bash
# Clone the repository
cd mobile

# Install dependencies
npm install

# Create environment file
cp .env.example .env

# Start development server
npm start
```

### Run on Device/Emulator

```bash
# iOS Simulator
npm run ios

# Android Emulator  
npm run android

# Web Browser
npm run web

# Physical device
npm start
# Scan QR code with Expo Go app
```

## 📁 Project Structure

```
mobile/
├── app/                              # Expo Router pages
│   ├── (auth)/                       # Auth routes (login, register)
│   ├── (tabs)/                       # Main app routes
│   │   ├── explore/                  # Property discovery
│   │   ├── bookings/                 # User bookings
│   │   ├── messages/                 # Messaging interface
│   │   └── profile/                  # User profile
│   └── _layout.tsx                   # Root layout
│
├── src/
│   ├── components/                   # Reusable components
│   │   ├── common/                   # Generic components
│   │   ├── property/                 # Property components
│   │   └── booking/                  # Booking components
│   │
│   ├── context/                      # React Context
│   │   ├── auth-context.tsx          # Auth state management
│   │   └── providers.tsx             # Combined providers
│   │
│   ├── hooks/                        # Custom hooks
│   │   ├── api-hooks.ts              # API operations
│   │   └── useUserLocation.ts        # Location hook
│   │
│   ├── services/
│   │   └── api-client.ts             # API client with interceptors
│   │
│   ├── types/
│   │   └── index.ts                  # TypeScript types
│   │
│   ├── utils/
│   │   └── helpers.ts                # Utility functions
│   │
│   └── constants/
│       └── index.ts                  # App constants
│
├── assets/                           # Images & static files
│
├── app.json                          # Expo configuration
├── package.json                      # Dependencies
├── tsconfig.json                     # TypeScript config
├── metro.config.js                   # Bundler config
├── jest.config.js                    # Test config
└── .env.example                      # Environment template
```

## 🔧 Environment Setup

Create `.env` file:

```env
# API Configuration
EXPO_PUBLIC_API_BASE_URL=http://localhost:8000/api/v1

# Third-party Services
EXPO_PUBLIC_MAPBOX_TOKEN=your_mapbox_token
EXPO_PUBLIC_STRIPE_KEY=your_stripe_key
```

## 📚 Core Modules

### Authentication (`src/context/auth-context.tsx`)
```typescript
const { user, isAuthenticated, login, register, logout } = useAuth();
```
- JWT token management
- Auto token refresh
- User profile caching
- Logout with cleanup

### API Integration (`src/services/api-client.ts`)
```typescript
await apiClient.getNearbyProperties(latitude, longitude, radius);
await apiClient.createBooking(bookingData);
await apiClient.sendMessage(conversationId, message);
```
- Axios with interceptors
- Automatic token refresh
- Request/response handling
- Error management

### Data Fetching (`src/hooks/api-hooks.ts`)
```typescript
const { data, isLoading } = useNearbyProperties(lat, lng, radius);
const { data: bookings } = useBookings();
const { mutate: sendMessage } = useSendMessage();
```
- React Query integration
- Automatic caching
- Stale data handling
- Mutation support

### Location Services (`src/hooks/useUserLocation.ts`)
```typescript
const { location, requestPermission } = useUserLocation();
```
- Permission handling
- Current location retrieval
- Background updates

## 🎨 Styling

Uses **NativeWind** (Tailwind CSS for React Native):

```typescript
<View className="flex-1 px-4 py-6 bg-white">
  <Text className="text-2xl font-bold text-primary-600">Hello</Text>
</View>
```

### Color Scheme
```
Primary:    #122f26 (Deep Forest)
Secondary:  #d9b168 (Safari Gold)
Background: #f4f1ea (Ivory Sand)
Accent:     #3a5c50 (Moss Green)
Text:       #0a1a15 (Savanna)
```

## 🔒 Security

- **Token Storage** - expo-secure-store for sensitive data
- **Token Refresh** - Automatic refresh on expiry
- **HTTPS Only** - All API calls over HTTPS
- **XSS Protection** - No eval or dangling references
- **CSRF Protection** - Via JWT tokens

## 📦 Dependencies

### Core
- react-native: 0.73
- expo: ~50.0.0
- typescript: 5.0+

### Navigation
- expo-router: ~3.4.0
- @react-navigation/native: 6.1.9

### Data Management
- @tanstack/react-query: 5.28.0
- axios: 1.6.2

### UI & Styling
- nativewind: 2.0.11 (Tailwind for RN)
- react-native-maps: 1.10.0

### Services
- expo-location: ~16.2.1
- expo-image-picker: ~14.3.1
- expo-notifications: ~0.23.0
- @stripe/stripe-react-native: 12.2.0
- mapbox-gl: 2.15.0

### Storage
- @react-native-async-storage/async-storage: 1.21.0
- expo-secure-store: ~12.3.0

## 🧪 Testing

```bash
# Run tests
npm test

# Run with coverage
npm test -- --coverage

# Watch mode
npm test -- --watch
```

## 📡 API Integration

All API endpoints follow RESTful conventions:

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/auth/login/` | POST | User login |
| `/auth/register/` | POST | User registration |
| `/properties/` | GET | List nearby properties |
| `/properties/{id}/` | GET | Property details |
| `/bookings/` | GET | User bookings |
| `/bookings/` | POST | Create booking |
| `/messaging/conversations/` | GET | User conversations |
| `/users/profile/` | GET | User profile |
| `/reviews/` | POST | Submit review |

## 🚀 Building for Production

### iOS Release
```bash
eas build --platform ios
eas submit --platform ios
```

### Android Release
```bash
eas build --platform android
eas submit --platform android
```

## 🐛 Debugging

### Development Tools
```bash
# With React Native Debugger
npm start -- --dev-client

# Clear cache
npm start -- --clear

# Verbose logging
npm start -- --verbose
```

### Common Issues

**Issue:** Module not found errors
- **Solution:** Check `tsconfig.json` path aliases match usage

**Issue:** Token not persisting
- **Solution:** Ensure expo-secure-store is installed: `expo install expo-secure-store`

**Issue:** Slow Expo build
- **Solution:** Clear cache: `npm start -- --clear`

**Issue:** Permissions not requested
- **Solution:** Check `app.json` plugins are properly configured

## 📈 Performance Tips

1. **Image Optimization** - Use `Image` component for lazy loading
2. **List Optimization** - Use `FlatList` with keyExtractor
3. **React Query** - Leverage automatic caching
4. **Code Splitting** - Expo Router automatically splits by route
5. **Avoid Inline Functions** - Move callbacks outside render

## 📖 Documentation

- **DEVELOPMENT.md** - Comprehensive development guide
- **API Integration** - See `src/services/api-client.ts`
- **Component Examples** - See `src/components/`
- **Type Definitions** - See `src/types/index.ts`

## 🤝 Contributing

1. Create feature branch: `git checkout -b feature/your-feature`
2. Commit changes: `git commit -m "Add your feature"`
3. Push to branch: `git push origin feature/your-feature`
4. Open pull request

## 📄 License

MIT

## 🆘 Support

For issues or questions:
- Check `DEVELOPMENT.md` for detailed guides
- Review component examples in `src/components/`
- Check API integration in `src/hooks/api-hooks.ts`
- Review environment configuration in `.env.example`

---

**Built with React Native + Expo for iOS and Android**

Perfect for discovering and booking properties on the go! 🏠✈️
