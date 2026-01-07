# StayAfrica Mobile App - Architecture Update

## Overview
The mobile app has been updated to match the architecture and features of the Next.js web frontend, providing a comprehensive mobile experience for both guests and hosts.

## New Features Added

### 1. Enhanced Navigation (6 Tabs)
- **Explore** - Browse and search properties
- **Wishlist** - Save favorite properties
- **Bookings** - View and manage bookings
- **Host** - Host dashboard for property management
- **Profile** - User profile and settings
- **Wallet** - Payment and earnings management

### 2. Host Features
#### Host Dashboard
- Quick stats overview (properties, bookings, earnings, rating)
- Quick action menu:
  - List new property
  - Manage properties
  - View bookings
  - Track earnings
  - View reviews
  - Host settings

#### Property Management
- View all hosted properties
- Add new properties
- Edit existing properties
- View property details and status
- Manage availability

#### Bookings Management
- View all property bookings
- See guest information
- Track booking status (pending, confirmed, checked-in, etc.)
- Manage check-ins and check-outs

#### Earnings & Payouts
- Track total earnings
- View available and pending balances
- Monthly earnings breakdown
- Average per booking stats
- Recent payout history
- Request payouts

### 3. Guest Features
#### Wishlist
- Save favorite properties
- Remove from wishlist
- Quick access to saved properties
- Empty state with call-to-action

#### Wallet
- View balance
- Track transactions
- Add funds
- Withdraw earnings
- Monthly spending/earning stats
- Payment method management

#### Reviews
- View all reviews
- Overall rating summary
- Detailed review categories (cleanliness, accuracy, communication)
- Host responses
- Review history

### 4. Enhanced Components
#### UI Components
- **Skeletons** - Loading states for better UX
  - PropertyCardSkeleton
  - BookingCardSkeleton
  - ProfileSkeleton
  - ListSkeleton
- **ErrorBoundary** - Error handling and recovery
- **ErrorMessage** - Consistent error display

#### Property Components
- Enhanced PropertyCard with wishlist support
- Heart icon for saved properties
- Remove from wishlist functionality

### 5. Extended API Integration

#### New API Hooks
```typescript
// Wishlist
useWishlist()
useAddToWishlist()
useRemoveFromWishlist()

// Host - Properties
useHostProperties()
useCreateProperty()
useUpdateProperty()
useDeleteProperty()

// Host - Bookings & Earnings
useHostBookings()
useHostEarnings()

// Wallet
useWalletBalance()
useTransactions()
useWithdrawFunds()

// Reviews
usePropertyReviews(propertyId)

// User
useUpdateProfile()
```

#### New API Client Methods
- `getWishlist()`, `addToWishlist()`, `removeFromWishlist()`
- `getHostProperties()`, `createProperty()`, `updateProperty()`, `deleteProperty()`
- `getHostBookings()`, `getHostEarnings()`
- `getWalletBalance()`, `getTransactions()`, `withdrawFunds()`
- `getPropertyReviews()`, `updateUserProfile()`

## Architecture Improvements

### 1. Navigation Structure
- Matches web app's routing patterns
- Intuitive tab-based navigation
- Consistent back navigation
- Deep linking support for property details

### 2. State Management
- React Query for server state
- Zustand for local state (via context)
- Automatic cache invalidation
- Optimistic updates for better UX

### 3. UI/UX Enhancements
- Brand colors (Moss Green #3A5C50, Safari Gold #D9B168)
- Consistent spacing and typography
- Modern card-based layouts
- Loading skeletons instead of spinners
- Empty states with helpful messages
- Error boundaries for graceful failures

### 4. Mobile-First Patterns
- Touch-optimized interactions
- Native mobile navigation patterns
- Responsive layouts
- Optimized for small screens
- Fast navigation transitions

## Backend Configuration

### ALLOWED_HOSTS
The backend already includes all necessary domains in ALLOWED_HOSTS:
```python
ALLOWED_HOSTS = [
    'localhost',
    '127.0.0.1',
    'backend',
    'api.zimlegend.online',
    'zimlegend.online',
    'stayafrica.app',
    'www.stayafrica.app',
    'api.stayafrica.app'
]
```

### API Endpoints
Base URLs:
- **Local Development**: `http://localhost:8000/api/v1`
- **Production**: `https://api.stayafrica.app/api/v1`
- **Alternative**: `https://api.zimlegend.online/api/v1`

## Setup Instructions

### 1. Environment Configuration
```bash
# Copy the example environment file
cp .env.example .env

# Update the API URL for your environment
# For local development:
EXPO_PUBLIC_API_BASE_URL=http://localhost:8000/api/v1

# For production:
# EXPO_PUBLIC_API_BASE_URL=https://api.stayafrica.app/api/v1
```

### 2. Install Dependencies
```bash
npm install
```

### 3. Start Development Server
```bash
# Start Expo dev server
npm start

# Run on iOS
npm run ios

# Run on Android
npm run android
```

### 4. Backend Connection
Ensure the backend is running and accessible:
```bash
# Test API connectivity
curl http://localhost:8000/api/v1/properties/

# Or for production
curl https://api.stayafrica.app/api/v1/properties/
```

## Screen Flow Diagrams

### Guest Flow
```
Explore → Property Detail → Booking Flow → Payment → Confirmation
   ↓                           ↓
Wishlist                    Bookings List
```

### Host Flow
```
Host Dashboard → Properties List → Property Detail → Edit Property
      ↓              ↓
   Bookings       Add Property
      ↓
   Earnings
```

## File Structure
```
mobile/
├── app/
│   ├── (auth)/              # Authentication screens
│   ├── (tabs)/              # Main tab navigation
│   │   ├── explore/         # Property browsing
│   │   ├── wishlist/        # Saved properties
│   │   ├── bookings/        # User bookings
│   │   ├── host/            # Host dashboard
│   │   ├── profile/         # User profile
│   │   └── wallet/          # Payments & wallet
│   ├── host/                # Host features
│   │   ├── properties/      # Property management
│   │   ├── bookings/        # Booking management
│   │   └── earnings/        # Earnings tracking
│   └── reviews/             # Reviews screen
├── src/
│   ├── components/
│   │   ├── common/          # Shared components
│   │   │   ├── ErrorBoundary.tsx
│   │   │   ├── Skeletons.tsx
│   │   │   └── LoadingStates.tsx
│   │   ├── property/        # Property components
│   │   └── booking/         # Booking components
│   ├── hooks/
│   │   └── api-hooks.ts     # Extended with new hooks
│   ├── services/
│   │   └── api-client.ts    # Extended with new methods
│   ├── context/             # React context providers
│   └── types/               # TypeScript definitions
└── .env.example             # Updated with backend URLs
```

## Testing Checklist

### Pre-Release Testing
- [ ] Test authentication flow (login, register, logout)
- [ ] Test property browsing and search
- [ ] Test wishlist (add, remove, view)
- [ ] Test property detail view
- [ ] Test bookings list and details
- [ ] Test host dashboard navigation
- [ ] Test host property management
- [ ] Test host bookings view
- [ ] Test earnings display
- [ ] Test wallet functionality
- [ ] Test reviews display
- [ ] Test error boundaries
- [ ] Test loading states
- [ ] Test offline behavior
- [ ] Test on iOS simulator
- [ ] Test on Android emulator
- [ ] Test on physical devices

### API Integration Testing
- [ ] Verify all API endpoints
- [ ] Test authentication tokens
- [ ] Test token refresh flow
- [ ] Test error handling
- [ ] Test network timeouts
- [ ] Test pagination
- [ ] Test search and filters

## Performance Optimizations

### Implemented
- React Query caching
- Image lazy loading
- List virtualization with FlatList
- Debounced search inputs
- Optimistic updates
- Skeleton loaders

### Future Improvements
- Image optimization and compression
- Offline-first architecture
- Background sync
- Push notifications
- Deep linking
- Analytics integration

## Known Limitations

### To Be Implemented
- Property creation form
- Property editing interface
- Booking creation flow
- Payment integration (Stripe)
- Image upload functionality
- Real-time messaging
- Push notifications
- Calendar availability
- Map integration with Mapbox
- Profile editing screens

## Development Notes

### Brand Colors
```typescript
primary: '#3A5C50'      // Moss Green
secondary: '#D9B168'    // Safari Gold
background: '#F4F1EA'   // Ivory Sand
accent: '#122F26'       // Deep Forest
text: '#0A1A15'         // Savanna Text
```

### API Response Handling
All API calls follow a consistent pattern:
```typescript
const { data, isLoading, error, refetch } = useHook();
```

### Error Handling
Three levels of error handling:
1. **Component Level** - ErrorMessage component
2. **Screen Level** - Try-catch with user feedback
3. **App Level** - ErrorBoundary wrapper

## Contributing

### Code Style
- Use TypeScript strict mode
- Follow existing component patterns
- Use NativeWind (Tailwind) for styling
- Write descriptive component names
- Add JSDoc comments for complex functions

### Pull Request Checklist
- [ ] Code follows style guidelines
- [ ] Components are properly typed
- [ ] No console errors
- [ ] Tested on iOS and Android
- [ ] Screenshots included for UI changes
- [ ] API changes documented

## Support

### Resources
- [Expo Documentation](https://docs.expo.dev/)
- [React Navigation](https://reactnavigation.org/)
- [React Query](https://tanstack.com/query/latest)
- [NativeWind](https://www.nativewind.dev/)

### Common Issues

**Issue**: "Metro bundler can't find module"
**Solution**: Clear cache with `npm start -- --clear`

**Issue**: "API connection timeout"
**Solution**: Check backend is running and API_BASE_URL is correct

**Issue**: "Authentication fails"
**Solution**: Verify tokens are being stored in SecureStore

## Version History

### v2.0.0 (Current)
- Added host dashboard and features
- Added wishlist functionality
- Added wallet/payments screen
- Added reviews screen
- Extended API integration
- Added UI enhancement components
- Updated navigation structure
- Improved error handling

### v1.0.0
- Initial mobile scaffolding
- Basic authentication
- Property browsing
- Booking management
- Messaging interface
- User profile

---

**Built with React Native + Expo for iOS and Android**

Perfect for discovering and booking properties on the go! 🏠✈️
