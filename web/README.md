# StayAfrica Web Frontend

Next.js 14 frontend for the StayAfrica property booking platform.

## Quick Start

### Prerequisites
- Node.js 18+ and npm/yarn
- Backend API running on `http://localhost:8000`

### Setup

1. **Install Dependencies**
   ```bash
   npm install
   ```

2. **Configure Environment**
   ```bash
   cp .env.example .env.local
   ```
   
   Update `.env.local` with:
   - `NEXT_PUBLIC_API_BASE_URL` - Backend API URL
   - `NEXT_PUBLIC_MAPBOX_TOKEN` - Mapbox token (optional)
   - `NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY` - Stripe key (optional)
   - `NEXTAUTH_SECRET` - Random secret for auth
   - `NEXTAUTH_URL` - App URL (http://localhost:3000 for dev)

3. **Start Development Server**
   ```bash
   npm run dev
   ```

   Open http://localhost:3000 in your browser

## Architecture

### Directory Structure
```
src/
├── app/              # Next.js 14 App Router pages
├── components/       # Reusable React components
├── context/          # React Context providers (Auth)
├── hooks/            # Custom React hooks (API calls)
├── services/         # API client & utilities
├── types/            # TypeScript type definitions
├── utils/            # Helper functions
└── styles/           # CSS files
```

### Key Technologies
- **Framework:** Next.js 14 (App Router)
- **Styling:** Tailwind CSS
- **Data Fetching:** React Query + Axios
- **Auth:** JWT with custom context
- **State:** Context API + Zustand ready
- **Components:** Shadcn/UI compatible

## Pages

### Public Routes
- `/` - Homepage
- `/explore` - Property search
- `/properties/[id]` - Property details
- `/login` - Login page
- `/register` - Registration page

### Protected Routes (Auth Required)
- `/dashboard` - User dashboard
- `/bookings` - My bookings
- `/messages` - Chat/messages
- `/profile` - User profile

## Components

### Common Components
- `Navigation` - Header with auth
- `Footer` - Footer with links
- `HeroSection` - Homepage hero
- `SearchSection` - Property search form
- `FeaturedProperties` - Property grid

### Custom Hooks
```typescript
// Property hooks
useProperties(filters)
useProperty(id)
useNearbyProperties(lat, lng, radius)

// Booking hooks
useBookings()
useCreateBooking()
useConfirmBooking()

// Auth hooks
useAuth() // Custom context hook
useUserProfile()

// Message hooks
useMessages()
useSendMessage()
useConversations()
```

## API Integration

The app connects to the backend at `/api/v1/`:

```typescript
import { useProperties } from '@/hooks/api-hooks';

function MyComponent() {
  const { data, isLoading } = useProperties({ city: 'Cape Town' });
  
  // Auto-loaded and cached by React Query
}
```

### Authentication
- JWT tokens stored in localStorage
- Auto-refresh on token expiry
- Logout on 401 errors
- User context available app-wide

```typescript
import { useAuth } from '@/context/auth-context';

function MyComponent() {
  const { user, login, logout, isAuthenticated } = useAuth();
}
```

## Development Commands

```bash
# Start dev server
npm run dev

# Build for production
npm run build

# Start production server
npm start

# Type checking
npm run type-check

# Format code
npm run format

# Run linter
npm run lint
```

## Environment Variables

```
NEXT_PUBLIC_API_BASE_URL=http://localhost:8000
NEXT_PUBLIC_API_VERSION=v1
NEXTAUTH_SECRET=your-secret
NEXTAUTH_URL=http://localhost:3000
NEXT_PUBLIC_MAPBOX_TOKEN=optional
NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY=optional
```

## Features Status

### Completed
- ✅ Project scaffolding
- ✅ TypeScript setup
- ✅ Tailwind CSS
- ✅ API client
- ✅ Auth context
- ✅ Page structure
- ✅ Component architecture

### In Progress
- 🚧 Feature page implementation
- 🚧 Form components
- 🚧 Payment integration

### Pending
- ❌ Mapbox integration
- ❌ Real-time messaging
- ❌ Admin dashboard

## Deployment

### Vercel (Recommended)
```bash
npm run build
# Deployment to Vercel automatic on git push
```

### Docker
```bash
docker build -t stayafrica-web .
docker run -p 3000:3000 stayafrica-web
```

### Environment for Production
```
NEXT_PUBLIC_API_BASE_URL=https://api.stayafrica.com
NEXTAUTH_URL=https://stayafrica.com
```

## Troubleshooting

### API Connection Issues
- Verify backend is running on the configured API_BASE_URL
- Check browser console for CORS errors
- Ensure JWT token is in localStorage

### Build Errors
- Clear `.next` directory: `rm -rf .next`
- Reinstall dependencies: `rm -rf node_modules && npm install`
- Check TypeScript: `npm run type-check`

### Performance
- React Query caching is enabled
- Image optimization via Next.js Image component
- Code splitting automatic with Next.js

## Contributing

1. Create a feature branch
2. Follow TypeScript strict mode
3. Use Tailwind for styling
4. Run type-check before commit: `npm run type-check`
5. Format code: `npm run format`

## License

MIT
