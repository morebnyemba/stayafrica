# Frontend Integration Blueprint
## Complete Implementation Guide for 11 Backend Features

This document provides a comprehensive blueprint for integrating all 11 backend features into the StayAfrica frontend (web and mobile).

---

## Overview

**Backend Status**: ✅ Complete (64 REST API endpoints + WebSocket)
**Frontend Status**: 🔨 In Progress (Foundation laid, components needed)

**Total Scope**: 60-80 frontend components/files across 3 phases

---

## Phase 1: Core Features (25 files) ⏳

### 1. Dynamic Pricing Integration (7 files)

**Status**: ✅ Types & API done, 🔨 Components in progress

**Files Created:**
- ✅ `types/pricing-types.ts` - Complete type definitions
- ✅ `services/pricing-api.ts` - API service
- ✅ `components/pricing/PricingCalendar.tsx` - Calendar component

**Files Needed:**
- `components/pricing/DynamicPricingDisplay.tsx` - Show dynamic price with breakdown
- `components/pricing/PriceBreakdownModal.tsx` - Detailed price calculation modal
- `components/pricing/PricingRuleIndicator.tsx` - Visual indicator for applied rules
- `hooks/useDynamicPricing.ts` - Reusable hook for pricing logic

**Integration Points:**
```tsx
// In PropertyCard.tsx
import { DynamicPricingDisplay } from '@/components/pricing';

<DynamicPricingDisplay 
  propertyId={property.id}
  checkIn={searchDates.checkIn}
  checkOut={searchDates.checkOut}
  basePrice={property.base_price}
/>

// In host dashboard
import { PricingCalendar } from '@/components/pricing';

<PricingCalendar propertyId={propertyId} />
```

**API Endpoints Used:**
- `GET /properties/{id}/pricing_calendar/`
- `GET /properties/{id}/availability/` (enhanced with dynamic pricing)
- `POST /bookings/calculate_total/`

---

### 2. Flexible Date Search (5 files)

**Status**: ✅ API extended, 🔨 Components needed

**Files Needed:**
- `components/search/FlexibleDateSearchPanel.tsx` - Date flexibility UI
- `components/search/FlexibleDateResults.tsx` - Results by date option
- `components/search/FlexibilityToggle.tsx` - Quick toggle component
- `hooks/useFlexibleSearch.ts` - Search logic hook
- Modify: `app/(main)/explore/page.tsx` - Add flexibility options

**Component Specs:**

```tsx
// FlexibleDateSearchPanel.tsx
interface FlexibleDateSearchPanelProps {
  checkIn: Date;
  checkOut: Date;
  onSearch: (flexibility: FlexibilityType, days?: number) => void;
}

// Features:
// - Radio buttons: Exact dates, ±3 days, Weekends, Month
// - Slider for flexible days (1-7 days)
// - "I'm flexible" toggle on homepage hero
```

**Integration:**
```tsx
// In explore page
const { data: results } = useFlexibleSearch({
  checkIn,
  checkOut,
  flexibility: 'flexible_days',
  days: 3,
});

// Show grouped results with price ranges
{results?.map(option => (
  <DateOption 
    dates={option.dates}
    properties={option.properties}
    priceRange={option.price_range}
  />
))}
```

**API Endpoint:**
- `GET /properties/flexible_search/`

---

### 3. Instant Booking (4 files)

**Status**: ✅ API extended, 🔨 Components needed

**Files Needed:**
- `components/booking/InstantBookButton.tsx` - One-click booking
- `components/booking/GuestQualificationBadge.tsx` - Eligibility indicator
- `components/host/InstantBookingSettings.tsx` - Host configuration
- `hooks/useInstantBooking.ts` - Booking logic

**Component Specs:**

```tsx
// InstantBookButton.tsx
interface InstantBookButtonProps {
  propertyId: string;
  checkIn: string;
  checkOut: string;
  guests: number;
  onSuccess: (booking: Booking) => void;
}

// Features:
// - Check guest qualification
// - Show "Instant Book" badge
// - One-click confirmation
// - Loading and error states

// InstantBookingSettings.tsx
// Host can configure:
// - Enable/disable instant booking
// - Require verified ID
// - Minimum reviews (0-10)
// - Minimum rating (3.0-5.0)
// - Require completed bookings
// - Require payment method
```

**Integration:**
```tsx
// In property detail
{property.instant_booking_enabled && (
  <InstantBookButton {...bookingDetails} />
)}

// In PropertyCard
{property.instant_booking_enabled && (
  <Badge>⚡ Instant Book</Badge>
)}
```

**API Endpoints:**
- `GET /properties/{id}/instant_booking_info/`
- `POST /properties/{id}/toggle_instant_booking/`

---

### 4. Local POI Display (5 files)

**Status**: ✅ Types & API done, 🔨 Components needed

**Files Created:**
- ✅ `types/poi-types.ts`
- ✅ `services/poi-api.ts`

**Files Needed:**
- `components/poi/POIMapDisplay.tsx` - Interactive map with markers
- `components/poi/POIList.tsx` - Categorized list
- `components/poi/POICategoryFilter.tsx` - Category selection
- `components/poi/POICard.tsx` - Individual POI display
- Modify: `components/property/property-details-content.tsx` - Add POI section

**Component Specs:**

```tsx
// POIMapDisplay.tsx
// Features:
// - Mapbox GL integration
// - Property marker (center)
// - POI markers (color-coded by category)
// - Click to show POI details
// - Distance circles (1km, 2km, 5km)

// POIList.tsx
interface POIListProps {
  propertyId: string;
  radiusKm?: number;
  categories?: POIType[];
}

// Features:
// - Group by category (collapsible)
// - Show distance, walking/driving time
// - Rating and price level
// - Host recommendations highlighted
// - Filter by category
```

**Integration:**
```tsx
// In property detail page
<section className="mt-8">
  <h2>What's Nearby</h2>
  <Tabs>
    <TabPanel>
      <POIMapDisplay propertyId={propertyId} />
    </TabPanel>
    <TabPanel>
      <POIList propertyId={propertyId} />
    </TabPanel>
  </Tabs>
</section>
```

**API Endpoints:**
- `GET /properties/{id}/nearby_pois/`
- `POST /properties/{id}/discover_pois/` (host only)

---

### 5. Enhanced Wishlists (6 files)

**Status**: ✅ Types & API done, 🔨 Components needed

**Files Created:**
- ✅ `types/wishlist-types.ts`
- ✅ `services/wishlist-api.ts`

**Files Needed:**
- `components/wishlist/CollaborativeWishlist.tsx` - Main interface
- `components/wishlist/WishlistItemCard.tsx` - Item with voting/comments
- `components/wishlist/WishlistVoting.tsx` - Vote UI
- `components/wishlist/WishlistComments.tsx` - Comment thread
- `components/wishlist/WishlistShare.tsx` - Share dialog
- `components/wishlist/WishlistCollaborators.tsx` - Manage collaborators

**Component Specs:**

```tsx
// CollaborativeWishlist.tsx
interface CollaborativeWishlistProps {
  wishlistId: string;
  isOwner: boolean;
  canEdit: boolean;
}

// Features:
// - Privacy toggle (private/shared/public)
// - Add/remove collaborators
// - Add properties with notes
// - Voting system
// - Comment threads
// - Share link generation
// - Preferred dates per property

// WishlistItemCard.tsx
// Shows:
// - Property image, name, price
// - Vote count with upvote/downvote buttons
// - Comment count with expand
// - Preferred dates (editable)
// - Personal notes
// - Added by user info
```

**Integration:**
```tsx
// In wishlist page
const { data: wishlist } = useQuery({
  queryKey: ['wishlist', wishlistId],
  queryFn: () => wishlistApi.getWishlist(wishlistId),
});

<CollaborativeWishlist 
  wishlistId={wishlistId}
  isOwner={wishlist.owner.id === currentUser.id}
  canEdit={isCollaborator || isOwner}
/>

// In property detail
<AddToWishlistButton propertyId={propertyId} />
```

**API Endpoints:**
- `GET/POST /wishlists/`
- `GET /wishlists/{id}/`
- `POST /wishlists/{id}/add_property/`
- `POST /wishlists/{id}/add_collaborator/`
- `POST /wishlists/{id}/vote_item/`
- `POST /wishlists/{id}/comment_item/`
- `GET /wishlists/{token}/shared/`

---

## Phase 2: Complex Features (30 files) 🔜

### 6. Push Notifications (8 files)

**Files Needed:**
- `services/firebase-config.ts` - Firebase initialization
- `services/notification-api.ts` - Notification API service
- `components/notifications/NotificationCenter.tsx` - Dropdown panel
- `components/notifications/NotificationItem.tsx` - Individual notification
- `components/notifications/NotificationPreferences.tsx` - Settings
- `hooks/usePushNotifications.ts` - FCM token management
- `hooks/useNotifications.ts` - Notification state
- `types/notification-types.ts` - Type definitions

**Setup Required:**
```bash
npm install firebase
```

**Firebase Config:**
```typescript
// firebase-config.ts
import { initializeApp } from 'firebase/app';
import { getMessaging } from 'firebase/messaging';

const firebaseConfig = {
  apiKey: process.env.NEXT_PUBLIC_FIREBASE_API_KEY,
  // ... other config
};

const app = initializeApp(firebaseConfig);
export const messaging = getMessaging(app);
```

**Component Specs:**

```tsx
// NotificationCenter.tsx
// Features:
// - Bell icon with unread count badge
// - Dropdown with recent notifications
// - Mark as read
// - Mark all as read
// - Link to notification settings
// - Deep links to related content

// NotificationPreferences.tsx
// User can toggle:
// - Booking confirmations
// - New messages
// - Payment notifications
// - Review notifications
// - Price drop alerts
// - Wishlist updates
```

**Integration:**
```tsx
// In app layout header
<NotificationCenter />

// Register FCM token on login
const { registerToken } = usePushNotifications();
useEffect(() => {
  registerToken();
}, []);
```

**API Endpoints:**
- `POST /notifications/tokens/` - Register device
- `GET /notifications/tokens/` - List tokens
- `POST /notifications/tokens/{id}/deactivate/`
- `GET /notifications/preferences/`
- `PUT /notifications/preferences/`
- `GET /notifications/notifications/`
- `GET /notifications/notifications/unread_count/`
- `POST /notifications/notifications/{id}/mark_read/`
- `POST /notifications/notifications/mark_all_read/`

---

### 7. WebSocket Messaging (7 files)

**Files Needed:**
- `services/websocket-client.ts` - WebSocket connection manager
- `components/messaging/RealTimeChatInterface.tsx` - Main chat UI
- `components/messaging/TypingIndicator.tsx` - "User is typing..."
- `components/messaging/ReadReceipts.tsx` - Message read status
- `components/messaging/MessageBubble.tsx` - Individual message
- `hooks/useWebSocket.ts` - WebSocket connection hook
- `hooks/useRealTimeChat.ts` - Chat state management

**WebSocket Client:**
```typescript
// websocket-client.ts
class WebSocketClient {
  private ws: WebSocket | null = null;
  
  connect(token: string) {
    const wsUrl = `${WS_BASE_URL}/ws/chat/`;
    this.ws = new WebSocket(`${wsUrl}?token=${token}`);
    
    this.ws.onmessage = (event) => {
      const data = JSON.parse(event.data);
      this.handleMessage(data);
    };
  }
  
  sendMessage(conversationId: number, text: string) {
    this.ws?.send(JSON.stringify({
      type: 'chat_message',
      conversation_id: conversationId,
      text: text,
    }));
  }
  
  sendTypingIndicator(conversationId: number, isTyping: boolean) {
    this.ws?.send(JSON.stringify({
      type: 'typing_indicator',
      conversation_id: conversationId,
      is_typing: isTyping,
    }));
  }
  
  sendReadReceipt(messageId: number) {
    this.ws?.send(JSON.stringify({
      type: 'read_receipt',
      message_id: messageId,
    }));
  }
}
```

**Component Specs:**
```tsx
// RealTimeChatInterface.tsx
// Features:
// - Real-time message delivery
// - Typing indicators
// - Read receipts
// - Message timestamps
// - Auto-scroll to bottom
// - Reconnection handling
// - Fallback to REST API

// TypingIndicator.tsx
// Shows: "John is typing..."
// Disappears after 3 seconds of inactivity
```

**Integration:**
```tsx
// In messages page
const { messages, sendMessage, isTyping } = useRealTimeChat(conversationId);

<RealTimeChatInterface 
  conversationId={conversationId}
  messages={messages}
  onSendMessage={sendMessage}
/>
```

**WebSocket Endpoint:**
- `ws://host/ws/chat/`

**Message Types:**
- `chat_message` - Send message
- `typing_indicator` - Typing status
- `read_receipt` - Mark as read
- `join_conversation` - Join room
- `leave_conversation` - Leave room

---

### 8. ID Verification (7 files)

**Files Needed:**
- `components/verification/VerificationWizard.tsx` - Multi-step upload
- `components/verification/DocumentUpload.tsx` - Document picker
- `components/verification/SelfieCapture.tsx` - Selfie camera
- `components/verification/VerificationStatus.tsx` - Status tracker
- `components/verification/VerificationBadge.tsx` - Verified indicator
- `services/verification-api.ts` - API service
- `types/verification-types.ts` - Type definitions

**Component Specs:**

```tsx
// VerificationWizard.tsx
// Steps:
// 1. Choose document type (passport, ID, license)
// 2. Upload document (front)
// 3. Upload document back (if applicable)
// 4. Take selfie
// 5. Review and submit

// DocumentUpload.tsx
// Features:
// - Drag and drop
// - File picker
// - Image preview
// - Validation (format, size)
// - Crop/rotate tools

// SelfieCapture.tsx
// Features:
// - Camera access
// - Live preview
// - Face detection guidance
// - Retake option
```

**Integration:**
```tsx
// In profile settings
<VerificationWizard 
  onComplete={() => {
    toast.success('Verification submitted!');
    refetch();
  }}
/>

// In user profile
<VerificationBadge 
  status={user.verification_status}
  verifiedAt={user.verified_at}
/>
```

**API Endpoints:**
- `POST /users/verification/` - Submit verification
- `GET /users/verification/current_status/` - Check status
- `GET /users/verification/` - List verifications
- `POST /users/verification/{id}/review/` - Admin review
- `GET /users/verification/pending_reviews/` - Admin queue
- `GET /users/verification/statistics/` - Admin stats

---

### 9. Tax Display (3 files)

**Files Needed:**
- `components/booking/TaxBreakdown.tsx` - Tax details
- `components/host/TaxSummary.tsx` - Host tax report
- `services/tax-api.ts` - Tax API service

**Component Specs:**

```tsx
// TaxBreakdown.tsx
interface TaxBreakdownProps {
  booking: Booking;
}

// Shows:
// - Base price
// - Cleaning fee
// - Service fee
// - Tax breakdown by jurisdiction
//   - Occupancy tax (10%)
//   - Sales tax (8%)
//   - Tourism fee ($5)
// - Total amount

// TaxSummary.tsx
// For hosts:
// - Period selection
// - Total tax collected
// - Breakdown by jurisdiction
// - Booking count
// - Export to CSV
```

**Integration:**
```tsx
// In booking confirmation
<PriceBreakdown booking={booking}>
  <TaxBreakdown booking={booking} />
</PriceBreakdown>

// In host dashboard
<TaxSummary 
  startDate="2026-01-01"
  endDate="2026-12-31"
/>
```

**API Endpoints:**
- `GET /payments/tax/booking-taxes/` - View booking taxes
- `GET /payments/tax/reports/host_summary/` - Tax summary

---

### 10. Automated Messaging (5 files)

**Files Needed:**
- `components/host/AutomatedMessageSettings.tsx` - Configure auto-responses
- `components/host/QuickRepliesManager.tsx` - Manage shortcuts
- `components/host/ScheduledMessagesPanel.tsx` - Schedule messages
- `components/messaging/QuickReplyButton.tsx` - Use quick reply
- `services/automated-messaging-api.ts` - API service

**Component Specs:**

```tsx
// AutomatedMessageSettings.tsx
// Features:
// - Toggle auto-responses
// - Configure triggers:
//   - Booking inquiry
//   - Booking confirmation
//   - Check-in reminder (24h before)
//   - Check-out reminder
//   - Review request (3 days after)
// - Message templates with variables
// - Delay configuration
// - Away mode

// QuickRepliesManager.tsx
// Features:
// - Create quick replies
// - Shortcut (e.g., /wifi, /checkin)
// - Message text
// - Usage stats
// - Edit/delete

// ScheduledMessagesPanel.tsx
// Features:
// - Schedule message
// - Select conversation
// - Date/time picker
// - Preview
// - Cancel scheduled
```

**Integration:**
```tsx
// In host settings
<Tabs>
  <TabPanel>
    <AutomatedMessageSettings />
  </TabPanel>
  <TabPanel>
    <QuickRepliesManager />
  </TabPanel>
  <TabPanel>
    <ScheduledMessagesPanel />
  </TabPanel>
</Tabs>

// In chat interface
<QuickReplyButton 
  onSelect={(reply) => insertText(reply.message_text)}
/>
```

**API Endpoints:**
- `GET/PUT /messaging/settings/`
- `GET/POST /messaging/automated-messages/`
- `GET/POST /messaging/scheduled-messages/`
- `POST /messaging/scheduled-messages/{id}/cancel/`
- `GET/POST /messaging/quick-replies/`
- `POST /messaging/quick-replies/{id}/use/`
- `GET /messaging/analytics/`
- `GET /messaging/analytics/summary/`

---

## Phase 3: Analytics Dashboard (15 files) 🔜

### 11. Host Analytics Dashboard (15 files)

**Files Needed:**
- `components/analytics/AnalyticsDashboard.tsx` - Main dashboard
- `components/analytics/RevenueChart.tsx` - Revenue chart (Recharts)
- `components/analytics/OccupancyTrendChart.tsx` - Occupancy chart
- `components/analytics/ConversionMetrics.tsx` - Conversion stats
- `components/analytics/PropertyPerformance.tsx` - Per-property stats
- `components/analytics/RevenuProjections.tsx` - Forecast chart
- `components/analytics/BenchmarkComparison.tsx` - Market comparison
- `components/analytics/InsightsPanel.tsx` - AI insights
- `components/analytics/MetricCard.tsx` - Stat card
- `components/analytics/PeriodSelector.tsx` - Date range picker
- `components/analytics/ExportButton.tsx` - Export data
- `services/analytics-api.ts` - API service
- `types/analytics-types.ts` - Type definitions
- `hooks/useAnalytics.ts` - Analytics state
- Modify: `app/(main)/host/page.tsx` - Add analytics section

**Component Specs:**

```tsx
// AnalyticsDashboard.tsx
interface AnalyticsDashboardProps {
  period: 'daily' | 'weekly' | 'monthly' | 'yearly';
}

// Sections:
// 1. Summary Cards
//    - Total revenue
//    - Total bookings
//    - Avg occupancy rate
//    - Avg rating
// 2. Revenue Chart (line chart)
// 3. Occupancy Trend (area chart)
// 4. Property Performance Table
// 5. Revenue Projections (forecast)
// 6. Benchmark Comparison
// 7. Insights & Recommendations

// RevenueChart.tsx
// Using Recharts:
// - Line chart with data points
// - Tooltip with details
// - Legend
// - Responsive
// - Export to image

// InsightsPanel.tsx
// Shows AI-generated insights:
// - "Your average price is 15% below market"
// - "Enable instant booking to increase conversions"
// - "Your occupancy is low for summer - consider promotions"
// - "Respond faster to inquiries (current: 4h, target: 2h)"
```

**Integration:**
```tsx
// In host dashboard
<AnalyticsDashboard period="monthly" />

// API usage
const { data: dashboard } = useQuery({
  queryKey: ['analytics', 'dashboard', period],
  queryFn: () => analyticsApi.getDashboard(period),
});
```

**API Endpoints:**
- `GET /properties/analytics/host/dashboard/`
- `GET /properties/analytics/host/revenue_chart/`
- `GET /properties/analytics/host/occupancy_trend/`
- `POST /properties/analytics/host/generate_projections/`
- `GET /properties/analytics/properties/`
- `GET /properties/analytics/benchmarks/`

**Charts Library:**
Already installed: `recharts@2.15.4`

**Example Chart:**
```tsx
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend } from 'recharts';

<LineChart width={600} height={300} data={revenueData}>
  <CartesianGrid strokeDasharray="3 3" />
  <XAxis dataKey="date" />
  <YAxis />
  <Tooltip />
  <Legend />
  <Line type="monotone" dataKey="revenue" stroke="#8884d8" />
</LineChart>
```

---

## Mobile App Integration

All components should have mobile equivalents in `/mobile/src/components/`.

**Mobile-Specific Considerations:**
1. **Push Notifications**: Use Expo Notifications
2. **Camera**: Use Expo Camera for selfie capture
3. **Maps**: Use React Native Maps
4. **WebSocket**: Use same WebSocket client
5. **File Upload**: Use Expo Image Picker
6. **Charts**: Use react-native-chart-kit or Victory Native

**Mobile Files Structure:**
```
mobile/src/
├── components/
│   ├── pricing/
│   ├── search/
│   ├── booking/
│   ├── poi/
│   ├── wishlist/
│   ├── notifications/
│   ├── messaging/
│   ├── verification/
│   └── analytics/
├── services/
│   ├── pricing-api.ts (same as web)
│   ├── wishlist-api.ts (same as web)
│   └── ...
└── types/ (shared with web)
```

---

## Implementation Checklist

### Phase 1: Core Features ⏳
- [x] Dynamic Pricing types & API
- [x] Wishlist types & API
- [x] POI types & API
- [x] Extended API client
- [x] PricingCalendar component
- [ ] Remaining pricing components (3)
- [ ] Flexible search components (5)
- [ ] Instant booking components (4)
- [ ] POI components (5)
- [ ] Wishlist components (6)

### Phase 2: Complex Features 🔜
- [ ] Push notifications (8 files)
- [ ] WebSocket messaging (7 files)
- [ ] ID verification (7 files)
- [ ] Tax display (3 files)
- [ ] Automated messaging (5 files)

### Phase 3: Analytics 🔜
- [ ] Analytics dashboard (15 files)

---

## Development Guidelines

### Code Standards
1. **TypeScript**: All components fully typed
2. **Error Handling**: Loading, error, empty states
3. **Accessibility**: ARIA labels, keyboard navigation
4. **Responsive**: Mobile-first design
5. **Performance**: React Query caching, lazy loading

### Component Pattern
```tsx
'use client';

import React from 'react';
import { useQuery } from '@tanstack/react-query';

interface ComponentProps {
  // Props
}

export default function Component({ prop }: ComponentProps) {
  // Hooks
  const { data, isLoading, error } = useQuery({
    queryKey: ['key'],
    queryFn: () => api.fetch(),
  });

  // Handlers
  const handleAction = () => {
    // Logic
  };

  // Loading state
  if (isLoading) return <LoadingSkeleton />;

  // Error state
  if (error) return <ErrorMessage error={error} />;

  // Empty state
  if (!data) return <EmptyState />;

  // Main render
  return (
    <div className="...">
      {/* Content */}
    </div>
  );
}
```

### API Service Pattern
```typescript
export const featureApi = {
  async getResource(id: string) {
    const response = await apiClient.get<ResourceType>(`/resource/${id}/`);
    return response.data;
  },

  async createResource(data: CreateRequest) {
    const response = await apiClient.post<ResourceType>('/resource/', data);
    return response.data;
  },
};
```

---

## Testing Strategy

### Unit Tests
- Component rendering
- User interactions
- API mocking
- Error scenarios

### Integration Tests
- Feature flows
- Multi-component interactions
- API integration

### E2E Tests
- Critical user journeys
- Booking flow
- Messaging flow
- Verification flow

---

## Deployment Checklist

### Environment Variables
```env
# Firebase (Push Notifications)
NEXT_PUBLIC_FIREBASE_API_KEY=
NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN=
NEXT_PUBLIC_FIREBASE_PROJECT_ID=
NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID=
NEXT_PUBLIC_FIREBASE_APP_ID=

# WebSocket
NEXT_PUBLIC_WS_BASE_URL=ws://localhost:8000

# Maps
NEXT_PUBLIC_MAPBOX_TOKEN=
```

### Dependencies to Install
```bash
# Firebase
npm install firebase

# WebSocket (already using native WebSocket)

# No additional dependencies needed for other features
```

### Backend Setup
1. Run migrations
2. Configure Redis for WebSocket
3. Upload Firebase credentials JSON
4. Configure media storage for verification
5. Start Daphne for WebSocket: `daphne stayafrica.asgi:application`

---

## Performance Optimization

### Code Splitting
```tsx
// Lazy load heavy components
const AnalyticsDashboard = lazy(() => import('@/components/analytics/AnalyticsDashboard'));
const POIMapDisplay = lazy(() => import('@/components/poi/POIMapDisplay'));
```

### Image Optimization
```tsx
// Use Next.js Image component
import Image from 'next/image';

<Image 
  src={property.image_url}
  alt={property.name}
  width={400}
  height={300}
  loading="lazy"
/>
```

### API Caching
```tsx
// React Query stale time
const { data } = useQuery({
  queryKey: ['properties'],
  queryFn: fetchProperties,
  staleTime: 5 * 60 * 1000, // 5 minutes
});
```

---

## Support & Resources

**Documentation:**
- [Next.js Docs](https://nextjs.org/docs)
- [React Query Docs](https://tanstack.com/query/latest)
- [Tailwind CSS](https://tailwindcss.com/docs)
- [Recharts](https://recharts.org/en-US/)

**Backend API:**
- API Base URL: `/api/v1/`
- WebSocket URL: `/ws/chat/`
- Total Endpoints: 64 REST + 1 WebSocket

**Project Status:**
- Backend: ✅ 97% feature parity achieved
- Frontend: 🔨 Foundation laid, integration in progress
- Target: Complete unified platform experience

---

## Next Steps

1. **Complete Phase 1** (remaining 18 components)
2. **Start Phase 2** (complex features)
3. **Implement Phase 3** (analytics dashboard)
4. **Mobile App** (parallel development)
5. **Testing** (unit, integration, E2E)
6. **Documentation** (user guides, API docs)
7. **Deployment** (staging → production)

---

**Last Updated**: January 16, 2026
**Status**: Phase 1 in progress (7/25 files complete)
**Estimated Completion**: 2-3 weeks for full integration
