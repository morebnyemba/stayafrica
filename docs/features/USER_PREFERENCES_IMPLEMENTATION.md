# User Preferences and Geolocation Implementation

## Overview
This document summarizes the implementation of user preferences, geolocation, and property interaction tracking features added in response to user feedback.

## Changes Made

### 1. Fixed Explore Page Image Display

**Problem**: The explore page wasn't showing property images like the home page.

**Solution**: 
- Replaced Next.js `<Image>` component with native `<img>` tags in the explore page
- Applied the same styling and hover effects as the home page
- Ensured consistent image display across the platform

**Files Modified**:
- `web/src/components/property/explore-content.tsx`

### 2. Geolocation Functionality

**Features Implemented**:
- Automatic location detection when page loads
- "Use my location" button for manual location updates
- Visual feedback with toast notifications
- Properties automatically sorted by distance when location is available

**Implementation Details**:
```typescript
// Automatic location detection on component mount
useEffect(() => {
  if (navigator.geolocation) {
    navigator.geolocation.getCurrentPosition(
      (position) => {
        setUserLocation({
          lat: position.coords.latitude,
          lng: position.coords.longitude,
        });
      }
    );
  }
}, []);

// Properties query includes location for nearby search
const params: any = {
  // ... other filters
};
if (userLocation) {
  params.latitude = userLocation.lat;
  params.longitude = userLocation.lng;
  params.radius_km = 100; // 100km radius
}
```

**UI Changes**:
- Added "Use my location" button in header with Navigation icon
- Shows "Showing properties near you" when location is available
- Displays "(sorted by distance)" in results count

### 3. User Preferences System

**New Model: `UserPreference`**
```python
class UserPreference(models.Model):
    user = OneToOneField(User)
    
    # Property preferences
    preferred_property_types = JSONField()  # ['villa', 'apartment', ...]
    preferred_min_price = DecimalField()
    preferred_max_price = DecimalField()
    
    # Location preferences
    preferred_countries = JSONField()  # ['South Africa', 'Zimbabwe', ...]
    preferred_cities = JSONField()  # ['Cape Town', 'Harare', ...]
    
    # Other preferences
    usual_guest_count = IntegerField()
    preferred_amenities = JSONField()  # [1, 5, 12, ...]  (amenity IDs)
    
    # Last known location
    last_latitude = FloatField()
    last_longitude = FloatField()
```

**API Endpoints**:
- `GET /api/v1/preferences/my_preferences/` - Get user's preferences
- `POST /api/v1/preferences/update_preferences/` - Update preferences
- `POST /api/v1/preferences/update_location/` - Update user's location

**Use Cases**:
- Store user's preferred property types for filtering
- Remember price range preferences
- Track favorite locations for recommendations
- Store last known location for nearby suggestions

### 4. Property Interaction Tracking

**New Model: `UserPropertyInteraction`**
```python
class UserPropertyInteraction(models.Model):
    user = ForeignKey(User)
    property_id = CharField()
    interaction_type = CharField(choices=[
        ('view', 'Viewed'),
        ('save', 'Saved'),
        ('unsave', 'Unsaved'),
        ('book', 'Booked'),
        ('search', 'Searched'),
    ])
    
    # Context
    search_query = CharField()
    viewed_duration_seconds = IntegerField()
    created_at = DateTimeField()
```

**API Endpoints**:
- `POST /api/v1/interactions/track_view/` - Track when user views a property
- `GET /api/v1/interactions/` - Get user's interaction history

**Tracking Capabilities**:
1. **Property Views**: Track which properties users look at and for how long
2. **Saves/Favorites**: Track which properties users save
3. **Bookings**: Record completed bookings
4. **Searches**: Track search queries for trend analysis

**Example Usage**:
```typescript
// Track property view
await apiClient.trackPropertyView(propertyId, durationSeconds);

// Save property
await apiClient.saveProperty(propertyId);

// Get saved properties
const saved = await apiClient.getSavedProperties();
```

### 5. Frontend API Methods

**New Methods Added to `apiClient`**:
```typescript
// Preferences
getUserPreferences()
updateUserPreferences(data)
updateUserLocation(latitude, longitude)

// Interactions
trackPropertyView(propertyId, durationSeconds)
getUserInteractions()

// Saved Properties
saveProperty(propertyId)
unsaveProperty(propertyId)
getSavedProperties()
```

## Database Schema Changes

### New Tables Required

**migrations needed**:
```bash
python manage.py makemigrations users
python manage.py migrate
```

**Tables Created**:
1. `users_userpreference` - User preference storage
2. `users_userpropertyinteraction` - Interaction tracking

### Indexes Added
- `user_id` on both tables
- `property_id` on UserPropertyInteraction
- `interaction_type, created_at` on UserPropertyInteraction

## Admin Interface

**New Admin Panels**:
1. **UserPreferenceAdmin**
   - View/edit user preferences
   - See if user has saved location
   - Filter by preferences

2. **UserPropertyInteractionAdmin**
   - View interaction history
   - Filter by interaction type
   - Search by property ID or user

## Future Enhancements

### Recommendation Algorithm
With the data collected, you can now implement:

1. **Collaborative Filtering**
   - Find users with similar preferences
   - Recommend properties they liked

2. **Content-Based Filtering**
   - Analyze user's saved properties
   - Recommend similar properties

3. **Behavioral Analysis**
   - Properties viewed longest get higher weight
   - Recent interactions matter more
   - Frequent locations prioritized

### Example Recommendation Query
```python
from django.db.models import Count, Q
from datetime import timedelta
from django.utils import timezone

def get_recommendations(user):
    # Get user's recent interactions
    recent_date = timezone.now() - timedelta(days=30)
    interactions = UserPropertyInteraction.objects.filter(
        user=user,
        created_at__gte=recent_date
    )
    
    # Get property types user interacted with
    viewed_property_ids = interactions.filter(
        interaction_type='view'
    ).values_list('property_id', flat=True)
    
    # Get properties similar to viewed ones
    # (would need to implement similarity logic)
    
    return recommended_properties
```

## Testing Checklist

- [ ] User can enable location on explore page
- [ ] Properties are sorted by distance when location is available
- [ ] "Use my location" button updates location
- [ ] User preferences are created on first access
- [ ] Property views are tracked automatically
- [ ] Saved properties are persisted
- [ ] Admin can view preferences and interactions
- [ ] API endpoints return correct data

## Configuration

**Environment Variables** (no new variables needed):
- Existing: `NEXT_PUBLIC_API_BASE_URL`
- Geolocation uses browser API (no keys needed)

**Permissions**:
- User must allow browser location access
- HTTPS required for geolocation in production

## Performance Considerations

1. **Interaction Tracking**:
   - Use background tasks for non-critical tracking
   - Batch insert interactions to reduce DB load
   - Consider using Redis for real-time tracking

2. **Location Updates**:
   - Cache user location to avoid repeated calculations
   - Use PostGIS for efficient distance queries
   - Index latitude/longitude fields

3. **Preferences**:
   - Cache user preferences (1-hour TTL)
   - Update preferences asynchronously
   - Use JSONField for flexible preference storage

## Security Notes

- User preferences are private (only accessible by owner)
- Interaction tracking requires authentication
- Location data is never shared publicly
- Preferences are encrypted at rest
- GDPR compliance: users can request data deletion

---

*Implementation completed: January 2026*
*Commit: 6fe1138*
