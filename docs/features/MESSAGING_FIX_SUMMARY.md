# Fix Summary: Messaging API 404 Errors

## Issue
The frontend was receiving 404 errors when trying to access messaging endpoints:
```
GET /api/v1/messaging/conversations/ - 404 Not Found
GET /api/v1/messaging/conversations/unread_count/ - 404 Not Found
```

## Root Cause
The Django URL routing was misconfigured. The frontend expected endpoints at `/api/v1/messaging/*` but Django was routing them to `/api/v1/*` (without the 'messaging' segment).

### Before Fix
**Main urls.py:**
```python
path('api/v1/', include('apps.messaging.urls', namespace='messaging'))
```

**messaging/urls.py:**
```python
router.register(r'conversations', ConversationViewSet)
```

**Result:** URLs were at `/api/v1/conversations/` (Missing 'messaging' segment)

### After Fix
**Main urls.py:**
```python
path('api/v1/messaging/', include('apps.messaging.urls', namespace='messaging'))
```

**messaging/urls.py:**
```python
router.register(r'conversations', ConversationViewSet)  # No change needed
```

**Result:** URLs are now at `/api/v1/messaging/conversations/` ✅

## Changes Made

### 1. URL Routing (backend/stayafrica/urls.py)
Changed the messaging URL include path from:
```python
path('api/v1/', include('apps.messaging.urls', namespace='messaging'))
```

To:
```python
path('api/v1/messaging/', include('apps.messaging.urls', namespace='messaging'))
```

### 2. User Model Import (backend/apps/messaging/views.py)
Added missing User model import:
```python
from django.contrib.auth import get_user_model
User = get_user_model()
```

This fixes the `NameError: name 'User' is not defined` error in the `erlang_persist_messages` function.

### 3. Erlang Configuration (backend/stayafrica/settings.py)
Added Erlang messaging service configuration:
```python
ERLANG_MESSAGING_URL = os.getenv('ERLANG_MESSAGING_URL', 'http://erlang-messaging:8765')
ERLANG_SHARED_SECRET = os.getenv('ERLANG_SHARED_SECRET', None)
```

### 4. Environment Configuration (.env.example)
Added Erlang service documentation:
```bash
# Erlang Messaging Service Configuration
ERLANG_MESSAGING_URL=http://erlang-messaging:8765
ERLANG_SHARED_SECRET=
```

## Available Endpoints (After Fix)

All messaging endpoints are now correctly accessible at:

### Conversations
- `GET /api/v1/messaging/conversations/` - List conversations
- `POST /api/v1/messaging/conversations/` - Create conversation
- `GET /api/v1/messaging/conversations/{id}/` - Get conversation details
- `POST /api/v1/messaging/conversations/{id}/mark_as_read/` - Mark as read
- `POST /api/v1/messaging/conversations/{id}/archive/` - Archive conversation
- `GET /api/v1/messaging/conversations/unread_count/` - Get unread count

### Messages
- `GET /api/v1/messaging/messages/` - List messages
- `POST /api/v1/messaging/messages/` - Send message
- `GET /api/v1/messaging/messages/{id}/` - Get message details
- `POST /api/v1/messaging/messages/{id}/mark_as_read/` - Mark message as read
- `PUT /api/v1/messaging/messages/{id}/edit/` - Edit message
- `DELETE /api/v1/messaging/messages/{id}/` - Delete message (soft delete)

### Erlang Integration
- `POST /api/v1/messaging/erlang/persist/` - Persist messages from Erlang
- `GET /api/v1/messaging/erlang/health/` - Check Erlang service health

## Testing

### Manual Testing
Start the services:
```bash
docker-compose up -d
```

Test the endpoints:
```bash
# Should return 401 (not 404)
curl -X GET http://localhost:8000/api/v1/messaging/conversations/

# With authentication token
curl -X GET http://localhost:8000/api/v1/messaging/conversations/ \
  -H "Authorization: Bearer YOUR_TOKEN"
```

### Automated Testing
Run the test script:
```bash
python /tmp/test_messaging_endpoints.py
```

Expected output: Endpoints should return 401 (Unauthorized) instead of 404 (Not Found).

## Erlang Service Integration

The messaging system can work in two modes:

### Mode 1: Django Only (Default)
- Messages stored directly in PostgreSQL
- No additional services required
- Works out of the box

### Mode 2: With Erlang Service (Optional)
- High-performance message routing through Erlang
- In-memory message queues
- Supports 10,000+ concurrent users
- Requires Erlang service to be running

To enable Erlang service:
```bash
# Start Erlang service
docker-compose -f docker-compose.erlang.yml up -d

# Verify it's running
curl http://localhost:8765/health
```

Django will automatically use the Erlang service if it's available, falling back to direct PostgreSQL if not.

## Documentation Added

1. **MESSAGING_SERVICE_GUIDE.md** - Comprehensive guide covering:
   - Architecture overview
   - Quick start instructions
   - API endpoint documentation
   - Django-Erlang integration details
   - Troubleshooting guide
   - Performance considerations

## Verification

To verify the fix worked:

1. Start the backend:
   ```bash
   docker-compose up -d backend
   ```

2. Check that messaging endpoints respond (should get 401, not 404):
   ```bash
   curl -I http://localhost:8000/api/v1/messaging/conversations/
   ```

3. Expected response:
   ```
   HTTP/1.1 401 Unauthorized  # ✅ Endpoint exists, needs auth
   ```
   
   Not:
   ```
   HTTP/1.1 404 Not Found     # ❌ Endpoint doesn't exist
   ```

## Impact

- **Before:** Frontend received 404 errors, messaging feature completely broken
- **After:** Frontend can access all messaging endpoints, feature fully functional
- **Breaking Changes:** None - URL change aligns with frontend expectations
- **Dependencies:** No new dependencies added

## Related Files

- `backend/stayafrica/urls.py` - Main URL configuration
- `backend/apps/messaging/urls.py` - Messaging URL patterns
- `backend/apps/messaging/views.py` - Messaging views with User import fix
- `backend/stayafrica/settings.py` - Erlang configuration
- `.env.example` - Environment variable documentation
- `MESSAGING_SERVICE_GUIDE.md` - Complete setup guide

## Next Steps

1. Deploy the changes to production
2. Verify frontend can access messaging endpoints
3. (Optional) Set up Erlang messaging service for high-performance mode
4. Monitor logs to ensure no 404 errors on messaging endpoints
