# Messaging Service Setup Guide

## Overview

StayAfrica uses a hybrid messaging architecture:
- **Django REST API**: Primary messaging endpoints for the frontend
- **Erlang/OTP Service** (Optional): High-performance real-time messaging service

The frontend interacts with Django's messaging endpoints at `/api/v1/messaging/`, which can optionally route messages through the Erlang service for enhanced performance and real-time capabilities.

## Architecture

```
┌──────────────┐     ┌──────────────────┐     ┌──────────────────┐
│   Frontend   │────▶│  Django Backend  │────▶│ Erlang Service   │
│  (Next.js)   │     │  (REST API)      │     │ (Optional)       │
└──────────────┘     └──────────────────┘     └──────────────────┘
                             │                         │
                             ▼                         ▼
                     ┌──────────────────┐     ┌──────────────┐
                     │   PostgreSQL     │     │  In-Memory   │
                     │   (Persistence)  │     │   Queues     │
                     └──────────────────┘     └──────────────┘
```

## Quick Start

### Option 1: Django Only (Basic Messaging)

The messaging endpoints will work without the Erlang service, storing all messages directly in PostgreSQL:

```bash
# Start the basic services
docker-compose up -d db redis backend
```

The following endpoints will be available:
- `http://localhost:8000/api/v1/messaging/conversations/` - List/create conversations
- `http://localhost:8000/api/v1/messaging/conversations/{id}/` - Conversation details
- `http://localhost:8000/api/v1/messaging/conversations/unread_count/` - Get unread count
- `http://localhost:8000/api/v1/messaging/messages/` - List/create messages

### Option 2: With Erlang Service (High Performance)

For production or high-traffic scenarios, start the Erlang messaging service:

```bash
# Create the network if it doesn't exist
docker network create stayafrica-network 2>/dev/null || true

# Start all services including Erlang
docker-compose up -d
docker-compose -f docker-compose.erlang.yml up -d
```

## Configuration

### Environment Variables

Add these to your `.env` file (see `.env.example`):

```bash
# Erlang Messaging Service
ERLANG_MESSAGING_URL=http://erlang-messaging:8765
ERLANG_SHARED_SECRET=  # Optional, for added security
```

### Django Settings

The Django backend automatically configures Erlang integration when the environment variables are set. See `backend/stayafrica/settings.py`:

```python
ERLANG_MESSAGING_URL = os.getenv('ERLANG_MESSAGING_URL', 'http://erlang-messaging:8765')
ERLANG_SHARED_SECRET = os.getenv('ERLANG_SHARED_SECRET', None)
```

## API Endpoints

### Conversations

**List Conversations**
```http
GET /api/v1/messaging/conversations/
Authorization: Bearer <token>
```

**Create Conversation**
```http
POST /api/v1/messaging/conversations/
Authorization: Bearer <token>
Content-Type: application/json

{
  "participants": [1, 2],
  "property": 1,
  "booking": null,
  "subject": "Inquiry about property"
}
```

**Get Unread Count**
```http
GET /api/v1/messaging/conversations/unread_count/
Authorization: Bearer <token>
```

**Archive Conversation**
```http
POST /api/v1/messaging/conversations/{id}/archive/
Authorization: Bearer <token>
```

### Messages

**List Messages**
```http
GET /api/v1/messaging/messages/?conversation={conversation_id}
Authorization: Bearer <token>
```

**Send Message**
```http
POST /api/v1/messaging/messages/
Authorization: Bearer <token>
Content-Type: application/json

{
  "conversation": 1,
  "receiver": 2,
  "text": "Hello, I'm interested in your property",
  "message_type": "text"
}
```

**Mark Message as Read**
```http
POST /api/v1/messaging/messages/{id}/mark_as_read/
Authorization: Bearer <token>
```

### Erlang Service Health

**Check Erlang Service Status**
```http
GET /api/v1/messaging/erlang/health/
Authorization: Bearer <token>
```

## How Django-Erlang Integration Works

1. **Message Creation**: When a message is created via Django API:
   - Message is saved to PostgreSQL database
   - If Erlang service is available, message is routed through Erlang for real-time delivery
   - If Erlang is unavailable, operation continues gracefully (fallback mode)

2. **Real-time Delivery** (with Erlang):
   - Erlang maintains in-memory message queues per user
   - Messages are prioritized (high, normal, low)
   - Erlang handles concurrent connections efficiently
   - Messages are batched back to Django for persistence

3. **Fallback Mode** (without Erlang):
   - All messages stored directly in PostgreSQL
   - Frontend polls for updates every 5-10 seconds
   - Full functionality maintained, slightly higher latency

## Starting Services

### Development (Local)

```bash
# 1. Start PostgreSQL and Redis
docker-compose up -d db redis

# 2. Start Django backend
cd backend
python manage.py runserver

# 3. (Optional) Start Erlang service
docker-compose -f docker-compose.erlang.yml up -d

# 4. Start frontend
cd web
npm run dev
```

### Production (Docker Compose)

```bash
# Start all services
docker-compose up -d

# Start Erlang messaging service
docker-compose -f docker-compose.erlang.yml up -d

# Verify all services are running
docker-compose ps
docker-compose -f docker-compose.erlang.yml ps
```

## Troubleshooting

### Issue: 404 on messaging endpoints

**Symptom**: Frontend receives 404 errors on `/api/v1/messaging/conversations/`

**Cause**: URL patterns not properly configured

**Solution**: Ensure the messaging app URLs include the 'messaging' prefix (already fixed in latest version)

### Issue: Erlang service not connecting

**Symptom**: Django logs show "Failed to route message through Erlang"

**Solutions**:
1. Check if Erlang service is running:
   ```bash
   docker ps | grep erlang
   curl http://localhost:8765/health
   ```

2. Check network connectivity:
   ```bash
   docker network inspect stayafrica-network
   ```

3. Verify environment variable:
   ```bash
   docker exec stayafrica_backend env | grep ERLANG
   ```

### Issue: Messages not appearing in real-time

**Symptom**: Messages take time to appear

**Solutions**:
1. Check if Erlang service is running (see above)
2. In development, frontend uses polling (5-10s refresh)
3. For production, consider implementing WebSocket connections

## Monitoring

### Check Service Health

```bash
# Django health
curl http://localhost:8000/api/health/

# Erlang health via Django
curl -H "Authorization: Bearer <token>" \
  http://localhost:8000/api/v1/messaging/erlang/health/

# Erlang health directly
curl http://localhost:8765/health
```

### View Logs

```bash
# Django logs
docker-compose logs -f backend

# Erlang logs
docker-compose -f docker-compose.erlang.yml logs -f erlang-messaging

# All services
docker-compose logs -f
```

## Performance Considerations

### Without Erlang Service
- **Throughput**: ~100-500 messages/second
- **Latency**: 100-500ms per message
- **Concurrent Users**: ~100-500 users
- **Storage**: PostgreSQL only

### With Erlang Service
- **Throughput**: ~10,000+ messages/second
- **Latency**: 10-50ms per message
- **Concurrent Users**: 10,000+ users
- **Storage**: In-memory queues + PostgreSQL persistence

## Migration Path

### From Django-only to Django+Erlang

1. Start Erlang service (no code changes needed)
2. Set `ERLANG_MESSAGING_URL` environment variable
3. Restart Django backend
4. Verify health endpoint shows Erlang is connected
5. No database migrations required

### Rollback to Django-only

1. Stop Erlang service
2. Remove/unset `ERLANG_MESSAGING_URL` environment variable
3. Restart Django backend
4. System continues working in fallback mode

## Additional Resources

- **Erlang Service Details**: See `erlang_messaging/README.md`
- **Frontend Implementation**: See `MESSAGING_FRONTEND.md`
- **API Documentation**: See `http://localhost:8000/api/docs/`
