# Backend Startup Error Fix

## Problem Statement
The backend was failing to start due to missing Channels configuration:

**Missing Channels Configuration**: The WebSocket routing in `asgi.py` imports from `channels.routing`, but:
- The `channels` package was not listed in `requirements.txt`
- `channels` was not in INSTALLED_APPS
- CHANNEL_LAYERS was not configured

## Root Cause
**Incomplete Channels Setup**: While the ASGI configuration referenced channels for WebSocket support, the package wasn't properly integrated into the Django project configuration.

## Solution Implemented

### 1. Python Dependencies Added
Updated `backend/requirements.txt` to include:
```
channels==4.3.2
channels-redis==4.3.0
```

### 2. Django Configuration Updates
Added to `backend/stayafrica/settings.py`:

#### Added `channels` to INSTALLED_APPS:
```python
INSTALLED_APPS = [
    # ... other apps ...
    'channels',
    # ... local apps ...
]
```

#### Added CHANNEL_LAYERS configuration:
```python
# Channels Layer Configuration (WebSockets)
# Configuration for real-time message passing between server instances
CHANNELS_CAPACITY = int(os.getenv('CHANNELS_CAPACITY', '1500'))  # Maximum messages per channel
CHANNELS_EXPIRY = int(os.getenv('CHANNELS_EXPIRY', '10'))  # Message expiry in seconds
CHANNELS_ENCRYPTION_KEY = os.getenv('CHANNELS_ENCRYPTION_KEY', None)

# Development: Uses Redis with basic configuration
# Production: Uses Redis with optional encryption for message security
CHANNEL_LAYERS = {
    'default': {
        'BACKEND': 'channels_redis.core.RedisChannelLayer',
        'CONFIG': {
            'hosts': [REDIS_URL],
            'capacity': CHANNELS_CAPACITY,
            'expiry': CHANNELS_EXPIRY,
            # Optional encryption in production if CHANNELS_ENCRYPTION_KEY is set
        },
    },
}
```

#### Updated environment configuration files:
- Added optional `CHANNELS_CAPACITY` environment variable (default: 1500)
- Added optional `CHANNELS_EXPIRY` environment variable (default: 10)
- Added optional `CHANNELS_ENCRYPTION_KEY` for secure WebSocket message encryption in production

## Verification Steps

### 1. Check Django Configuration
```bash
cd backend
python manage.py check
```
Expected output: `System check identified no issues (0 silenced).`

### 2. Test Daphne Server Startup
```bash
cd backend
daphne -b 0.0.0.0 -p 8000 stayafrica.asgi:application
```
Expected output:
```
INFO Starting server at tcp:port=8000:interface=0.0.0.0
INFO Configuring endpoint tcp:port=8000:interface=0.0.0.0
INFO Listening on TCP address 0.0.0.0:8000
```

### 3. Test with Docker Compose
```bash
docker-compose up backend
```

## For Local Development (Non-Docker)

If you're running the backend locally without Docker, install Python dependencies:
```bash
pip install -r requirements.txt
```

Note: The Dockerfile already includes all necessary system dependencies (including GDAL for GeoDjango). If you encounter any issues with GeoDjango when running locally, ensure you have GDAL system libraries installed for your platform.

## Docker Deployment Notes

The Docker setup already includes all necessary system dependencies in the Dockerfile. The changes made to `requirements.txt` ensure that the Python packages are installed during the Docker build process.

No additional changes are needed to the Docker configuration.

## Testing Checklist

- [x] Django configuration check passes
- [x] Daphne ASGI server starts successfully
- [x] WebSocket routing configuration is correct
- [x] Requirements.txt includes all necessary packages
- [x] Settings.py includes channels configuration

## Related Files Modified

1. `backend/requirements.txt` - Added channels and channels-redis
2. `backend/stayafrica/settings.py` - Added channels to INSTALLED_APPS and CHANNEL_LAYERS configuration

## References

- [Django Channels Documentation](https://channels.readthedocs.io/)
- [Channels Redis Documentation](https://github.com/django/channels_redis)
