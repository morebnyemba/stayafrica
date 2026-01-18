# Backend Startup Error Fix

## Problem Statement
The backend was failing to start due to two separate issues:

1. **GDAL Library Error**: When testing locally without Docker:
   ```
   django.core.exceptions.ImproperlyConfigured: Could not find the GDAL library
   ```
   This error occurred because GeoDjango requires GDAL (Geospatial Data Abstraction Library) system libraries.

2. **Missing Channels Configuration**: The WebSocket routing in `asgi.py` imports from `channels.routing`, but:
   - The `channels` package was not listed in `requirements.txt`
   - `channels` was not in INSTALLED_APPS
   - CHANNEL_LAYERS was not configured

## Root Cause
1. **Missing GDAL System Libraries**: The backend uses Django's GIS (Geographic Information System) functionality through `django.contrib.gis`, which requires GDAL to be installed at the system level. This was only an issue when running locally outside of Docker, as the Dockerfile already includes GDAL.

2. **Incomplete Channels Setup**: While the ASGI configuration referenced channels for WebSocket support, the package wasn't properly integrated into the Django project configuration.

## Solution Implemented

### 1. System Dependencies (Already in Dockerfile)
The `backend/Dockerfile` already includes the necessary GDAL packages:
```dockerfile
RUN apt-get update && apt-get install -y \
    gdal-bin \
    libgdal-dev \
    libproj-dev \
    ...
```

### 2. Python Dependencies Added
Updated `backend/requirements.txt` to include:
```
channels==4.3.2
channels-redis==4.3.0
```

### 3. Django Configuration Updates
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
CHANNEL_LAYERS = {
    'default': {
        'BACKEND': 'channels_redis.core.RedisChannelLayer',
        'CONFIG': {
            'hosts': [REDIS_URL],
        },
    },
}
```

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

If you're running the backend locally without Docker, you need to install GDAL system libraries:

### Ubuntu/Debian:
```bash
sudo apt-get update
sudo apt-get install -y gdal-bin libgdal-dev python3-gdal
```

### macOS (using Homebrew):
```bash
brew install gdal
```

### Windows:
1. Download and install OSGeo4W from https://trac.osgeo.org/osgeo4w/
2. Add GDAL/GEOS library paths to your Django settings (already configured in settings.py for Windows)

Then install Python dependencies:
```bash
pip install -r requirements.txt
```

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
- [GeoDjango Documentation](https://docs.djangoproject.com/en/5.0/ref/contrib/gis/)
- [GDAL Installation Guide](https://gdal.org/download.html)
