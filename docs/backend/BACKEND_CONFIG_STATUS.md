# Backend Configuration Status

## ALLOWED_HOSTS Configuration ✅

The backend Django application is already correctly configured with the necessary domains in `ALLOWED_HOSTS`.

### Current Configuration

**File**: `/backend/stayafrica/settings.py` (Lines 56-59)
```python
ALLOWED_HOSTS = os.getenv(
    'ALLOWED_HOSTS',
    'localhost,127.0.0.1,backend,api.zimlegend.online,zimlegend.online,stayafrica.app,www.stayafrica.app,api.stayafrica.app'
).split(',')
```

**File**: `/.env` (Line 12)
```
ALLOWED_HOSTS=api.zimlegend.online,zimlegend.online,stayafrica.app,www.stayafrica.app,api.stayafrica.app,localhost,127.0.0.1
```

### Included Domains ✅

The following domains are properly configured:
- ✅ `api.stayafrica.app` - Production API domain
- ✅ `stayafrica.app` - Production web domain
- ✅ `www.stayafrica.app` - Production web with www
- ✅ `api.zimlegend.online` - Alternative API domain
- ✅ `zimlegend.online` - Alternative web domain
- ✅ `localhost` - Local development
- ✅ `127.0.0.1` - Local IP
- ✅ `backend` - Docker container name

### Invalid HTTP_HOST Header Error

If you're still seeing "Invalid HTTP_HOST header: 'api.stayafrica.app'" errors, it's likely due to one of the following:

#### 1. Environment Variables Not Loaded
**Issue**: The `.env` file may not be loaded properly by Docker or the application.

**Solution**:
```bash
# Restart the backend service to reload environment variables
docker compose -f docker-compose.prod.yml restart backend

# Or rebuild if needed
docker compose -f docker-compose.prod.yml up -d --build backend
```

#### 2. Cache or Old Settings
**Issue**: Django may be using cached settings.

**Solution**:
```bash
# Clear Python cache
find backend -type d -name __pycache__ -exec rm -rf {} +
find backend -type f -name "*.pyc" -delete

# Restart services
docker compose -f docker-compose.prod.yml restart
```

#### 3. Nginx Proxy Configuration
**Issue**: Nginx may not be forwarding the correct headers.

**Solution**: Check `nginx/nginx.conf` has the correct proxy headers:
```nginx
proxy_set_header Host $host;
proxy_set_header X-Real-IP $remote_addr;
proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
proxy_set_header X-Forwarded-Proto $scheme;
proxy_set_header X-Forwarded-Host $host;
```

#### 4. Docker Compose Environment
**Issue**: Environment variable in docker-compose.prod.yml may override .env file.

**Check**: `docker-compose.prod.yml`
```yaml
backend:
  environment:
    - ALLOWED_HOSTS=api.zimlegend.online,zimlegend.online,stayafrica.app,www.stayafrica.app,api.stayafrica.app,localhost,127.0.0.1
```

### Verification Steps

#### Step 1: Check Current ALLOWED_HOSTS
```bash
# From host machine
docker compose -f docker-compose.prod.yml exec backend python manage.py shell

# In Django shell
from django.conf import settings
print(settings.ALLOWED_HOSTS)
# Should output: ['api.zimlegend.online', 'zimlegend.online', 'stayafrica.app', ...]
```

#### Step 2: Test API Connectivity
```bash
# Test from local machine
curl -H "Host: api.stayafrica.app" http://localhost:8000/api/v1/properties/

# Test from outside
curl https://api.stayafrica.app/api/v1/properties/
```

#### Step 3: Check Logs
```bash
# View backend logs
docker compose -f docker-compose.prod.yml logs -f backend

# Look for ALLOWED_HOSTS errors
docker compose -f docker-compose.prod.yml logs backend | grep "ALLOWED_HOSTS"
```

### Mobile App Configuration

The mobile app is configured to connect to the backend:

**File**: `/mobile/.env.example`
```env
# For local development:
EXPO_PUBLIC_API_BASE_URL=http://localhost:8000/api/v1

# For production:
# EXPO_PUBLIC_API_BASE_URL=https://api.stayafrica.app/api/v1
```

**File**: `/mobile/src/services/api-client.ts`
```typescript
const API_BASE_URL = process.env.EXPO_PUBLIC_API_BASE_URL || 'http://localhost:8000/api/v1';
```

### Testing Backend Connection

#### From Mobile App (Development)
1. Ensure backend is running:
   ```bash
   docker compose -f docker-compose.prod.yml ps
   ```

2. Update mobile `.env` file:
   ```env
   # For local testing (backend on host machine)
   EXPO_PUBLIC_API_BASE_URL=http://localhost:8000/api/v1
   
   # For local testing (backend in Docker on same network)
   # EXPO_PUBLIC_API_BASE_URL=http://backend:8000/api/v1
   
   # For production testing
   # EXPO_PUBLIC_API_BASE_URL=https://api.stayafrica.app/api/v1
   ```

3. Test API from mobile:
   - Start the mobile app: `npm start`
   - Watch for API errors in logs
   - Use React Native Debugger to inspect network requests

#### From Production
1. Mobile app should use production API:
   ```env
   EXPO_PUBLIC_API_BASE_URL=https://api.stayafrica.app/api/v1
   ```

2. Ensure SSL is properly configured
3. Check CORS settings allow mobile app requests

### CORS Configuration

The backend is already configured to allow mobile app requests:

**File**: `/backend/stayafrica/settings.py` (Lines 237-277)
```python
CORS_ALLOWED_ORIGINS = [
    'http://localhost:3000',      # Web dev
    'http://localhost:3001',
    'http://127.0.0.1:3000',
    'http://backend:8000',
    'https://zimlegend.online',
    'https://api.zimlegend.online',
    'https://stayafrica.app',
    'https://www.stayafrica.app',
    'https://api.stayafrica.app',
]

CORS_ALLOW_CREDENTIALS = True
```

**Note**: Mobile apps don't send CORS headers for API requests, so CORS_ALLOWED_ORIGINS mainly affects web clients. Mobile apps should work regardless of CORS settings.

### Security Headers

The backend is configured to work with proxies:

```python
# Lines 280-281
USE_X_FORWARDED_HOST = True
SECURE_PROXY_SSL_HEADER = ('HTTP_X_FORWARDED_PROTO', 'https')
```

This ensures Django recognizes HTTPS requests forwarded by Nginx.

### Summary

✅ **ALLOWED_HOSTS**: Properly configured with all necessary domains
✅ **CORS**: Configured for web clients
✅ **Proxy Headers**: Configured to work with Nginx
✅ **Environment Variables**: Defined in .env and docker-compose
✅ **Mobile Configuration**: API client configured with correct endpoints

### Next Steps

If you continue to see HTTP_HOST errors:

1. **Restart Services**:
   ```bash
   docker compose -f docker-compose.prod.yml restart
   ```

2. **Check Nginx Configuration**:
   ```bash
   docker compose -f docker-compose.prod.yml exec nginx nginx -t
   ```

3. **Verify Environment Loading**:
   ```bash
   docker compose -f docker-compose.prod.yml exec backend env | grep ALLOWED_HOSTS
   ```

4. **Check Django Settings**:
   ```bash
   docker compose -f docker-compose.prod.yml exec backend python manage.py check
   ```

5. **Test Direct Backend Access**:
   ```bash
   # Bypass Nginx and test backend directly
   docker compose -f docker-compose.prod.yml exec backend curl http://localhost:8000/api/v1/properties/
   ```

### Contact Support

If issues persist after following these steps, provide:
- Backend logs: `docker compose -f docker-compose.prod.yml logs backend > backend.log`
- Nginx logs: `docker compose -f docker-compose.prod.yml logs nginx > nginx.log`
- Environment check: `docker compose -f docker-compose.prod.yml exec backend env > env.log`
- Django check: `docker compose -f docker-compose.prod.yml exec backend python manage.py check > check.log`
