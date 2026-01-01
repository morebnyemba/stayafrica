# Media Files & Geocoding Fix - Deployment Guide

## Issues Fixed

1. **Images not being served** - Media files weren't accessible from backend
2. **Geocoding 404 error** - Endpoint was configured but routing needed verification

## Changes Made

### 1. Docker Compose Update
- **Replaced**: Nginx Proxy Manager with custom Nginx container
- **Reason**: NPM couldn't access backend media volumes properly
- **Benefits**: 
  - Direct access to media files from backend container
  - Better performance
  - Easier configuration

### 2. Nginx Configuration
- Added CORS headers to media files
- Configured proper caching headers
- Added `try_files` directive for better error handling

### 3. Directory Structure
Created `init-ssl-dirs.sh` to set up SSL certificate directories

## Deployment Steps

### Step 1: Stop Current Services
```bash
cd ~/stayafrica
docker compose -f docker-compose.prod.yml down
```

### Step 2: Create SSL Directories
```bash
chmod +x init-ssl-dirs.sh
./init-ssl-dirs.sh
```

### Step 3: Update SSL Setup Script
Edit `setup-ssl.sh` and replace `your-email@example.com` with your actual email.

### Step 4: Obtain SSL Certificates
```bash
chmod +x setup-ssl.sh
./setup-ssl.sh
```

### Step 5: Rebuild and Start Services
```bash
# Pull latest changes
git pull

# Rebuild containers
docker compose -f docker-compose.prod.yml build

# Start services
docker compose -f docker-compose.prod.yml up -d

# Check logs
docker compose -f docker-compose.prod.yml logs -f
```

### Step 6: Collect Static Files
```bash
docker compose -f docker-compose.prod.yml exec backend python manage.py collectstatic --noinput
```

### Step 7: Verify Services

1. **Check Nginx is running**:
```bash
docker compose -f docker-compose.prod.yml ps nginx
```

2. **Test media file access**:
```bash
curl -I https://api.zimlegend.online/media/
```

3. **Test geocoding endpoint**:
```bash
curl -X POST https://api.zimlegend.online/api/v1/properties/geocode/ \
  -H "Content-Type: application/json" \
  -d '{"address": "Harare, Zimbabwe"}'
```

## Verification Checklist

- [ ] All containers are running (`docker compose ps`)
- [ ] Nginx container has access to media volume
- [ ] SSL certificates are valid
- [ ] Frontend can load images from backend
- [ ] Geocoding API returns results
- [ ] Property creation with images works
- [ ] Property edit shows existing images

## Troubleshooting

### Images still not loading

1. Check media volume mount:
```bash
docker compose -f docker-compose.prod.yml exec nginx ls -la /app/media/
```

2. Verify backend is saving files:
```bash
docker compose -f docker-compose.prod.yml exec backend ls -la /app/media/
```

3. Check Nginx logs:
```bash
docker compose -f docker-compose.prod.yml logs nginx
```

### Geocoding still failing

1. Check backend logs:
```bash
docker compose -f docker-compose.prod.yml logs backend | grep geocode
```

2. Verify the endpoint exists:
```bash
docker compose -f docker-compose.prod.yml exec backend python manage.py show_urls | grep geocode
```

### SSL Certificate Issues

If certificates fail to renew:
```bash
# Test renewal
docker compose -f docker-compose.prod.yml run --rm certbot renew --dry-run

# Force renewal
docker compose -f docker-compose.prod.yml run --rm certbot renew --force-renewal
```

## Architecture After Fix

```
┌─────────────────┐
│   Nginx (443)   │
│  (SSL Termination)
└────────┬────────┘
         │
    ┌────┴────┐
    │         │
┌───▼────┐ ┌─▼────────┐
│Frontend│ │ Backend  │
│ (3000) │ │  (8000)  │
└────────┘ └─────┬────┘
              │
         ┌────┴────┐
         │  Media  │
         │ Volume  │
         └─────────┘
```

## Notes

- Media files are now served directly by Nginx
- CORS is configured for cross-origin image access
- Caching headers optimize performance
- SSL auto-renewal is configured via certbot

## Next Steps

After deployment:
1. Test property creation with image upload
2. Test property editing (verify existing images display)
3. Test geocoding in property form
4. Verify all images load on property detail pages
