# Custom Nginx Deployment - Ready for Production

## ✅ What's Been Configured

### 1. Docker Compose Setup
**File**: `docker-compose.prod.yml`

- ✅ Custom Nginx container (instead of Nginx Proxy Manager)
- ✅ Certbot for SSL certificate auto-renewal
- ✅ Proper volume mounts for media and static files
- ✅ All services configured (backend, frontend, db, redis, celery)

### 2. Nginx Configuration
**File**: `nginx/nginx.conf`

**Frontend (zimlegend.online)**
- Proxies to Next.js container
- SSL/TLS termination
- CORS headers configured
- HTTP/2 support

**Backend API (api.zimlegend.online)**
- Proxies to Django backend
- Media files served at `/media/` with 7-day cache
- Static files served at `/static/` with 30-day cache
- CORS headers configured for cross-origin requests
- Large file upload support (20MB client_max_body_size)

### 3. Deployment Scripts

**Linux/Mac**: `deploy-prod.sh`
- Stops services
- Creates SSL directories
- Builds containers
- Starts services
- Runs migrations
- Collects static files
- Shows service status

**Windows**: `deploy-prod.ps1`
- Same functionality as Linux version
- Uses PowerShell syntax

### 4. SSL Setup
**File**: `setup-ssl.sh`
- Obtains SSL certificates from Let's Encrypt
- Sets up auto-renewal via certbot
- Covers all domains:
  - zimlegend.online
  - www.zimlegend.online
  - api.zimlegend.online

### 5. Documentation

**NGINX_DEPLOYMENT.md**
- Complete deployment guide
- Architecture overview
- Common tasks and commands
- Troubleshooting section
- Security checklist
- Backup/recovery procedures

**MEDIA_GEOCODING_FIX.md**
- Detailed explanation of fixes
- Step-by-step deployment instructions
- Verification checklist

## 🚀 Deployment Steps

### Step 1: SSH into Production Server

```bash
ssh user@api.zimlegend.online
cd ~/stayafrica
```

### Step 2: Make Scripts Executable

```bash
chmod +x deploy-prod.sh setup-ssl.sh init-ssl-dirs.sh verify-deployment.sh
```

### Step 3: Configure SSL Setup

Edit `setup-ssl.sh` and replace `your-email@example.com`:

```bash
nano setup-ssl.sh
# Change: --email your-email@example.com
# To: --email your-actual-email@example.com
```

### Step 4: Obtain SSL Certificates

```bash
./setup-ssl.sh
```

This will:
- Create `/etc/letsencrypt/live/` directories
- Obtain certificates for all domains
- Show success message with URLs

### Step 5: Deploy Application

```bash
./deploy-prod.sh
```

This will:
- Stop any running services
- Build new containers
- Start all services
- Run migrations
- Collect static files
- Show status of all containers

### Step 6: Verify Deployment

```bash
./verify-deployment.sh
```

This tests:
- All containers are running
- Volume mounts are correct
- API endpoints respond
- Media files are accessible
- Database connection works
- Redis cache works

## 🔧 Key Configuration Changes

### Why Custom Nginx?

**Problem with Nginx Proxy Manager (NPM):**
- NPM runs in a Docker container with its own file system
- Can't directly mount backend media volumes
- Would need to copy files between containers (slow and complex)

**Solution with Custom Nginx:**
- Nginx container mounts same media volume as backend
- Direct file access: `/app/media/` on both containers
- Simple, performant, reliable

### Media File Flow

```
1. User uploads image via frontend
2. Frontend sends to: /api/v1/properties/ (create/update)
3. Backend receives, processes, saves to: /app/media/
4. PropertyImageSerializer generates URL: https://api.zimlegend.online/media/properties/...
5. Frontend requests image from: https://api.zimlegend.online/media/properties/...
6. Nginx intercepts /media/ requests
7. Nginx serves from: /app/media/ (shared volume)
8. Image displays on page
```

### Geocoding Flow

```
1. User enters address in property form
2. Frontend calls: POST /api/v1/properties/geocode/
3. Backend GeocodingService processes address
4. Returns latitude/longitude
5. Frontend receives response
6. Updates map with coordinates
7. User confirms location
```

## 📝 Files to Edit Before Deployment

### 1. setup-ssl.sh
- Change email address (required by Let's Encrypt)

### 2. docker-compose.prod.yml
- SECRET_KEY: Django secret key (currently has placeholder)
- POSTGRES_PASSWORD: Database password (if changing default)
- ALLOWED_HOSTS: Add your domain
- CORS_ALLOWED_ORIGINS: Add your frontend domain

### 3. nginx/nginx.conf
- server_name: Update domain names if different
- Access-Control-Allow-Origin: Update to your frontend domain

## ✨ What Works After Deployment

### Images
- ✅ Image upload in property creation
- ✅ Image display on property detail pages
- ✅ Multiple images per property
- ✅ Image editing (remove/reorder)
- ✅ 7-day browser caching for images

### Geocoding
- ✅ Address autocomplete
- ✅ Geocode address → coordinates
- ✅ Reverse geocode coordinates → address
- ✅ Map display with property location

### Host Features
- ✅ Create properties with images
- ✅ Edit properties (keep existing images)
- ✅ View host properties dashboard
- ✅ Property analytics
- ✅ Earnings/payment tracking

### API Features
- ✅ Public property listings (with images)
- ✅ Property detail page (with images)
- ✅ Booking system (with image gallery)
- ✅ Reviews with images
- ✅ User messaging
- ✅ Payment integration

## 🔒 Security Notes

1. **SSL Certificates**
   - Automatically managed by certbot
   - Auto-renew 30 days before expiry
   - All traffic redirected to HTTPS

2. **CORS Configuration**
   - Only allows requests from your domain
   - Credentials supported
   - Preflight requests handled

3. **Secrets Management**
   - Django SECRET_KEY must be changed
   - Database password should be strong
   - Environment variables not in git

4. **File Permissions**
   - Media files owned by Django process
   - Nginx has read-only access
   - Static files collected from containers

## 📊 Performance

### Caching Headers
- **Media files**: 7 days (images don't change often)
- **Static files**: 30 days (JS/CSS bundles are immutable)
- **API responses**: Redis caching (configured in Django)

### Optimization
- Gzip compression enabled in Nginx
- HTTP/2 support enabled
- Image lazy loading in frontend
- Database query optimization with select_related

## 🆘 If Something Goes Wrong

### Quick Troubleshooting

**Services not starting:**
```bash
docker compose -f docker-compose.prod.yml logs
```

**Images not loading:**
```bash
docker compose -f docker-compose.prod.yml exec nginx ls -la /app/media/
```

**Geocoding 404:**
```bash
curl -X POST https://api.zimlegend.online/api/v1/properties/geocode/ \
  -H "Content-Type: application/json" \
  -d '{"address":"Harare, Zimbabwe"}'
```

**Database issues:**
```bash
docker compose -f docker-compose.prod.yml exec backend python manage.py migrate
```

**SSL certificate renewal:**
```bash
docker compose -f docker-compose.prod.yml run --rm certbot renew --force-renewal
```

See `NGINX_DEPLOYMENT.md` for detailed troubleshooting.

## 📋 Deployment Checklist

Before running deployment script:
- [ ] Cloned/pulled latest code
- [ ] SSH access to production server
- [ ] Edited `setup-ssl.sh` with correct email
- [ ] Edited `docker-compose.prod.yml` with correct SECRET_KEY
- [ ] Updated DNS records (if new domains)
- [ ] Backed up existing database (if upgrading)
- [ ] Read through this document

During deployment:
- [ ] Run `./setup-ssl.sh` and note certificate paths
- [ ] Run `./deploy-prod.sh` and watch for errors
- [ ] Run `./verify-deployment.sh` and ensure all pass
- [ ] Test frontend: https://zimlegend.online
- [ ] Test API: https://api.zimlegend.online/api/v1/properties/

After deployment:
- [ ] Test property creation with image
- [ ] Test image display on detail page
- [ ] Test geocoding in property form
- [ ] Check host dashboard works
- [ ] Monitor logs for errors

## 📚 Next Steps

1. **Immediate**: Follow "Deployment Steps" section above
2. **After deployment**: Run verification script
3. **Testing**: Test all major features
4. **Monitoring**: Set up log rotation, alerts
5. **Backup**: Configure automated database backups
6. **Documentation**: Update any custom configs

## 🎉 You're Ready!

All files are configured and ready for deployment. The scripts are idempotent (safe to run multiple times) and include error handling.

Once deployed:
- Frontend will be available at **https://zimlegend.online**
- Backend API at **https://api.zimlegend.online**
- Media files served at **https://api.zimlegend.online/media/**
- All with automatic SSL renewal

**Total deployment time**: ~5-10 minutes (depending on Docker build speed)

---

**Questions?** Check NGINX_DEPLOYMENT.md for detailed documentation.
