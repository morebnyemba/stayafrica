# Custom Nginx Deployment Setup

This guide covers the production deployment using **Custom Nginx** (not Nginx Proxy Manager).

## Why Custom Nginx?

- **Direct volume access** to backend media files
- **Better performance** - no additional reverse proxy layer
- **Full control** over CORS, caching, and routing
- **Easy to debug** - nginx.conf is simple and readable

## Architecture

```
┌─────────────────────────────────────────┐
│          Internet (HTTPS/80,443)        │
└─────────────────┬───────────────────────┘
                  │
         ┌────────▼────────┐
         │  Nginx (Custom) │
         │  - SSL/TLS      │
         │  - Media Files  │
         │  - Static Files │
         │  - Reverse Proxy│
         └────────┬────────┘
                  │
         ┌────────┴─────────┐
         │                  │
    ┌────▼─────┐      ┌─────▼──────┐
    │ Frontend  │      │  Backend   │
    │ (Next.js) │      │  (Django)  │
    │ :3000     │      │  :8000     │
    └───────────┘      └─────┬──────┘
                             │
                    ┌────────┴────────┐
                    │  Media Volume   │
                    │  (Shared)       │
                    └─────────────────┘
```

## Files Included

### Docker Compose
- **docker-compose.prod.yml** - Production configuration with custom Nginx

### Nginx
- **nginx/nginx.conf** - Nginx reverse proxy configuration
- **nginx/certbot/** - SSL certificate storage (created during setup)

### Deployment Scripts
- **deploy-prod.sh** - Linux/Mac deployment script
- **deploy-prod.ps1** - Windows PowerShell deployment script
- **setup-ssl.sh** - SSL certificate setup (certbot)
- **init-ssl-dirs.sh** - Create SSL directory structure

### Documentation
- **MEDIA_GEOCODING_FIX.md** - Detailed fix documentation

## Quick Start

### 1. Initial Setup

```bash
# Clone repo and enter directory
cd ~/stayafrica

# Make scripts executable
chmod +x deploy-prod.sh setup-ssl.sh init-ssl-dirs.sh verify-deployment.sh

# Create SSL directories
./init-ssl-dirs.sh
```

### 2. SSL Certificate Setup

Edit `setup-ssl.sh` and replace `your-email@example.com` with your actual email:

```bash
# Then run
./setup-ssl.sh
```

This will obtain SSL certificates from Let's Encrypt for:
- `zimlegend.online`
- `www.zimlegend.online`
- `api.zimlegend.online`

### 3. Deploy

```bash
# Run deployment script
./deploy-prod.sh
```

This will:
1. Stop existing services
2. Create SSL directories
3. Build new containers
4. Start services
5. Run migrations
6. Collect static files

### 4. Verify Deployment

```bash
# Run verification tests
./verify-deployment.sh
```

## Configuration

### Environment Variables

Key environment variables in `docker-compose.prod.yml`:

```yaml
Backend:
  - ALLOWED_HOSTS: localhost,127.0.0.1,api.zimlegend.online,zimlegend.online
  - SECRET_KEY: change-this-to-a-secure-key-in-production
  - DEBUG: "False"
  - REDIS_URL: redis://redis:6379/0

Frontend:
  - NEXT_PUBLIC_API_BASE_URL: https://api.zimlegend.online
  - NEXT_PUBLIC_API_VERSION: v1
```

**⚠️ Before deployment:**
1. Change `SECRET_KEY` to a secure random string
2. Update database password
3. Update all default credentials

### Nginx Configuration

The nginx config includes:

**Frontend (zimlegend.online)**
- Proxies to Next.js frontend container
- CORS headers for API requests
- HTTP/2 support
- SSL/TLS with auto-renewal

**Backend API (api.zimlegend.online)**
- Proxies to Django backend
- Media files served at `/media/`
- Static files served at `/static/`
- CORS headers for cross-origin requests
- 7-day cache for media files

## Common Tasks

### View Logs

```bash
# All services
docker compose -f docker-compose.prod.yml logs -f

# Specific service
docker compose -f docker-compose.prod.yml logs -f backend
docker compose -f docker-compose.prod.yml logs -f nginx
docker compose -f docker-compose.prod.yml logs -f celery
```

### Check Service Status

```bash
docker compose -f docker-compose.prod.yml ps
```

### Run Management Commands

```bash
# Create superuser
docker compose -f docker-compose.prod.yml exec backend python manage.py createsuperuser

# Run migrations
docker compose -f docker-compose.prod.yml exec backend python manage.py migrate

# Collect static files
docker compose -f docker-compose.prod.yml exec backend python manage.py collectstatic --noinput
```

### Access Database

```bash
# PostgreSQL shell
docker compose -f docker-compose.prod.yml exec db psql -U stayafrica_user -d stayafrica_db

# Redis CLI
docker compose -f docker-compose.prod.yml exec redis redis-cli
```

### Stop/Start Services

```bash
# Stop all
docker compose -f docker-compose.prod.yml down

# Start all
docker compose -f docker-compose.prod.yml up -d

# Restart specific service
docker compose -f docker-compose.prod.yml restart backend
```

## Troubleshooting

### Images Not Loading

1. **Check media volume is mounted:**
   ```bash
   docker compose -f docker-compose.prod.yml exec nginx ls -la /app/media/
   ```

2. **Verify backend is saving files:**
   ```bash
   docker compose -f docker-compose.prod.yml exec backend ls -la /app/media/
   ```

3. **Check Nginx logs:**
   ```bash
   docker compose -f docker-compose.prod.yml logs nginx
   ```

### SSL Certificate Issues

1. **Check certificate status:**
   ```bash
   docker compose -f docker-compose.prod.yml exec certbot certbot certificates
   ```

2. **Dry run renewal:**
   ```bash
   docker compose -f docker-compose.prod.yml run --rm certbot renew --dry-run
   ```

3. **Force renewal:**
   ```bash
   docker compose -f docker-compose.prod.yml run --rm certbot renew --force-renewal
   ```

### Geocoding 404 Errors

1. **Verify endpoint:**
   ```bash
   curl -X POST https://api.zimlegend.online/api/v1/properties/geocode/ \
     -H "Content-Type: application/json" \
     -d '{"address":"Harare, Zimbabwe"}'
   ```

2. **Check backend logs:**
   ```bash
   docker compose -f docker-compose.prod.yml logs backend | grep geocode
   ```

### Database Connection Issues

1. **Verify database is running:**
   ```bash
   docker compose -f docker-compose.prod.yml ps db
   ```

2. **Check logs:**
   ```bash
   docker compose -f docker-compose.prod.yml logs db
   ```

3. **Test connection:**
   ```bash
   docker compose -f docker-compose.prod.yml exec backend python manage.py dbshell
   ```

## Security Checklist

Before production deployment:

- [ ] Changed `SECRET_KEY` in django settings
- [ ] Updated database password
- [ ] Set `DEBUG = False`
- [ ] Updated `ALLOWED_HOSTS`
- [ ] Updated CORS origins to your domain
- [ ] Obtained SSL certificates
- [ ] Reviewed and updated all environment variables
- [ ] Set up automated backups for database
- [ ] Configured log rotation
- [ ] Set up monitoring/alerting

## Performance Optimization

### Caching Strategy

- **Media files**: 7-day cache
- **Static files**: 30-day immutable cache
- **API responses**: Handled by Django cache framework (Redis)

### Database

- **Connection pooling**: Configured in Django
- **Query optimization**: Use select_related/prefetch_related
- **Indexing**: Ensure database has proper indexes

### Frontend

- **Image optimization**: Use Next.js Image component
- **Code splitting**: Enabled by default in Next.js
- **Compression**: Gzip enabled in Nginx

## Monitoring

### Health Checks

All services have health checks defined:

```bash
# Frontend health
curl https://zimlegend.online/api/health

# Backend health
curl https://api.zimlegend.online/health/

# Database
docker compose -f docker-compose.prod.yml exec db pg_isready

# Redis
docker compose -f docker-compose.prod.yml exec redis redis-cli ping
```

### Logging

- **Nginx logs**: `/var/log/nginx/`
- **Django logs**: Configure in settings.py
- **Celery logs**: Run `docker logs stayafrica_celery`
- **Database logs**: Run `docker logs stayafrica_db`

## Backup & Recovery

### Database Backup

```bash
# Manual backup
docker compose -f docker-compose.prod.yml exec db pg_dump \
  -U stayafrica_user stayafrica_db > backup_$(date +%Y%m%d_%H%M%S).sql

# Restore from backup
docker compose -f docker-compose.prod.yml exec -T db psql \
  -U stayafrica_user stayafrica_db < backup_file.sql
```

### Media Files Backup

```bash
# Backup media volume
docker run --rm -v media_volume:/media -v $(pwd):/backup \
  alpine tar czf /backup/media_backup_$(date +%Y%m%d).tar.gz /media
```

## Next Steps

1. ✅ Deploy using `./deploy-prod.sh`
2. ✅ Verify deployment using `./verify-deployment.sh`
3. ✅ Test property creation with image upload
4. ✅ Test geocoding in property form
5. ✅ Set up automated backups
6. ✅ Set up monitoring/alerting
7. ✅ Document any custom configurations

## Support

For issues or questions, check:
1. Docker logs: `docker compose -f docker-compose.prod.yml logs`
2. Nginx logs: Inside container at `/var/log/nginx/access.log`
3. Backend logs: Check Django logging configuration
4. This documentation

---

**Last Updated**: January 2026
**Deployment Type**: Docker Compose with Custom Nginx
**SSL Provider**: Let's Encrypt (certbot)
