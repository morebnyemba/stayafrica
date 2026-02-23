# 📋 Production Deployment Checklist

## Pre-Deployment

### Security & Configuration
- [ ] SSH access to production server verified
- [ ] Git repository cloned/pulled to server
- [ ] `docker` and `docker-compose` installed
- [ ] Sufficient disk space available (check with `df -h`)
- [ ] Current DNS records point to server IP

### Configuration Files
- [ ] `setup-ssl.sh` - Email address updated
- [ ] `docker-compose.prod.yml` - SECRET_KEY changed to secure value
- [ ] `docker-compose.prod.yml` - POSTGRES_PASSWORD updated
- [ ] `docker-compose.prod.yml` - ALLOWED_HOSTS updated to your domain
- [ ] `nginx/nginx.conf` - Domain names verified (if not using zimlegend.online)

### SSL Certificates
- [ ] Let's Encrypt account email prepared
- [ ] Firewall allows port 80 (required for cert validation)
- [ ] Firewall allows port 443 (for HTTPS)

### Backup
- [ ] Existing database backed up (if migrating)
- [ ] Existing media files backed up (if migrating)

---

## During Deployment

### Step 1: SSL Setup
- [ ] Run: `./setup-ssl.sh`
- [ ] No errors in output
- [ ] Certificates visible in `nginx/certbot/conf/`

### Step 2: Deployment
- [ ] Run: `./deploy-prod.sh`
- [ ] No critical errors in output
- [ ] Script completes successfully

### Step 3: Verification
- [ ] Run: `./verify-deployment.sh`
- [ ] All checks marked with ✅
- [ ] No ❌ failures

---

## Post-Deployment Testing

### Accessibility
- [ ] Frontend loads: https://zimlegend.online
- [ ] Backend responds: https://api.zimlegend.online/health/
- [ ] Admin accessible: https://api.zimlegend.online/admin/
- [ ] No SSL/certificate warnings

### API Endpoints
- [ ] Properties list works: `https://api.zimlegend.online/api/v1/properties/`
- [ ] Geocoding works: Send POST to `/api/v1/properties/geocode/`
- [ ] Reverse geocoding works: Send POST to `/api/v1/properties/reverse_geocode/`

### Media Files
- [ ] Images load from database in detail page
- [ ] Image path format: `https://api.zimlegend.online/media/...`
- [ ] 7-day cache headers present (check browser DevTools)

### User Features
- [ ] User registration works
- [ ] User login/logout works
- [ ] Property creation accessible
- [ ] Image upload from form works

### Host Dashboard
- [ ] Host can view their properties
- [ ] Host can create new property
- [ ] Image upload in property form works
- [ ] Geocoding shows map correctly
- [ ] Property editing works
- [ ] Images persist after editing

### Admin Panel
- [ ] Admin login works
- [ ] Can view all properties
- [ ] Can view all users
- [ ] Can view all bookings
- [ ] Statistics show correctly

---

## Monitoring & Health

### Container Status
- [ ] All containers running: `docker compose -f docker-compose.prod.yml ps`
  - [ ] stayafrica_nginx (nginx)
  - [ ] stayafrica_backend (Django)
  - [ ] stayafrica_frontend (Next.js)
  - [ ] stayafrica_db (PostgreSQL)
  - [ ] stayafrica_redis (Redis)
  - [ ] stayafrica_celery (Celery worker)
  - [ ] stayafrica_celery_beat (Celery scheduler)
  - [ ] stayafrica_certbot (SSL renewal)

### Resource Usage
- [ ] CPU usage normal: `docker stats` (should be < 50% for most)
- [ ] Memory usage reasonable: `docker stats` (backend < 500MB)
- [ ] Disk usage acceptable: `df -h` (media volume not growing unusually)

### Logs
- [ ] Backend logs show no errors: `docker logs stayafrica_backend`
- [ ] Nginx logs show valid requests: `docker logs stayafrica_nginx`
- [ ] Database logs show no issues: `docker logs stayafrica_db`

---

## Security Verification

### SSL/TLS
- [ ] HTTPS redirect works (HTTP → HTTPS)
- [ ] SSL certificate valid: Check browser certificate info
- [ ] No mixed content warnings in browser console
- [ ] TLS version 1.2+ only: `curl -I https://api.zimlegend.online`

### CORS
- [ ] Frontend can make API requests (no CORS errors in console)
- [ ] Preflight requests succeed
- [ ] Custom headers accepted (Authorization, etc.)

### Authentication
- [ ] Session tokens stored in HTTP-only cookies
- [ ] CSRF protection enabled
- [ ] Rate limiting works (test with rapid requests)

### Data Privacy
- [ ] User passwords hashed (not plain text)
- [ ] Media files not directly accessible without authentication (if required)
- [ ] PII not in logs

---

## Performance Checks

### Response Times
- [ ] Frontend loads in < 3 seconds
- [ ] API responds in < 500ms (check Network tab in DevTools)
- [ ] Image loads in < 1 second (with cache)

### Database
- [ ] Slow query logs empty or minimal
- [ ] Connection pool working (check `SELECT count(*) FROM pg_stat_activity;`)
- [ ] Query time < 100ms average

### Redis
- [ ] Redis is responding: `docker compose -f docker-compose.prod.yml exec redis redis-cli ping`
- [ ] Cache hit rate high (monitor with `INFO stats`)
- [ ] No memory issues: `docker compose -f docker-compose.prod.yml exec redis redis-cli INFO memory`

---

## Common Issues & Solutions

### Issue: Images not loading
- [ ] Check: `docker compose -f docker-compose.prod.yml exec nginx ls -la /app/media/`
- [ ] Check Nginx logs: `docker logs stayafrica_nginx | grep media`
- [ ] Verify URL format in database

### Issue: Geocoding 404
- [ ] Check endpoint exists: `curl https://api.zimlegend.online/api/v1/properties/geocode/`
- [ ] Check with POST (not GET): `curl -X POST ...`
- [ ] Check backend logs: `docker logs stayafrica_backend | grep geocode`

### Issue: SSL Certificate errors
- [ ] Check cert status: `docker compose -f docker-compose.prod.yml exec certbot certbot certificates`
- [ ] Test renewal: `docker compose -f docker-compose.prod.yml run --rm certbot renew --dry-run`
- [ ] Check cert expiry: `openssl s_client -connect api.zimlegend.online:443 -showcerts`

### Issue: Database connection issues
- [ ] Check database running: `docker logs stayafrica_db`
- [ ] Test connection: `docker compose -f docker-compose.prod.yml exec backend python manage.py dbshell -c "SELECT 1;"`
- [ ] Check password/host in environment variables

---

## Backup Procedures

### Database Backup
```bash
# Daily backup command
docker compose -f docker-compose.prod.yml exec -T db pg_dump \
  -U stayafrica_user stayafrica_db > backup_$(date +%Y%m%d).sql
```

### Media Backup
```bash
# Weekly backup command
docker run --rm -v media_volume:/media -v $(pwd):/backup alpine \
  tar czf /backup/media_$(date +%Y%m%d).tar.gz /media
```

---

## Maintenance Schedule

### Daily
- [ ] Monitor error logs
- [ ] Check disk space usage
- [ ] Monitor system resources

### Weekly
- [ ] Backup database
- [ ] Backup media files
- [ ] Review error trends
- [ ] Check SSL certificate expiry (should auto-renew at 30 days)

### Monthly
- [ ] Review database performance
- [ ] Clean up old logs
- [ ] Update packages (security patches)
- [ ] Load test with simulated traffic

### Quarterly
- [ ] Full security audit
- [ ] Performance optimization review
- [ ] Disaster recovery drill

---

## Final Sign-Off

### Deployment Manager
- [ ] Name: ___________________
- [ ] Date: ___________________
- [ ] Time: ___________________
- [ ] All boxes above checked
- [ ] All tests passed
- [ ] Ready for production ✅

### System Administrator
- [ ] Monitoring configured
- [ ] Backups scheduled
- [ ] Alerts configured
- [ ] Documentation updated
- [ ] Ready for production ✅

---

## Contact & Support

**Emergency Contact**: [Your emergency contact]
**Backup Contact**: [Backup contact]
**Escalation Path**: [Support escalation procedure]

**Important URLs**:
- Frontend: https://zimlegend.online
- Backend: https://api.zimlegend.online
- Admin: https://api.zimlegend.online/admin/
- API Docs: https://api.zimlegend.online/api/v1/docs/

**Useful Commands**:
```bash
# View all logs
docker compose -f docker-compose.prod.yml logs -f

# Restart all services
docker compose -f docker-compose.prod.yml restart

# Check service status
docker compose -f docker-compose.prod.yml ps

# Stop all services
docker compose -f docker-compose.prod.yml down
```

---

**Document Version**: 1.0
**Last Updated**: January 2026
**Status**: ✅ Production Ready
