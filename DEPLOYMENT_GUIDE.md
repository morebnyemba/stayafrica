# Deployment Guide: Database Authentication Fix

## Problem Summary
After recreating the database volume, Celery workers were unable to authenticate with the PostgreSQL database, resulting in the error:
```
FATAL: password authentication failed for user "postgres"
```

## Solution Applied
Updated `docker-compose.prod.yml` to explicitly define database credentials for all services (backend, celery, celery-beat), ensuring consistency.

## Deployment Steps

### Prerequisites
- SSH access to the production server
- Docker and Docker Compose installed
- Current working directory: `~/stayafrica/`

### Step 1: Pull the Latest Changes

```bash
cd ~/stayafrica
git pull origin copilot/update-docker-compose-settings
```

### Step 2: Review the Changes (Optional)

```bash
# View the changes to docker-compose.prod.yml
git diff HEAD~4 docker-compose.prod.yml

# Verify YAML syntax
docker compose -f docker-compose.prod.yml config > /dev/null 2>&1 && echo "✅ YAML valid" || echo "❌ YAML error"
```

### Step 3: Stop Current Services

```bash
# Gracefully stop all running services
docker compose -f docker-compose.prod.yml down

# Verify all containers are stopped
docker ps -a | grep stayafrica
```

### Step 4: Start Services with New Configuration

```bash
# Start all services in detached mode
docker compose -f docker-compose.prod.yml up -d

# Wait for services to be healthy (about 10-20 seconds)
sleep 15
```

### Step 5: Verify Services are Running

```bash
# Check container status
docker compose -f docker-compose.prod.yml ps

# Expected output: All services should be "running" or "healthy"
```

### Step 6: Monitor Logs

```bash
# Monitor all services
docker compose -f docker-compose.prod.yml logs -f

# Or monitor specific services
docker compose -f docker-compose.prod.yml logs -f backend celery celery-beat

# Press Ctrl+C to stop following logs
```

### Step 7: Verify Database Connectivity

```bash
# Check backend logs for successful startup
docker compose -f docker-compose.prod.yml logs backend | grep -E "(Database is ready|Starting Gunicorn)"

# Check celery worker logs for errors
docker compose -f docker-compose.prod.yml logs celery | grep -E "(ERROR|password authentication)"

# Check celery-beat logs for errors  
docker compose -f docker-compose.prod.yml logs celery-beat | grep -E "(ERROR|password authentication)"
```

### Expected Success Indicators

✅ **Backend Service**:
```
✅ Database is ready
🔄 Running database migrations...
🚀 Starting Gunicorn server...
[INFO] Listening at: http://0.0.0.0:8000
```

✅ **Celery Worker**:
```
[INFO/MainProcess] Connected to redis://redis:6379/0
[INFO/MainProcess] mingle: searching for neighbors
[INFO/MainProcess] mingle: all alone
[INFO/MainProcess] celery@<hostname> ready.
```

✅ **Celery Beat**:
```
[INFO/MainProcess] beat: Starting...
[INFO/MainProcess] Scheduler: Sending due task
```

✅ **No Authentication Errors**:
- No messages containing "password authentication failed"
- No "OperationalError" related to database connections

### Step 8: Test the Application

```bash
# Test API health endpoint (if available)
curl http://localhost:8000/api/health/ || curl http://localhost:8000/

# Test admin access (should load without database errors)
curl http://localhost:8000/admin/
```

## Rollback Procedure

If issues occur, rollback to the previous version:

```bash
# Stop services
docker compose -f docker-compose.prod.yml down

# Checkout previous version
git checkout HEAD~4

# Start services with old configuration
docker compose -f docker-compose.prod.yml up -d
```

## Troubleshooting

### Issue: Services still show authentication errors

**Solution**: Verify environment variables are loaded correctly
```bash
# Check environment variables in running container
docker exec stayafrica_celery env | grep DATABASE

# Should show:
# DATABASE_HOST=db
# DATABASE_PORT=5432
# DATABASE_USER=postgres
# DATABASE_PASSWORD=postgres
# DATABASE_NAME=stayafrica_db
```

### Issue: Database container is not starting

**Solution**: Check database logs
```bash
docker compose -f docker-compose.prod.yml logs db
```

### Issue: Port conflicts

**Solution**: Ensure ports 80, 443, 81, 5432, 6379, 8000, 3000 are available
```bash
netstat -tulpn | grep -E ":(80|443|81|5432|6379|8000|3000)"
```

### Issue: Volume permission errors

**Solution**: Check volume permissions
```bash
docker volume inspect stayafrica_postgres_data
ls -la /var/lib/docker/volumes/stayafrica_postgres_data/
```

## Post-Deployment Tasks

### 1. Monitor for 24 Hours
Keep an eye on logs for any unexpected errors:
```bash
docker compose -f docker-compose.prod.yml logs -f --tail=100
```

### 2. Verify Celery Tasks are Processing
Check that scheduled tasks are running:
```bash
docker compose -f docker-compose.prod.yml logs celery-beat | tail -20
```

### 3. Test Admin Interface
- Navigate to https://api.zimlegend.online/admin/
- Log in with admin credentials
- Verify dashboard loads without errors

### 4. Review Security Recommendations
After confirming the fix works:
```bash
cat SECURITY_RECOMMENDATIONS.md
```

Consider implementing:
- Environment variable-based credential management
- Changing default database password
- Restricting database port exposure

## Additional Resources

- Docker Compose Documentation: https://docs.docker.com/compose/
- PostgreSQL Docker Documentation: https://hub.docker.com/_/postgres
- Django Database Configuration: https://docs.djangoproject.com/en/stable/ref/databases/
- Celery Documentation: https://docs.celeryproject.org/

## Support

If you encounter issues not covered in this guide:
1. Check the full logs: `docker compose -f docker-compose.prod.yml logs --tail=500`
2. Review Docker container status: `docker ps -a`
3. Check disk space: `df -h`
4. Verify Docker daemon is running: `systemctl status docker`

## Summary

This deployment fixes the database authentication issue by ensuring all Docker services (backend, celery, celery-beat) use consistent database credentials. The fix is minimal, maintains compatibility with the existing infrastructure, and follows the current configuration patterns.
