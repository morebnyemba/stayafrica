# Deployment Guide: Centralized Configuration & ASGI Server

## Problem Summary
The application was experiencing database authentication issues between backend and workers (celery/celery-beat) due to:
- Scattered environment configuration across multiple `.env.prod` files
- Inconsistent database credentials between services
- Complex entrypoint scripts creating potential for credential mismatches

Error example:
```
FATAL: password authentication failed for user "postgres"
```

## Solution Applied
1. **Centralized Configuration**: All environment variables now in single root-level `.env` file
2. **ASGI Server**: Migrated from Gunicorn (WSGI) to Daphne (ASGI) for better async support
3. **Simplified Docker**: Removed entrypoint scripts, using direct Docker Compose configuration
4. **Dedicated Migration Service**: Ensures migrations run before application starts

## Deployment Steps

### Prerequisites
- SSH access to the production server
- Docker and Docker Compose installed
- Current working directory: `~/stayafrica/`

### Step 1: Pull the Latest Changes

```bash
cd ~/stayafrica
git pull origin copilot/use-env-for-db-credentials
```

### Step 2: Create/Update Root .env File

**CRITICAL**: This is the single source of truth for all configuration.

```bash
# Copy the example file
cp .env.example .env

# Edit with your production values
nano .env
```

**Required Changes in .env**:
```bash
# Django Core
SECRET_KEY=your-secure-random-key-here  # Generate a new one!
DEBUG=False
ALLOWED_HOSTS=api.zimlegend.online,zimlegend.online,localhost,127.0.0.1

# Database - MUST match db service
DATABASE_PASSWORD=your-strong-password-here  # Change this!

# JWT
JWT_SECRET_KEY=your-jwt-secret-key-here  # Generate a new one!

# Email (if using email features)
EMAIL_HOST_USER=your-email@gmail.com
EMAIL_HOST_PASSWORD=your-app-password

# Update other settings as needed
```

**Generate Strong Keys**:
```bash
# Generate SECRET_KEY
python3 -c "from django.core.management.utils import get_random_secret_key; print(get_random_secret_key())"

# Generate JWT_SECRET_KEY
python3 -c "import secrets; print(secrets.token_urlsafe(50))"
```

### Step 3: Review the Changes (Optional)

```bash
# View the changes to docker-compose.prod.yml
git diff HEAD~5 docker-compose.prod.yml

# Verify YAML syntax and see how env vars are loaded
docker compose -f docker-compose.prod.yml config > /dev/null 2>&1 && echo "✅ YAML valid" || echo "❌ YAML error"

# Check that .env is loaded correctly
docker compose -f docker-compose.prod.yml config | grep DATABASE_PASSWORD
```

### Step 4: Stop Current Services

```bash
# Gracefully stop all running services
docker compose -f docker-compose.prod.yml down

# Verify all containers are stopped
docker ps -a | grep stayafrica
```

### Step 5: (Optional) Clean Database Volume

**⚠️ WARNING**: This deletes ALL database data! Only do this if you want a fresh start.

```bash
# Remove the database volume
docker volume rm stayafrica_postgres_data

# Verify it's removed
docker volume ls | grep stayafrica
```

### Step 6: Rebuild and Start Services

```bash
# Rebuild the backend image with Daphne
docker compose -f docker-compose.prod.yml build backend

# Start all services in detached mode
docker compose -f docker-compose.prod.yml up -d

# Wait for services to be healthy (about 10-20 seconds)
sleep 15
```

### Step 7: Verify Services are Running

```bash
# Check container status
docker compose -f docker-compose.prod.yml ps

# Expected output: All services should be "running" or "healthy"
# The migrate service will show "Exit 0" (completed successfully)
```

### Step 8: Monitor Logs

```bash
# Monitor all services
docker compose -f docker-compose.prod.yml logs -f

# Or monitor specific services
docker compose -f docker-compose.prod.yml logs -f backend celery celery-beat

# Press Ctrl+C to stop following logs
```

### Step 9: Verify Database Connectivity

```bash
# Check migrate service completed successfully
docker compose -f docker-compose.prod.yml logs migrate | grep -E "(Applying|OK)"

# Check backend logs for successful startup (now using Daphne)
docker compose -f docker-compose.prod.yml logs backend | grep -E "(Starting|Listening)"

# Check celery worker logs for errors
docker compose -f docker-compose.prod.yml logs celery | grep -E "(ERROR|password authentication|ready)"

# Check celery-beat logs for errors  
docker compose -f docker-compose.prod.yml logs celery-beat | grep -E "(ERROR|password authentication|Starting)"
```

### Expected Success Indicators

✅ **Migration Service** (runs once and exits):
```
Applying contenttypes.0001_initial... OK
Applying auth.0001_initial... OK
...
Operations to perform: XX migrations applied
```

✅ **Backend Service** (using Daphne ASGI):
```
Starting server at tcp:port=8000:interface=0.0.0.0
HTTP/2 support not enabled (install the http2 and tls Twisted extras)
Configuring endpoint tcp:port=8000:interface=0.0.0.0
Listening on TCP address 0.0.0.0:8000
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
- All services connect to database successfully

### Step 10: Test the Application

```bash
# Test API health endpoint (if available)
curl http://localhost:8000/api/health/ || curl http://localhost:8000/

# Test admin access (should load without database errors)
curl http://localhost:8000/admin/
```

## Architecture Changes

### Before (Old Setup)
```
┌──────────────────┐     ┌──────────────────┐
│ backend/.env.prod│     │  web/.env.prod   │
└──────────────────┘     └──────────────────┘
         │                        │
    ┌────▼────┐              ┌───▼────┐
    │ Backend │              │  Web   │
    │+Gunicorn│              └────────┘
    │+script  │
    └────┬────┘
         │
    ┌────▼────┐     ┌──────────────┐
    │ Celery  │     │ Celery-Beat  │
    │ Worker  │     │  Scheduler   │
    └─────────┘     └──────────────┘
    (Different env files = credential mismatch!)
```

### After (New Setup)
```
         ┌─────────────────┐
         │  Root .env      │
         │ (Single source) │
         └────────┬────────┘
                  │
    ┌─────────────┼─────────────┐
    │             │             │
┌───▼────┐  ┌────▼────┐  ┌────▼────┐
│Migrate │  │ Backend │  │  Web    │
│(once)  │  │ +Daphne │  └─────────┘
└────────┘  │  ASGI   │
            └────┬────┘
                 │
    ┌────────────┼────────────┐
    │            │            │
┌───▼────┐  ┌───▼────┐  ┌───▼────┐
│Celery  │  │Celery  │  │  DB    │
│Worker  │  │ Beat   │  │+PostGIS│
└────────┘  └────────┘  └────────┘
(Same credentials everywhere!)
```

## Rollback Procedure

If issues occur, rollback to the previous version:

```bash
# Stop services
docker compose -f docker-compose.prod.yml down

# Checkout previous version (adjust commit number as needed)
git checkout <previous-commit-hash>

# Restore old .env files
# You may need to restore backend/.env.prod and web/.env.prod

# Start services with old configuration
docker compose -f docker-compose.prod.yml up -d
```

## Troubleshooting

### Issue: Services still show authentication errors

**Solution**: Verify environment variables are loaded correctly
```bash
# Check that root .env file exists
ls -la .env

# Check environment variables in running containers
docker exec stayafrica_backend env | grep DATABASE
docker exec stayafrica_celery env | grep DATABASE

# Should show identical values:
# DATABASE_HOST=db
# DATABASE_PORT=5432
# DATABASE_USER=postgres
# DATABASE_PASSWORD=<your-password>
# DATABASE_NAME=stayafrica_db
```

### Issue: "No module named 'daphne'" error

**Solution**: Rebuild the backend image
```bash
docker compose -f docker-compose.prod.yml build --no-cache backend
docker compose -f docker-compose.prod.yml up -d
```

### Issue: Migrations not running

**Solution**: Check migrate service logs
```bash
docker compose -f docker-compose.prod.yml logs migrate

# If needed, run migrations manually
docker compose -f docker-compose.prod.yml run --rm backend python manage.py migrate
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

### Issue: Old .env.prod files interfering

**Solution**: The new setup only uses root .env file
```bash
# Check what's being loaded
docker compose -f docker-compose.prod.yml config | grep -A 5 "env_file"

# All services should show:
# env_file:
#   - .env
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

### 3. Secure the .env File
```bash
# Restrict permissions (only owner can read/write)
chmod 600 .env

# Verify permissions
ls -la .env
# Should show: -rw------- (600)
```

### 4. Backup Configuration
```bash
# Create a secure backup of .env (store safely, NOT in git)
cp .env .env.backup.$(date +%Y%m%d)
chmod 600 .env.backup.*
```

### 5. Test Admin Interface
- Navigate to https://api.zimlegend.online/admin/
- Log in with admin credentials
- Verify dashboard loads without errors

### 6. Test Celery Tasks
```bash
# Check celery worker is processing tasks
docker compose -f docker-compose.prod.yml logs celery | grep "Task"

# Check celery-beat is scheduling tasks
docker compose -f docker-compose.prod.yml logs celery-beat | grep "Sending due task"
```

## Key Changes Summary

### What Changed
1. ✅ **Centralized Configuration**: Single `.env` file at root (was: multiple `.env.prod` files)
2. ✅ **ASGI Server**: Daphne (was: Gunicorn WSGI)
3. ✅ **Simplified Docker**: Direct CMD (was: entrypoint.sh script)
4. ✅ **Migration Service**: Dedicated service (was: part of entrypoint)
5. ✅ **Consistent Credentials**: Same source for all services (was: scattered)

### What Was Removed
- ❌ `backend/entrypoint.sh` - No longer needed
- ❌ `backend/.env.prod` - Replaced by root `.env`
- ❌ `web/.env.prod` - Replaced by root `.env`
- ❌ Gunicorn dependencies - Replaced by Daphne

### What to Keep
- ✅ Root `.env` file - **CRITICAL**, never commit to git
- ✅ `.env.example` - Template for new deployments
- ✅ `CONFIGURATION_MIGRATION.md` - Complete migration documentation

## Security Best Practices

### Critical Steps
1. **Change default passwords** in `.env`:
   - `SECRET_KEY` - Generate unique key
   - `DATABASE_PASSWORD` - Use strong password
   - `JWT_SECRET_KEY` - Generate unique key

2. **Secure the .env file**:
   ```bash
   chmod 600 .env
   ```

3. **Never commit .env** to version control:
   - Already in `.gitignore`
   - Only commit `.env.example`

4. **Rotate secrets regularly**:
   - Change passwords every 90 days
   - Update API keys when exposed

## Additional Resources

- **Configuration Migration**: See `CONFIGURATION_MIGRATION.md`
- Docker Compose Documentation: https://docs.docker.com/compose/
- PostgreSQL Docker Documentation: https://hub.docker.com/_/postgres
- Django Database Configuration: https://docs.djangoproject.com/en/stable/ref/databases/
- Celery Documentation: https://docs.celeryproject.org/
- Daphne Documentation: https://github.com/django/daphne

## Support

If you encounter issues not covered in this guide:
1. Check the full logs: `docker compose -f docker-compose.prod.yml logs --tail=500`
2. Review Docker container status: `docker ps -a`
3. Check disk space: `df -h`
4. Verify Docker daemon is running: `systemctl status docker`

## Summary

This deployment fixes the database authentication issue by ensuring all Docker services (backend, celery, celery-beat) use consistent database credentials. The fix is minimal, maintains compatibility with the existing infrastructure, and follows the current configuration patterns.
