# Configuration Migration Guide

## Summary of Changes

This update centralizes all environment configuration into a single `.env` file at the root level and migrates from Gunicorn (WSGI) to Daphne (ASGI) server.

## Key Changes

### 1. Centralized Environment Configuration
- **New**: Single root-level `.env` file containing ALL configuration
- **Removed**: Individual `.env.prod` files in `backend/` and `web/` directories
- **Benefit**: Single source of truth for database credentials and all other settings

### 2. ASGI Server (Daphne)
- **Changed**: Replaced Gunicorn with Daphne ASGI server
- **Updated**: `requirements.txt` now includes `daphne==4.1.0`
- **Updated**: `settings.py` includes `ASGI_APPLICATION` setting
- **Benefit**: Better support for async operations and WebSockets

### 3. Simplified Docker Setup
- **Removed**: `entrypoint.sh` script complexity
- **New**: Dedicated `migrate` service for database migrations
- **Updated**: All services use `env_file: - .env` pointing to root `.env`
- **Benefit**: Clearer separation of concerns, easier debugging

### 4. Database Credentials Consistency
- **Fixed**: All services (backend, celery, celery-beat) now read from same `.env` file
- **Fixed**: No more authentication failures between services
- **Benefit**: Guaranteed credential consistency

## Migration Steps

### For Production Deployment

1. **Stop existing containers**:
   ```bash
   docker compose -f docker-compose.prod.yml down
   ```

2. **Copy the example environment file**:
   ```bash
   cp .env.example .env
   ```

3. **Edit `.env` with your production values**:
   ```bash
   nano .env  # or use your preferred editor
   ```
   
   **Critical**: Update these values:
   - `SECRET_KEY` - Use a strong, random key
   - `DATABASE_PASSWORD` - Use a strong password
   - `JWT_SECRET_KEY` - Use a different strong, random key
   - `ALLOWED_HOSTS` - Your domain(s)
   - Email settings if using email features
   - Payment provider keys if using payments

4. **Remove old database volume (if needed)**:
   ```bash
   docker volume rm stayafrica_postgres_data
   ```
   ⚠️ **Warning**: This deletes all database data!

5. **Build and start services**:
   ```bash
   docker compose -f docker-compose.prod.yml build
   docker compose -f docker-compose.prod.yml up -d
   ```

6. **Verify services are running**:
   ```bash
   docker compose -f docker-compose.prod.yml ps
   docker compose -f docker-compose.prod.yml logs backend
   docker compose -f docker-compose.prod.yml logs celery
   ```

### For Development

1. Copy and edit `.env`:
   ```bash
   cp .env.example .env
   # Edit .env with development settings
   ```

2. Start development server:
   ```bash
   cd backend
   python manage.py runserver
   ```

## Service Architecture

```
┌─────────────────────────────────────────┐
│          Root .env File                 │
│   (Single source of truth)              │
└─────────────────────────────────────────┘
                    │
                    ├─────────────────────────────────┐
                    │                                 │
        ┌───────────▼──────────┐         ┌───────────▼──────────┐
        │  Database (Postgres) │         │   Redis              │
        │  - PostGIS enabled   │         │   - Celery broker    │
        └───────────┬──────────┘         └───────────┬──────────┘
                    │                                 │
        ┌───────────▼─────────────────────────────────▼──────────┐
        │                                                          │
        │                   Services                              │
        │  ┌──────────┐  ┌────────┐  ┌─────────────┐            │
        │  │ Backend  │  │ Celery │  │ Celery-Beat │            │
        │  │ (Daphne) │  │ Worker │  │  Scheduler  │            │
        │  └──────────┘  └────────┘  └─────────────┘            │
        │       │                                                 │
        └───────┼─────────────────────────────────────────────────┘
                │
        ┌───────▼──────────┐
        │   Frontend       │
        │   (Next.js)      │
        └──────────────────┘
```

## Troubleshooting

### Database Connection Errors

If you see "password authentication failed":
1. Verify `.env` file exists at root level
2. Check that `DATABASE_PASSWORD` matches in `.env` and `POSTGRES_PASSWORD`
3. Ensure all services use `env_file: - .env` in docker-compose.prod.yml
4. Remove old containers and volumes, then rebuild:
   ```bash
   docker compose -f docker-compose.prod.yml down -v
   docker compose -f docker-compose.prod.yml up -d --build
   ```

### Migration Issues

If migrations fail:
1. Check the `migrate` service logs:
   ```bash
   docker compose -f docker-compose.prod.yml logs migrate
   ```
2. The `migrate` service must complete before backend/celery start
3. If needed, run migrations manually:
   ```bash
   docker compose -f docker-compose.prod.yml run --rm backend python manage.py migrate
   ```

### Backend Won't Start

1. Check backend logs:
   ```bash
   docker compose -f docker-compose.prod.yml logs backend
   ```
2. Verify Daphne is installed:
   ```bash
   docker compose -f docker-compose.prod.yml run --rm backend pip list | grep daphne
   ```
3. Check ASGI configuration in settings.py

## File Changes Summary

### New Files
- `/.env` - Centralized environment configuration (not in git)
- `/.env.example` - Template for environment configuration
- `/CONFIGURATION_MIGRATION.md` - This file

### Modified Files
- `/docker-compose.prod.yml` - Uses root .env, adds migrate service, all services simplified
- `/backend/Dockerfile` - Uses Daphne, removes entrypoint.sh
- `/backend/requirements.txt` - Replaced gunicorn with daphne
- `/backend/stayafrica/settings.py` - Added ASGI_APPLICATION setting

### Deprecated Files (can be removed)
- `/backend/entrypoint.sh` - No longer needed
- `/backend/.env.prod` - Replaced by root .env
- `/web/.env.prod` - Replaced by root .env

## Security Notes

1. **Never commit `.env` to git** - It contains secrets
2. **Always use strong passwords** for `SECRET_KEY`, `JWT_SECRET_KEY`, and `DATABASE_PASSWORD`
3. **Set DEBUG=False in production**
4. **Keep `.env` file permissions restricted**: `chmod 600 .env`
5. **Regularly rotate secrets** in production

## Benefits of This Architecture

1. ✅ **Single source of truth**: All configuration in one place
2. ✅ **No credential mismatches**: All services read from same file
3. ✅ **ASGI support**: Better async/WebSocket support with Daphne
4. ✅ **Clearer Docker setup**: No complex entrypoint scripts
5. ✅ **Better debugging**: Separate migration service, clearer logs
6. ✅ **Easier maintenance**: One file to update instead of multiple
