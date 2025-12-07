# Backend Development Setup Guide

## Overview

This guide covers setting up the StayAfrica Django backend with Python 3.13.9 and all the latest compatibility fixes.

## Prerequisites

### System Requirements
- **OS:** Windows, macOS, or Linux
- **Python:** 3.11+ (tested with 3.13.9)
- **Database:** PostgreSQL 15+ (optional - uses local SQLite for development)
- **Redis:** 7.0+ (optional - for Celery tasks)

### Download & Install
1. **Python:** https://www.python.org/downloads/ (choose 3.12 or 3.13)
2. **PostgreSQL:** https://www.postgresql.org/download/
3. **Redis:** https://redis.io/download (Windows: use WSL2 or Docker)

## Installation Steps

### 1. Clone Repository
```bash
git clone https://github.com/morebnyemba/stayafrica.git
cd stayafrica/backend
```

### 2. Create Virtual Environment
```bash
# Windows
python -m venv venv
venv\Scripts\activate

# macOS/Linux
python3 -m venv venv
source venv/bin/activate
```

### 3. Upgrade pip and Tools
```bash
pip install --upgrade pip setuptools wheel
```

### 4. Install Core Dependencies
```bash
# Uses psycopg3 and compatible versions
pip install -r requirements.txt
```

**What's included in core requirements:**
- Django 5.0.0
- Django REST Framework 3.14.0
- psycopg[binary] 3.1.18 (PostgreSQL driver - Windows compatible)
- Celery 5.3.4 (async tasks)
- Redis 5.0.1 (caching & message broker)
- djangorestframework-simplejwt 5.5.1 (JWT authentication)
- All other core dependencies

### 5. (Optional) Install Development Dependencies
```bash
# For image processing, AWS S3, error tracking, etc.
pip install -r requirements-dev.txt
```

**What's included in dev requirements:**
- Pillow >= 10.2.0 (image processing)
- django-imagekit 5.0.2 (image resizing)
- django-storages 1.14.2 (AWS S3 storage)
- boto3 1.34.0 (AWS SDK)
- sentry-sdk 1.39.1 (error tracking)

### 6. Configure Environment Variables
```bash
# Create .env file in backend/ directory
copy .env.example .env  # Windows
# or
cp .env.example .env    # macOS/Linux
```

**Edit `.env` with:**
```
DEBUG=True
SECRET_KEY=your-secret-key-here-change-in-production

# Database (use PostgreSQL in production)
DATABASE_ENGINE=django.contrib.gis.db.backends.postgis
DATABASE_NAME=stayafrica_db
DATABASE_USER=postgres
DATABASE_PASSWORD=postgres
DATABASE_HOST=localhost
DATABASE_PORT=5432

# Cache & Message Broker
REDIS_URL=redis://localhost:6379/1
CELERY_BROKER_URL=redis://localhost:6379/0

# Frontend URL for email links
FRONTEND_URL=http://localhost:3000

# Email (uses console backend in development)
EMAIL_BACKEND=django.core.mail.backends.console.EmailBackend

# Optional: Sentry for error tracking
SENTRY_DSN=https://your-sentry-dsn@sentry.io/your-project-id
```

### 7. Run Migrations
```bash
python manage.py migrate
```

### 8. Create Superuser
```bash
python manage.py createsuperuser
# Follow the prompts to create admin account
```

### 9. Collect Static Files
```bash
python manage.py collectstatic --noinput
```

### 10. Start Development Server
```bash
python manage.py runserver
```

Server runs at: http://localhost:8000

## API Documentation

### Swagger UI
- **URL:** http://localhost:8000/api/docs/
- **Interactive API exploration and testing**

### ReDoc
- **URL:** http://localhost:8000/api/redoc/
- **API documentation in ReDoc format**

### OpenAPI Schema
- **URL:** http://localhost:8000/api/schema/

## Database Setup

### Using PostgreSQL (Recommended for Production)

```bash
# 1. Install PostgreSQL
# https://www.postgresql.org/download/

# 2. Create database and user
psql -U postgres
CREATE DATABASE stayafrica_db;
CREATE USER stayafrica WITH PASSWORD 'secure_password';
ALTER ROLE stayafrica SET client_encoding TO 'utf8';
ALTER ROLE stayafrica SET default_transaction_isolation TO 'read committed';
ALTER ROLE stayafrica SET default_transaction_deferrable TO on;
ALTER ROLE stayafrica SET timezone TO 'UTC';
GRANT ALL PRIVILEGES ON DATABASE stayafrica_db TO stayafrica;
\q

# 3. Install PostGIS for geographic queries
psql -U postgres -d stayafrica_db -c "CREATE EXTENSION postgis;"
psql -U postgres -d stayafrica_db -c "CREATE EXTENSION postgis_topology;"

# 4. Update .env with PostgreSQL credentials
```

### Using SQLite (Development Only)

SQLite is configured automatically in development:
```
DATABASE_ENGINE=django.db.backends.sqlite3
DATABASE_NAME=db.sqlite3
```

**Note:** SQLite doesn't support PostGIS, so location queries won't work. Use PostgreSQL for full functionality.

## Async Tasks (Celery)

### Starting Celery Worker
```bash
# In a new terminal, activate venv first
celery -A stayafrica worker -l info
```

### Starting Celery Beat (Scheduled Tasks)
```bash
# In another new terminal
celery -A stayafrica beat -l info
```

## Testing

### Run All Tests
```bash
pytest
```

### Run Tests with Coverage
```bash
pytest --cov=apps --cov-report=html
```

### Run Specific Test File
```bash
pytest apps/users/tests/test_views.py
```

### Run Specific Test Case
```bash
pytest apps/users/tests/test_views.py::UserTestCase::test_user_registration
```

## Common Issues & Solutions

### Issue 1: `ModuleNotFoundError: No module named 'django_filters'`

**Cause:** Incomplete or interrupted installation

**Solution:**
```bash
pip install --force-reinstall -r requirements.txt
```

### Issue 2: `psycopg compilation error` on Windows

**Cause:** Missing Visual C++ build tools or incompatible binary

**Solution:** Already fixed! We use `psycopg[binary]` 3.1.18 which includes pre-built binaries for Windows.

```bash
pip install -r requirements.txt
```

### Issue 3: `djangorestframework-simplejwt` version not found

**Cause:** Old requirements used non-existent version 5.3.2

**Solution:** Already fixed! Requirements now use valid version 5.5.1.

Verify:
```bash
pip show djangorestframework-simplejwt
# Should show 5.5.1
```

### Issue 4: `ImportError: No module named 'sentry_sdk'`

**Cause:** Sentry is optional but was hard-required in settings

**Solution:** Already fixed! Settings.py now makes sentry optional.

If you want error tracking:
```bash
pip install sentry-sdk
# Then set SENTRY_DSN in .env
```

### Issue 5: PostgreSQL connection refused

**Cause:** PostgreSQL service not running

**Solution:**
```bash
# Windows
net start postgresql-x64-15  # or your version

# macOS
brew services start postgresql

# Linux
sudo systemctl start postgresql
```

### Issue 6: Redis connection refused

**Cause:** Redis service not running

**Solution:**
```bash
# Windows (with WSL2)
wsl redis-server

# or use Docker
docker run -d -p 6379:6379 redis:7

# macOS
brew services start redis

# Linux
sudo systemctl start redis-server
```

## Docker Setup (Alternative)

If you don't want to install PostgreSQL and Redis locally, use Docker:

```bash
# Start PostgreSQL and Redis with Docker Compose
docker-compose up -d

# Run migrations
python manage.py migrate

# Start dev server
python manage.py runserver
```

## Project Structure

```
backend/
├── stayafrica/              # Django project settings
│   ├── settings.py          # Main settings
│   ├── urls.py              # URL routing
│   ├── wsgi.py              # WSGI configuration
│   └── celery.py            # Celery configuration
├── apps/                    # Django applications
│   ├── users/               # User management & authentication
│   ├── properties/          # Property listings
│   ├── bookings/            # Booking management
│   ├── payments/            # Payment processing
│   ├── reviews/             # Reviews & ratings
│   ├── messaging/           # Messaging system
│   └── admin_dashboard/     # Admin dashboard
├── services/                # Business logic layer
│   ├── payment_gateway.py   # Payment integration
│   ├── email_service.py     # Email handling
│   ├── image_processor.py   # Image processing
│   └── audit_logger.py      # Audit logging
├── tests/                   # Test suite
├── manage.py                # Django management script
├── requirements.txt         # Core dependencies
├── requirements-dev.txt     # Optional dependencies
└── SETUP.md                 # This file
```

## Key Files

### `requirements.txt` (Core Dependencies)
Only essential packages that work on all platforms:
- Django 5.0.0
- djangorestframework 3.14.0
- psycopg[binary] 3.1.18 (Windows compatible)
- Celery, Redis, PostgreSQL driver
- JWT, CORS, filtering, documentation

### `requirements-dev.txt` (Optional)
Additional packages for advanced features:
- Pillow, django-imagekit (image processing)
- django-storages, boto3 (AWS S3)
- sentry-sdk (error tracking)
- Testing tools (pytest, coverage)

### `stayafrica/settings.py` (Configuration)
- All imports are conditional (optional packages)
- Environment variables for all secrets
- Database, cache, and async task configuration
- Email and storage backends
- API documentation enabled

## Useful Commands

```bash
# Database
python manage.py makemigrations          # Create migrations
python manage.py migrate                 # Run migrations
python manage.py migrate --fake-initial  # Fake initial migration
python manage.py dbshell                 # Open database shell

# Admin
python manage.py createsuperuser         # Create admin user
python manage.py changepassword username # Change user password

# Static files
python manage.py collectstatic           # Collect static files

# Shell
python manage.py shell                   # Interactive Python shell

# Debugging
python manage.py runserver --reload      # Auto-reload on file changes
python manage.py runserver 0.0.0.0:8001  # Accessible from network

# Celery
celery -A stayafrica worker -l debug     # Debug mode
celery -A stayafrica beat -l info        # Scheduled tasks
celery -A stayafrica purge               # Clear message queue
```

## Next Steps

1. **Frontend Development:** See `../../web/README.md`
2. **Mobile Development:** See `../../mobile/README.md`
3. **API Documentation:** Visit http://localhost:8000/api/docs/
4. **Feature Implementation:** Check the service layer in `services/`

## Support

For issues or questions:
1. Check this guide's troubleshooting section
2. Review Django documentation: https://docs.djangoproject.com/
3. Check DRF documentation: https://www.django-rest-framework.org/
4. Open an issue on GitHub

---

**Last Updated:** December 7, 2025
**Python Version Tested:** 3.13.9
**Django Version:** 5.0.0
