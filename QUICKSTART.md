# Quick Start Guide - After Configuration Changes

## 🚀 What Changed?

Your StayAfrica application has been updated with:
- **Single `.env` file** for all configuration (root level)
- **Daphne ASGI server** instead of Gunicorn
- **No more entrypoint scripts** - cleaner Docker setup
- **Guaranteed credential consistency** across all services

## ⚡ Quick Deployment (5 minutes)

### Step 1: Pull Changes
```bash
cd ~/stayafrica
git pull
```

### Step 2: Create .env File
```bash
cp .env.example .env
nano .env  # Edit with your settings
```

**Required changes:**
- Set `SECRET_KEY` (generate new one)
- Set `DATABASE_PASSWORD` (use strong password)
- Set `JWT_SECRET_KEY` (generate new one)
- Update `ALLOWED_HOSTS` with your domain

### Step 3: Deploy
```bash
# Stop old services
docker compose -f docker-compose.prod.yml down

# Optional: Clean database (⚠️ deletes all data!)
docker volume rm stayafrica_postgres_data

# Rebuild and start
docker compose -f docker-compose.prod.yml build backend
docker compose -f docker-compose.prod.yml up -d
```

### Step 4: Verify
```bash
# Check all services are running
docker compose -f docker-compose.prod.yml ps

# Watch logs
docker compose -f docker-compose.prod.yml logs -f
```

## 🔐 Security Checklist

- [ ] Changed `SECRET_KEY` to unique value
- [ ] Changed `DATABASE_PASSWORD` to strong password
- [ ] Changed `JWT_SECRET_KEY` to unique value
- [ ] Set `DEBUG=False` in production
- [ ] Secured .env file: `chmod 600 .env`
- [ ] Verified .env is NOT in git: `git status`
- [ ] Updated email settings (if using email)

## ✅ Success Indicators

**Backend (Daphne):**
```
Starting server at tcp:port=8000:interface=0.0.0.0
Listening on TCP address 0.0.0.0:8000
```

**Celery Worker:**
```
celery@<hostname> ready.
```

**Celery Beat:**
```
beat: Starting...
```

**No errors like:**
- ❌ "password authentication failed"
- ❌ "No module named 'daphne'"
- ❌ Connection refused

## 📚 Documentation

- **Full Migration Guide**: `CONFIGURATION_MIGRATION.md`
- **Deployment Guide**: `DEPLOYMENT_GUIDE.md`
- **Security Tips**: See "Security Best Practices" in DEPLOYMENT_GUIDE.md

## 🆘 Common Issues

### "password authentication failed"
```bash
# Verify .env exists and is loaded
ls -la .env
docker compose -f docker-compose.prod.yml config | grep DATABASE_PASSWORD
```

### "No module named 'daphne'"
```bash
# Rebuild backend
docker compose -f docker-compose.prod.yml build --no-cache backend
docker compose -f docker-compose.prod.yml up -d
```

### Migrations not running
```bash
# Check migrate service
docker compose -f docker-compose.prod.yml logs migrate

# Run manually if needed
docker compose -f docker-compose.prod.yml run --rm backend python manage.py migrate
```

## 🎯 Key Files

| File | Purpose | In Git? |
|------|---------|---------|
| `.env` | Production config (secrets) | ❌ No |
| `.env.example` | Template for .env | ✅ Yes |
| `docker-compose.prod.yml` | Service orchestration | ✅ Yes |
| `backend/Dockerfile` | Backend image definition | ✅ Yes |
| `CONFIGURATION_MIGRATION.md` | Full migration guide | ✅ Yes |
| `DEPLOYMENT_GUIDE.md` | Deployment procedures | ✅ Yes |

## 💡 Pro Tips

1. **Backup your .env**: `cp .env .env.backup`
2. **Monitor logs**: Use `docker compose logs -f --tail=50`
3. **Check health**: All services should show "healthy" or "running"
4. **Generate keys**: Use Python to generate secure keys:
   ```bash
   python3 -c "from django.core.management.utils import get_random_secret_key; print(get_random_secret_key())"
   ```

## 🏗️ Architecture Overview

```
┌─────────────────────────────────────┐
│        Root .env File               │
│    (Single Source of Truth)         │
└─────────────┬───────────────────────┘
              │
    ┌─────────┼──────────┐
    ▼         ▼          ▼
  ┌────┐  ┌────────┐  ┌────────┐
  │ DB │  │Backend │  │Celery  │
  │    │  │Daphne  │  │Workers │
  └────┘  └────────┘  └────────┘
  
  All services read from same .env
  → No credential mismatches!
```

## 🎉 Benefits

✅ **Simpler**: One config file vs multiple  
✅ **Safer**: Guaranteed credential consistency  
✅ **Faster**: ASGI support for async operations  
✅ **Clearer**: No complex entrypoint scripts  
✅ **Easier**: Simpler troubleshooting and debugging  

---

**Need help?** See full documentation in CONFIGURATION_MIGRATION.md and DEPLOYMENT_GUIDE.md
