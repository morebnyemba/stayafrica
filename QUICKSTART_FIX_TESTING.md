# Quick Start: Testing the Fixes

**Date**: January 19, 2026  
**Purpose**: Quick guide to verify the URL namespace and migration fixes work correctly

---

## ✅ Validate Fixes (Without Docker)

Run the automated test suite:

```bash
cd backend
python scripts/test_fixes.py
```

**Expected Output**:
```
============================================================
StayAfrica Fix Validation Tests
============================================================

✓ URL namespace correctly set to 'admin_dashboard'
✓ Migration fix script exists
✓ Entrypoint calls fix_migration_sequence.py
✓ Fix script runs before migrations (correct order)
✓ Database cleanup script exists
✓ Main URLs file exists
✓ Admin dashboard URLs included

5/5 tests passed

All tests passed! ✓
```

---

## 🚀 Test With Docker (Full Integration)

### Option 1: Fresh Start (Clean Database)

```bash
# Navigate to project root
cd /home/runner/work/stayafrica/stayafrica

# Start services
docker compose up -d

# Watch the logs
docker compose logs backend -f
```

**What to Look For**:
```
✅ Database ready
🔧 Fixing migration sequence if needed...
✓ Sequence does not exist yet (first run)
🔄 Running database migrations...
Operations to perform:
  Apply all migrations: [list of apps]
Running migrations:
  Applying contenttypes.0001_initial... OK
  Applying auth.0001_initial... OK
  [more migrations...]
🚀 Starting Daphne ASGI server...
```

**Success Indicators**:
- ✅ No "django_migrations_id_seq already exists" error
- ✅ No "URL namespace 'admin' isn't unique" warning
- ✅ All migrations complete successfully
- ✅ Server starts without errors

### Option 2: Test With Existing Database

```bash
# Stop containers but keep volumes
docker compose down

# Start again (this simulates the original error condition)
docker compose up -d

# Watch logs
docker compose logs backend -f
```

**What to Look For**:
```
✅ Database ready
🔧 Fixing migration sequence if needed...
✓ django_migrations table exists, checking sequence...
✓ Sequence reset to [number]
🔄 Running database migrations...
No migrations to apply.
🚀 Starting Daphne ASGI server...
```

**Success Indicators**:
- ✅ Sequence automatically fixed before migrations
- ✅ No errors about duplicate sequences
- ✅ Migrations complete (or skip if already applied)

---

## 🔍 Verify Services Are Running

```bash
# Check all containers are up
docker compose ps

# Should show:
# stayafrica_backend   Up
# stayafrica_db        Up (healthy)
# stayafrica_redis     Up (healthy)
# stayafrica_celery    Up
# stayafrica_celery_beat  Up
```

### Test API Endpoints

```bash
# Health check
curl http://localhost:8000/api/health/

# Expected: {"status": "ok"}

# API docs (check no admin namespace conflicts)
curl http://localhost:8000/api/docs/

# Should load Swagger UI without errors
```

### Test Admin Interface

```bash
# Access Django admin (should work without namespace conflicts)
# Open browser to: http://localhost:8000/admin/

# Access custom admin dashboard API
curl http://localhost:8000/api/v1/admin/stats/dashboard/

# Should return dashboard stats (requires authentication)
```

---

## 🧪 Test Database Cleanup Tool

```bash
# Run the diagnostic tool
docker compose exec backend python scripts/cleanup_database.py
```

**Expected Output**:
```
============================================================
Database Cleanup Script
============================================================

1. Checking migration sequence...
  ✓ Sequence reset to [number]

2. Current migration status:
  Last 10 applied migrations:
    - users: 0001_initial
    - properties: 0001_initial
    [more migrations...]

3. Checking for duplicate sequences...
  ✓ No duplicate sequences found

============================================================
Cleanup complete!
============================================================
```

---

## 🐛 Troubleshooting

### If Container Fails to Start

1. **Check logs**:
   ```bash
   docker compose logs backend --tail=50
   ```

2. **Run cleanup manually**:
   ```bash
   docker compose exec backend python scripts/cleanup_database.py
   ```

3. **Check database connection**:
   ```bash
   docker compose exec backend python manage.py dbshell
   # If this fails, database is not accessible
   ```

### If "Sequence Already Exists" Error Still Occurs

1. **Stop all services**:
   ```bash
   docker compose down
   ```

2. **Remove database volume** (⚠️ destroys data):
   ```bash
   docker volume rm stayafrica_postgres_data
   ```

3. **Start fresh**:
   ```bash
   docker compose up -d
   ```

### If URL Namespace Warning Appears

1. **Verify the fix is applied**:
   ```bash
   cd backend
   python scripts/test_fixes.py
   ```

2. **Check the admin_dashboard URLs file**:
   ```bash
   grep "app_name" apps/admin_dashboard/urls.py
   # Should output: app_name = 'admin_dashboard'
   ```

3. **If still showing 'admin'**, update it:
   ```bash
   # Edit the file
   nano apps/admin_dashboard/urls.py
   # Change: app_name = 'admin'
   # To:     app_name = 'admin_dashboard'
   ```

---

## 📊 Verify Backend-Frontend Integration

### Check API Endpoints

```bash
# Properties
curl http://localhost:8000/api/v1/properties/

# Users (requires auth)
curl http://localhost:8000/api/v1/users/profile/ \
  -H "Authorization: Bearer YOUR_TOKEN"

# Admin dashboard (requires admin auth)
curl http://localhost:8000/api/v1/admin/stats/dashboard/ \
  -H "Authorization: Bearer ADMIN_TOKEN"
```

### Test Frontend Connection

1. **Start the web frontend** (separate terminal):
   ```bash
   cd web
   npm install
   npm run dev
   ```

2. **Open browser**: http://localhost:3000

3. **Verify features work**:
   - ✅ Login/Register
   - ✅ Browse properties
   - ✅ Search and filters
   - ✅ User dashboard
   - ✅ Host dashboard (if host user)

---

## ✨ Success Checklist

After following this guide, you should have:

- [ ] All validation tests passing (5/5)
- [ ] Backend container running successfully
- [ ] Database migrations completed without errors
- [ ] No URL namespace warnings
- [ ] No migration sequence errors
- [ ] API endpoints responding correctly
- [ ] Admin interface accessible
- [ ] Custom admin dashboard accessible
- [ ] Frontend can connect to backend APIs

---

## 🎯 Next Steps

If all checks pass:

1. **Deploy to staging/production**
2. **Monitor logs** for the first few deployments
3. **Run validation tests** periodically
4. **Keep database backups**

If any checks fail:

1. **Review error messages** in logs
2. **Run diagnostic tool**: `python scripts/cleanup_database.py`
3. **Check troubleshooting section** above
4. **Refer to main documentation**: `FIX_SUMMARY_URL_MIGRATION_JAN2026.md`

---

## 📚 Related Documentation

- `FIX_SUMMARY_URL_MIGRATION_JAN2026.md` - Complete fix documentation
- `BACKEND_FRONTEND_INTEGRATION_STATUS.md` - Integration analysis
- `backend/scripts/test_fixes.py` - Validation test suite
- `backend/scripts/cleanup_database.py` - Database diagnostic tool

---

**Ready to deploy? All systems go! 🚀**
