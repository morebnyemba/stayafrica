# Property ID Migration Guide

## Overview
Migrating Property model from integer primary key to 10-digit string primary key.

## Changes Made

### Backend
1. **Property Model** (`backend/apps/properties/models.py`):
   - Changed `id` field to `CharField(max_length=10, primary_key=True)`
   - Auto-generates 10-digit random numeric string on save

2. **SavedPropertySerializer** (`backend/apps/properties/serializers.py`):
   - Changed `property_id` from `IntegerField` to `CharField(max_length=10)`

### Frontend
- **Already Compatible**: All property ID references use `string` type ✓

## Migration Steps

### Step 1: Create Migrations (in Docker)
```bash
docker compose -f docker-compose.prod.yml exec backend python manage.py makemigrations properties --name change_property_id_to_string
```

### Step 2: Review Migration
The migration will:
- Add temporary `new_id` CharField field
- Generate 10-digit IDs for existing properties
- Update all foreign key references
- Drop old integer `id` field
- Rename `new_id` to `id`

### Step 3: Backup Database
```bash
docker compose -f docker-compose.prod.yml exec db pg_dump -U postgres stayafrica > backup_before_id_migration.sql
```

### Step 4: Apply Migration
```bash
docker compose -f docker-compose.prod.yml exec backend python manage.py migrate properties
```

### Step 5: Rebuild Backend
```bash
docker compose -f docker-compose.prod.yml build backend
docker compose -f docker-compose.prod.yml up -d backend celery celery-beat
```

### Step 6: Test
- Create new property → should get 10-digit ID
- View existing property → URLs should work
- Edit property → should work with new ID
- Save property to wishlist → should work

## Rollback Plan
If something goes wrong:
```bash
docker compose -f docker-compose.prod.yml exec db psql -U postgres stayafrica < backup_before_id_migration.sql
```

## Expected Results
- Properties will have IDs like: `1234567890`
- URLs: `https://zimlegend.online/property/1234567890`
- All foreign key relationships maintained
- Frontend works without changes
