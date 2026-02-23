# Fix: Database Migration Issue - instant_booking_enabled Field

## Problem
The application was experiencing `ProgrammingError: column properties_property.instant_booking_enabled does not exist` errors when accessing property-related endpoints.

## Root Cause
The migration file `0002_property_instant_booking.py` was orphaned in the migration chain. The merge migration `0003_merge_0002_initial_0002_savedproperty.py` only merged two of the three conflicting `0002_*` migrations, leaving the instant booking fields unapplied to the database.

## Solution
1. **Created new migration** (`0006_property_instant_booking_fields.py`) to add the missing fields:
   - `instant_booking_enabled` (BooleanField)
   - `instant_booking_requirements` (JSONField)

2. **Renamed orphaned migration** from `0002_property_instant_booking.py` to `0002_property_instant_booking.py.orphaned` to prevent confusion.

3. **Updated entrypoint.sh** to automatically run migrations on container startup, ensuring the database schema is always up-to-date.

## How to Apply
For existing deployments:

1. **Pull the latest changes:**
   ```bash
   git pull origin main
   ```

2. **Rebuild and restart containers:**
   ```bash
   docker-compose down
   docker-compose build backend
   docker-compose up -d
   ```

   Or for production:
   ```bash
   docker compose -f docker-compose.prod.yml down
   docker compose -f docker-compose.prod.yml build backend
   docker compose -f docker-compose.prod.yml up -d
   ```

3. **The migration will run automatically** via the updated entrypoint.sh script.

Alternatively, you can manually run migrations:
```bash
docker-compose exec backend python manage.py migrate
```

## Verification
After applying the fix, test the following endpoints:
- `GET /api/v1/properties/` - Should return property list without errors
- `GET /api/v1/properties/saved/` - Should return saved properties without errors

## Files Changed
- `backend/apps/properties/migrations/0006_property_instant_booking_fields.py` - New migration
- `backend/apps/properties/migrations/0002_property_instant_booking.py.orphaned` - Renamed orphaned migration
- `backend/entrypoint.sh` - Added automatic migration execution
