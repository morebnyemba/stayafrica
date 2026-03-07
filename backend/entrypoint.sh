#!/bin/sh
set -e

echo "⏳ Waiting for database..."

# Simple netcat-based DB wait (more reliable than Python script)
until nc -z -w1 $DB_HOST $DB_PORT; do
  echo "Database not ready, waiting..."
  sleep 1
done

echo "✅ Database ready"

# ---------------------------------------------------------------------------
# Smart migration flow:
# 1. Check for pending migrations BEFORE running makemigrations
# 2. Only makemigrations in dev (DJANGO_ENV != production)
# 3. Apply with --fake-initial fallback for initial migrations
# 4. Run schema drift check to catch issues early
# ---------------------------------------------------------------------------

# Step 1: Show migration status
echo "📋 Checking migration status..."
python manage.py showmigrations --plan 2>&1 | grep "\[ \]" | head -20 || echo "  All migrations applied."

# Step 2: makemigrations only in non-production (prod should ship migration files)
if [ "${DJANGO_ENV}" = "production" ]; then
  echo "🔒 Production mode — skipping makemigrations (migrations must be committed)"
else
  echo "📝 Dev mode — generating migration files..."
  python manage.py makemigrations --noinput 2>&1 || echo "⚠️ makemigrations had issues (may be OK if migrations are pre-built)"
fi

# Step 3: Apply migrations with smart fallback
echo "🔄 Running database migrations..."
if ! python manage.py migrate --noinput 2>&1; then
  echo "⚠️ Standard migrate failed, trying --fake-initial..."
  if ! python manage.py migrate --fake-initial --noinput 2>&1; then
    echo "⚠️ --fake-initial also failed, running diagnostics..."
    python manage.py fix_migrations 2>&1 || true
    echo "❌ Migration failed — check output above for details"
    exit 1
  fi
fi

# Step 4: Quick schema drift check (non-blocking)
echo "🔍 Verifying schema integrity..."
python manage.py fix_migrations 2>&1 || echo "⚠️ Schema check had issues (non-blocking)"

# If a command was passed (e.g. from docker-compose "command:"), run it.
# Otherwise default to starting Uvicorn.
if [ $# -gt 0 ]; then
  echo "🚀 Running command: $@"
  exec "$@"
else
  echo "🚀 Starting Uvicorn ASGI server..."
  exec uvicorn stayafrica.asgi:application --host 0.0.0.0 --port 8000 --workers 4 --loop uvloop --http httptools --ws websockets --log-level info
fi
