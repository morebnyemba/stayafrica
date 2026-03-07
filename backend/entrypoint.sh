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
# Migration flow:
# 1. Restore previously generated migrations from persistent volume
# 2. Auto-generate any new migrations from model changes
# 3. Persist generated migrations back to volume
# 4. Apply with --fake-initial fallback
# 5. Verify schema integrity
# ---------------------------------------------------------------------------

MIGRATIONS_STORE="/app/migrations_store"

# Step 1: Restore migrations from persistent volume (if it exists)
if [ -d "$MIGRATIONS_STORE" ]; then
  echo "📂 Restoring migrations from persistent storage..."
  for app_dir in "$MIGRATIONS_STORE"/*/; do
    app_name=$(basename "$app_dir")
    target="/app/apps/$app_name/migrations"
    if [ -d "$target" ]; then
      cp -n "$app_dir"*.py "$target/" 2>/dev/null || true
      echo "  ✓ Restored migrations for $app_name"
    fi
  done
else
  echo "📂 Creating migrations store..."
  mkdir -p "$MIGRATIONS_STORE"
fi

# Step 2: Show pending migrations
echo "📋 Checking migration status..."
python manage.py showmigrations --plan 2>&1 | grep "\[ \]" | head -20 || echo "  All migrations applied."

# Step 3: Auto-generate migrations from model changes
echo "📝 Generating migration files..."
python manage.py makemigrations --noinput 2>&1 || echo "⚠️ makemigrations had issues (may be OK if migrations are pre-built)"

# Step 4: Persist all migrations back to the volume
echo "💾 Persisting migrations to storage..."
for app_migrations in /app/apps/*/migrations/; do
  app_name=$(basename "$(dirname "$app_migrations")")
  store_dir="$MIGRATIONS_STORE/$app_name"
  mkdir -p "$store_dir"
  cp "$app_migrations"*.py "$store_dir/" 2>/dev/null || true
done

# Step 5: Apply migrations with smart fallback
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

# Step 6: Quick schema drift check (non-blocking)
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
