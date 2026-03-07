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
# 1. Auto-generate any new migrations from model changes
# 2. Apply with --fake-initial fallback
# 3. Verify schema integrity
# Source code is bind-mounted (./backend:/app) so migrations persist to disk.
# ---------------------------------------------------------------------------

# Step 1: Show pending migrations
echo "📋 Checking migration status..."
python manage.py showmigrations --plan 2>&1 | grep "\[ \]" | head -20 || echo "  All migrations applied."

# Step 2: Auto-generate migrations from model changes
echo "📝 Generating migration files..."
python manage.py makemigrations --noinput 2>&1 || echo "⚠️ makemigrations had issues (may be OK if migrations are pre-built)"

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
