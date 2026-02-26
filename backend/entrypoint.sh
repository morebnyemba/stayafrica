#!/bin/sh
set -e

echo "⏳ Waiting for database..."

# Simple netcat-based DB wait (more reliable than Python script)
until nc -z -w1 $DB_HOST $DB_PORT; do
  echo "Database not ready, waiting..."
  sleep 1
done

echo "✅ Database ready"

# Generate migration files for local apps (they are not committed to the repo)
echo "📝 Generating migration files..."
python manage.py makemigrations users properties bookings payments reviews messaging admin_dashboard experiences notifications health --noinput || true

echo "🔧 Fixing migration sequence / stale records if needed..."
python scripts/fix_migration_sequence.py

echo "🔄 Running database migrations..."
if ! python manage.py migrate --fake-initial --noinput; then
  echo "❌ Migration failed"
  exit 1
fi

echo "🚀 Starting Daphne ASGI server..."
exec daphne -b 0.0.0.0 -p 8000 stayafrica.asgi:application
