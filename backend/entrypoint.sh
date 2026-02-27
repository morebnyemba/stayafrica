#!/bin/sh
set -e

echo "⏳ Waiting for database..."

# Simple netcat-based DB wait (more reliable than Python script)
until nc -z -w1 $DB_HOST $DB_PORT; do
  echo "Database not ready, waiting..."
  sleep 1
done

echo "✅ Database ready"

echo "� Generating migration files..."
python manage.py makemigrations --noinput 2>&1 || echo "⚠️ makemigrations had issues (may be OK if migrations are pre-built)"

echo "🔄 Running database migrations..."
if ! python manage.py migrate --noinput 2>&1; then
  echo "⚠️ Standard migrate failed, trying --fake-initial..."
  if ! python manage.py migrate --fake-initial --noinput; then
    echo "❌ Migration failed"
    exit 1
  fi
fi

echo "🚀 Starting Uvicorn ASGI server..."
exec uvicorn stayafrica.asgi:application --host 0.0.0.0 --port 8000 --workers 4 --loop uvloop --http httptools --ws websockets --log-level info
