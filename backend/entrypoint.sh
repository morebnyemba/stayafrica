#!/bin/sh
set -e

echo "⏳ Waiting for database..."

# Simple netcat-based DB wait (more reliable than Python script)
until nc -z -w1 $DB_HOST $DB_PORT; do
  echo "Database not ready, waiting..."
  sleep 1
done

echo "✅ Database ready"

# Only collect static files if this is the backend service (not celery workers)
# Check if we're running the main backend server
if [ "$1" = "" ] || [ "$1" = "daphne" ]; then
  echo "📦 Collecting static files..."
  python manage.py collectstatic --noinput
  echo "🚀 Starting Daphne ASGI server..."
  exec daphne -b 0.0.0.0 -p 8000 stayafrica.asgi:application
else
  # For celery workers, just execute the provided command
  exec "$@"
fi
