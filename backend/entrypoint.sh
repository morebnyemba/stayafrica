#!/usr/bin/env sh
set -e

echo "⏳ Waiting for database port to be open..."

# Simple TCP connection check - no authentication needed
until sh -c 'exec 3<>/dev/tcp/'"${DATABASE_HOST:-db}"'/'"${DATABASE_PORT:-5432}"' && exec 3<&- && exec 3>&-' 2>/dev/null
do
    echo "⚠️  Database port not responding, retrying in 2 seconds..."
    sleep 2
done

echo "✅ Database port is open"
echo "🔄 Running database migrations..."
python manage.py migrate --noinput

echo "🚀 Starting Gunicorn server..."
exec gunicorn stayafrica.wsgi:application \
    --bind 0.0.0.0:8000 \
    --workers "${GUNICORN_WORKERS:-4}" \
    --timeout "${GUNICORN_TIMEOUT:-60}"
