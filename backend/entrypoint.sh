#!/usr/bin/env sh
set -e

echo "⏳ Waiting for database to be ready..."

# Wait for PostgreSQL to be ready using pg_isready (no auth required)
until pg_isready -h "${DATABASE_HOST:-db}" -p "${DATABASE_PORT:-5432}" -U "${DATABASE_USER:-postgres}" > /dev/null 2>&1
do
    echo "⚠️  Database not ready yet, retrying in 2 seconds..."
    sleep 2
done

echo "✅ Database is ready"
echo "🔄 Running database migrations..."
python manage.py migrate --noinput

echo "🚀 Starting Gunicorn server..."
exec gunicorn stayafrica.wsgi:application \
    --bind 0.0.0.0:8000 \
    --workers "${GUNICORN_WORKERS:-4}" \
    --timeout "${GUNICORN_TIMEOUT:-60}"
