#!/usr/bin/env sh
set -e

echo "⏳ Waiting for database to be ready..."

# Wait for PostgreSQL to be ready and accepting connections
# Use the password from environment which should be loaded from .env.prod
DB_PASSWORD="${DATABASE_PASSWORD}"
DB_USER="${DATABASE_USER:-postgres}"
DB_HOST="${DATABASE_HOST:-db}"
DB_PORT="${DATABASE_PORT:-5432}"
DB_NAME="${DATABASE_NAME:-stayafrica_db}"

until python -c "
import sys, psycopg
try:
    conn = psycopg.connect(
        host='$DB_HOST',
        port=$DB_PORT,
        user='$DB_USER',
        password='$DB_PASSWORD',
        dbname='$DB_NAME',
        connect_timeout=3
    )
    conn.close()
    print('✅ Database is ready')
except Exception as e:
    print(f'⚠️  Waiting for database: {e}', file=sys.stderr)
    sys.exit(1)
" 2>&1
do
    echo "Database not ready yet, retrying in 2 seconds..."
    sleep 2
done

echo "🔄 Running database migrations..."
python manage.py migrate --noinput

echo "🚀 Starting Gunicorn server..."
exec gunicorn stayafrica.wsgi:application \
    --bind 0.0.0.0:8000 \
    --workers "${GUNICORN_WORKERS:-4}" \
    --timeout "${GUNICORN_TIMEOUT:-60}"
