#!/usr/bin/env sh
set -e

echo "⏳ Waiting for database to be ready..."

# Wait for PostgreSQL to be ready and accepting connections
until python -c "
import os, sys, psycopg
try:
    conn = psycopg.connect(
        host=os.getenv('DATABASE_HOST', 'db'),
        port=int(os.getenv('DATABASE_PORT', '5432')),
        user=os.getenv('DATABASE_USER', 'postgres'),
        password=os.getenv('DATABASE_PASSWORD', 'postgres'),
        dbname=os.getenv('DATABASE_NAME', 'stayafrica_db'),
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
