#!/usr/bin/env sh
set -e

# Run database migrations before starting the app
python manage.py migrate --noinput

# Start gunicorn
exec gunicorn stayafrica.wsgi:application \
    --bind 0.0.0.0:8000 \
    --workers "${GUNICORN_WORKERS:-4}" \
    --timeout "${GUNICORN_TIMEOUT:-60}"
