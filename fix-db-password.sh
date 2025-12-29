#!/usr/bin/env bash
# Quick fix for production DB password mismatch
# Run this on the production server to sync the DB password with .env.prod

set -e

echo "🔧 Syncing database password for production environment..."

# Read password from .env.prod
if [ -f backend/.env.prod ]; then
    export $(grep DATABASE_PASSWORD backend/.env.prod | xargs)
    export $(grep DATABASE_USER backend/.env.prod | xargs)
else
    echo "❌ backend/.env.prod not found!"
    exit 1
fi

echo "Setting password for user: $DATABASE_USER"

# Update password inside the running DB
docker exec -i stayafrica_db psql -U postgres -c "ALTER USER $DATABASE_USER WITH PASSWORD '$DATABASE_PASSWORD';" 2>&1

if [ $? -eq 0 ]; then
    echo "✅ Password updated successfully"
    echo "🔄 Rebuilding and restarting backend services..."
    docker compose -f docker-compose.prod.yml build backend
    docker compose -f docker-compose.prod.yml up -d backend celery celery-beat
    echo "✅ Services restarted"
else
    echo "❌ Failed to update password"
    exit 1
fi
