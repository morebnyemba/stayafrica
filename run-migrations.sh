#!/bin/bash
# Database Migration Script for StayAfrica
# This script runs Django migrations on the backend container

echo "=========================================="
echo "StayAfrica Database Migration Script"
echo "=========================================="
echo ""

# Check if docker-compose or docker compose is available
if command -v docker-compose >/dev/null 2>&1; then
    DOCKER_COMPOSE="docker-compose"
elif docker compose version >/dev/null 2>&1; then
    DOCKER_COMPOSE="docker compose"
else
    echo "❌ Error: docker-compose/docker compose is not available"
    exit 1
fi

# Check if backend container is running
check_backend_running() {
    # Try newer docker compose ps syntax first
    if $DOCKER_COMPOSE ps backend --status running --services >/dev/null 2>&1; then
        # If command succeeds, backend is running (service name is already filtered)
        return 0
    else
        # Fall back to parsing ps output for older docker-compose versions
        $DOCKER_COMPOSE ps | grep -q "stayafrica_backend.*Up"
        return $?
    fi
}

if ! check_backend_running; then
    echo "❌ Error: Backend container is not running"
    echo "Please start the containers first with: $DOCKER_COMPOSE up -d"
    exit 1
fi

echo "✅ Backend container is running"
echo ""

# Run migrations
echo "🔄 Running database migrations..."
if $DOCKER_COMPOSE exec backend python manage.py migrate; then
    echo ""
    echo "✅ Migrations completed successfully"
    echo ""
    echo "🎉 Database is ready to use"
else
    echo ""
    echo "❌ Migration failed"
    exit 1
fi
