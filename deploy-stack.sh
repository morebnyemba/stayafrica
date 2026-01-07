#!/bin/bash

# StayAfrica Docker Stack Deployment Script
# Run this after deploy-setup.sh has completed and certificates are issued
# Usage: bash deploy-stack.sh /path/to/docker-compose.prod.yml

set -e

COMPOSE_FILE="${1:-docker-compose.prod.yml}"
PROJECT_NAME="stayafrica"

if [ ! -f "$COMPOSE_FILE" ]; then
    echo "Error: $COMPOSE_FILE not found"
    echo "Usage: bash deploy-stack.sh /path/to/docker-compose.prod.yml"
    exit 1
fi

echo "===== StayAfrica Stack Deployment ====="
echo "Compose file: $COMPOSE_FILE"
echo ""

# Check for required environment files
if [ ! -f ".env" ]; then
    echo "Error: .env file not found in current directory"
    exit 1
fi

if [ ! -f ".env.prod" ]; then
    echo "Error: .env.prod file not found in current directory"
    exit 1
fi

echo "✓ Environment files found"
echo ""

# Check certificate directories
CERT_DIR="/etc/letsencrypt/live"
if [ ! -d "$CERT_DIR/stayafrica.app" ]; then
    echo "Warning: Certificate directory $CERT_DIR/stayafrica.app not found"
    echo "Make sure certificates are issued before starting the stack"
fi

if [ ! -d "$CERT_DIR/api.stayafrica.app" ]; then
    echo "Warning: Certificate directory $CERT_DIR/api.stayafrica.app not found"
    echo "Make sure certificates are issued before starting the stack"
fi

echo ""

# Create logs directory
mkdir -p ./logs
chmod 755 ./logs

# Check Docker status
echo "Checking Docker daemon..."
if ! docker ps > /dev/null 2>&1; then
    echo "Error: Docker daemon is not running"
    exit 1
fi
echo "✓ Docker daemon is running"
echo ""

# Pull latest images
echo "Pulling latest images..."
docker compose -f "$COMPOSE_FILE" pull

# Stop existing containers (if any)
echo "Stopping existing containers..."
docker compose -f "$COMPOSE_FILE" down || true

# Start the stack
echo "Starting StayAfrica stack..."
docker compose -f "$COMPOSE_FILE" up -d

# Wait for services to start
echo "Waiting for services to initialize..."
sleep 15

# Check service status
echo ""
echo "===== Service Status ====="
docker compose -f "$COMPOSE_FILE" ps

echo ""
echo "===== Container Logs (last 20 lines per service) ====="
docker compose -f "$COMPOSE_FILE" logs --tail=20

echo ""
echo "===== Deployment Complete ====="
echo ""
echo "Services running:"
echo "  Frontend:  https://stayafrica.app"
echo "  API:       https://api.stayafrica.app"
echo "  Portainer: https://your-server-ip:9443"
echo ""
echo "To view logs:"
echo "  All services: docker compose -f $COMPOSE_FILE logs -f"
echo "  Specific:     docker compose -f $COMPOSE_FILE logs -f <service>"
echo ""
echo "To stop the stack:"
echo "  docker compose -f $COMPOSE_FILE down"
echo ""
echo "To restart the stack:"
echo "  docker compose -f $COMPOSE_FILE restart"
echo ""
echo "To remove everything (volumes included):"
echo "  docker compose -f $COMPOSE_FILE down -v"
echo ""
