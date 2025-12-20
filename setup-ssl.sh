#!/bin/bash

# SSL Certificate Setup Script for zimlegend.online domains

echo "Step 1: Obtain SSL certificates"
echo "================================"

# Stop nginx temporarily
docker compose -f docker-compose.prod.yml stop nginx

# Get certificate for main domain
docker compose -f docker-compose.prod.yml run --rm certbot certonly \
    --standalone \
    --email your-email@example.com \
    --agree-tos \
    --no-eff-email \
    -d zimlegend.online \
    -d www.zimlegend.online

# Get certificate for API domain
docker compose -f docker-compose.prod.yml run --rm certbot certonly \
    --standalone \
    --email your-email@example.com \
    --agree-tos \
    --no-eff-email \
    -d api.zimlegend.online

echo ""
echo "Step 2: Start nginx with SSL"
echo "============================="
docker compose -f docker-compose.prod.yml up -d nginx

echo ""
echo "✅ SSL certificates installed!"
echo "Your sites are now accessible at:"
echo "  - https://zimlegend.online (Frontend)"
echo "  - https://api.zimlegend.online (Backend API)"
