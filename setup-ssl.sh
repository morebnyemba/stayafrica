#!/bin/bash

# SSL Certificate Setup Script for zimlegend.online domains

echo "Step 1: Create SSL directories"
echo "==============================="
mkdir -p nginx/certbot/conf
mkdir -p nginx/certbot/www

echo ""
echo "Step 2: Stop all containers"
echo "==========================="
docker compose -f docker-compose.prod.yml down

echo ""
echo "Step 3: Obtain SSL certificates (standalone mode)"
echo "==================================================="

# Get certificate for main domain (zimlegend.online + www)
docker compose -f docker-compose.prod.yml run --rm -p 80:80 -p 443:443 \
    -v ./nginx/certbot/conf:/etc/letsencrypt \
    -v ./nginx/certbot/www:/var/www/certbot \
    certbot certonly \
    --standalone \
    --email mnyemba123@gmail.com \
    --agree-tos \
    --no-eff-email \
    --force-renewal \
    -d zimlegend.online \
    -d www.zimlegend.online

# Get certificate for API domain
docker compose -f docker-compose.prod.yml run --rm -p 80:80 -p 443:443 \
    -v ./nginx/certbot/conf:/etc/letsencrypt \
    -v ./nginx/certbot/www:/var/www/certbot \
    certbot certonly \
    --standalone \
    --email mnyemba123@gmail.com \
    --agree-tos \
    --no-eff-email \
    --force-renewal \
    -d api.zimlegend.online

echo ""
echo "Step 4: Verify certificates were created"
echo "========================================="
ls -la nginx/certbot/conf/live/

echo ""
echo "Step 5: Start all services"
echo "=========================="
docker compose -f docker-compose.prod.yml up -d

echo ""
echo "✅ SSL certificates installed!"
echo "Your sites are now accessible at:"
echo "  - https://zimlegend.online (Frontend)"
echo "  - https://api.zimlegend.online (Backend API)"
echo ""
echo "Check nginx status: docker compose -f docker-compose.prod.yml logs nginx"
