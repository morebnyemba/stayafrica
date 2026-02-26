#!/bin/bash

# StayAfrica Production Deployment Script
# This script deploys the updated configuration with media file serving fixes

set -e  # Exit on error

echo -e "\033[0;36m🚀 StayAfrica Production Deployment\033[0m"
echo -e "\033[0;36m===================================\033[0m"
echo ""

# Step 1: Stop existing services
echo -e "\033[0;33m📦 Step 1: Stopping existing services...\033[0m"
docker compose -f docker-compose.prod.yml down
echo -e "\033[0;32m✅ Services stopped\033[0m"
echo ""

# Step 2: Create SSL directories
echo -e "\033[0;33m📁 Step 2: Creating SSL certificate directories...\033[0m"
mkdir -p nginx/certbot/conf
mkdir -p nginx/certbot/www
echo -e "\033[0;32m✅ SSL directories created\033[0m"
echo ""

# Step 3: Check for SSL certificates
echo -e "\033[0;33m🔐 Step 3: Checking SSL certificates...\033[0m"
if [ -f "nginx/certbot/conf/live/api.zimlegend.online/fullchain.pem" ]; then
    echo -e "\033[0;32m✅ SSL certificates found\033[0m"
else
    echo -e "\033[0;33m⚠️  SSL certificates not found\033[0m"
    echo -e "\033[0;33m   Run ./setup-ssl.sh to obtain certificates\033[0m"
    echo -e "\033[0;33m   Continuing without SSL (HTTP only)...\033[0m"
fi
echo ""

# Step 4: Build containers
echo -e "\033[0;33m🏗️  Step 4: Building containers...\033[0m"
docker compose -f docker-compose.prod.yml build
echo -e "\033[0;32m✅ Containers built\033[0m"
echo ""

# Step 5: Start services
echo -e "\033[0;33m🚀 Step 5: Starting services...\033[0m"
docker compose -f docker-compose.prod.yml up -d
echo -e "\033[0;32m✅ Services started\033[0m"
echo ""

# Step 6: Wait for backend to be ready
echo -e "\033[0;33m⏳ Step 6: Waiting for backend to be ready...\033[0m"
sleep 10
echo -e "\033[0;32m✅ Backend should be ready\033[0m"
echo ""

# Step 7: Collect static files
echo -e "\033[0;33m📦 Step 7: Collecting static files...\033[0m"
if docker compose -f docker-compose.prod.yml exec -T backend python manage.py collectstatic --noinput; then
    echo -e "\033[0;32m✅ Static files collected\033[0m"
else
    echo -e "\033[0;33m⚠️  Static files collection had issues (this is often not critical)\033[0m"
fi
echo ""

# Step 8: Run migrations
echo -e "\033[0;33m🗄️  Step 8: Running database migrations...\033[0m"
docker compose -f docker-compose.prod.yml exec -T backend python manage.py migrate --noinput
echo -e "\033[0;32m✅ Migrations completed\033[0m"
echo ""

# Step 9: Check service status
echo -e "\033[0;33m🔍 Step 9: Checking service status...\033[0m"
echo ""
docker compose -f docker-compose.prod.yml ps
echo ""

# Summary
echo -e "\033[0;36m========================================\033[0m"
echo -e "\033[0;32m✅ Deployment Complete!\033[0m"
echo -e "\033[0;36m========================================\033[0m"
echo ""
echo -e "🌐 Frontend: https://zimlegend.online"
echo -e "🔌 Backend API: https://api.zimlegend.online"
echo -e "📁 Media Files: https://api.zimlegend.online/media/"
echo ""
echo -e "\033[0;33m📝 Next Steps:\033[0m"
echo "1. Test image uploads from frontend"
echo "2. Test geocoding in property form"
echo "3. Verify existing images are loading"
echo ""
echo -e "\033[0;33m📊 Monitor logs with:\033[0m"
echo -e "\033[0;90m   docker compose -f docker-compose.prod.yml logs -f\033[0m"
echo ""

# Ask if user wants to see logs
read -p "Show container logs? (y/N): " show_logs
if [[ "$show_logs" == "y" || "$show_logs" == "Y" ]]; then
    echo ""
    echo -e "\033[0;36m📜 Container Logs (Ctrl+C to exit):\033[0m"
    docker compose -f docker-compose.prod.yml logs -f
fi
