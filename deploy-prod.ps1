# StayAfrica Production Deployment Script
# This script deploys the updated configuration with media file serving fixes

Write-Host "🚀 StayAfrica Production Deployment" -ForegroundColor Cyan
Write-Host "===================================" -ForegroundColor Cyan
Write-Host ""

# Step 1: Stop existing services
Write-Host "📦 Step 1: Stopping existing services..." -ForegroundColor Yellow
docker compose -f docker-compose.prod.yml down
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Failed to stop services" -ForegroundColor Red
    exit 1
}
Write-Host "✅ Services stopped" -ForegroundColor Green
Write-Host ""

# Step 2: Create SSL directories
Write-Host "📁 Step 2: Creating SSL certificate directories..." -ForegroundColor Yellow
if (-not (Test-Path "nginx/certbot/conf")) {
    New-Item -ItemType Directory -Path "nginx/certbot/conf" -Force | Out-Null
}
if (-not (Test-Path "nginx/certbot/www")) {
    New-Item -ItemType Directory -Path "nginx/certbot/www" -Force | Out-Null
}
Write-Host "✅ SSL directories created" -ForegroundColor Green
Write-Host ""

# Step 3: Check for SSL certificates
Write-Host "🔐 Step 3: Checking SSL certificates..." -ForegroundColor Yellow
$certExists = Test-Path "nginx/certbot/conf/live/api.zimlegend.online/fullchain.pem"
if (-not $certExists) {
    Write-Host "⚠️  SSL certificates not found" -ForegroundColor Yellow
    Write-Host "   Run ./setup-ssl.sh to obtain certificates" -ForegroundColor Yellow
    Write-Host "   Continuing without SSL (HTTP only)..." -ForegroundColor Yellow
} else {
    Write-Host "✅ SSL certificates found" -ForegroundColor Green
}
Write-Host ""

# Step 4: Build containers
Write-Host "🏗️  Step 4: Building containers..." -ForegroundColor Yellow
docker compose -f docker-compose.prod.yml build
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Failed to build containers" -ForegroundColor Red
    exit 1
}
Write-Host "✅ Containers built" -ForegroundColor Green
Write-Host ""

# Step 5: Start services
Write-Host "🚀 Step 5: Starting services..." -ForegroundColor Yellow
docker compose -f docker-compose.prod.yml up -d
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Failed to start services" -ForegroundColor Red
    exit 1
}
Write-Host "✅ Services started" -ForegroundColor Green
Write-Host ""

# Step 6: Wait for backend to be ready
Write-Host "⏳ Step 6: Waiting for backend to be ready..." -ForegroundColor Yellow
Start-Sleep -Seconds 10
Write-Host "✅ Backend should be ready" -ForegroundColor Green
Write-Host ""

# Step 7: Collect static files
Write-Host "📦 Step 7: Collecting static files..." -ForegroundColor Yellow
docker compose -f docker-compose.prod.yml exec -T backend python manage.py collectstatic --noinput
if ($LASTEXITCODE -ne 0) {
    Write-Host "⚠️  Static files collection had issues (this is often not critical)" -ForegroundColor Yellow
} else {
    Write-Host "✅ Static files collected" -ForegroundColor Green
}
Write-Host ""

# Step 8: Run migrations
Write-Host "🗄️  Step 8: Running database migrations..." -ForegroundColor Yellow
docker compose -f docker-compose.prod.yml exec -T backend python manage.py migrate --noinput
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Failed to run migrations" -ForegroundColor Red
    exit 1
}
Write-Host "✅ Migrations completed" -ForegroundColor Green
Write-Host ""

# Step 9: Check service status
Write-Host "🔍 Step 9: Checking service status..." -ForegroundColor Yellow
Write-Host ""
docker compose -f docker-compose.prod.yml ps
Write-Host ""

# Summary
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "✅ Deployment Complete!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "🌐 Frontend: https://zimlegend.online" -ForegroundColor White
Write-Host "🔌 Backend API: https://api.zimlegend.online" -ForegroundColor White
Write-Host "📁 Media Files: https://api.zimlegend.online/media/" -ForegroundColor White
Write-Host ""
Write-Host "📝 Next Steps:" -ForegroundColor Yellow
Write-Host "1. Test image uploads from frontend" -ForegroundColor White
Write-Host "2. Test geocoding in property form" -ForegroundColor White
Write-Host "3. Verify existing images are loading" -ForegroundColor White
Write-Host ""
Write-Host "📊 Monitor logs with:" -ForegroundColor Yellow
Write-Host "   docker compose -f docker-compose.prod.yml logs -f" -ForegroundColor Gray
Write-Host ""

# Show container logs for debugging
$showLogs = Read-Host "Show container logs? (y/N)"
if ($showLogs -eq "y" -or $showLogs -eq "Y") {
    Write-Host ""
    Write-Host "📜 Container Logs (Ctrl+C to exit):" -ForegroundColor Cyan
    docker compose -f docker-compose.prod.yml logs -f
}
