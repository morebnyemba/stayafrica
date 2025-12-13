# StayAfrica - Start with Workers
# This script starts Backend, Frontend, Mobile, and Celery workers

Write-Host "Starting StayAfrica Development Environment with Workers..." -ForegroundColor Green

# Check if Redis is running
$redisRunning = Get-Process redis-server -ErrorAction SilentlyContinue
if (-not $redisRunning) {
    Write-Host "Redis is not running. Starting Redis via Docker..." -ForegroundColor Yellow
    docker run -d -p 6379:6379 --name stayafrica-redis redis:alpine
    Start-Sleep -Seconds 3
}

# Backend (Django)
Write-Host "Starting Django Backend..." -ForegroundColor Cyan
Start-Process powershell -ArgumentList "-NoExit", "-Command", "cd 'd:\High Value Projects\stayafrica\backend'; if (Test-Path venv) { .\venv\Scripts\Activate.ps1; python manage.py runserver } else { Write-Host 'Virtual environment not found. Run: python -m venv venv' -ForegroundColor Red; pause }"

Start-Sleep -Seconds 2

# Celery Worker
Write-Host "Starting Celery Worker..." -ForegroundColor Cyan
Start-Process powershell -ArgumentList "-NoExit", "-Command", "cd 'd:\High Value Projects\stayafrica\backend'; if (Test-Path venv) { .\venv\Scripts\Activate.ps1; celery -A config worker -l info -P eventlet } else { Write-Host 'Virtual environment not found.' -ForegroundColor Red; pause }"

Start-Sleep -Seconds 2

# Celery Beat
Write-Host "Starting Celery Beat (Scheduler)..." -ForegroundColor Cyan
Start-Process powershell -ArgumentList "-NoExit", "-Command", "cd 'd:\High Value Projects\stayafrica\backend'; if (Test-Path venv) { .\venv\Scripts\Activate.ps1; celery -A config beat -l info } else { Write-Host 'Virtual environment not found.' -ForegroundColor Red; pause }"

Start-Sleep -Seconds 2

# Frontend (Next.js)
Write-Host "Starting Next.js Frontend..." -ForegroundColor Cyan
Start-Process powershell -ArgumentList "-NoExit", "-Command", "cd 'd:\High Value Projects\stayafrica\frontend'; npm run dev"

Start-Sleep -Seconds 2

# Mobile (Expo)
Write-Host "Starting Expo Mobile..." -ForegroundColor Cyan
Start-Process powershell -ArgumentList "-NoExit", "-Command", "cd 'd:\High Value Projects\stayafrica\mobile'; npx expo start"

Write-Host ""
Write-Host "All services are starting in separate windows!" -ForegroundColor Green
Write-Host ""
Write-Host "Service URLs:" -ForegroundColor Yellow
Write-Host "  Backend:  http://localhost:8000" -ForegroundColor White
Write-Host "  Frontend: http://localhost:3000" -ForegroundColor White
Write-Host "  Mobile:   Check Expo terminal for QR code" -ForegroundColor White
Write-Host "  Redis:    localhost:6379" -ForegroundColor White
Write-Host ""
Write-Host "Workers:" -ForegroundColor Yellow
Write-Host "  Celery Worker: Running" -ForegroundColor White
Write-Host "  Celery Beat:   Running" -ForegroundColor White
Write-Host ""
Write-Host "Press any key to exit..." -ForegroundColor Gray
$null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
