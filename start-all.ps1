# StayAfrica - Start All Services
# This script starts Backend, Frontend, and Mobile in separate windows

Write-Host "Starting StayAfrica Development Environment..." -ForegroundColor Green

# Backend (Django)
Write-Host "Starting Django Backend..." -ForegroundColor Cyan
Start-Process powershell -ArgumentList "-NoExit", "-Command", "cd 'd:\High Value Projects\stayafrica\backend'; if (Test-Path venv) { .\venv\Scripts\Activate.ps1; python manage.py runserver } else { Write-Host 'Virtual environment not found. Please run: python -m venv venv' -ForegroundColor Red; pause }"

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
Write-Host ""
Write-Host "Press any key to exit..." -ForegroundColor Gray
$null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
