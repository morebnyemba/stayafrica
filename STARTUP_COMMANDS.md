# StayAfrica Development Startup Commands

## Backend (Django API)
```powershell
# Terminal 1: Django Backend
cd "d:\High Value Projects\stayafrica\backend"
python -m venv venv
.\venv\Scripts\Activate.ps1
pip install -r requirements.txt
python manage.py migrate
python manage.py runserver
```

## Frontend Web (Next.js)
```powershell
# Terminal 2: Next.js Web App
cd "d:\High Value Projects\stayafrica\frontend"
npm install
npm run dev
```

## Mobile (React Native/Expo)
```powershell
# Terminal 3: Expo Mobile App
cd "d:\High Value Projects\stayafrica\mobile"
npm install --legacy-peer-deps
npx expo start --clear
```

## Background Workers (Celery)
```powershell
# Terminal 4: Celery Worker
cd "d:\High Value Projects\stayafrica\backend"
.\venv\Scripts\Activate.ps1
celery -A config worker -l info -P eventlet

# Terminal 5: Celery Beat (Scheduler)
cd "d:\High Value Projects\stayafrica\backend"
.\venv\Scripts\Activate.ps1
celery -A config beat -l info
```

## Redis (Required for Celery)
```powershell
# If you have Redis installed via WSL or Windows:
redis-server

# Or use Docker:
docker run -d -p 6379:6379 redis:alpine
```

## Quick Start All Services (Sequential)

### Option 1: Manual (Recommended for first time)
Open 5 separate PowerShell terminals and run each command block above.

### Option 2: Background Jobs
```powershell
# Start Backend
Start-Job -ScriptBlock {
    cd "d:\High Value Projects\stayafrica\backend"
    .\venv\Scripts\Activate.ps1
    python manage.py runserver
}

# Start Frontend
Start-Job -ScriptBlock {
    cd "d:\High Value Projects\stayafrica\frontend"
    npm run dev
}

# Start Mobile
Start-Job -ScriptBlock {
    cd "d:\High Value Projects\stayafrica\mobile"
    npx expo start
}

# Check running jobs
Get-Job
```

## Service URLs
- **Backend API**: http://localhost:8000
- **Frontend Web**: http://localhost:3000
- **Mobile Expo**: http://localhost:8081 (or scan QR code)
- **Django Admin**: http://localhost:8000/admin

## Troubleshooting

### Port Already in Use
```powershell
# Find and kill process on port 8000 (Django)
netstat -ano | findstr :8000
taskkill /PID <PID> /F

# Find and kill process on port 3000 (Next.js)
netstat -ano | findstr :3000
taskkill /PID <PID> /F

# Find and kill process on port 8081 (Expo)
netstat -ano | findstr :8081
taskkill /PID <PID> /F
```

### Database Migrations
```powershell
cd "d:\High Value Projects\stayafrica\backend"
.\venv\Scripts\Activate.ps1
python manage.py makemigrations
python manage.py migrate
```

### Create Superuser
```powershell
cd "d:\High Value Projects\stayafrica\backend"
.\venv\Scripts\Activate.ps1
python manage.py createsuperuser
```

### Clear Node Modules Cache
```powershell
# Frontend
cd "d:\High Value Projects\stayafrica\frontend"
Remove-Item -Recurse -Force node_modules
Remove-Item package-lock.json
npm install

# Mobile
cd "d:\High Value Projects\stayafrica\mobile"
Remove-Item -Recurse -Force node_modules
Remove-Item package-lock.json
npm install --legacy-peer-deps
```

### Expo Cache Issues
```powershell
cd "d:\High Value Projects\stayafrica\mobile"
npx expo start --clear
# or
Remove-Item -Recurse -Force .expo
Remove-Item -Recurse -Force node_modules/.cache
```
