@echo off
REM StayAfrica Mobile App - Startup Script for Windows

echo.
echo ===============================================
echo  StayAfrica Mobile App - Development Startup
echo ===============================================
echo.

REM Check if Node.js is installed
node --version >nul 2>&1
if %errorlevel% neq 0 (
    echo [ERROR] Node.js is not installed!
    echo Please download and install Node.js from https://nodejs.org
    pause
    exit /b 1
)

REM Check if Expo CLI is installed
expo --version >nul 2>&1
if %errorlevel% neq 0 (
    echo [INFO] Installing Expo CLI globally...
    npm install -g expo-cli
)

echo.
echo [INFO] Setting up mobile app...
cd /d "%~dp0"

REM Install dependencies if node_modules doesn't exist
if not exist "node_modules" (
    echo [INFO] Installing dependencies...
    call npm install
)

echo.
echo ===============================================
echo [SUCCESS] Starting Expo development server
echo ===============================================
echo.
echo Press 'a' to start Android emulator
echo Press 'i' to start iOS simulator
echo Press 'w' for web preview
echo Press 'e' to send to your phone
echo.

call npm start

pause
