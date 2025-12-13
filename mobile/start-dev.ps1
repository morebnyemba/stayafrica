#!/usr/bin/env pwsh

# StayAfrica Mobile App - Startup Script for PowerShell

Write-Host ""
Write-Host "===============================================" -ForegroundColor Cyan
Write-Host "  StayAfrica Mobile App - Development Startup" -ForegroundColor Cyan
Write-Host "===============================================" -ForegroundColor Cyan
Write-Host ""

# Check if Node.js is installed
try {
    node --version | Out-Null
    $nodeVersion = node --version
    Write-Host "[✓] Node.js $nodeVersion found" -ForegroundColor Green
} catch {
    Write-Host "[✗] Node.js is not installed!" -ForegroundColor Red
    Write-Host "    Please download from https://nodejs.org" -ForegroundColor Yellow
    exit 1
}

# Check if Expo CLI is installed
try {
    expo --version | Out-Null
    Write-Host "[✓] Expo CLI is installed" -ForegroundColor Green
} catch {
    Write-Host "[!] Installing Expo CLI globally..." -ForegroundColor Yellow
    npm install -g expo-cli
}

Write-Host ""
Write-Host "[!] Setting up mobile app..." -ForegroundColor Yellow

# Navigate to mobile directory
Set-Location $PSScriptRoot

# Install dependencies if needed
if (-not (Test-Path "node_modules")) {
    Write-Host "[!] Installing dependencies..." -ForegroundColor Yellow
    npm install
    Write-Host "[✓] Dependencies installed" -ForegroundColor Green
}

Write-Host ""
Write-Host "===============================================" -ForegroundColor Green
Write-Host "[✓] Starting Expo development server..." -ForegroundColor Green
Write-Host "===============================================" -ForegroundColor Green
Write-Host ""
Write-Host "Commands:" -ForegroundColor Cyan
Write-Host "  a  - Start Android emulator" -ForegroundColor Yellow
Write-Host "  i  - Start iOS simulator" -ForegroundColor Yellow
Write-Host "  w  - Web preview" -ForegroundColor Yellow
Write-Host "  e  - Send to phone via email" -ForegroundColor Yellow
Write-Host ""

npm start
