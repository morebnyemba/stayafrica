# Remote Testing Guide for StayAfrica Mobile App

## Problem
Local ngrok tunneling is failing due to network/ISP restrictions or firewall blocking.

## Solutions for Remote User Testing

### ✅ Option 1: Expo Application Services (EAS) - RECOMMENDED
Build and deploy your app to Expo's cloud infrastructure for remote testing.

#### Setup:
```bash
cd D:\Projects\stayafrica-1\mobile

# Install EAS CLI globally
npm install -g eas-cli

# Login to Expo (create account if needed at expo.dev)
eas login

# Configure EAS Build
eas build:configure

# Build for Android (development build)
eas build --profile development --platform android

# Build for iOS (development build) - requires Apple Developer account
eas build --profile development --platform ios
```

#### Share with Remote Testers:
- After build completes, EAS provides a **shareable URL**
- Remote users download the app from that URL
- Install on their device and test
- No need for Expo Go

**Pros:**
- Works from anywhere, no network restrictions
- Professional deployment pipeline
- Persistent builds for QA team
- Works with custom native code

---

### ✅ Option 2: Cloudflare Tunnel (Alternative to ngrok)
Use Cloudflare's free tunnel service instead of ngrok.

#### Setup:
```bash
# Install cloudflared
# Download from: https://github.com/cloudflare/cloudflared/releases

# In one terminal, start Expo on LAN
cd D:\Projects\stayafrica-1\mobile
npx expo start --lan

# In another terminal, create tunnel to port 8081
cloudflared tunnel --url http://localhost:8081
```

**Pros:**
- Free, no account needed for testing
- Often works when ngrok is blocked
- Provides shareable HTTPS URL

---

### ✅ Option 3: LocalTunnel (Simplest Alternative)
Lightweight tunnel service, easier than ngrok.

#### Setup:
```bash
# Install localtunnel globally
npm install -g localtunnel

# In one terminal, start Expo on LAN
cd D:\Projects\stayafrica-1\mobile
npx expo start --lan

# In another terminal, expose port 8081
lt --port 8081 --subdomain stayafrica-test
```

Remote users access: `https://stayafrica-test.loca.lt`

**Pros:**
- Very simple setup
- No account required
- Custom subdomain support

---

### ✅ Option 4: Deploy to Web + Capacitor (For Quick Demo)
Convert your Expo app to web and deploy to free hosting.

#### Setup:
```bash
cd D:\Projects\stayafrica-1\mobile

# Run web version
npx expo start --web

# Build for production web
npx expo export:web

# Deploy to Vercel/Netlify/Cloudflare Pages
# Example with Vercel:
npm install -g vercel
cd web-build
vercel --prod
```

**Pros:**
- No app installation needed
- Shareable URL immediately
- Works on any device with browser
- Great for quick stakeholder demos

---

### ✅ Option 5: TestFlight (iOS) / Firebase App Distribution (Android)
Professional beta testing platform.

#### iOS (TestFlight):
```bash
# Build with EAS
eas build --profile preview --platform ios

# Submit to TestFlight
eas submit --platform ios
```

Remote testers:
1. Get invited via email
2. Install TestFlight app
3. Download your app from TestFlight

#### Android (Firebase):
```bash
# Build APK
eas build --profile preview --platform android

# Upload to Firebase App Distribution manually
# Or use Firebase CLI
```

**Pros:**
- Professional beta distribution
- Crash reporting and analytics
- User feedback collection
- Production-like testing

---

## Recommended Workflow

For **immediate remote testing** (next 30 minutes):
→ **Use Option 2 (Cloudflare Tunnel)** or **Option 3 (LocalTunnel)**

For **ongoing QA and stakeholder demos** (next few days):
→ **Use Option 1 (EAS Build)** - one-time setup, persistent URLs

For **production beta testing** (pre-launch):
→ **Use Option 5 (TestFlight/Firebase)** - professional distribution

---

## Troubleshooting ngrok/Tunnel Issues

If you still want to fix ngrok tunneling:

### 1. Explicitly Allow ngrok in Windows Defender Firewall
```powershell
# Find ngrok executable path
Get-ChildItem -Path "$env:USERPROFILE\.expo" -Recurse -Filter "ngrok.exe" -ErrorAction SilentlyContinue

# Add firewall rule (replace PATH with actual ngrok.exe path)
New-NetFirewallRule -DisplayName "ngrok Expo" -Direction Inbound -Action Allow -Program "C:\Users\PC\.expo\ngrok-bin\ngrok.exe" -Protocol TCP
New-NetFirewallRule -DisplayName "ngrok Expo Out" -Direction Outbound -Action Allow -Program "C:\Users\PC\.expo\ngrok-bin\ngrok.exe" -Protocol TCP
```

### 2. Check if ISP/Corporate Network Blocks ngrok
```powershell
# Test ngrok connectivity
curl https://ngrok.com
Test-NetConnection -ComputerName ngrok.com -Port 443
```

If this fails, your ISP/network blocks ngrok domains.

### 3. Try Mobile Hotspot
- Connect PC to phone's mobile hotspot
- Bypass ISP/corporate restrictions
- Run `npx expo start --tunnel`

### 4. Use VPN
- Connect to VPN service (ProtonVPN, Windscribe free tier)
- Retry tunnel

---

## Quick Start Commands

```powershell
# Option 1: EAS Build (best)
cd D:\Projects\stayafrica-1\mobile
npm install -g eas-cli
eas login
eas build --profile development --platform android

# Option 2: Cloudflare Tunnel
# Download cloudflared.exe first
npx expo start --lan  # Terminal 1
cloudflared tunnel --url http://localhost:8081  # Terminal 2

# Option 3: LocalTunnel
npm install -g localtunnel
npx expo start --lan  # Terminal 1
lt --port 8081 --subdomain stayafrica  # Terminal 2
```

---

## Support

- EAS docs: https://docs.expo.dev/build/introduction/
- Cloudflare Tunnel: https://developers.cloudflare.com/cloudflare-one/connections/connect-apps/
- LocalTunnel: https://theboroer.github.io/localtunnel-www/
