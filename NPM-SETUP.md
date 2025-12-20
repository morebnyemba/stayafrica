# Nginx Proxy Manager Setup Guide

## Initial Setup

1. **Start all services:**
   ```bash
   docker compose -f docker-compose.prod.yml up -d
   ```

2. **Access Nginx Proxy Manager UI:**
   - URL: `http://YOUR_SERVER_IP:81`
   - Default credentials:
     - Email: `admin@example.com`
     - Password: `changeme`
   - **Change these immediately after first login!**

## Configure Your Domains

### 1. Frontend (zimlegend.online)

**Add Proxy Host:**
- **Domain Names:** `zimlegend.online`, `www.zimlegend.online`
- **Scheme:** `http`
- **Forward Hostname/IP:** `frontend`
- **Forward Port:** `3000`
- **Cache Assets:** ✓
- **Block Common Exploits:** ✓
- **Websockets Support:** ✓

**SSL Tab:**
- **SSL Certificate:** Request a new SSL Certificate with Let's Encrypt
- **Force SSL:** ✓
- **HTTP/2 Support:** ✓
- **HSTS Enabled:** ✓
- **Email:** your-email@example.com
- **I Agree to Terms:** ✓

### 2. Backend API (api.zimlegend.online)

**Add Proxy Host:**
- **Domain Names:** `api.zimlegend.online`
- **Scheme:** `http`
- **Forward Hostname/IP:** `backend`
- **Forward Port:** `8000`
- **Cache Assets:** ✗ (API shouldn't cache)
- **Block Common Exploits:** ✓
- **Websockets Support:** ✗

**Custom Locations (Advanced Tab):**
Add these for static/media files:

```nginx
location /static/ {
    proxy_pass http://backend:8000/static/;
    proxy_set_header Host $host;
    expires 30d;
    add_header Cache-Control "public, immutable";
}

location /media/ {
    proxy_pass http://backend:8000/media/;
    proxy_set_header Host $host;
    expires 30d;
    add_header Cache-Control "public, immutable";
}
```

**SSL Tab:**
- **SSL Certificate:** Request a new SSL Certificate with Let's Encrypt
- **Force SSL:** ✓
- **HTTP/2 Support:** ✓
- **HSTS Enabled:** ✓
- **Email:** your-email@example.com
- **I Agree to Terms:** ✓

## DNS Configuration

Before requesting SSL certificates, ensure your domains point to your server:

```
zimlegend.online        A    YOUR_SERVER_IP
www.zimlegend.online    A    YOUR_SERVER_IP
api.zimlegend.online    A    YOUR_SERVER_IP
```

## Access Your Sites

After setup:
- Frontend: https://zimlegend.online
- Backend API: https://api.zimlegend.online
- NPM Admin: http://YOUR_SERVER_IP:81

## Troubleshooting

**SSL Certificate Fails:**
- Verify DNS is propagated: `nslookup zimlegend.online`
- Check ports 80/443 are open on firewall
- Wait 5-10 minutes after DNS changes

**Can't Access Services:**
- Check logs: `docker compose -f docker-compose.prod.yml logs nginx-proxy-manager`
- Verify containers are running: `docker compose -f docker-compose.prod.yml ps`
- Test internal connectivity: `docker compose -f docker-compose.prod.yml exec nginx-proxy-manager ping backend`

## Security Notes

1. **Change default NPM credentials immediately**
2. **Restrict port 81 access** (NPM admin) via firewall after setup
3. **Enable 2FA** in NPM user settings
4. **Regular backups** of npm_data volume
