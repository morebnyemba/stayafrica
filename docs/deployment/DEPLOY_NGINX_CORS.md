# Deploy Steps: Apply CORS/Nginx changes

## If managing Nginx directly (system service)
```bash
sudo nginx -t
sudo systemctl reload nginx
```

## If Nginx runs in Docker
```bash
docker exec -it <nginx_container_name> nginx -t
docker exec -it <nginx_container_name> nginx -s reload
```

## If using Nginx Proxy Manager (NPM)
- In the Proxy Host for api.zimlegend.online, open the Advanced tab and add:
```
add_header Access-Control-Allow-Origin "https://zimlegend.online" always;
add_header Access-Control-Allow-Credentials "true" always;
add_header Access-Control-Allow-Methods "GET, POST, PUT, PATCH, DELETE, OPTIONS" always;
add_header Access-Control-Allow-Headers "Authorization, Content-Type, Accept, Origin, X-Requested-With" always;
```
- Save and force a refresh of the proxy host.

## Backend restart (Django) after settings change
```bash
# Docker Compose example
docker compose build backend
docker compose up -d backend

# Or Kubernetes/other orchestration: redeploy the backend pod/deployment
```
