#!/bin/bash
# StayAfrica Blue-Green Deployment Script
# Zero-downtime deployment with instant rollback capability
# Usage: ./deploy-blue-green.sh [deploy|rollback|status]

set -euo pipefail

ACTION="${1:-deploy}"
COMPOSE_FILE="docker-compose.prod.yml"
DEPLOY_DIR="/opt/stayafrica"
BLUE_PORT=8001
GREEN_PORT=8002
NGINX_CONF="/etc/nginx/conf.d/stayafrica-upstream.conf"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

get_active_color() {
    if docker ps --format '{{.Names}}' | grep -q 'stayafrica_backend_blue'; then
        if curl -sf "http://localhost:${BLUE_PORT}/api/health/" > /dev/null 2>&1; then
            echo "blue"
            return
        fi
    fi
    echo "green"
}

get_inactive_color() {
    local active=$(get_active_color)
    if [ "${active}" = "blue" ]; then
        echo "green"
    else
        echo "blue"
    fi
}

deploy() {
    local active=$(get_active_color)
    local target=$(get_inactive_color)
    local target_port=$([[ "${target}" == "blue" ]] && echo ${BLUE_PORT} || echo ${GREEN_PORT})

    log "Active: ${active}, Deploying to: ${target} (port ${target_port})"

    # Pre-deploy backup
    log "Creating pre-deploy backup..."
    ./backup-db.sh manual || log "Backup skipped (non-critical)"

    # Pull latest code
    log "Pulling latest code..."
    git pull origin main

    # Build new version
    log "Building ${target} containers..."
    docker compose -f ${COMPOSE_FILE} build backend frontend

    # Start new containers on target color
    log "Starting ${target} backend on port ${target_port}..."
    docker compose -f ${COMPOSE_FILE} run -d \
        --name "stayafrica_backend_${target}" \
        -p "${target_port}:8000" \
        -e "DEPLOYMENT_COLOR=${target}" \
        backend

    # Wait for health check
    log "Waiting for ${target} to become healthy..."
    local attempts=0
    while [ $attempts -lt 60 ]; do
        if curl -sf "http://localhost:${target_port}/api/health/" > /dev/null 2>&1; then
            log "${target} is healthy!"
            break
        fi
        sleep 2
        attempts=$((attempts + 1))
    done

    if [ $attempts -ge 60 ]; then
        log "ERROR: ${target} failed health check! Aborting."
        docker stop "stayafrica_backend_${target}" 2>/dev/null || true
        docker rm "stayafrica_backend_${target}" 2>/dev/null || true
        exit 1
    fi

    # Run migrations
    log "Running database migrations..."
    docker exec "stayafrica_backend_${target}" python manage.py migrate --noinput

    # Switch nginx upstream
    log "Switching traffic to ${target}..."
    cat > "${NGINX_CONF}" <<EOF
upstream stayafrica_backend {
    server 127.0.0.1:${target_port};
}
EOF
    nginx -s reload

    # Verify traffic is flowing to new version
    sleep 3
    if curl -sf "http://localhost/api/health/" > /dev/null 2>&1; then
        log "Traffic successfully routed to ${target}"
    else
        log "ERROR: Traffic routing failed, rolling back..."
        rollback
        exit 1
    fi

    # Graceful shutdown of old version
    log "Gracefully stopping ${active}..."
    docker stop "stayafrica_backend_${active}" 2>/dev/null || true

    # Keep old container for quick rollback (don't remove)
    log "Deployment complete! Active: ${target}"
    log "Old ${active} container kept for rollback. Run 'docker rm stayafrica_backend_${active}' to clean up."
}

rollback() {
    local active=$(get_active_color)
    local target=$(get_inactive_color)
    local target_port=$([[ "${target}" == "blue" ]] && echo ${BLUE_PORT} || echo ${GREEN_PORT})

    log "Rolling back from ${active} to ${target}..."

    # Start old container if stopped
    docker start "stayafrica_backend_${target}" 2>/dev/null || {
        log "ERROR: Cannot start ${target} container for rollback"
        exit 1
    }

    # Wait for health
    sleep 5
    if ! curl -sf "http://localhost:${target_port}/api/health/" > /dev/null 2>&1; then
        log "ERROR: ${target} is not healthy for rollback!"
        exit 1
    fi

    # Switch nginx
    cat > "${NGINX_CONF}" <<EOF
upstream stayafrica_backend {
    server 127.0.0.1:${target_port};
}
EOF
    nginx -s reload

    log "Rolled back to ${target}. Current active: ${target}"
}

show_status() {
    local active=$(get_active_color)
    log "=== Deployment Status ==="
    log "Active color: ${active}"
    log ""
    log "Blue container:"
    docker ps -a --filter "name=stayafrica_backend_blue" --format "  Status: {{.Status}}" 2>/dev/null || echo "  Not found"
    log "Green container:"
    docker ps -a --filter "name=stayafrica_backend_green" --format "  Status: {{.Status}}" 2>/dev/null || echo "  Not found"
    log ""
    log "Health check:"
    curl -sf "http://localhost/api/health/" 2>/dev/null && log "  API: UP" || log "  API: DOWN"
}

case "${ACTION}" in
    deploy)   deploy ;;
    rollback) rollback ;;
    status)   show_status ;;
    *)
        echo "Usage: $0 {deploy|rollback|status}"
        exit 1
        ;;
esac
