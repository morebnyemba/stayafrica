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
BLUE_FE_PORT=3001
GREEN_FE_PORT=3002
NGINX_CONF="./nginx/upstreams.conf"

LOCK_FILE="/tmp/stayafrica_deploy.lock"

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] [INFO] $1"; }
warn() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] [WARN] $1" >&2; }
error() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] [ERROR] $1" >&2; }

check_dependencies() {
    local deps=("docker" "curl" "git")
    for dep in "${deps[@]}"; do
        if ! command -v "$dep" >/dev/null 2>&1; then
            error "Dependency missing: $dep. Please install it first."
            exit 1
        fi
    done
}

validate_env() {
    if [ ! -f "${COMPOSE_FILE}" ]; then
        error "${COMPOSE_FILE} not found in current directory."
        exit 1
    fi
    if [ ! -f ".env" ] && [ ! -f ".env.prod" ]; then
        error "No environment file (.env or .env.prod) found."
        exit 1
    fi
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
    check_dependencies
    validate_env

    # Concurrency Lock
    if [ -f "${LOCK_FILE}" ]; then
        error "Deployment already in progress (lock file exists: ${LOCK_FILE})."
        exit 1
    fi
    touch "${LOCK_FILE}"
    trap 'rm -f "${LOCK_FILE}"' EXIT

    local active=$(get_active_color)
    local target=$(get_inactive_color)
    local target_port=$([[ "${target}" == "blue" ]] && echo ${BLUE_PORT} || echo ${GREEN_PORT})
    local target_fe_port=$([[ "${target}" == "blue" ]] && echo ${BLUE_FE_PORT} || echo ${GREEN_FE_PORT})

    log "Active: ${active}, Deploying to: ${target} (Backend: ${target_port}, Frontend: ${target_fe_port})"

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

    log "Starting ${target} frontend on port ${target_fe_port}..."
    docker compose -f ${COMPOSE_FILE} run -d \
        --name "stayafrica_frontend_${target}" \
        -p "${target_fe_port}:3000" \
        frontend

    # Wait for health check
    log "Waiting for ${target} to become healthy..."
    local attempts=0
    while [ $attempts -lt 60 ]; do
        local be_ok=0
        local fe_ok=0
        
        if curl -sf "http://localhost:${target_port}/api/health/" > /dev/null 2>&1; then be_ok=1; fi
        if curl -sf "http://localhost:${target_fe_port}/" > /dev/null 2>&1; then fe_ok=1; fi

        if [ $be_ok -eq 1 ] && [ $fe_ok -eq 1 ]; then
            log "${target} stack is healthy!"
            break
        fi
        sleep 2
        attempts=$((attempts + 1))
    done

    if [ $attempts -ge 60 ]; then
        log "ERROR: ${target} failed health check! Aborting."
        docker stop "stayafrica_backend_${target}" "stayafrica_frontend_${target}" 2>/dev/null || true
        docker rm "stayafrica_backend_${target}" "stayafrica_frontend_${target}" 2>/dev/null || true
        exit 1
    fi

    # Run migrations
    log "Running database migrations..."
    docker exec "stayafrica_backend_${target}" python manage.py migrate --noinput

    # Switch nginx upstream
    log "Switching traffic to ${target}..."
    cat > "${NGINX_CONF}" <<EOF
upstream backend {
    server stayafrica_backend_${target}:8000;
}
upstream frontend {
    server stayafrica_frontend_${target}:3000;
}
EOF
    # Reload nginx in container
    log "Verifying Nginx configuration..."
    if ! docker exec stayafrica_nginx nginx -t > /dev/null 2>&1; then
        error "Nginx configuration test failed! Reverting upstreams.conf..."
        rollback
        exit 1
    fi

    log "Reloading Nginx container..."
    if docker exec stayafrica_nginx nginx -s reload; then
        log "Nginx reloaded successfully"
    else
        error "Could not reload nginx. Check logs: docker logs stayafrica_nginx"
        exit 1
    fi

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
    log "Gracefully stopping ${active} stack..."
    docker stop "stayafrica_backend_${active}" "stayafrica_frontend_${active}" 2>/dev/null || true

    # Keep old container for quick rollback (don't remove)
    log "Deployment complete! Active: ${target}"
    log "Old ${active} containers kept for rollback. Run 'docker rm stayafrica_backend_${active} stayafrica_frontend_${active}' to clean up."
    
    # Cleanup dangling images to save space
    log "Pruning old Docker images..."
    docker image prune -f || warn "Image prune failed (non-critical)"
}

rollback() {
    check_dependencies
    validate_env
    
    local active=$(get_active_color)
    local target=$(get_inactive_color)
    local target_port=$([[ "${target}" == "blue" ]] && echo ${BLUE_PORT} || echo ${GREEN_PORT})

    log "Rolling back from ${active} to ${target}..."

    # Start old containers if stopped
    docker start "stayafrica_backend_${target}" "stayafrica_frontend_${target}" 2>/dev/null || {
        error "Cannot start ${target} stack for rollback"
        exit 1
    }

    # Wait for health
    sleep 5
    if ! curl -sf "http://localhost:${target_port}/api/health/" > /dev/null 2>&1; then
        error "${target} is not healthy for rollback!"
        exit 1
    fi

    # Switch nginx
    cat > "${NGINX_CONF}" <<EOF
upstream backend {
    server stayafrica_backend_${target}:8000;
}
upstream frontend {
    server stayafrica_frontend_${target}:3000;
}
EOF
    docker exec stayafrica_nginx nginx -s reload || warn "Manual nginx reload needed"

    log "Rolled back to ${target}. Current active: ${target}"
}

show_status() {
    check_dependencies
    
    local active=$(get_active_color)
    log "=== Deployment Status ==="
    log "Active color: ${active}"
    log ""
    log "Blue containers:"
    docker ps -a --filter "name=stayafrica_backend_blue" --format "  Backend:  {{.Status}}" 2>/dev/null || echo "  Backend:  Not found"
    docker ps -a --filter "name=stayafrica_frontend_blue" --format "  Frontend: {{.Status}}" 2>/dev/null || echo "  Frontend: Not found"
    log "Green containers:"
    docker ps -a --filter "name=stayafrica_backend_green" --format "  Backend:  {{.Status}}" 2>/dev/null || echo "  Backend:  Not found"
    docker ps -a --filter "name=stayafrica_frontend_green" --format "  Frontend: {{.Status}}" 2>/dev/null || echo "  Frontend: Not found"
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
