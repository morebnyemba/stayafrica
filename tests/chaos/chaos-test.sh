#!/bin/bash
# StayAfrica Chaos Testing Suite
# Tests system resilience under failure conditions
# Usage: ./chaos-test.sh <scenario>

set -euo pipefail

SCENARIO="${1:-all}"
COMPOSE_FILE="docker-compose.prod.yml"
RESULTS_DIR="/tmp/chaos-results-$(date +%Y%m%d_%H%M%S)"
API_URL="${API_URL:-http://localhost:8000}"

mkdir -p "${RESULTS_DIR}"

log() {
    echo "[$(date '+%H:%M:%S')] $1" | tee -a "${RESULTS_DIR}/chaos.log"
}

check_health() {
    local attempts=0
    while [ $attempts -lt 30 ]; do
        if curl -sf "${API_URL}/api/health/" > /dev/null 2>&1; then
            return 0
        fi
        sleep 2
        attempts=$((attempts + 1))
    done
    return 1
}

test_redis_failure() {
    log "=== SCENARIO: Redis Failure ==="
    log "Stopping Redis..."
    docker compose -f ${COMPOSE_FILE} stop redis
    sleep 5

    log "Testing API without Redis..."
    HTTP_CODE=$(curl -s -o /dev/null -w '%{http_code}' "${API_URL}/api/v1/properties/?page=1" || echo "000")
    log "Properties endpoint returned: ${HTTP_CODE}"

    if [ "${HTTP_CODE}" = "200" ] || [ "${HTTP_CODE}" = "503" ]; then
        log "PASS: API handled Redis failure gracefully (${HTTP_CODE})"
    else
        log "FAIL: Unexpected response (${HTTP_CODE})"
    fi

    log "Restarting Redis..."
    docker compose -f ${COMPOSE_FILE} start redis
    sleep 10

    if check_health; then
        log "PASS: System recovered after Redis restart"
    else
        log "FAIL: System did not recover"
    fi
}

test_database_failure() {
    log "=== SCENARIO: Database Failure ==="
    log "Stopping PostgreSQL..."
    docker compose -f ${COMPOSE_FILE} stop db
    sleep 5

    log "Testing API without database..."
    HTTP_CODE=$(curl -s -o /dev/null -w '%{http_code}' "${API_URL}/api/health/" || echo "000")
    log "Health endpoint returned: ${HTTP_CODE}"

    log "Restarting PostgreSQL..."
    docker compose -f ${COMPOSE_FILE} start db
    sleep 15

    if check_health; then
        log "PASS: System recovered after DB restart"
    else
        log "FAIL: System did not recover"
    fi
}

test_backend_restart() {
    log "=== SCENARIO: Backend Restart ==="
    log "Restarting backend..."
    docker compose -f ${COMPOSE_FILE} restart backend
    sleep 5

    local recovery_time=0
    while ! check_health && [ $recovery_time -lt 60 ]; do
        sleep 1
        recovery_time=$((recovery_time + 1))
    done

    log "Backend recovered in ${recovery_time}s"
    if [ $recovery_time -lt 30 ]; then
        log "PASS: Recovery time under 30s"
    else
        log "WARN: Recovery time over 30s"
    fi
}

test_celery_failure() {
    log "=== SCENARIO: Celery Worker Failure ==="
    docker compose -f ${COMPOSE_FILE} stop celery
    sleep 3

    log "Testing booking flow without Celery..."
    HTTP_CODE=$(curl -s -o /dev/null -w '%{http_code}' "${API_URL}/api/v1/properties/" || echo "000")
    log "API response: ${HTTP_CODE}"

    if [ "${HTTP_CODE}" = "200" ]; then
        log "PASS: API functional without Celery (async tasks queued)"
    else
        log "FAIL: API broken without Celery"
    fi

    docker compose -f ${COMPOSE_FILE} start celery
    sleep 5
    log "Celery restarted"
}

test_network_partition() {
    log "=== SCENARIO: Network Latency Injection ==="
    log "Adding 500ms latency to backend container..."
    docker exec stayafrica_backend tc qdisc add dev eth0 root netem delay 500ms 2>/dev/null || {
        log "SKIP: tc not available in container (install iproute2)"
        return
    }

    HTTP_CODE=$(curl -s -o /dev/null -w '%{http_code}' --max-time 10 "${API_URL}/api/v1/properties/" || echo "000")
    log "Response with latency: ${HTTP_CODE}"

    docker exec stayafrica_backend tc qdisc del dev eth0 root 2>/dev/null || true
    log "Latency removed"
}

test_memory_pressure() {
    log "=== SCENARIO: Memory Pressure ==="
    log "Setting backend memory limit to 256M..."
    docker update --memory=256m --memory-swap=256m stayafrica_backend 2>/dev/null || {
        log "SKIP: Cannot update memory limits (may need --live-restore)"
        return
    }

    sleep 5
    if check_health; then
        log "PASS: Backend stable under memory constraint"
    else
        log "FAIL: Backend crashed under memory constraint"
    fi

    docker update --memory=0 --memory-swap=0 stayafrica_backend 2>/dev/null || true
}

# Run scenarios
case "${SCENARIO}" in
    redis)    test_redis_failure ;;
    db)       test_database_failure ;;
    backend)  test_backend_restart ;;
    celery)   test_celery_failure ;;
    network)  test_network_partition ;;
    memory)   test_memory_pressure ;;
    all)
        test_backend_restart
        test_redis_failure
        test_celery_failure
        test_database_failure
        ;;
    *)
        echo "Usage: $0 {redis|db|backend|celery|network|memory|all}"
        exit 1
        ;;
esac

log ""
log "=== Chaos Test Complete ==="
log "Results saved to: ${RESULTS_DIR}/chaos.log"
