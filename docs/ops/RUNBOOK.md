# StayAfrica Operations Runbook

## Quick Reference

| Service | Container | Port | Health Check |
|---------|-----------|------|-------------|
| Backend API | stayafrica_backend | 8000 | `/api/health/` |
| Frontend | stayafrica_frontend | 3000 | `curl localhost:3000` |
| PostgreSQL | stayafrica_db | 5432 | `pg_isready` |
| Redis | stayafrica_redis | 6379 | `redis-cli ping` |
| Celery Worker | stayafrica_celery | - | `celery inspect ping` |
| Celery Beat | stayafrica_celery_beat | - | Check logs |
| Erlang Messaging | stayafrica_erlang_messaging | 8765 | `/health` |
| Prometheus | stayafrica_prometheus | 9090 | `/-/healthy` |
| Grafana | stayafrica_grafana | 3001 | `/api/health` |
| Nginx | stayafrica_nginx | 80/443 | `curl localhost` |

---

## 1. Common Issues & Fixes

### 1.1 Backend Not Responding

**Symptoms:** 502/504 from Nginx, health check failing

```bash
# Check container status
docker compose -f docker-compose.prod.yml ps backend

# Check logs
docker logs stayafrica_backend --tail 100

# Restart
docker compose -f docker-compose.prod.yml restart backend

# If OOM killed
docker inspect stayafrica_backend | grep -A5 "State"
docker compose -f docker-compose.prod.yml up -d --force-recreate backend
```

### 1.2 Database Connection Issues

**Symptoms:** 500 errors, "connection refused" in logs

```bash
# Check PostgreSQL
docker exec stayafrica_db pg_isready -U stayafrica_user

# Check connection count
docker exec stayafrica_db psql -U stayafrica_user -d stayafrica_db \
  -c "SELECT count(*) FROM pg_stat_activity;"

# Kill idle connections
docker exec stayafrica_db psql -U stayafrica_user -d stayafrica_db \
  -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE state = 'idle' AND query_start < now() - interval '1 hour';"

# Emergency restart
docker compose -f docker-compose.prod.yml restart db
sleep 10
docker compose -f docker-compose.prod.yml restart backend celery
```

### 1.3 Redis Down

**Symptoms:** Slow responses, cache misses, Celery tasks not processing

```bash
# Check Redis
docker exec stayafrica_redis redis-cli -a $REDIS_PASSWORD ping

# Memory usage
docker exec stayafrica_redis redis-cli -a $REDIS_PASSWORD info memory

# Flush cache (safe - only clears cached data)
docker exec stayafrica_redis redis-cli -a $REDIS_PASSWORD FLUSHDB

# Restart
docker compose -f docker-compose.prod.yml restart redis
sleep 5
docker compose -f docker-compose.prod.yml restart backend celery celery-beat
```

### 1.4 Payment Processing Failures

**Symptoms:** Users report payment errors, payment status stuck at "initiated"

```bash
# Check payment logs
docker logs stayafrica_backend --tail 200 | grep -i "payment"

# Check Celery queue
docker exec stayafrica_celery celery -A stayafrica inspect active

# Retry stuck payments (Django shell)
docker exec -it stayafrica_backend python manage.py shell <<EOF
from apps.payments.models import Payment
stuck = Payment.objects.filter(status='initiated', created_at__lt=timezone.now()-timedelta(hours=1))
print(f"Stuck payments: {stuck.count()}")
for p in stuck:
    print(f"  {p.gateway_ref} - {p.provider} - {p.amount}")
EOF
```

### 1.5 SSL Certificate Renewal

**Symptoms:** HTTPS errors, certificate expiry warnings

```bash
# Check cert expiry
docker exec stayafrica_certbot certbot certificates

# Force renewal
docker exec stayafrica_certbot certbot renew --force-renewal

# Reload nginx
docker exec stayafrica_nginx nginx -s reload
```

---

## 2. Deployment Procedures

### 2.1 Standard Deployment

```bash
cd /opt/stayafrica
git pull origin main

# Build and deploy
docker compose -f docker-compose.prod.yml build
docker compose -f docker-compose.prod.yml up -d

# Run migrations
docker exec stayafrica_backend python manage.py migrate --noinput

# Collect static files
docker exec stayafrica_backend python manage.py collectstatic --noinput

# Verify
curl -f https://api.stayafrica.app/api/health/
```

### 2.2 Blue-Green Deployment (Zero Downtime)

```bash
./deploy-blue-green.sh deploy    # Deploy to inactive color
./deploy-blue-green.sh status    # Check status
./deploy-blue-green.sh rollback  # Rollback if issues
```

### 2.3 Rollback

```bash
# Quick rollback to previous image
docker compose -f docker-compose.prod.yml stop backend
docker compose -f docker-compose.prod.yml up -d backend  # Uses cached image

# Or rollback code
git log --oneline -5
git checkout <previous_commit>
docker compose -f docker-compose.prod.yml build backend
docker compose -f docker-compose.prod.yml up -d backend
```

---

## 3. Backup & Recovery

### 3.1 Automated Backups

```bash
# Daily backup (add to crontab)
0 2 * * * /opt/stayafrica/backup-db.sh daily
0 3 * * 0 /opt/stayafrica/backup-db.sh weekly

# Manual backup before risky operations
./backup-db.sh manual
```

### 3.2 Restore

```bash
./restore-db.sh /var/backups/stayafrica/daily/latest.sql.gz --confirm
```

---

## 4. Monitoring

### 4.1 Key Dashboards

- **Grafana:** http://monitoring.stayafrica.app (port 3001)
- **Prometheus:** http://localhost:9090 (internal only)

### 4.2 Key Metrics to Watch

| Metric | Warning | Critical |
|--------|---------|----------|
| API p95 latency | >1s | >2s |
| Error rate | >2% | >5% |
| DB connections | >80% pool | >90% pool |
| Memory usage | >400MB | >512MB |
| Celery queue | >500 tasks | >1000 tasks |
| Payment failure rate | >5% | >10% |

### 4.3 Log Investigation

```bash
# Backend errors
docker logs stayafrica_backend 2>&1 | grep ERROR | tail -20

# All services
docker compose -f docker-compose.prod.yml logs --tail 50

# Specific time range (with Loki/Grafana)
# Use Grafana → Explore → Loki → {job="stayafrica"} |= "ERROR"
```

---

## 5. Data Retention (GDPR/POPIA)

```bash
# Dry run
docker exec stayafrica_backend python manage.py enforce_data_retention --dry-run

# Execute
docker exec stayafrica_backend python manage.py enforce_data_retention

# Custom retention periods
docker exec stayafrica_backend python manage.py enforce_data_retention \
  --inactive-days 365 \
  --delete-unverified-days 14 \
  --log-retention-days 60
```

---

## 6. Emergency Contacts

| Role | Contact |
|------|---------|
| DevOps | TBD |
| Backend Lead | TBD |
| Database Admin | TBD |

---

## 7. Escalation Matrix

1. **P1 (Site Down):** Restart services → Check logs → Rollback → Page on-call
2. **P2 (Degraded):** Check metrics → Identify bottleneck → Scale/restart affected service
3. **P3 (Minor):** Log ticket → Fix in next deploy cycle
4. **P4 (Enhancement):** Add to backlog
