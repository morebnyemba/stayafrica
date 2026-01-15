# Redis Security Configuration for StayAfrica

## Overview

This document describes the Redis security measures implemented to protect the StayAfrica application from unauthorized access and exploitation.

## Security Vulnerabilities Fixed

### Previous Configuration Issues
1. **Public Port Exposure**: Redis was exposed on port 6379 to the host machine and potentially the public internet
2. **No Authentication**: Redis accepted connections without password authentication
3. **No Network Isolation**: Redis could be accessed from outside the Docker network
4. **Dangerous Commands Enabled**: Commands like FLUSHALL, FLUSHDB, CONFIG were available

### Current Security Measures

#### 1. Network Isolation
- **Removed Public Port Exposure**: Redis is no longer accessible from outside the Docker network
- **Internal Network Only**: Services communicate with Redis via the internal Docker network `stayafrica_network`
- **No Host Port Binding**: The `ports` directive has been removed from Redis service configuration

#### 2. Authentication
- **Password Protection**: All Redis connections now require authentication via the `REDIS_PASSWORD` environment variable
- **Connection String Format**: `redis://:PASSWORD@redis:6379/0`
- **Environment-Based Configuration**: Password is managed via environment variables, not hardcoded

#### 3. Dangerous Command Restrictions
A custom Redis configuration disables or renames dangerous commands:
- `FLUSHDB` - Prevents database wipes
- `FLUSHALL` - Prevents all database wipes
- `KEYS` - Prevents performance issues from key scanning
- `CONFIG` - Prevents runtime configuration changes
- `SHUTDOWN` - Prevents unauthorized service shutdown
- `DEBUG` - Prevents debugging access

#### 4. Data Persistence
- **AOF (Append Only File)**: Enabled for data durability
- **Volume Mounting**: Redis data persists across container restarts via Docker volumes

## Configuration Files

### Docker Compose Files

#### Development (`docker-compose.yml`)
```yaml
redis:
  image: redis:7-alpine
  container_name: stayafrica_redis
  command: redis-server --requirepass ${REDIS_PASSWORD:-devpassword123}
  environment:
    - REDIS_PASSWORD=${REDIS_PASSWORD:-devpassword123}
  volumes:
    - redis_data:/data
  # NO ports exposed to host
```

#### Production (`docker-compose.prod.yml`)
```yaml
redis:
  image: redis:7-alpine
  container_name: stayafrica_redis
  command: redis-server --requirepass ${REDIS_PASSWORD} --appendonly yes
  environment:
    - REDIS_PASSWORD=${REDIS_PASSWORD}
  volumes:
    - redis_data:/data
  # NO ports exposed to host
```

### Environment Configuration (`.env`)

**Development**:
```bash
REDIS_PASSWORD=devpassword123
```

**Production** (use a strong password):
```bash
# Generate a strong password:
# openssl rand -base64 32
REDIS_PASSWORD=YOUR_STRONG_RANDOM_PASSWORD_HERE
```

### Service Connection Strings

All services (Backend, Celery, Celery Beat) connect to Redis using:
```bash
REDIS_URL=redis://:${REDIS_PASSWORD}@redis:6379/0
CELERY_BROKER_URL=redis://:${REDIS_PASSWORD}@redis:6379/0
CELERY_RESULT_BACKEND=redis://:${REDIS_PASSWORD}@redis:6379/0
```

## Deployment Instructions

### Initial Setup

1. **Generate a Strong Redis Password**:
   ```bash
   openssl rand -base64 32
   ```

2. **Update `.env` file**:
   ```bash
   # Copy example and update
   cp .env.example .env
   nano .env  # Set REDIS_PASSWORD to generated value
   ```

3. **Verify Docker Compose Configuration**:
   ```bash
   # Check configuration without starting
   docker-compose config
   
   # For production
   docker-compose -f docker-compose.prod.yml config
   ```

4. **Start Services**:
   ```bash
   # Development
   docker-compose up -d
   
   # Production
   docker-compose -f docker-compose.prod.yml up -d
   ```

### Verification

#### 1. Verify Redis is NOT Accessible from Host
```bash
# This should FAIL (connection refused or timeout)
redis-cli -h localhost -p 6379 ping
```

#### 2. Verify Redis is Accessible Within Docker Network
```bash
# This should succeed
docker exec -it stayafrica_backend sh -c 'echo "ping" | redis-cli -h redis -a $REDIS_PASSWORD'
```

Expected output: `PONG`

#### 3. Verify Service Connections
```bash
# Check backend logs
docker logs stayafrica_backend | grep -i redis

# Check Celery worker logs
docker logs stayafrica_celery | grep -i redis

# Should see successful connections, no authentication errors
```

#### 4. Verify Data Persistence
```bash
# Check Redis data directory
docker exec -it stayafrica_redis ls -la /data

# Should see appendonly.aof file
```

## Security Best Practices

### Password Requirements

#### Development
- Minimum 12 characters
- Use default: `devpassword123` or similar

#### Production
- **CRITICAL**: Change from default
- Minimum 32 characters
- Include uppercase, lowercase, numbers, and special characters
- Use a password manager or generation tool
- Never commit passwords to version control

### Example Strong Password Generation
```bash
# Method 1: OpenSSL
openssl rand -base64 32

# Method 2: /dev/urandom
tr -dc 'A-Za-z0-9!@#$%^&*' < /dev/urandom | head -c 32; echo

# Method 3: Python
python3 -c "import secrets, string; print(''.join(secrets.choice(string.ascii_letters + string.digits + '!@#$%^&*') for _ in range(32)))"
```

### Network Security

1. **Firewall Rules**: Ensure port 6379 is not exposed in firewall rules
2. **Docker Network**: Use custom Docker networks for isolation
3. **Container Communication**: Services communicate via Docker DNS (container names)
4. **No External Access**: Redis should never be directly accessible from the internet

### Monitoring

#### Check for Unauthorized Access Attempts
```bash
# View Redis logs
docker logs stayafrica_redis

# Look for authentication failures
docker logs stayafrica_redis 2>&1 | grep -i "auth"
```

#### Monitor Redis Memory Usage
```bash
# Check Redis info
docker exec -it stayafrica_redis redis-cli -a $REDIS_PASSWORD INFO memory
```

#### Check Connected Clients
```bash
# List connected clients
docker exec -it stayafrica_redis redis-cli -a $REDIS_PASSWORD CLIENT LIST
```

### Regular Maintenance

1. **Rotate Passwords**: Change Redis password periodically (every 90 days)
2. **Update Redis Image**: Keep Redis version up to date
3. **Monitor Logs**: Regular log review for suspicious activity
4. **Backup Data**: Regular backups of Redis AOF file
5. **Audit Access**: Review which services have Redis access

## Troubleshooting

### Issue: Services Cannot Connect to Redis

**Symptoms**:
- Backend logs show "Connection refused" or "Authentication failed"
- Celery workers won't start
- Application cannot cache data

**Solution**:
```bash
# 1. Check Redis is running
docker ps | grep redis

# 2. Verify password in .env matches docker-compose
cat .env | grep REDIS_PASSWORD

# 3. Test connection from backend container
docker exec -it stayafrica_backend sh -c 'echo "ping" | redis-cli -h redis -a $REDIS_PASSWORD'

# 4. Check Redis logs for errors
docker logs stayafrica_redis

# 5. Restart services
docker-compose restart redis backend celery celery-beat
```

### Issue: Redis Data Lost After Restart

**Symptoms**:
- Cached data disappears
- Sessions are lost
- Celery tasks are missing

**Solution**:
```bash
# 1. Verify volume is mounted
docker inspect stayafrica_redis | grep Mounts -A 10

# 2. Check AOF file exists
docker exec -it stayafrica_redis ls -la /data

# 3. Verify AOF is enabled
docker exec -it stayafrica_redis redis-cli -a $REDIS_PASSWORD CONFIG GET appendonly

# Should return: appendonly yes
```

### Issue: High Memory Usage

**Symptoms**:
- Redis consuming excessive memory
- OOM (Out of Memory) errors

**Solution**:
```bash
# 1. Check memory usage
docker exec -it stayafrica_redis redis-cli -a $REDIS_PASSWORD INFO memory

# 2. Check maxmemory setting
docker exec -it stayafrica_redis redis-cli -a $REDIS_PASSWORD CONFIG GET maxmemory

# 3. Clear cache if needed (CAREFUL - only in emergency)
docker exec -it stayafrica_redis redis-cli -a $REDIS_PASSWORD -n 1 DBSIZE
```

## Migration from Unsecured Redis

If you're upgrading from an unsecured Redis configuration:

1. **Backup Data** (if needed):
   ```bash
   # Export data before changes
   docker exec -it stayafrica_redis redis-cli SAVE
   docker cp stayafrica_redis:/data/dump.rdb ./redis-backup.rdb
   ```

2. **Update Configuration Files**: Apply changes from this guide

3. **Update Environment Variables**: Add `REDIS_PASSWORD` to `.env`

4. **Restart All Services**:
   ```bash
   docker-compose down
   docker-compose up -d
   ```

5. **Verify Connections**: Follow verification steps above

## Security Incident Response

If you suspect Redis has been compromised:

1. **Immediate Actions**:
   ```bash
   # Stop Redis immediately
   docker stop stayafrica_redis
   
   # Check logs for suspicious activity
   docker logs stayafrica_redis > redis-incident-logs.txt
   
   # Review connected clients before shutdown
   # (if Redis is still running)
   docker exec -it stayafrica_redis redis-cli -a $REDIS_PASSWORD CLIENT LIST
   ```

2. **Investigate**:
   - Review Redis logs for unauthorized access
   - Check Docker logs for port exposure
   - Verify firewall rules
   - Check for data exfiltration

3. **Remediate**:
   - Change Redis password immediately
   - Review and update all connection strings
   - Verify network isolation
   - Update firewall rules
   - Restart all services with new credentials

4. **Prevent**:
   - Implement monitoring alerts
   - Regular security audits
   - Password rotation policy
   - Access logging

## Compliance Notes

### Data Privacy
- Session data in Redis contains user information
- Ensure Redis is not exposed to unauthorized access
- Implement proper backup encryption

### Access Control
- Only authorized services should connect to Redis
- Use service accounts with minimal permissions
- Audit access logs regularly

### Data Retention
- Redis cache expires based on TTL settings
- Session data should have appropriate expiration
- AOF file contains historical data - manage accordingly

## Additional Resources

- [Redis Security Documentation](https://redis.io/topics/security)
- [Docker Security Best Practices](https://docs.docker.com/engine/security/)
- [OWASP Redis Security Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Redis_Security_Cheat_Sheet.html)

## Contact

For security concerns or questions about Redis configuration, contact the development team.

---

**Last Updated**: January 2026  
**Version**: 1.0  
**Maintained By**: StayAfrica DevOps Team
