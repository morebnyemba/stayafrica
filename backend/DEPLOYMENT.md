# StayAfrica Backend - Deployment Guide

This guide covers deploying the StayAfrica backend API to production environments.

## Pre-Deployment Checklist

### Security
- [ ] Set `DEBUG=False` in production
- [ ] Generate and set a strong `SECRET_KEY`
- [ ] Configure `ALLOWED_HOSTS` with your domain(s)
- [ ] Set up HTTPS/SSL certificates
- [ ] Configure CORS for your frontend domains only
- [ ] Review and set all security middleware
- [ ] Enable CSRF protection
- [ ] Set secure session/cookie settings
- [ ] Configure Sentry DSN for error tracking

### Database
- [ ] Set up production PostgreSQL database with PostGIS
- [ ] Run all migrations
- [ ] Set up database backups (daily recommended)
- [ ] Configure connection pooling
- [ ] Set up read replicas (if needed)

### Storage
- [ ] Set up AWS S3 bucket for media files
- [ ] Configure bucket permissions and CORS
- [ ] Set up CDN (CloudFront) for media delivery
- [ ] Configure presigned URLs for secure access

### Email
- [ ] Configure production email service (SendGrid, AWS SES, etc.)
- [ ] Set up email templates
- [ ] Configure SPF and DKIM records
- [ ] Test email delivery

### Caching & Queue
- [ ] Set up production Redis instance
- [ ] Configure Redis for both cache and Celery broker
- [ ] Set up Celery workers
- [ ] Configure Celery Beat for scheduled tasks
- [ ] Monitor queue length and worker health

### Monitoring
- [ ] Configure Sentry for error tracking
- [ ] Set up application logging
- [ ] Configure log aggregation (ELK, CloudWatch, etc.)
- [ ] Set up uptime monitoring
- [ ] Configure alerts for critical errors
- [ ] Set up performance monitoring

## Deployment Options

### Option 1: Docker Deployment

#### 1. Build Production Image

```bash
# Build the image
docker build -t stayafrica-api:v1.0.0 -f Dockerfile.prod .

# Tag for registry
docker tag stayafrica-api:v1.0.0 your-registry/stayafrica-api:v1.0.0

# Push to registry
docker push your-registry/stayafrica-api:v1.0.0
```

#### 2. Docker Compose Production

```yaml
# docker-compose.prod.yml
version: '3.9'

services:
  web:
    image: your-registry/stayafrica-api:v1.0.0
    command: gunicorn stayafrica.wsgi:application --bind 0.0.0.0:8000 --workers 4
    environment:
      - DEBUG=False
      - SECRET_KEY=${SECRET_KEY}
      - DATABASE_HOST=${DATABASE_HOST}
      - DATABASE_NAME=${DATABASE_NAME}
      - DATABASE_USER=${DATABASE_USER}
      - DATABASE_PASSWORD=${DATABASE_PASSWORD}
      - REDIS_URL=${REDIS_URL}
      - SENTRY_DSN=${SENTRY_DSN}
    ports:
      - "8000:8000"
    depends_on:
      - redis
    restart: always

  celery:
    image: your-registry/stayafrica-api:v1.0.0
    command: celery -A stayafrica worker --loglevel=info --concurrency=4
    environment:
      - DEBUG=False
      - DATABASE_HOST=${DATABASE_HOST}
      - REDIS_URL=${REDIS_URL}
    depends_on:
      - redis
      - web
    restart: always

  celery-beat:
    image: your-registry/stayafrica-api:v1.0.0
    command: celery -A stayafrica beat --loglevel=info
    environment:
      - DEBUG=False
      - DATABASE_HOST=${DATABASE_HOST}
      - REDIS_URL=${REDIS_URL}
    depends_on:
      - redis
      - web
    restart: always

  redis:
    image: redis:7-alpine
    restart: always

  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
      - ./ssl:/etc/nginx/ssl
    depends_on:
      - web
    restart: always
```

#### 3. Run Production Stack

```bash
docker-compose -f docker-compose.prod.yml up -d
```

### Option 2: Kubernetes Deployment

#### 1. Create Kubernetes Manifests

**Deployment (api-deployment.yaml)**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: stayafrica-api
spec:
  replicas: 3
  selector:
    matchLabels:
      app: stayafrica-api
  template:
    metadata:
      labels:
        app: stayafrica-api
    spec:
      containers:
      - name: api
        image: your-registry/stayafrica-api:v1.0.0
        ports:
        - containerPort: 8000
        env:
        - name: DEBUG
          value: "False"
        - name: SECRET_KEY
          valueFrom:
            secretKeyRef:
              name: stayafrica-secrets
              key: secret-key
        - name: DATABASE_HOST
          valueFrom:
            configMapKeyRef:
              name: stayafrica-config
              key: database-host
        resources:
          requests:
            memory: "512Mi"
            cpu: "500m"
          limits:
            memory: "1Gi"
            cpu: "1000m"
        livenessProbe:
          httpGet:
            path: /api/health/live/
            port: 8000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /api/health/ready/
            port: 8000
          initialDelaySeconds: 10
          periodSeconds: 5
```

**Service (api-service.yaml)**
```yaml
apiVersion: v1
kind: Service
metadata:
  name: stayafrica-api
spec:
  selector:
    app: stayafrica-api
  ports:
  - port: 80
    targetPort: 8000
  type: LoadBalancer
```

**Celery Worker (celery-deployment.yaml)**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: stayafrica-celery
spec:
  replicas: 2
  selector:
    matchLabels:
      app: stayafrica-celery
  template:
    metadata:
      labels:
        app: stayafrica-celery
    spec:
      containers:
      - name: celery-worker
        image: your-registry/stayafrica-api:v1.0.0
        command: ["celery", "-A", "stayafrica", "worker", "--loglevel=info"]
        env:
        - name: DATABASE_HOST
          valueFrom:
            configMapKeyRef:
              name: stayafrica-config
              key: database-host
        - name: REDIS_URL
          valueFrom:
            configMapKeyRef:
              name: stayafrica-config
              key: redis-url
        resources:
          requests:
            memory: "512Mi"
            cpu: "500m"
```

#### 2. Apply Manifests

```bash
kubectl apply -f k8s/
```

### Option 3: AWS ECS/Fargate

#### 1. Create Task Definition

```json
{
  "family": "stayafrica-api",
  "networkMode": "awsvpc",
  "requiresCompatibilities": ["FARGATE"],
  "cpu": "1024",
  "memory": "2048",
  "containerDefinitions": [
    {
      "name": "api",
      "image": "your-ecr-repo/stayafrica-api:v1.0.0",
      "portMappings": [
        {
          "containerPort": 8000,
          "protocol": "tcp"
        }
      ],
      "environment": [
        {
          "name": "DEBUG",
          "value": "False"
        }
      ],
      "secrets": [
        {
          "name": "SECRET_KEY",
          "valueFrom": "arn:aws:secretsmanager:region:account:secret:name"
        }
      ],
      "logConfiguration": {
        "logDriver": "awslogs",
        "options": {
          "awslogs-group": "/ecs/stayafrica-api",
          "awslogs-region": "us-east-1",
          "awslogs-stream-prefix": "ecs"
        }
      }
    }
  ]
}
```

## Environment Variables

### Required Production Variables

```bash
# Django
DEBUG=False
SECRET_KEY=your-secret-key-min-50-chars
ALLOWED_HOSTS=api.stayafrica.com,www.stayafrica.com
FRONTEND_URL=https://www.stayafrica.com

# Database
DATABASE_ENGINE=django.contrib.gis.db.backends.postgis
DATABASE_NAME=stayafrica_prod
DATABASE_USER=stayafrica_user
DATABASE_PASSWORD=strong-password-here
DATABASE_HOST=your-rds-endpoint.amazonaws.com
DATABASE_PORT=5432

# Redis
REDIS_URL=redis://your-redis-endpoint:6379/0
CELERY_BROKER_URL=redis://your-redis-endpoint:6379/0
CELERY_RESULT_BACKEND=redis://your-redis-endpoint:6379/0

# AWS S3
USE_S3=True
AWS_ACCESS_KEY_ID=your-access-key
AWS_SECRET_ACCESS_KEY=your-secret-key
AWS_STORAGE_BUCKET_NAME=stayafrica-media
AWS_S3_REGION_NAME=us-east-1

# Email
EMAIL_BACKEND=django.core.mail.backends.smtp.EmailBackend
EMAIL_HOST=email-smtp.us-east-1.amazonaws.com
EMAIL_PORT=587
EMAIL_USE_TLS=True
EMAIL_HOST_USER=your-ses-user
EMAIL_HOST_PASSWORD=your-ses-password
DEFAULT_FROM_EMAIL=noreply@stayafrica.com

# JWT
JWT_EXPIRATION_HOURS=24
JWT_REFRESH_EXPIRATION_DAYS=7
JWT_ALGORITHM=HS256
JWT_SECRET_KEY=different-from-django-secret

# Monitoring
SENTRY_DSN=https://your-sentry-dsn@sentry.io/project-id

# Application
COMMISSION_RATE=0.07
SERVICE_FEE=3.00
DEFAULT_CURRENCY=USD

# Rate Limiting
RATELIMIT_ENABLE=True

# Payment Providers
PAYNOW_WEBHOOK_SECRET=your-webhook-secret
PAYFAST_WEBHOOK_SECRET=your-webhook-secret
STRIPE_SECRET_KEY=your-stripe-secret
```

## Database Migrations

```bash
# Collect static files
python manage.py collectstatic --noinput

# Run migrations
python manage.py migrate

# Create initial data (if needed)
python manage.py loaddata initial_data.json
```

## Nginx Configuration

```nginx
upstream stayafrica_api {
    server web:8000;
}

server {
    listen 80;
    server_name api.stayafrica.com;
    
    # Redirect to HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name api.stayafrica.com;

    ssl_certificate /etc/nginx/ssl/cert.pem;
    ssl_certificate_key /etc/nginx/ssl/key.pem;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers HIGH:!aNULL:!MD5;

    client_max_body_size 20M;

    location / {
        proxy_pass http://stayafrica_api;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_redirect off;
    }

    location /static/ {
        alias /app/staticfiles/;
    }

    location /media/ {
        alias /app/media/;
    }

    # Health check endpoint
    location /api/health/live/ {
        proxy_pass http://stayafrica_api;
        access_log off;
    }
}
```

## Monitoring & Alerts

### CloudWatch Alarms (AWS)

```bash
# High error rate
aws cloudwatch put-metric-alarm \
  --alarm-name stayafrica-api-errors \
  --alarm-description "API error rate is too high" \
  --metric-name 5XXError \
  --namespace AWS/ApplicationELB \
  --statistic Sum \
  --period 300 \
  --threshold 10 \
  --comparison-operator GreaterThanThreshold \
  --evaluation-periods 2

# High CPU
aws cloudwatch put-metric-alarm \
  --alarm-name stayafrica-api-cpu \
  --metric-name CPUUtilization \
  --namespace AWS/ECS \
  --statistic Average \
  --period 300 \
  --threshold 80 \
  --comparison-operator GreaterThanThreshold \
  --evaluation-periods 2
```

## Rollback Procedure

### Docker
```bash
# Revert to previous version
docker-compose -f docker-compose.prod.yml down
docker-compose -f docker-compose.prod.yml up -d
```

### Kubernetes
```bash
# Rollback deployment
kubectl rollout undo deployment/stayafrica-api

# Check rollout status
kubectl rollout status deployment/stayafrica-api
```

## Backup & Restore

### Database Backup

```bash
# Backup
pg_dump -h your-db-host -U stayafrica_user -d stayafrica_prod > backup_$(date +%Y%m%d).sql

# Restore
psql -h your-db-host -U stayafrica_user -d stayafrica_prod < backup_20241206.sql
```

### Media Files Backup

```bash
# Sync S3 bucket
aws s3 sync s3://stayafrica-media s3://stayafrica-media-backup
```

## Post-Deployment Testing

```bash
# Health check
curl https://api.stayafrica.com/api/health/

# Detailed health check
curl https://api.stayafrica.com/api/health/detailed/

# API endpoints
curl https://api.stayafrica.com/api/v1/properties/

# Login
curl -X POST https://api.stayafrica.com/api/v1/auth/login/ \
  -H "Content-Type: application/json" \
  -d '{"email":"test@example.com","password":"password"}'
```

## Troubleshooting

### Common Issues

1. **Database connection errors**
   - Check security groups/firewall
   - Verify DATABASE_HOST is correct
   - Test connection: `psql -h $DATABASE_HOST -U $DATABASE_USER -d $DATABASE_NAME`

2. **Redis connection errors**
   - Check REDIS_URL
   - Verify Redis is running: `redis-cli -h your-redis-host ping`

3. **Static files not loading**
   - Run `python manage.py collectstatic`
   - Check S3 permissions
   - Verify nginx configuration

4. **Celery tasks not processing**
   - Check Celery logs: `docker logs stayafrica_celery`
   - Verify Redis connection
   - Check task queue: `celery -A stayafrica inspect active`

## Performance Optimization

1. **Database**
   - Enable connection pooling
   - Add database indexes
   - Use read replicas for read-heavy operations

2. **Caching**
   - Enable Redis caching
   - Cache expensive queries
   - Use ETags for API responses

3. **Static Files**
   - Use CDN for static files
   - Enable gzip compression
   - Set appropriate cache headers

4. **Application**
   - Use Gunicorn with multiple workers
   - Enable async I/O where possible
   - Profile slow endpoints

## Security Hardening

1. **Network**
   - Use VPC/private subnets
   - Restrict security groups
   - Use WAF for DDoS protection

2. **Application**
   - Keep dependencies updated
   - Run security audits: `safety check`
   - Enable rate limiting
   - Use prepared statements (Django ORM)

3. **Secrets Management**
   - Use AWS Secrets Manager or HashiCorp Vault
   - Rotate credentials regularly
   - Never commit secrets to git

## Support

For deployment issues, contact: devops@stayafrica.com
