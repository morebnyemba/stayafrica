# StayAfrica Backend API

Django REST Framework API for the StayAfrica booking platform.

## Features

- **Multi-Regional Payment Processing**: Supports Paynow (Zimbabwe), PayFast (South Africa), Stripe (International), and Cash on Arrival
- **Geospatial Search**: PostGIS-powered property search by location and radius
- **Async Task Processing**: Celery for email notifications and image processing
- **Rate Limiting**: Protection against abuse on sensitive endpoints
- **Comprehensive Validation**: Input sanitization and business logic validation
- **Audit Logging**: Track all critical user actions
- **Health Checks**: Kubernetes-ready liveness and readiness probes
- **API Documentation**: Auto-generated OpenAPI/Swagger documentation

## Tech Stack

- **Framework**: Django 5.0 + Django REST Framework
- **Database**: PostgreSQL 15 with PostGIS extension
- **Cache/Queue**: Redis
- **Task Queue**: Celery + Celery Beat
- **Authentication**: JWT (Simple JWT)
- **Storage**: AWS S3 (via django-storages)
- **Monitoring**: Sentry error tracking
- **API Docs**: drf-spectacular (OpenAPI 3.0)

## Project Structure

```
backend/
├── apps/                       # Django applications
│   ├── users/                  # User authentication & profiles
│   ├── properties/             # Property listings (PostGIS)
│   ├── bookings/               # Booking management
│   ├── payments/               # Payment processing
│   ├── reviews/                # Review system
│   ├── messaging/              # User messaging
│   ├── admin_dashboard/        # Admin panel
│   └── health/                 # Health check endpoints
├── services/                   # Business logic services
│   ├── payment_gateway.py      # Payment provider switching
│   ├── email_service.py        # Email notifications
│   ├── image_processor.py      # Image optimization
│   └── audit_logger.py         # Audit trail
├── tasks/                      # Celery tasks
│   ├── email_tasks.py          # Email sending
│   ├── image_tasks.py          # Image processing
│   └── notification_tasks.py   # Push notifications
├── utils/                      # Utility functions
│   ├── validators.py           # Custom validators
│   ├── decorators.py           # Custom decorators
│   └── helpers.py              # Helper functions
├── stayafrica/                 # Django project settings
│   ├── settings.py             # Main settings
│   ├── urls.py                 # URL configuration
│   └── celery.py               # Celery configuration
├── manage.py                   # Django management script
├── requirements.txt            # Python dependencies
├── pytest.ini                  # Test configuration
└── .env.example                # Environment variables template
```

## Setup

### Prerequisites

- Python 3.11+
- PostgreSQL 15+ with PostGIS extension
- Redis 7+
- Docker & Docker Compose (optional)

### Local Development

1. **Clone the repository**
   ```bash
   git clone https://github.com/morebnyemba/stayafrica.git
   cd stayafrica/backend
   ```

2. **Create virtual environment**
   ```bash
   python -m venv venv
   source venv/bin/activate  # On Windows: venv\Scripts\activate
   ```

3. **Install dependencies**
   ```bash
   pip install -r requirements.txt
   ```

4. **Set up environment variables**
   ```bash
   cp .env.example .env
   # Edit .env with your configuration
   ```

5. **Run database migrations**
   ```bash
   python manage.py migrate
   ```

6. **Create superuser**
   ```bash
   python manage.py createsuperuser
   ```

7. **Run development server**
   ```bash
   python manage.py runserver
   ```

### Docker Development

1. **Start all services**
   ```bash
   cd .. # Go to project root
   docker compose up -d
   ```

2. **Run migrations**
   ```bash
   docker compose exec web python manage.py migrate
   ```

3. **Create superuser**
   ```bash
   docker compose exec web python manage.py createsuperuser
   ```

4. **Access services**
   - API: http://localhost:8000
   - API Docs: http://localhost:8000/api/docs/
   - Admin: http://localhost:8000/admin/

### Run Celery Workers

```bash
# Worker
celery -A stayafrica worker --loglevel=info

# Beat (scheduler)
celery -A stayafrica beat --loglevel=info
```

## Environment Variables

Key environment variables (see `.env.example` for complete list):

```bash
# Django
SECRET_KEY=your-secret-key
DEBUG=True
ALLOWED_HOSTS=localhost,127.0.0.1

# Database
DATABASE_NAME=stayafrica_db
DATABASE_USER=postgres
DATABASE_PASSWORD=postgres
DATABASE_HOST=localhost
DATABASE_PORT=5432

# Redis
REDIS_URL=redis://localhost:6379/0
CELERY_BROKER_URL=redis://localhost:6379/0

# JWT
JWT_EXPIRATION_HOURS=24
JWT_REFRESH_EXPIRATION_DAYS=7

# Email
EMAIL_BACKEND=django.core.mail.backends.console.EmailBackend
EMAIL_HOST=smtp.gmail.com
EMAIL_PORT=587
EMAIL_USE_TLS=True
EMAIL_HOST_USER=your-email@example.com
EMAIL_HOST_PASSWORD=your-password

# AWS S3 (optional)
USE_S3=False
AWS_ACCESS_KEY_ID=your-key
AWS_SECRET_ACCESS_KEY=your-secret
AWS_STORAGE_BUCKET_NAME=stayafrica-media
AWS_S3_REGION_NAME=us-east-1

# Sentry (optional)
SENTRY_DSN=your-sentry-dsn

# Application
COMMISSION_RATE=0.07
SERVICE_FEE=3.00
DEFAULT_CURRENCY=USD
FRONTEND_URL=http://localhost:3000

# Rate Limiting
RATELIMIT_ENABLE=True
```

## API Endpoints

### Authentication
- `POST /api/v1/auth/login/` - JWT login
- `POST /api/v1/auth/refresh/` - Refresh token
- `POST /api/v1/users/register/` - Register user
- `POST /api/v1/users/request_password_reset/` - Request password reset

### Users
- `GET /api/v1/users/profile/` - Get user profile
- `PUT /api/v1/users/profile/` - Update profile
- `POST /api/v1/users/change_password/` - Change password

### Properties
- `GET /api/v1/properties/` - List properties
- `POST /api/v1/properties/` - Create property (host)
- `GET /api/v1/properties/{id}/` - Property details
- `GET /api/v1/properties/search_nearby/` - Geospatial search
- `GET /api/v1/properties/{id}/availability/` - Check availability

### Bookings
- `GET /api/v1/bookings/` - List bookings
- `POST /api/v1/bookings/` - Create booking
- `GET /api/v1/bookings/{id}/` - Booking details
- `POST /api/v1/bookings/{id}/confirm/` - Confirm booking (host)
- `POST /api/v1/bookings/{id}/cancel/` - Cancel booking
- `POST /api/v1/bookings/{id}/complete/` - Mark as complete (host)

### Payments
- `POST /api/v1/payments/initiate/` - Initiate payment
- `POST /api/v1/payments/webhook/` - Payment webhook
- `GET /api/v1/payments/{id}/status/` - Payment status

### Reviews
- `GET /api/v1/reviews/` - List reviews
- `POST /api/v1/reviews/` - Create review
- `GET /api/v1/reviews/property_reviews/` - Property reviews
- `GET /api/v1/reviews/host_stats/` - Host statistics

### Health Checks
- `GET /api/health/` - Simple health check
- `GET /api/health/detailed/` - Detailed health check
- `GET /api/health/ready/` - Readiness probe
- `GET /api/health/live/` - Liveness probe

## Testing

Run tests with pytest:

```bash
# Run all tests
pytest

# Run with coverage
pytest --cov

# Run specific test file
pytest apps/users/tests.py

# Run specific test
pytest apps/users/tests.py::TestUserModel::test_user_creation
```

## Rate Limiting

Sensitive endpoints are rate-limited:
- Authentication: 3/hour
- Registration: 3/hour
- Password reset: 3/hour
- Booking creation: 10/minute
- Payment initiation: 5/minute
- Review creation: 5/hour

## Security Features

- JWT authentication with refresh tokens
- Password strength validation
- Input sanitization (XSS prevention)
- Rate limiting on sensitive endpoints
- Webhook signature verification
- SQL injection protection (Django ORM)
- CSRF protection
- Audit logging for compliance

## Deployment

### Production Checklist

1. Set `DEBUG=False`
2. Configure `ALLOWED_HOSTS`
3. Set secure `SECRET_KEY`
4. Configure production database
5. Set up SSL/HTTPS
6. Configure email service
7. Set up S3 for media files
8. Configure Sentry for error tracking
9. Set up Celery workers with supervisor
10. Configure Redis for caching
11. Set up monitoring and alerts

### Docker Production

```bash
# Build production image
docker build -t stayafrica-api:latest .

# Run with production settings
docker run -d \
  -e DEBUG=False \
  -e SECRET_KEY=your-secret-key \
  -e DATABASE_HOST=your-db-host \
  -p 8000:8000 \
  stayafrica-api:latest
```

### Kubernetes Deployment

The API includes Kubernetes-ready health check endpoints:
- Liveness: `/api/health/live/`
- Readiness: `/api/health/ready/`

## Monitoring

### Sentry Integration

Error tracking is configured via `SENTRY_DSN` environment variable.

### Logging

Logs are written to:
- Console (stdout)
- File: `logs/stayafrica.log` (rotating, 10MB max, 5 backups)

### Metrics

Health check endpoints provide system metrics:
- Database connectivity
- Cache connectivity
- Celery worker status

## Contributing

1. Create a feature branch
2. Make your changes
3. Run tests: `pytest`
4. Run linters: `flake8 apps services utils`
5. Submit a pull request

## License

Proprietary - All rights reserved

## Support

For issues or questions, contact: support@stayafrica.com
