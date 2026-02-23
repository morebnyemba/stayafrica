# Quick Reference - StayAfrica Backend

## 🚀 Getting Started

### 1. Start Docker Services
```bash
docker-compose up -d
```

### 2. Run Migrations
```bash
docker-compose exec web python manage.py migrate
```

### 3. Create Superuser
```bash
docker-compose exec web python manage.py createsuperuser
```

### 4. Access Services

| Service | URL | Purpose |
|---------|-----|---------|
| API | http://localhost:8000 | REST API endpoints |
| Swagger Docs | http://localhost:8000/api/docs/ | Interactive API documentation |
| ReDoc | http://localhost:8000/api/redoc/ | Beautiful API documentation |
| Admin | http://localhost:8000/admin/ | Django admin panel |
| Database | localhost:5432 | PostgreSQL with PostGIS |
| Redis | localhost:6379 | Caching & Celery broker |

---

## 📚 Core API Endpoints

### Authentication
```
POST   /api/v1/auth/login/              Login with email & password
POST   /api/v1/auth/refresh/            Refresh JWT token
POST   /api/v1/users/register/          Register new user
```

### Users
```
GET    /api/v1/users/profile/           Get current user profile
PUT    /api/v1/users/profile/           Update profile
POST   /api/v1/users/change_password/   Change password
```

### Properties
```
GET    /api/v1/properties/              List all (with filters)
POST   /api/v1/properties/              Create property (host)
GET    /api/v1/properties/{id}/         Get property details
PUT    /api/v1/properties/{id}/         Update property (host)
DELETE /api/v1/properties/{id}/         Delete property (host)
GET    /api/v1/properties/search_nearby/?lat=X&lon=Y&radius=10
```

### Bookings
```
POST   /api/v1/bookings/                Create booking
GET    /api/v1/bookings/{id}/           Get booking details
PUT    /api/v1/bookings/{id}/           Update booking
POST   /api/v1/bookings/{id}/confirm/   Confirm booking
POST   /api/v1/bookings/{id}/cancel/    Cancel booking
```

### Payments
```
POST   /api/v1/payments/initiate/       Initiate payment
GET    /api/v1/payments/{id}/           Get payment status
POST   /api/v1/payments/webhook/        Payment webhook
```

### Reviews
```
POST   /api/v1/reviews/                 Create review (after checkout)
GET    /api/v1/reviews/{id}/            Get review
PUT    /api/v1/reviews/{id}/            Update review
```

### Messages
```
POST   /api/v1/messages/                Send message
GET    /api/v1/messages/                Get messages
GET    /api/v1/messages/conversations/  Get conversation list
GET    /api/v1/messages/unread/         Get unread count
```

---

## 🔐 Authentication

### Login Request
```bash
curl -X POST http://localhost:8000/api/v1/auth/login/ \
  -H "Content-Type: application/json" \
  -d '{"email":"user@example.com", "password":"password123"}'
```

### Response
```json
{
  "refresh": "eyJ0eXAiOiJKV1QiLCJhbGc...",
  "access": "eyJ0eXAiOiJKV1QiLCJhbGc...",
  "user": {
    "id": 1,
    "email": "user@example.com",
    "role": "guest",
    "is_verified": true
  }
}
```

### Using Access Token
```bash
curl -X GET http://localhost:8000/api/v1/users/profile/ \
  -H "Authorization: Bearer <access_token>"
```

---

## 💾 Database Models

### User
```python
- id (PK)
- email (unique)
- phone_number
- role (guest|host|admin)
- country_of_residence
- is_verified
- profile_picture
- created_at, updated_at
```

### Property
```python
- id (PK)
- host_id (FK)
- title, description
- property_type (lodge|cottage|room|apartment|house|villa)
- location (PointField - PostGIS)
- country, city, suburb, address
- price_per_night, currency
- amenities (M2M)
- status (active|inactive|pending_approval)
- bedrooms, bathrooms, max_guests
- created_at, updated_at
```

### Booking
```python
- id (PK)
- booking_ref (unique)
- guest_id (FK)
- property_id (FK)
- check_in, check_out
- nightly_total, service_fee, commission_fee, cleaning_fee, grand_total
- currency
- status (pending|confirmed|cancelled|completed)
- created_at, updated_at
```

### Payment
```python
- id (PK)
- booking_id (FK - OneToOne)
- provider (paynow|payfast|stripe|ozow|cash_on_arrival)
- gateway_ref (unique)
- status (initiated|success|failed|pending)
- amount, currency
- created_at, updated_at
```

---

## 🔄 Workflows

### Property Listing Workflow
```
1. Host registers → role: host
2. Host creates property → status: pending_approval
3. Admin approves property → status: active
4. Guest can now search and view property
5. Guest can book property
```

### Booking Workflow
```
1. Guest creates booking → status: pending
2. Host reviews booking
3. Host/Guest confirms → status: confirmed
4. Guest completes stay
5. Booking marked → status: completed
6. Guest can leave review (after checkout)
```

### Payment Workflow
```
1. Booking created → check user country
2. Payment system suggests providers for country
3. Guest selects provider
4. Payment initiated → status: initiated
5. Provider processes payment
6. Webhook updates status → success|failed
7. If success → booking confirmed
```

---

## 🛠️ Common Tasks

### Create a Test User
```bash
docker-compose exec web python manage.py shell
>>> from apps.users.models import User
>>> User.objects.create_user(
...     email='test@example.com',
...     username='testuser',
...     password='testpass123',
...     role='guest'
... )
```

### Create a Property (as Host)
```bash
# Use API: POST /api/v1/properties/
{
  "title": "Beautiful Beach Lodge",
  "description": "A beautiful lodge by the beach",
  "property_type": "lodge",
  "location": {
    "type": "Point",
    "coordinates": [-17.8252, 31.0335]  # lon, lat
  },
  "country": "Zimbabwe",
  "city": "Victoria Falls",
  "address": "Main Street 123",
  "price_per_night": 150.00,
  "bedrooms": 3,
  "bathrooms": 2,
  "max_guests": 6
}
```

### Search Nearby Properties
```bash
GET /api/v1/properties/search_nearby/?lat=-17.8252&lon=31.0335&radius=10
```

### Create a Booking
```bash
POST /api/v1/bookings/
{
  "property": 1,
  "check_in": "2025-12-15",
  "check_out": "2025-12-20",
  "cleaning_fee": 25.00
}
```

---

## 🐛 Debugging

### Check Docker Logs
```bash
# Django web server
docker-compose logs -f web

# Celery worker
docker-compose logs -f celery

# Database
docker-compose logs -f db
```

### Access Django Shell
```bash
docker-compose exec web python manage.py shell
```

### Check Database
```bash
docker-compose exec db psql -U postgres -d stayafrica_db
```

---

## 📊 Admin Panel Features

1. **User Management** - Create, edit, verify users
2. **Property Approval** - Review and approve pending properties
3. **Booking Management** - View all bookings and statuses
4. **Payment Tracking** - Monitor transactions
5. **Review Moderation** - Review user reviews
6. **Audit Logs** - Track all actions
7. **Statistics Dashboard** - Revenue, users, bookings metrics

---

## 🔗 Service Layer

### PaymentGatewayService
```python
from services.payment_gateway import PaymentGatewayService

service = PaymentGatewayService()

# Get providers for country
providers = service.get_available_providers('Zimbabwe')
# Returns: ['paynow', 'cash_on_arrival']

# Calculate fees
fees = service.calculate_fees(base_price=100, cleaning_fee=25)
# Returns: {
#   'base_price': 100,
#   'service_fee': 3,
#   'commission_fee': 7.21,
#   'cleaning_fee': 25,
#   'total_fees': 35.21,
#   'host_payout': 117.79
# }
```

### EmailService
```python
from services.email_service import EmailService

# Send booking confirmation (async)
EmailService.send_booking_confirmation(booking)

# Send payment receipt
EmailService.send_payment_receipt(payment)
```

### AuditLoggerService
```python
from services.audit_logger import AuditLoggerService

# Log action
AuditLoggerService.log_booking_action(
    user=user,
    booking=booking,
    action='booking_confirmed'
)
```

---

## 📦 Environment Variables (.env)

```env
# Django
DEBUG=True
SECRET_KEY=your-secret-key
ALLOWED_HOSTS=localhost,127.0.0.1

# Database
DATABASE_NAME=stayafrica_db
DATABASE_USER=postgres
DATABASE_PASSWORD=postgres
DATABASE_HOST=db
DATABASE_PORT=5432

# Redis & Celery
REDIS_URL=redis://redis:6379/0
CELERY_BROKER_URL=redis://redis:6379/0

# AWS S3
USE_S3=False
AWS_STORAGE_BUCKET_NAME=stayafrica-bucket

# Payment Providers
PAYNOW_MERCHANT_ID=...
PAYFAST_MERCHANT_ID=...
STRIPE_API_KEY=...
```

---

## 🚀 Deployment Checklist

- [ ] Set `DEBUG=False` in production
- [ ] Generate new `SECRET_KEY`
- [ ] Configure allowed hosts
- [ ] Setup AWS S3 credentials
- [ ] Configure email backend
- [ ] Setup Sentry DSN
- [ ] Setup payment provider credentials
- [ ] Run database migrations
- [ ] Collect static files
- [ ] Configure nginx reverse proxy
- [ ] Setup SSL certificates
- [ ] Configure backup strategy
- [ ] Setup monitoring

---

## 📖 Useful Commands

```bash
# Create migrations for a new app
docker-compose exec web python manage.py makemigrations apps.users

# Apply migrations
docker-compose exec web python manage.py migrate

# Create superuser
docker-compose exec web python manage.py createsuperuser

# Run Django shell
docker-compose exec web python manage.py shell

# Collect static files
docker-compose exec web python manage.py collectstatic --noinput

# Run tests
docker-compose exec web python manage.py test

# Check for errors
docker-compose exec web python manage.py check

# Clear cache
docker-compose exec redis redis-cli FLUSHALL

# Stop all services
docker-compose down

# Stop and remove volumes (reset database)
docker-compose down -v
```

---

**Backend Ready! 🎉**  
**Next: Create Frontend (Next.js)**
