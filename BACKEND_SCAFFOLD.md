# Backend Scaffolding Progress - StayAfrica

**Project:** StayAfrica (MVP)  
**Date Started:** December 6, 2025  
**Backend Tech:** Django 5.0+, DRF, PostgreSQL + PostGIS, Celery, Redis

---

## ✅ Completed Tasks

### Phase 1: Foundation & Backend Core (✅ COMPLETED)

- [x] Create project directory structure and monorepo layout
- [x] Setup `docker-compose.yml` (Django, Postgres/PostGIS, Redis, Celery)
- [x] Initialize Django project (`manage.py`, settings, urls, wsgi, asgi)
- [x] Create `users` app with custom user model (email auth, roles, verification)
- [x] Create `properties` app with PostGIS integration (PointField, geospatial search)
- [x] Create `bookings` app with booking logic and cost calculation
- [x] Create `payments` app with multi-provider gateway (Paynow, PayFast, Stripe, Ozow)
- [x] Create `reviews` app with post-checkout review system
- [x] Create `messaging` app with simple chat API
- [x] Create `admin_dashboard` app with AuditLog model and statistics
- [x] Implement service layer (PaymentGateway, EmailService, ImageProcessor, AuditLogger)
- [x] Setup Celery for async tasks (emails, image processing)
- [x] Add Sentry integration for error tracking
- [x] Implement AuditLog model and basic logging
- [x] Create API versioning structure (`/api/v1/`) with OpenAPI documentation

---

## 📋 Directory Structure

```
stayafrica/
├── backend/
│   ├── manage.py
│   ├── stayafrica/
│   │   ├── settings.py
│   │   ├── urls.py
│   │   ├── wsgi.py
│   │   └── asgi.py
│   ├── apps/
│   │   ├── users/
│   │   ├── properties/
│   │   ├── bookings/
│   │   ├── payments/
│   │   ├── reviews/
│   │   ├── messaging/
│   │   └── admin/
│   ├── services/
│   │   ├── payment_gateway.py
│   │   ├── email_service.py
│   │   ├── image_processor.py
│   │   └── audit_logger.py
│   ├── api/
│   │   ├── v1/
│   │   │   ├── users/
│   │   │   ├── properties/
│   │   │   ├── bookings/
│   │   │   └── payments/
│   │   └── serializers/
│   ├── utils/
│   │   ├── validators.py
│   │   ├── decorators.py
│   │   └── helpers.py
│   ├── tasks/
│   │   ├── email_tasks.py
│   │   ├── image_tasks.py
│   │   └── notification_tasks.py
│   ├── requirements.txt
│   ├── Dockerfile
│   └── .env.example
├── docker-compose.yml
├── MASTER_PLAN.md
├── BACKEND_SCAFFOLD.md
├── FRONTEND_SCAFFOLD.md
└── MOBILE_SCAFFOLD.md
```

---

## 🔧 Apps Overview

### 1. `users` App
- Custom User Model (email, phone, role, country_of_residence, is_verified)
- JWT Authentication (drf-simplejwt)
- User verification workflow
- Profile management

### 2. `properties` App
- Property CRUD (title, description, property_type, location, price_per_night)
- PostGIS integration (location as PointField)
- Amenities (M2M relationship)
- Status tracking (active, inactive, pending_approval)

### 3. `bookings` App
- Booking logic (check_in, check_out, availability check)
- Cost calculation (nightly_total, service_fee, commission_fee, cleaning_fee, grand_total)
- Booking status (pending, confirmed, cancelled, completed)

### 4. `payments` App
- Payment Gateway service (Paynow, PayFast, Stripe, Cash on Arrival)
- Regional payment switch logic
- Transaction logging
- Webhook handlers

### 5. `reviews` App
- Review creation (only after checkout)
- Rating system
- Review management

### 6. `messaging` App
- Simple chat API (sender, receiver, text)
- Message history
- Notification triggers

### 7. `admin` App
- Custom Django Unfold dashboards
- Stats (Total Revenue, Active Hosts)
- Bulk actions (approval, payouts, moderation)
- Audit logs

---

## 🔐 Service Layer

### Core Services

#### `PaymentGateway` (payments_gateway.py)
```python
class PaymentGateway:
    def get_provider(user_country, amount) -> Provider
    def initiate_payment(booking, provider) -> TransactionID
    def handle_webhook(provider, data) -> Status
```

#### `EmailService` (email_service.py)
```python
class EmailService:
    def send_booking_confirmation(booking)
    def send_payment_receipt(payment)
    def send_verification_email(user)
```

#### `ImageProcessor` (image_processor.py)
```python
class ImageProcessor:
    def optimize_property_image(image) -> Async Task
    def generate_thumbnails(image) -> Async Task
```

#### `AuditLogger` (audit_logger.py)
```python
class AuditLogger:
    def log_action(user, action, model, object_id, changes)
```

---

## 📊 Database Models Summary

| App | Model | Key Fields |
|-----|-------|-----------|
| users | User | email, phone, role, is_verified, country_of_residence |
| properties | Property | host, title, location (PointField), price_per_night, status |
| properties | Amenity | name, icon |
| bookings | Booking | guest, property, check_in, check_out, status, grand_total |
| payments | Payment | booking, provider, status, gateway_ref |
| reviews | Review | booking, guest, host, rating, text, created_at |
| messaging | Message | sender, receiver, text, created_at |
| admin | AuditLog | user, action, model, object_id, changes, timestamp |

---

## 🎯 API Endpoints (Phase 1-2)

### Users
- `POST /api/v1/auth/register/` - Register
- `POST /api/v1/auth/login/` - Login
- `POST /api/v1/auth/refresh/` - Refresh token
- `GET /api/v1/users/profile/` - Get profile

### Properties
- `GET /api/v1/properties/` - List (with filters: country, city, price)
- `POST /api/v1/properties/` - Create (host only)
- `GET /api/v1/properties/{id}/` - Detail
- `PUT /api/v1/properties/{id}/` - Update (host only)
- `DELETE /api/v1/properties/{id}/` - Delete (host only)

### Bookings
- `POST /api/v1/bookings/` - Create booking
- `GET /api/v1/bookings/{id}/` - Get booking details
- `PUT /api/v1/bookings/{id}/` - Update status

### Payments
- `POST /api/v1/payments/` - Initiate payment
- `POST /api/v1/payments/webhook/{provider}/` - Webhook handler
- `GET /api/v1/payments/{id}/` - Payment status

---

## 🚀 Next Steps

1. ✅ Create directory structure
2. ✅ Set up Docker Compose with all services
3. ✅ Initialize Django project with all configuration
4. ✅ Create all 7 apps (users, properties, bookings, payments, reviews, messaging, admin)
5. ✅ Implement service layer with business logic
6. ✅ Setup Celery, Redis, and async tasks
7. ⏳ **NEXT:** Test Docker setup and database migrations
8. ⏳ Create frontend scaffolding (Next.js)
9. ⏳ Create mobile app scaffolding (React Native/Expo)
10. ⏳ Implement advanced features (analytics, notifications, etc.)

---

## 📝 Installation & Running

### Prerequisites
- Docker & Docker Compose
- Python 3.11+
- PostgreSQL with PostGIS (or use Docker)

### Setup

```bash
# Navigate to backend directory
cd backend

# Create .env file from example
cp .env.example .env

# From project root, start Docker services
docker-compose up -d

# Run migrations
docker-compose exec web python manage.py migrate

# Create superuser
docker-compose exec web python manage.py createsuperuser

# Access services:
# - Backend API: http://localhost:8000
# - API Docs: http://localhost:8000/api/docs/
# - Admin: http://localhost:8000/admin/
# - Swagger UI: http://localhost:8000/api/docs/
# - ReDoc: http://localhost:8000/api/redoc/
```

---

## 📊 API Endpoints Summary

**Authentication:**
- `POST /api/v1/auth/login/` - JWT login
- `POST /api/v1/auth/refresh/` - Refresh token
- `POST /api/v1/users/register/` - Register

**Properties:**
- `GET /api/v1/properties/` - List with filters
- `POST /api/v1/properties/` - Create (hosts)
- `GET /api/v1/properties/{id}/` - Details
- `GET /api/v1/properties/search_nearby/?lat=X&lon=Y&radius=10` - Geospatial search

**Bookings:**
- `POST /api/v1/bookings/` - Create booking
- `GET /api/v1/bookings/{id}/` - Booking details
- `POST /api/v1/bookings/{id}/confirm/` - Confirm
- `POST /api/v1/bookings/{id}/cancel/` - Cancel

**Payments:**
- `POST /api/v1/payments/initiate/` - Initiate payment (regional provider)
- `POST /api/v1/payments/webhook/{provider}/` - Webhook handler

**Reviews, Messaging, Admin:**
- All with standard CRUD operations

---

## 🔐 Key Architecture Decisions

✅ **Monorepo Structure** - Single repository for easier management
✅ **PostGIS** - Native geospatial queries for property location
✅ **Service Layer** - Business logic separate from views
✅ **Celery + Redis** - Async tasks for emails, image processing
✅ **JWT Authentication** - Stateless API with refresh tokens
✅ **Regional Payment Switch** - Dynamic provider selection based on user country
✅ **Docker Compose** - Complete dev environment with all services
✅ **OpenAPI Documentation** - Auto-generated API docs
✅ **Sentry Integration** - Error tracking and monitoring
✅ **Audit Logging** - Track all important user actions

---

**Last Updated:** December 6, 2025  
**Status:** ✅ Phase 1 Complete - Backend Infrastructure Ready
