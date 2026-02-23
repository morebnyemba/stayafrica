# 🏗️ StayAfrica Backend Architecture - Visual Summary

**Status:** ✅ Complete | **Date:** December 6, 2025

---

## 🎯 Project Overview

```
StayAfrica - AirBNB-like Property Booking Platform
├─ Target Markets: Zimbabwe, South Africa, Botswana, Namibia, Zambia
├─ Tech Stack: Django, PostgreSQL/PostGIS, React Native, Next.js
└─ Status: Backend ✅ | Frontend ⏳ | Mobile ⏳
```

---

## 🏗️ System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                          CLIENTS                                 │
├─────────────┬──────────────────────────┬─────────────────────────┤
│   Web App   │    Mobile App (Expo)     │   Admin Dashboard       │
│ (Next.js)   │  (React Native + TS)     │   (Django Unfold)       │
└──────┬──────┴────────────┬─────────────┴──────────┬──────────────┘
       │                    │                       │
       └────────────────────┼───────────────────────┘
                            │
       ┌────────────────────▼──────────────────────┐
       │       REST API Gateway (DRF)              │
       │    (/api/v1/ versioning)                  │
       │    - OpenAPI Documentation                │
       │    - JWT Authentication                   │
       │    - Rate Limiting Ready                  │
       └────┬───┬───┬───┬───┬───┬────────────────┘
            │   │   │   │   │   │
    ┌───────┴───┴───┴───┴───┴───┴────────────┐
    │         Django Applications             │
    ├──────┬──────┬──────┬──────┬─────┬──────┤
    │Users │Props │Books │Pays  │Revs │Msgs  │ Admin
    │      │erties│ings  │ments │iews │      │ Dashboard
    └──────┴──────┴──────┴──────┴─────┴──────┘
       │
    ┌──┴─────────────────────────────────────┐
    │      Service Layer (Business Logic)    │
    ├──────────────────────────────────────┬─┤
    │PaymentGateway│Email│ImageProc│Audit │ │
    └──────────────────────────────────────┴─┘
       │
    ┌──┴──────────────────────────────────────────────────┐
    │               Data Layer                            │
    ├────────────────┬─────────────┬─────────────┬────────┤
    │PostgreSQL 15   │ PostGIS     │ Indexes     │ Models │
    │+ PostGIS       │ Extension   │ Configured  │ 15+    │
    └────────────────┴─────────────┴─────────────┴────────┘

┌──────────────────────────────────────────────────────────────┐
│                    Infrastructure                            │
├─────────────┬─────────────┬──────────┬──────────┬────────────┤
│Redis Cache  │Celery Tasks │Celery    │Sentry    │   S3/CDN   │
│(Broker)     │(Async)      │Beat      │(Errors)  │ (Storage)  │
└─────────────┴─────────────┴──────────┴──────────┴────────────┘
```

---

## 📊 Data Flow Diagrams

### Booking Flow
```
Guest
  │
  ├─→ Search Properties
  │   └─→ PostGIS Query (geospatial)
  │
  ├─→ View Property Details
  │
  ├─→ Create Booking
  │   ├─→ Availability Check
  │   └─→ Cost Calculation
  │       ├─ Base Price × Nights
  │       ├─ Service Fee ($3)
  │       ├─ Commission (7%)
  │       └─ Cleaning Fee (optional)
  │
  ├─→ Initiate Payment
  │   ├─→ Regional Provider Selection
  │   │   ├─ Zimbabwe: Paynow, Cash
  │   │   ├─ South Africa: PayFast, Ozow
  │   │   └─ International: Stripe
  │   │
  │   └─→ Provider Processes Payment
  │       └─→ Webhook Confirms
  │
  ├─→ Booking Confirmed
  │
  ├─→ Stay Completes
  │
  └─→ Leave Review
      (Only after checkout date)
```

### Property Listing Flow
```
Host
  │
  ├─→ Register → role: host
  │
  ├─→ Create Property
  │   ├─ Title, Description
  │   ├─ Location (PostGIS PointField)
  │   ├─ Price/Night
  │   ├─ Amenities (M2M)
  │   └─ Images (Multiple)
  │
  ├─→ Property Status: pending_approval
  │
  ├─→ Admin Reviews & Approves
  │   └─→ Status: active
  │
  ├─→ Guest Can Now Book
  │
  ├─→ Receive Bookings
  │   ├─ Incoming Requests
  │   └─ Host Dashboard
  │
  └─→ After Guest Checkout
      └─→ Receive Review & Rating
```

### Payment Gateway Logic
```
Booking Created
        │
        ├─→ Check User.billing_country
        │
        ├─→ Zimbabwe?
        │   └─→ Show: [Paynow, Cash on Arrival]
        │
        ├─→ South Africa?
        │   └─→ Show: [PayFast, Ozow]
        │
        └─→ Other?
            └─→ Show: [Stripe]

Selected Provider
        │
        ├─→ Calculate Fees
        │   ├─ Commission = 7% of (base + $3)
        │   └─ Total Guest Pays = base + $3 + commission + cleaning
        │
        ├─→ Initiate Payment with Provider
        │
        └─→ Wait for Webhook
            ├─ Success → Booking Confirmed
            └─ Failed → Show Error
```

---

## 📱 API Layer Structure

```
/api/v1/
├─ /auth/
│  ├─ POST   login/              (JWT)
│  ├─ POST   refresh/            (Token Refresh)
│  └─ POST   register/           (User Creation)
│
├─ /users/
│  ├─ GET    profile/            (Current User)
│  ├─ PUT    profile/            (Update Profile)
│  └─ POST   change_password/
│
├─ /properties/
│  ├─ GET    /                   (List + Filters)
│  ├─ POST   /                   (Create - Host)
│  ├─ GET    /{id}/              (Detail)
│  ├─ PUT    /{id}/              (Update - Host)
│  ├─ DELETE /{id}/              (Delete - Host)
│  ├─ GET    /search_nearby/     (Geospatial)
│  └─ GET    /{id}/availability/ (Check Dates)
│
├─ /bookings/
│  ├─ POST   /                   (Create)
│  ├─ GET    /{id}/              (Detail)
│  ├─ PUT    /{id}/              (Update)
│  ├─ POST   /{id}/confirm/      (Confirm)
│  └─ POST   /{id}/cancel/       (Cancel)
│
├─ /payments/
│  ├─ POST   /initiate/          (Start Payment)
│  ├─ GET    /{id}/              (Status)
│  └─ POST   /webhook/           (Provider Webhook)
│
├─ /reviews/
│  ├─ POST   /                   (Create)
│  ├─ GET    /{id}/              (Detail)
│  ├─ PUT    /{id}/              (Update)
│  └─ GET    /                   (List)
│
├─ /messages/
│  ├─ POST   /                   (Send)
│  ├─ GET    /                   (List)
│  ├─ GET    /conversations/     (Chats)
│  └─ GET    /unread/            (Unread Count)
│
└─ /admin/
   ├─ GET    /stats/dashboard/   (Stats)
   ├─ POST   /bulk_approve_properties/
   ├─ POST   /bulk_payout/
   └─ GET    /audit-logs/        (Audit Trail)
```

---

## 🗄️ Database Schema

```
┌─────────────────────────────────────────────────────────┐
│                        USERS TABLE                      │
├──────────┬────────────┬──────────┬──────────────────────┤
│ id (PK)  │ email      │ role     │ is_verified          │
│ password │ phone      │ country  │ profile_picture      │
└─────────────────────────────────────────────────────────┘
          │ 1
          └─→ Many ┌──────────────────────────────────┐
                    │     PROPERTIES TABLE            │
                    ├──────────────────────────────────┤
                    │ id (PK)  │ title                │
                    │ location │ (PostGIS PointField) │
                    │ price    │ status               │
                    └──────────────────────────────────┘
                          │ 1
                          └─→ Many ┌──────────────────────────┐
                                    │   BOOKINGS TABLE        │
                                    ├──────────────────────────┤
                                    │ booking_ref             │
                                    │ check_in / check_out    │
                                    │ grand_total             │
                                    │ status                  │
                                    └────────┬─────────────────┘
                                             │
                                             └─→ 1 ┌──────────────────┐
                                                    │ PAYMENTS TABLE  │
                                                    ├──────────────────┤
                                                    │ gateway_ref      │
                                                    │ provider         │
                                                    │ status           │
                                                    └──────────────────┘

PROPERTIES ─M2M─→ AMENITIES
BOOKINGS ─→ REVIEWS (After Checkout)
USERS ─→ MESSAGES (Sender/Receiver)
USERS ─→ AUDIT_LOGS (Action Tracking)
```

---

## 🔧 Service Layer

```
┌────────────────────────────────────────────────────────┐
│              SERVICE LAYER                             │
├────────────────────────────────────────────────────────┤
│                                                         │
│  PaymentGatewayService                                 │
│  ├─ get_available_providers(country)                   │
│  ├─ calculate_fees(base_price, cleaning_fee)           │
│  └─ initiate_payment(booking, provider)                │
│                                                         │
│  EmailService (Celery Tasks)                           │
│  ├─ send_verification_email()                          │
│  ├─ send_booking_confirmation()                        │
│  ├─ send_payment_receipt()                             │
│  └─ send_host_notification()                           │
│                                                         │
│  ImageProcessorService (Celery Tasks)                  │
│  ├─ optimize_image()                                   │
│  └─ generate_thumbnails()                              │
│                                                         │
│  AuditLoggerService                                    │
│  ├─ log_action()                                       │
│  ├─ log_booking_action()                               │
│  └─ log_payment_action()                               │
│                                                         │
└────────────────────────────────────────────────────────┘
```

---

## 🐳 Docker Infrastructure

```
docker-compose up -d

┌──────────────────────────────────────────────────────┐
│                                                       │
│  ┌──────────┐    ┌──────────┐    ┌──────────┐       │
│  │   web    │    │   db     │    │  redis   │       │
│  │ Django   │    │ PostgreSQL   │ Cache    │       │
│  │ 8000:8000│    │ 5432:5432    │ 6379:6379│       │
│  └────┬─────┘    └──────────┘    └────┬─────┘       │
│       │                              │              │
│       │          ┌────────────┐      │              │
│       └─────────→│  celery    │←─────┘              │
│                  │ worker     │                     │
│                  └────────────┘                     │
│                                                       │
│                  ┌────────────┐                      │
│                  │celery-beat │                      │
│                  │ scheduler  │                      │
│                  └────────────┘                      │
│                                                       │
└──────────────────────────────────────────────────────┘
```

---

## 🎯 Role-Based Access Control

```
┌─────────────────────────────────────────────────┐
│            USER ROLES & PERMISSIONS             │
├─────────────────────────────────────────────────┤
│                                                  │
│ GUEST                                            │
│ ├─ Browse properties                            │
│ ├─ Search with filters                          │
│ ├─ Create bookings                              │
│ ├─ Pay for bookings                             │
│ ├─ Leave reviews (after checkout)               │
│ └─ Message hosts                                │
│                                                  │
│ HOST                                             │
│ ├─ Create properties                            │
│ ├─ Edit own properties                          │
│ ├─ View bookings                                │
│ ├─ Receive payments (via system)                │
│ ├─ View reviews/ratings                         │
│ └─ Message guests                               │
│                                                  │
│ ADMIN                                            │
│ ├─ Approve properties                           │
│ ├─ Verify users                                 │
│ ├─ View all bookings                            │
│ ├─ View all payments                            │
│ ├─ Process payouts                              │
│ ├─ Moderate reviews                             │
│ ├─ View audit logs                              │
│ └─ View analytics dashboard                     │
│                                                  │
└─────────────────────────────────────────────────┘
```

---

## 📈 Scaling Architecture

```
Current (Single Server)          Scaled (Production)
┌──────────────────┐            ┌─────────────────────┐
│   Django Web     │            │  Load Balancer      │
│ (1 Instance)     │            └──────────┬──────────┘
└──────────────────┘                       │
                                ┌──────────┴──────────┐
                                │                     │
                          ┌──────▼──────┐      ┌──────▼──────┐
                          │ Django Web  │      │ Django Web  │
                          │ (Instance 1)│      │ (Instance 2)│
                          └─────────────┘      └─────────────┘
                                │                     │
                                └────────┬────────────┘
                                         │
                          ┌──────────────▼────────────────┐
                          │  PostgreSQL (Master)          │
                          │  + Read Replicas (Multiple)   │
                          └────────────────────────────────┘
                          
┌─────────────────────────────────────────────────────────┐
│  Redis Cluster │  Celery Workers │  CDN  │  S3        │
└─────────────────────────────────────────────────────────┘
```

---

## 🎓 Technology Stack Summary

```
┌─────────────────────────────────────────────────────┐
│              BACKEND TECHNOLOGY STACK               │
├─────────────────────────────────────────────────────┤
│                                                      │
│ FRAMEWORK                                            │
│  • Django 5.0 + Django REST Framework               │
│  • API Versioning (/api/v1/)                        │
│  • OpenAPI Documentation (drf-spectacular)          │
│                                                      │
│ DATABASE                                             │
│  • PostgreSQL 15                                    │
│  • PostGIS Extension (Geospatial Queries)           │
│  • Proper Indexing & Relationships                  │
│                                                      │
│ AUTHENTICATION                                       │
│  • JWT (djangorestframework-simplejwt)              │
│  • Custom User Model                                │
│  • Role-Based Access Control                        │
│                                                      │
│ ASYNC PROCESSING                                     │
│  • Celery Task Queue                                │
│  • Redis Broker & Backend                           │
│  • Celery Beat for Scheduling                       │
│                                                      │
│ STORAGE & MEDIA                                      │
│  • AWS S3 via django-storages                       │
│  • Image Optimization (django-imagekit)             │
│  • CDN Integration Ready (CloudFront)               │
│                                                      │
│ SECURITY & MONITORING                                │
│  • Sentry for Error Tracking                        │
│  • CORS & CSRF Protection                           │
│  • Rate Limiting (django-ratelimit)                 │
│  • Security Headers                                 │
│                                                      │
│ CONTAINERIZATION                                     │
│  • Docker & Docker Compose                          │
│  • Multi-Service Setup (6 Services)                 │
│  • Development & Production Ready                   │
│                                                      │
│ ADMIN INTERFACE                                      │
│  • Django Admin                                     │
│  • Django Unfold (Tailwind UI)                      │
│  • Custom Dashboards                                │
│                                                      │
└─────────────────────────────────────────────────────┘
```

---

## ✅ Completion Status

```
Phase 1: Backend Foundation
├─ Project Structure           ✅
├─ Django Setup                ✅
├─ Apps (7 Total)              ✅
├─ Service Layer               ✅
├─ Database Models             ✅
├─ API Endpoints (30+)         ✅
├─ Authentication              ✅
├─ Payment Gateway             ✅
├─ Docker Setup                ✅
├─ Documentation               ✅
└─ Testing Structure           ✅

Overall: 100% COMPLETE ✅

Next Phases Ready to Start:
├─ Phase 2: Frontend (Next.js)
├─ Phase 3: Mobile (React Native/Expo)
└─ Phase 4: Advanced Features
```

---

**Architecture Design Complete! 🎉**  
**Backend Ready for Deployment! 🚀**  
**Date: December 6, 2025**
