# Project: StayAfrica (MVP)
**Tagline:** Stay Anywhere. Africa Awaits.
**Target Markets:** Zimbabwe, South Africa, Botswana, Namibia, Zambia.
**Platform:** Mobile App (iOS/Android) + Web Platform (Guest & Admin).

---

## 1. 🏗️ Enhanced Architecture Overview

- **Monorepo:** Organize backend, web, and mobile apps in a single repository for shared logic and streamlined CI/CD.
- **API-First:** All clients interact via a versioned REST API (OpenAPI via drf-spectacular).
- **Async Tasks:** Use Celery (with Redis) for background jobs (emails, notifications, image processing).
- **Observability:** Integrate Sentry for error tracking and Prometheus/Grafana for metrics.
- **Security:** JWT auth, HTTPS everywhere, role-based access control, optional 2FA for hosts/admins.
- **Service Layer:** Centralize business logic in a `services/` directory for maintainability.
- **Audit Trail:** Add a generic `AuditLog` model for tracking changes/actions.
- **Rate Limiting:** Use `django-ratelimit` for sensitive API endpoints.
- **Frontend Enhancements:** SSR/SSG and PWA for Next.js, internationalization, offline support for mobile, push notifications.
- **Media Delivery:** Use presigned URLs and CDN (CloudFront) for secure, fast media access.

## 1. 🛠 Technical Stack (Strict)

---

## 2. 🛠 Technical Stack

- **Backend:** Python Django 5.0+, Django REST Framework (DRF).
- **Database:** PostgreSQL (Production) with **PostGIS** extension (Required for "Drop Pin" & Radius Search).
- **Mobile App:** React Native (Expo) + TypeScript.
- **Web Frontend:** Next.js (App Router) + Tailwind CSS + Shadcn/UI.
- **Admin Panel:** Django Unfold (Tailwind-based Admin).
- **DevOps:** Docker, Docker Compose, Gunicorn, Nginx.
- **Media:** AWS S3 (via `django-storages`) + `django-imagekit` (Image optimization).

---

## 📦 Additional Python Packages

- celery
- django-ratelimit
- sentry-sdk
- cryptography
- django-celery-beat
- django-celery-results

## 2. 🗺️ Database Schema & Data Models

---

## 3. 🗺️ Database Schema & Data Models

**A. Users (`users` app)**
* `email` (Unique, Username), `phone_number`, `password_hash`.
* `role`: Enum [`guest`, `host`, `admin`].
* `country_of_residence`.
* `is_verified`: Boolean.

**B. Properties (`properties` app)**
* `host` (FK -> User).
* `title`, `description`, `property_type` (Lodge, Cottage, Room, etc).
* `location`: **PointField** (Latitude/Longitude).
* `country`, `city`, `suburb`.
* `price_per_night` (Decimal), `currency` (Default USD).
* `amenities`: M2M (WiFi, Pool, etc).
* `status`: Enum [`active`, `inactive`, `pending_approval`].

**C. Bookings & Financials (`bookings` app)**
* `booking_ref`: Unique ID.
* `guest` (FK), `property` (FK).
* `dates`: `check_in`, `check_out`.
* `pricing`:
    * `nightly_total`: (Price * Nights).
    * `service_fee`: (Fixed fee e.g. $3).
    * `commission_fee`: (7% deducted from Host payout).
    * `cleaning_fee`: (Optional).
    * `grand_total`: (Total Guest Pays).
* `status`: [`pending`, `confirmed`, `cancelled`, `completed`].

**D. Payments (`payments` app)**
* `booking` (FK).
* `provider`: [`paynow`, `payfast`, `stripe`, `cash_on_arrival`].
* `gateway_ref`: Transaction ID from provider.
* `status`: [`initiated`, `success`, `failed`].

## 3. 🧠 Business Logic & Rules (Crucial)

---

## 4. 🧠 Business Logic & Rules

**A. The "Payment Switch" (Regional Logic)**
* *Backend Logic:* When a booking is initiated, check `User.billing_country`:
    * **Zimbabwe:** Present `Paynow` (Ecocash/Visa) or `Cash on Arrival`.
    * **South Africa:** Present `PayFast` or `Ozow`.
    * **International:** Present `Stripe`.

**B. Commission Model**
* StayAfrica takes **7%** of the `base_price` + `service_fee`.
* Host receives: `(base_price + cleaning_fee) - 7%`.

**C. Host Restrictions**
* Host cannot accept bookings until `is_verified` = True (Admin approval).
* Property location must be stored as Geospatial data (PostGIS) to enable "Search Nearby" features.

## 4. 🚀 Implementation Roadmap (8-Week Plan)

---

## 5. 🚀 Implementation Roadmap (8-Week Plan)

**Phase 1: Foundation & Backend Core (Weeks 1-2)**
* **Goal:** Dockerized Django Environment with PostGIS.
* **Tasks:**
    1.  Setup `docker-compose.yml` (Django, Postgres/PostGIS, Redis).
    2.  Initialize Django with `users` app (Custom User Model).
    3.  Install `django-unfold` and configure the Admin Dashboard.
    4.  Configure `django-storages` for S3 (Image uploads).
     5.  Set up Celery for async tasks (emails, image processing).
     6.  Add Sentry for error tracking and Prometheus/Grafana for metrics.
     7.  Implement AuditLog model and basic logging.

**Phase 2: Property & Guest API (Weeks 3-4)**
* **Goal:** Search & Property Details working.
* **Tasks:**
    1.  Create `properties` CRUD API.
    2.  Implement `django-filter` for Country, City, Price, Guests.
    3.  **Next.js Web:** Build Landing Page + Search Results Grid.
    4.  **React Native:** Build "Explore" Tab + "Property Details" Screen (Carousel + Map).
     5.  Add SSR/SSG and PWA support to Next.js.
     6.  Add internationalization to web app.
     7.  Implement offline support and push notifications for mobile app.

**Phase 3: Booking & Payments Engine (Weeks 5-6)**
* **Goal:** Users can book and pay based on their region.
* **Tasks:**
    1.  Implement `Booking` logic (Availability check + Cost Calculation).
    2.  Integrate **Paynow** (Zim) and **PayFast** (SA) Python SDKs.
    3.  **React Native:** Build "Booking Summary" & Payment WebView.
    4.  **Host Dashboard:** Build "My Listings" & "Incoming Requests" (Accept/Decline).
     5.  Implement PaymentGateway service class and webhook endpoints.
     6.  Add rate limiting to booking/payment endpoints.
     7.  Add basic fraud detection logic.

**Phase 4: Social & Admin Polish (Weeks 7-8)**
* **Goal:** Messaging, Reviews, and Admin control.
* **Tasks:**
    1.  **Messaging:** Simple API-based Chat (`sender`, `receiver`, `text`).
    2.  **Reviews:** Logic to allow reviews *only* after `checkout_date`.
    3.  **Admin:** Stats Dashboard (Total Revenue, Active Hosts) using Django Unfold.
     4.  Implement role-based dashboards and bulk actions in admin panel.
     5.  Add analytics tracking for key events.
     6.  Finalize observability and monitoring setup.

---

## 📦 Required Python Packages (`requirements.txt`)

---

## 6. 📦 Required & Recommended Python Packages

### requirements.txt
Django>=5.0
djangorestframework
psycopg2-binary
gunicorn
uvicorn
python-dotenv
django-cors-headers
django-filter
django-storages
boto3
Pillow
django-imagekit
django-unfold  # For the Admin UI
djangorestframework-simplejwt
drf-spectacular # For API Documentation

### Additional Recommended Packages
celery
django-ratelimit
sentry-sdk
cryptography
django-celery-beat
django-celery-results