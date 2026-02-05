# StayAfrica - Production Readiness Improvements Plan

**Date Created:** February 5, 2026  
**Status:** Implementation Required  
**Priority:** Critical for Production Launch

---

## 🎯 Executive Summary

The StayAfrica platform is **70% production-ready**. The core infrastructure is solid with excellent architecture, but critical security and operational features must be implemented before production deployment.

**Key Metrics:**
- ✅ Backend Apps: 10/10 implemented
- ✅ Core Features: 100% complete
- ⚠️ Security: 70% complete (missing 2FA, secrets management)
- ⚠️ Monitoring: 60% complete (Sentry optional)
- ✅ Infrastructure: 95% complete

---

## 🚨 Critical Security Gaps (Must Fix Before Launch)

### 1. Two-Factor Authentication (2FA/MFA) - HIGH PRIORITY ⚠️

**Current State:** Not implemented  
**Required By:** MASTER_PLAN.md Section 1 - "optional 2FA for hosts/admins"  
**Impact:** High - Required for host and admin accounts

**Implementation Steps:**

#### Option A: Django OTP (Recommended)
```bash
# Add to requirements.txt
django-otp==1.5.0
qrcode==7.4.2
```

**Changes Required:**
1. Add to `INSTALLED_APPS`:
   - `django_otp`
   - `django_otp.plugins.otp_totp`
   - `django_otp.plugins.otp_static`

2. Create middleware for 2FA verification
3. Add API endpoints:
   - `/api/v1/auth/2fa/setup/` (GET) - Get QR code
   - `/api/v1/auth/2fa/verify/` (POST) - Verify TOTP token
   - `/api/v1/auth/2fa/enable/` (POST) - Enable 2FA
   - `/api/v1/auth/2fa/disable/` (POST) - Disable 2FA
   - `/api/v1/auth/2fa/backup-codes/` (GET) - Get backup codes

4. Update User model with `two_factor_enabled` boolean field
5. Modify login flow to check for 2FA requirement
6. Create mobile/web UI for 2FA setup and verification

**Timeline:** 8-12 hours  
**Files to Create:**
- `backend/apps/users/two_factor.py` - 2FA service
- `backend/apps/users/serializers_2fa.py` - 2FA serializers
- `backend/apps/users/views_2fa.py` - 2FA views
- `mobile/app/(auth)/two-factor.tsx` - Mobile 2FA screen
- `web/src/app/(auth)/two-factor/page.tsx` - Web 2FA screen

---

### 2. Secure Secret Management - CRITICAL ⚠️

**Current State:** Hardcoded secrets in docker-compose.prod.yml  
**Impact:** CRITICAL - Security breach risk

**Problems:**
```yaml
# docker-compose.prod.yml - EXPOSED SECRETS
SECRET_KEY: change-this-to-a-secure-key-in-production  # ❌ Hardcoded
POSTGRES_PASSWORD: StayAfr1ca$2025!Secure              # ❌ Hardcoded
DB_PASSWORD: StayAfr1ca$2025!Secure                    # ❌ Duplicate
```

**Solution:**

#### Step 1: Generate Secure Secrets
```bash
# Generate Django SECRET_KEY (50+ characters)
python3 -c "from django.core.management.utils import get_random_secret_key; print(get_random_secret_key())"

# Generate database password (32+ characters)
openssl rand -base64 32

# Generate Redis password (32+ characters)
openssl rand -base64 32
```

#### Step 2: Update .env File
Create `/home/runner/work/stayafrica/stayafrica/.env` with:
```bash
# Django
SECRET_KEY=<generated-secret-key>

# Database
DB_PASSWORD=<generated-db-password>
POSTGRES_PASSWORD=<generated-db-password>

# Redis
REDIS_PASSWORD=<generated-redis-password>

# JWT
JWT_SECRET_KEY=<generated-jwt-secret>
```

#### Step 3: Update docker-compose.prod.yml
Replace all hardcoded values with environment variables:
```yaml
environment:
  SECRET_KEY: ${SECRET_KEY}
  DB_PASSWORD: ${DB_PASSWORD}
  POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
  REDIS_PASSWORD: ${REDIS_PASSWORD}
```

**Timeline:** 2 hours  
**Files to Modify:**
- `.env` (create with real secrets)
- `docker-compose.prod.yml` (use env vars)
- `.env.example` (update template)

---

### 3. Mandatory Error Tracking - HIGH PRIORITY ⚠️

**Current State:** Sentry is optional (only loads if sentry-sdk installed)  
**Impact:** High - Production issues may go unnoticed

**Current Code:**
```python
# settings.py
try:
    import sentry_sdk
    HAS_SENTRY = True
except ImportError:
    HAS_SENTRY = False
```

**Solution:**

#### Step 1: Make Sentry Required
```python
# settings.py - UPDATED
import sentry_sdk
from sentry_sdk.integrations.django import DjangoIntegration
from sentry_sdk.integrations.celery import CeleryIntegration
from sentry_sdk.integrations.redis import RedisIntegration

# Sentry configuration
SENTRY_DSN = os.getenv('SENTRY_DSN', '')
if not DEBUG and not SENTRY_DSN:
    raise ValueError("SENTRY_DSN is required in production (DEBUG=False)")

if SENTRY_DSN:
    sentry_sdk.init(
        dsn=SENTRY_DSN,
        integrations=[
            DjangoIntegration(),
            CeleryIntegration(),
            RedisIntegration(),
        ],
        traces_sample_rate=0.1,  # 10% of requests traced
        send_default_pii=False,   # Don't send PII
        environment="production" if not DEBUG else "development",
        release=os.getenv('APP_VERSION', 'unknown'),
    )
```

#### Step 2: Add to .env
```bash
SENTRY_DSN=https://your-sentry-dsn@sentry.io/project-id
APP_VERSION=1.0.0
```

**Timeline:** 1 hour  
**Files to Modify:**
- `backend/stayafrica/settings.py`
- `.env.example`

---

### 4. Webhook Signature Validation - HIGH PRIORITY ⚠️

**Current State:** Payment webhooks don't validate signatures  
**Impact:** High - Fake payment confirmations possible

**Problem:**
```python
# backend/apps/payments/views.py - MISSING VALIDATION
@api_view(['POST'])
def payment_webhook(request):
    # ❌ No signature validation
    data = request.data
    # Process payment...
```

**Solution:**

#### For Stripe:
```python
import stripe
from django.conf import settings

@api_view(['POST'])
def stripe_webhook(request):
    payload = request.body
    sig_header = request.META.get('HTTP_STRIPE_SIGNATURE')
    
    try:
        event = stripe.Webhook.construct_event(
            payload, sig_header, settings.STRIPE_WEBHOOK_SECRET
        )
    except stripe.error.SignatureVerificationError:
        return Response({'error': 'Invalid signature'}, status=400)
    
    # Process verified event...
```

#### For Paynow:
```python
from paynow import Paynow
import hashlib

def verify_paynow_webhook(request):
    # Verify hash
    received_hash = request.data.get('hash')
    values = [str(v) for k, v in sorted(request.data.items()) if k != 'hash']
    calculated_hash = hashlib.sha512(''.join(values).encode()).hexdigest()
    
    if received_hash != calculated_hash:
        raise ValueError("Invalid webhook signature")
```

**Timeline:** 4 hours  
**Files to Modify:**
- `backend/apps/payments/views.py`
- `backend/apps/payments/services.py`

---

### 5. Enhanced Rate Limiting - MEDIUM PRIORITY ⚠️

**Current State:** Rate limiting only on payment endpoints  
**Impact:** Medium - API abuse possible

**Current Limits:**
```python
# Only on payments:
@ratelimit(key='ip', rate='5/m', method='POST')  # Initiate payment
@ratelimit(key='ip', rate='20/m', method='POST')  # Webhook
@ratelimit(key='ip', rate='100/h', method='GET')  # Transaction history
```

**Recommended Additions:**

#### Authentication Endpoints
```python
# apps/users/views.py
from django_ratelimit.decorators import ratelimit

@ratelimit(key='ip', rate='5/m', method='POST')  # Login attempts
@ratelimit(key='ip', rate='3/m', method='POST')  # Registration
@ratelimit(key='ip', rate='3/10m', method='POST')  # Password reset
```

#### Public Endpoints
```python
# Anonymous users
'anon': '100/hour'  # Instead of 100/day

# Authenticated users  
'user': '1000/hour'

# Search endpoints
@ratelimit(key='ip', rate='30/m', method='GET')  # Property search
@ratelimit(key='ip', rate='60/m', method='GET')  # Property details
```

**Timeline:** 3 hours  
**Files to Modify:**
- `backend/apps/users/views.py`
- `backend/apps/properties/views.py`
- `backend/stayafrica/settings.py` (REST_FRAMEWORK throttle rates)

---

### 6. Database Connection Security - MEDIUM PRIORITY ⚠️

**Current State:** SSL mode not enforced  
**Impact:** Medium - Data in transit not encrypted

**Solution:**

#### Update Database Configuration
```python
# settings.py
DATABASES = {
    'default': {
        'ENGINE': os.getenv('DB_ENGINE', 'django.contrib.gis.db.backends.postgis'),
        'NAME': os.getenv('DB_NAME', 'stayafrica_db'),
        'USER': os.getenv('DB_USER', 'postgres'),
        'PASSWORD': os.getenv('DB_PASSWORD', 'postgres'),
        'HOST': os.getenv('DB_HOST', 'db'),
        'PORT': os.getenv('DB_PORT', '5432'),
        'OPTIONS': {
            'sslmode': 'require' if not DEBUG else 'prefer',
            'connect_timeout': 10,
            'options': '-c statement_timeout=30000',  # 30s query timeout
        },
    }
}
```

**Timeline:** 1 hour  
**Files to Modify:**
- `backend/stayafrica/settings.py`

---

### 7. Encryption at Rest - LOW PRIORITY (Post-Launch) 🟢

**Current State:** No field-level encryption  
**Impact:** Low - Sensitive data stored in plaintext

**Recommended:** Use `django-encrypted-model-fields`

```python
from encrypted_model_fields.fields import EncryptedCharField

class User(AbstractUser):
    phone_number = EncryptedCharField(max_length=20)
    tax_id = EncryptedCharField(max_length=50, blank=True)
```

**Timeline:** 8 hours (post-launch)

---

## 📊 Implementation Priority Matrix

| Task | Priority | Impact | Effort | Timeline |
|------|----------|--------|--------|----------|
| 1. Secure Secret Management | CRITICAL | High | Low | 2h |
| 2. Mandatory Error Tracking | HIGH | High | Low | 1h |
| 3. Webhook Signature Validation | HIGH | High | Medium | 4h |
| 4. Enhanced Rate Limiting | MEDIUM | Medium | Medium | 3h |
| 5. Database SSL Mode | MEDIUM | Medium | Low | 1h |
| 6. Two-Factor Authentication | HIGH | High | High | 12h |
| 7. Encryption at Rest | LOW | Low | High | 8h+ |

**Total Critical Path:** ~23 hours of development time

---

## 🛠️ Implementation Checklist

### Phase 1: Critical Security (Must do before launch)
- [ ] Generate and store secure secrets in .env
- [ ] Update docker-compose.prod.yml to use env vars
- [ ] Make Sentry mandatory in production
- [ ] Implement webhook signature validation
- [ ] Test all payment webhooks with signature verification

### Phase 2: Enhanced Security (Do before launch if possible)
- [ ] Add rate limiting to auth endpoints
- [ ] Add rate limiting to public endpoints  
- [ ] Enable database SSL mode
- [ ] Test rate limiting behavior

### Phase 3: 2FA Implementation (Can launch without, add within 2 weeks)
- [ ] Install django-otp
- [ ] Create 2FA models and migrations
- [ ] Create 2FA API endpoints
- [ ] Create mobile 2FA screens
- [ ] Create web 2FA screens
- [ ] Test 2FA flow end-to-end
- [ ] Make 2FA mandatory for admins
- [ ] Make 2FA optional for hosts

### Phase 4: Post-Launch Enhancements
- [ ] Implement field-level encryption
- [ ] Add API key rotation mechanism
- [ ] Implement advanced fraud detection
- [ ] Add IP geolocation blocking
- [ ] Implement device fingerprinting

---

## 📚 Additional Recommendations

### 1. Security Audit
Before launch, conduct:
- [ ] Penetration testing
- [ ] OWASP Top 10 vulnerability scan
- [ ] Dependency vulnerability scan (`safety check`)
- [ ] Code security review (Bandit for Python)

### 2. Performance Optimization
- [ ] Add database connection pooling (PgBouncer)
- [ ] Enable query result caching
- [ ] Add CDN for media files (CloudFront)
- [ ] Implement Redis clustering for high availability

### 3. Backup & Disaster Recovery
- [ ] Automated daily database backups
- [ ] Backup retention policy (30 days)
- [ ] Test restore procedure monthly
- [ ] Document recovery process

### 4. Compliance
- [ ] GDPR compliance review (if serving EU users)
- [ ] PCI DSS compliance (for payment data)
- [ ] Privacy policy updates
- [ ] Terms of service review

---

## 🎯 Success Criteria

The platform will be production-ready when:
- ✅ All secrets stored securely in .env (not hardcoded)
- ✅ Sentry error tracking active and tested
- ✅ All payment webhooks validated with signatures
- ✅ Rate limiting active on all sensitive endpoints
- ✅ Database connections use SSL
- ✅ Security audit completed with no critical issues
- ⏳ 2FA implemented for admins (can be post-launch for hosts)

---

## 📞 Support & Resources

**Security Best Practices:**
- Django Security Documentation: https://docs.djangoproject.com/en/5.0/topics/security/
- OWASP Top 10: https://owasp.org/www-project-top-ten/
- Django-OTP Documentation: https://django-otp-official.readthedocs.io/

**Monitoring:**
- Sentry Django Integration: https://docs.sentry.io/platforms/python/guides/django/
- Prometheus + Grafana: For metrics visualization

**Compliance:**
- PCI DSS Quick Reference: https://www.pcisecuritystandards.org/
- GDPR Compliance Checklist: https://gdpr.eu/checklist/

---

**Last Updated:** February 5, 2026  
**Version:** 1.0  
**Status:** Ready for Implementation
