import os
from pathlib import Path
from django.templatetags.static import static
from dotenv import load_dotenv

# Set GDAL library path for GeoDjango
GDAL_LIBRARY_PATH = r"C:\Users\Administrator\AppData\Local\Programs\Python\Python313\Lib\site-packages\osgeo\gdal.dll"

# Set GEOS library path for GeoDjango
GEOS_LIBRARY_PATH = r"C:\Users\Administrator\AppData\Local\Programs\Python\Python313\Lib\site-packages\osgeo\geos_c.dll"

# Optional: Sentry error tracking (install sentry-sdk separately)
try:
    import sentry_sdk
    from sentry_sdk.integrations.django import DjangoIntegration
    HAS_SENTRY = True
except ImportError:
    HAS_SENTRY = False

load_dotenv()

# Build paths
BASE_DIR = Path(__file__).resolve().parent.parent

# Security
SECRET_KEY = os.getenv('SECRET_KEY', 'django-insecure-dev-key-change-in-production')
DEBUG = os.getenv('DEBUG', 'True') == 'True'
ALLOWED_HOSTS = os.getenv('ALLOWED_HOSTS', 'localhost,127.0.0.1').split(',')

# Application definition
INSTALLED_APPS = [
    # Unfold must be before django.contrib.admin
    'unfold',
    'unfold.contrib.filters',
    'unfold.contrib.import_export',
    
    # Django apps
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'django.contrib.gis',
    
    # Third-party apps
    'rest_framework',
    'corsheaders',
    'django_filters',
    'drf_spectacular',
    'django_celery_beat',
    'django_celery_results',
    
    # Local apps
    'apps.users',
    'apps.properties',
    'apps.bookings',
    'apps.payments',
    'apps.reviews',
    'apps.messaging',
    'apps.admin_dashboard',
]

MIDDLEWARE = [
    'django.middleware.security.SecurityMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'corsheaders.middleware.CorsMiddleware',
    'django.middleware.common.CommonMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    'django.middleware.clickjacking.XFrameOptionsMiddleware',
]

ROOT_URLCONF = 'stayafrica.urls'

TEMPLATES = [
    {
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
        'DIRS': [],
        'APP_DIRS': True,
        'OPTIONS': {
            'context_processors': [
                'django.template.context_processors.debug',
                'django.template.context_processors.request',
                'django.contrib.auth.context_processors.auth',
                'django.contrib.messages.context_processors.messages',
            ],
        },
    },
]

WSGI_APPLICATION = 'stayafrica.wsgi.application'

# Database
# Use SQLite for development, PostgreSQL for production
if DEBUG:
    # Development: SQLite
    DATABASES = {
        'default': {
            'ENGINE': 'django.contrib.gis.db.backends.spatialite',
            'NAME': BASE_DIR / 'db.sqlite3',
        }
    }
else:
    # Production: PostgreSQL with GIS support
    DATABASES = {
        'default': {
            'ENGINE': os.getenv('DATABASE_ENGINE', 'django.contrib.gis.db.backends.postgis'),
            'NAME': os.getenv('DATABASE_NAME', 'stayafrica_db'),
            'USER': os.getenv('DATABASE_USER', 'postgres'),
            'PASSWORD': os.getenv('DATABASE_PASSWORD', 'postgres'),
            'HOST': os.getenv('DATABASE_HOST', 'localhost'),
            'PORT': os.getenv('DATABASE_PORT', '5432'),
        }
    }

# Password validation
AUTH_PASSWORD_VALIDATORS = [
    {'NAME': 'django.contrib.auth.password_validation.UserAttributeSimilarityValidator'},
    {'NAME': 'django.contrib.auth.password_validation.MinimumLengthValidator'},
    {'NAME': 'django.contrib.auth.password_validation.CommonPasswordValidator'},
    {'NAME': 'django.contrib.auth.password_validation.NumericPasswordValidator'},
]

# Internationalization
LANGUAGE_CODE = 'en-us'
TIME_ZONE = 'UTC'
USE_I18N = True
USE_TZ = True

# Static files
STATIC_URL = '/static/'
STATIC_ROOT = BASE_DIR / 'staticfiles'
STATICFILES_DIRS = [BASE_DIR / 'static']

# Media files
MEDIA_URL = '/media/'
MEDIA_ROOT = BASE_DIR / 'media'

# AWS S3 Configuration (Optional: install django-storages and boto3 separately)
USE_S3 = os.getenv('USE_S3', 'False') == 'True'
if USE_S3:
    try:
        AWS_ACCESS_KEY_ID = os.getenv('AWS_ACCESS_KEY_ID')
        AWS_SECRET_ACCESS_KEY = os.getenv('AWS_SECRET_ACCESS_KEY')
        AWS_STORAGE_BUCKET_NAME = os.getenv('AWS_STORAGE_BUCKET_NAME')
        AWS_S3_REGION_NAME = os.getenv('AWS_S3_REGION_NAME', 'us-east-1')
        AWS_S3_CUSTOM_DOMAIN = f'{AWS_STORAGE_BUCKET_NAME}.s3.amazonaws.com'
        STATIC_URL = f'https://{AWS_S3_CUSTOM_DOMAIN}/static/'
        MEDIA_URL = f'https://{AWS_S3_CUSTOM_DOMAIN}/media/'
        STORAGES = {
            'default': 'storages.backends.s3boto3.S3Boto3Storage',
            'staticfiles': 'storages.backends.s3boto3.S3StaticStorage',
        }
    except Exception as e:
        print(f'Warning: S3 configuration error. Using local storage. Error: {e}')

# Custom User Model
AUTH_USER_MODEL = 'users.User'

# DRF Configuration
REST_FRAMEWORK = {
    'DEFAULT_AUTHENTICATION_CLASSES': (
        'rest_framework_simplejwt.authentication.JWTAuthentication',
    ),
    'DEFAULT_PERMISSION_CLASSES': (
        'rest_framework.permissions.IsAuthenticated',
    ),
    'DEFAULT_FILTER_BACKENDS': (
        'django_filters.rest_framework.DjangoFilterBackend',
        'rest_framework.filters.SearchFilter',
        'rest_framework.filters.OrderingFilter',
    ),
    'DEFAULT_SCHEMA_CLASS': 'drf_spectacular.openapi.AutoSchema',
    'DEFAULT_PAGINATION_CLASS': 'rest_framework.pagination.PageNumberPagination',
    'PAGE_SIZE': 20,
}

# JWT Configuration
from datetime import timedelta
SIMPLE_JWT = {
    'ACCESS_TOKEN_LIFETIME': timedelta(hours=int(os.getenv('JWT_EXPIRATION_HOURS', '24'))),
    'REFRESH_TOKEN_LIFETIME': timedelta(days=int(os.getenv('JWT_REFRESH_EXPIRATION_DAYS', '7'))),
    'ALGORITHM': os.getenv('JWT_ALGORITHM', 'HS256'),
    'SIGNING_KEY': os.getenv('JWT_SECRET_KEY', SECRET_KEY),
}

# CORS Configuration
CORS_ALLOWED_ORIGINS = [
    'http://localhost:3000',
    'http://localhost:3001',
    'http://127.0.0.1:3000',
]

# Celery Configuration
CELERY_BROKER_URL = os.getenv('CELERY_BROKER_URL', 'redis://localhost:6379/0')
CELERY_RESULT_BACKEND = os.getenv('CELERY_RESULT_BACKEND', 'redis://localhost:6379/0')
CELERY_ACCEPT_CONTENT = ['json']
CELERY_TASK_SERIALIZER = 'json'
CELERY_RESULT_SERIALIZER = 'json'
CELERY_TIMEZONE = 'UTC'
CELERY_TASK_TRACK_STARTED = True
CELERY_TASK_TIME_LIMIT = 30 * 60  # 30 minutes

# Celery Beat Configuration
CELERY_BEAT_SCHEDULE = {
    'send-pending-emails': {
        'task': 'apps.tasks.email_tasks.send_pending_emails',
        'schedule': timedelta(minutes=5),
    },
}

# Sentry Configuration (Optional)
SENTRY_DSN = os.getenv('SENTRY_DSN')
if SENTRY_DSN and HAS_SENTRY:
    try:
        sentry_sdk.init(
            dsn=SENTRY_DSN,
            integrations=[DjangoIntegration()],
            traces_sample_rate=0.1,
            send_default_pii=False,
        )
    except Exception as e:
        print(f'Warning: Sentry initialization failed. Error: {e}')

# ImageKit Configuration (Optional: install pillow and django-imagekit separately)
try:
    IMAGEKIT_DEFAULT_CACHEFILE_DIR = 'CACHE/images'
    IMAGEKIT_SPEC_CACHEFILE_DIR = 'CACHE/images/%s/%s'
    IMAGEKIT_PILLOW_DEFAULT_OPTIONS = {'quality': 92}
except Exception as e:
    print(f'Warning: ImageKit not available. Install pillow and django-imagekit. Error: {e}')

# Application Settings
COMMISSION_RATE = float(os.getenv('COMMISSION_RATE', '0.07'))
SERVICE_FEE = float(os.getenv('SERVICE_FEE', '3.00'))
DEFAULT_CURRENCY = os.getenv('DEFAULT_CURRENCY', 'USD')

# Email Configuration
EMAIL_BACKEND = os.getenv('EMAIL_BACKEND', 'django.core.mail.backends.console.EmailBackend')
EMAIL_HOST = os.getenv('EMAIL_HOST', 'smtp.gmail.com')
EMAIL_PORT = int(os.getenv('EMAIL_PORT', '587'))
EMAIL_USE_TLS = os.getenv('EMAIL_USE_TLS', 'True') == 'True'
EMAIL_HOST_USER = os.getenv('EMAIL_HOST_USER', 'noreply@stayafrica.com')
EMAIL_HOST_PASSWORD = os.getenv('EMAIL_HOST_PASSWORD', '')
DEFAULT_FROM_EMAIL = os.getenv('DEFAULT_FROM_EMAIL', 'StayAfrica <noreply@stayafrica.com>')

# Frontend URL for emails
FRONTEND_URL = os.getenv('FRONTEND_URL', 'http://localhost:3000')

# Create logs directory if it doesn't exist
import os as log_os
log_os.makedirs(BASE_DIR / 'logs', exist_ok=True)

# Logging
LOGGING = {
    'version': 1,
    'disable_existing_loggers': False,
    'formatters': {
        'verbose': {
            'format': '{levelname} {asctime} {module} {process:d} {thread:d} {message}',
            'style': '{',
        },
        'simple': {
            'format': '{levelname} {asctime} {message}',
            'style': '{',
        },
    },
    'handlers': {
        'console': {
            'class': 'logging.StreamHandler',
            'formatter': 'verbose',
        },
        'file': {
            'class': 'logging.handlers.RotatingFileHandler',
            'filename': BASE_DIR / 'logs/stayafrica.log',
            'formatter': 'simple',
            'maxBytes': 1024 * 1024 * 10,  # 10MB
            'backupCount': 5,
        },
    },
    'root': {
        'handlers': ['console', 'file'],
        'level': 'INFO',
    },
    'loggers': {
        'django': {
            'handlers': ['console', 'file'],
            'level': 'INFO',
            'propagate': False,
        },
        'apps': {
            'handlers': ['console', 'file'],
            'level': 'INFO',
            'propagate': False,
        },
        'services': {
            'handlers': ['console', 'file'],
            'level': 'INFO',
            'propagate': False,
        },
        'tasks': {
            'handlers': ['console', 'file'],
            'level': 'INFO',
            'propagate': False,
        },
    },
}

# Cache Configuration
CACHES = {
    'default': {
        'BACKEND': 'django.core.cache.backends.redis.RedisCache',
        'LOCATION': os.getenv('REDIS_URL', 'redis://localhost:6379/1'),
        'OPTIONS': {
            'CLIENT_CLASS': 'django_redis.client.DefaultClient',
        },
        'KEY_PREFIX': 'stayafrica',
        'TIMEOUT': 300,  # 5 minutes default
    }
}

# Rate Limiting
RATELIMIT_ENABLE = os.getenv('RATELIMIT_ENABLE', 'True') == 'True'
RATELIMIT_USE_CACHE = 'default'

# Default primary key field type
DEFAULT_AUTO_FIELD = 'django.db.models.BigAutoField'

# Django Unfold Configuration - StayAfrica Brand Colors
UNFOLD = {
    "SITE_TITLE": "StayAfrica Admin",
    "SITE_HEADER": "StayAfrica Administration",
    "SITE_URL": "/",
    "SITE_ICON": None,  # Add your logo path here
    "SITE_LOGO": None,  # Add your logo path here
    "SITE_SYMBOL": "travel_explore",  # Material icon
    "SHOW_HISTORY": True,
    "SHOW_VIEW_ON_SITE": True,
    "STYLES": [],
    "COLORS": {
        "primary": {
            "50": "250 248 245",   # Off-white/Ivory Sand - very light
            "100": "245 239 229",  # Light Ivory Sand
            "200": "235 215 185",  # Light Safari Gold
            "300": "225 190 145",  # Medium Safari Gold #D9B168
            "400": "210 170 100",  # Deep Safari Gold
            "500": "58 92 80",     # Moss Green #3A5C50 (brand primary)
            "600": "40 65 55",     # Dark Moss Green
            "700": "25 50 40",     # Darker Moss Green
            "800": "18 47 38",     # Deep Forest #122F26 (brand dark)
            "900": "10 26 21",     # Savanna Text #0A1A15 (brand darkest)
            "950": "5 15 12",      # Almost Black
        },
    },
    "EXTENSIONS": {
        "modeltranslation": {
            "flags": {
                "en": "🇬🇧",
                "fr": "🇫🇷",
                "sw": "🇹🇿",
            },
        },
    },
    "SIDEBAR": {
        "show_search": True,
        "show_all_applications": True,
        "navigation": [
            {
                "title": "Dashboard",
                "separator": False,
                "items": [
                    {
                        "title": "Overview",
                        "icon": "dashboard",
                        "link": "/admin/",
                    },
                ],
            },
            {
                "title": "User Management",
                "separator": True,
                "collapsible": True,
                "items": [
                    {
                        "title": "Users",
                        "icon": "people",
                        "link": "/admin/users/user/",
                    },
                ],
            },
            {
                "title": "Property Management",
                "separator": True,
                "collapsible": True,
                "items": [
                    {
                        "title": "Properties",
                        "icon": "home",
                        "link": "/admin/properties/property/",
                    },
                    {
                        "title": "Amenities",
                        "icon": "star",
                        "link": "/admin/properties/amenity/",
                    },
                    {
                        "title": "Property Images",
                        "icon": "photo_library",
                        "link": "/admin/properties/propertyimage/",
                    },
                ],
            },
            {
                "title": "Bookings & Payments",
                "separator": True,
                "collapsible": True,
                "items": [
                    {
                        "title": "Bookings",
                        "icon": "calendar_today",
                        "link": "/admin/bookings/booking/",
                    },
                    {
                        "title": "Payments",
                        "icon": "payments",
                        "link": "/admin/payments/payment/",
                    },
                ],
            },
            {
                "title": "Communication",
                "separator": True,
                "collapsible": True,
                "items": [
                    {
                        "title": "Messages",
                        "icon": "mail",
                        "link": "/admin/messaging/message/",
                    },
                    {
                        "title": "Reviews",
                        "icon": "rate_review",
                        "link": "/admin/reviews/review/",
                    },
                ],
            },
            {
                "title": "System",
                "separator": True,
                "collapsible": True,
                "items": [
                    {
                        "title": "Configuration",
                        "icon": "settings",
                        "link": "/admin/admin_dashboard/systemconfiguration/",
                    },
                    {
                        "title": "Audit Logs",
                        "icon": "history",
                        "link": "/admin/admin_dashboard/auditlog/",
                    },
                    {
                        "title": "Statistics",
                        "icon": "analytics",
                        "link": "/admin/admin_dashboard/adminstats/",
                    },
                ],
            },
        ],
    },
    "TABS": [
        {
            "models": [
                "users.user",
            ],
            "items": [
                {
                    "title": "User Details",
                    "link": "/admin/users/user/{id}/change/",
                    "permission": lambda request: True,
                },
                {
                    "title": "Bookings",
                    "link": "/admin/bookings/booking/?guest__id__exact={id}",
                    "permission": lambda request: True,
                },
                {
                    "title": "Reviews",
                    "link": "/admin/reviews/review/?guest__id__exact={id}",
                    "permission": lambda request: True,
                },
            ],
        },
        {
            "models": [
                "properties.property",
            ],
            "items": [
                {
                    "title": "Property Details",
                    "link": "/admin/properties/property/{id}/change/",
                    "permission": lambda request: True,
                },
                {
                    "title": "Images",
                    "link": "/admin/properties/propertyimage/?property__id__exact={id}",
                    "permission": lambda request: True,
                },
                {
                    "title": "Bookings",
                    "link": "/admin/bookings/booking/?rental_property__id__exact={id}",
                    "permission": lambda request: True,
                },
            ],
        },
    ],
}
