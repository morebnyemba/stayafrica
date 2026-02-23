"""
Test settings for running pytest on Windows without GDAL/PostGIS.
Uses regular SQLite instead of SpatiaLite.
"""
import os
import sys
from pathlib import Path

# Skip GDAL initialization entirely for tests
os.environ['GDAL_LIBRARY_PATH'] = ''
os.environ['GEOS_LIBRARY_PATH'] = ''

# Monkey-patch django.contrib.gis to prevent GDAL import errors
# This allows tests to run on systems without GDAL installed
import importlib

# Must happen before django.setup()
BASE_DIR = Path(__file__).resolve().parent

# Basic Django settings for testing
SECRET_KEY = 'test-secret-key-do-not-use-in-production'
DEBUG = True

ALLOWED_HOSTS = ['*']

INSTALLED_APPS = [
    # Unfold must be before django.contrib.admin
    'unfold',
    'unfold.contrib.filters',
    'unfold.contrib.import_export',

    # Django apps (using non-GIS versions for tests)
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'django.contrib.sites',

    # Third-party apps
    'rest_framework',
    'corsheaders',
    'django_filters',
    'drf_spectacular',
    'channels',

    # 2FA and Social Auth
    'django_otp',
    'django_otp.plugins.otp_totp',
    'django_otp.plugins.otp_static',
    'allauth',
    'allauth.account',
    'allauth.socialaccount',
    'allauth.socialaccount.providers.google',
    'allauth.socialaccount.providers.facebook',
    'allauth.socialaccount.providers.apple',
    'dj_rest_auth',
    'dj_rest_auth.registration',

    # Local apps
    'apps.users',
    'apps.properties',
    'apps.bookings',
    'apps.payments',
    'apps.reviews',
    'apps.messaging',
    'apps.admin_dashboard',
    'apps.experiences',
    'apps.notifications',
    'apps.health',
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
    'allauth.account.middleware.AccountMiddleware',
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

# Use regular SQLite for tests (no GDAL required)
DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.sqlite3',
        'NAME': ':memory:',
    }
}

AUTH_USER_MODEL = 'users.User'

# DRF Configuration
REST_FRAMEWORK = {
    'DEFAULT_AUTHENTICATION_CLASSES': (
        'rest_framework_simplejwt.authentication.JWTAuthentication',
    ),
    'DEFAULT_PERMISSION_CLASSES': (
        'rest_framework.permissions.IsAuthenticated',
    ),
    'DEFAULT_SCHEMA_CLASS': 'drf_spectacular.openapi.AutoSchema',
    'DEFAULT_PAGINATION_CLASS': 'rest_framework.pagination.PageNumberPagination',
    'PAGE_SIZE': 20,
    'EXCEPTION_HANDLER': 'utils.exception_handlers.custom_exception_handler',
}

# JWT Configuration
from datetime import timedelta
SIMPLE_JWT = {
    'ACCESS_TOKEN_LIFETIME': timedelta(hours=24),
    'REFRESH_TOKEN_LIFETIME': timedelta(days=7),
    'ALGORITHM': 'HS256',
    'SIGNING_KEY': SECRET_KEY,
}

# Channels (use in-memory for tests)
CHANNEL_LAYERS = {
    'default': {
        'BACKEND': 'channels.layers.InMemoryChannelLayer',
    },
}

# Cache (use local memory for tests)
CACHES = {
    'default': {
        'BACKEND': 'django.core.cache.backends.locmem.LocMemCache',
    },
}

# Static files
STATIC_URL = '/static/'
STATIC_ROOT = BASE_DIR / 'staticfiles'
STATICFILES_DIRS = [BASE_DIR / 'static'] if (BASE_DIR / 'static').exists() else []

MEDIA_URL = '/media/'
MEDIA_ROOT = BASE_DIR / 'media'

# Authentication
AUTHENTICATION_BACKENDS = [
    'django.contrib.auth.backends.ModelBackend',
    'allauth.account.auth_backends.AuthenticationBackend',
]

# Allauth
ACCOUNT_EMAIL_REQUIRED = True
ACCOUNT_USERNAME_REQUIRED = False
ACCOUNT_AUTHENTICATION_METHOD = 'email'
SITE_ID = 1

# Application Settings
COMMISSION_RATE = 0.07
SERVICE_FEE = 3.00
DEFAULT_CURRENCY = 'USD'

# Email
EMAIL_BACKEND = 'django.core.mail.backends.locmem.EmailBackend'

# Frontend URL
FRONTEND_URL = 'http://localhost:3000'
SITE_URL = 'http://localhost:3000'

DEFAULT_AUTO_FIELD = 'django.db.models.BigAutoField'

# Rate Limiting
RATELIMIT_ENABLE = False  # Disable for tests

# Logging - minimal for tests
LOGGING = {
    'version': 1,
    'disable_existing_loggers': True,
    'handlers': {
        'null': {
            'class': 'logging.NullHandler',
        },
    },
    'root': {
        'handlers': ['null'],
        'level': 'CRITICAL',
    },
}

# Unfold minimal config for tests
UNFOLD = {
    "SITE_TITLE": "StayAfrica Test",
}
