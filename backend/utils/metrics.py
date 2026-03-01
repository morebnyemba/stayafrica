"""
Prometheus metrics configuration for StayAfrica.

Provides application-level metrics for monitoring via Prometheus/Grafana.
Uses django-prometheus for automatic Django metrics + custom business metrics.
"""
import time
import logging
from functools import wraps
from django.conf import settings

logger = logging.getLogger(__name__)

# Try to import prometheus_client; gracefully degrade if not installed
try:
    from prometheus_client import (
        Counter, Histogram, Gauge, Info,
        generate_latest, CONTENT_TYPE_LATEST,
    )
    HAS_PROMETHEUS = True
except ImportError:
    HAS_PROMETHEUS = False
    logger.info("prometheus_client not installed; metrics disabled")


if HAS_PROMETHEUS:
    # --- Business Metrics ---

    BOOKINGS_CREATED = Counter(
        'stayafrica_bookings_created_total',
        'Total bookings created',
        ['country', 'status'],
    )

    BOOKINGS_COMPLETED = Counter(
        'stayafrica_bookings_completed_total',
        'Total bookings completed',
        ['country'],
    )

    PAYMENTS_PROCESSED = Counter(
        'stayafrica_payments_processed_total',
        'Total payments processed',
        ['provider', 'status'],
    )

    PAYMENT_AMOUNT = Histogram(
        'stayafrica_payment_amount_usd',
        'Payment amounts in USD',
        buckets=[10, 25, 50, 100, 250, 500, 1000, 2500, 5000],
    )

    ACTIVE_USERS = Gauge(
        'stayafrica_active_users',
        'Number of active users',
        ['role'],
    )

    ACTIVE_PROPERTIES = Gauge(
        'stayafrica_active_properties',
        'Number of active property listings',
        ['country'],
    )

    FRAUD_CHECKS = Counter(
        'stayafrica_fraud_checks_total',
        'Total fraud checks performed',
        ['risk_level', 'action'],
    )

    API_REQUEST_DURATION = Histogram(
        'stayafrica_api_request_duration_seconds',
        'API request duration in seconds',
        ['method', 'endpoint', 'status'],
        buckets=[0.01, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0],
    )

    APP_INFO = Info(
        'stayafrica_app',
        'StayAfrica application information',
    )
    APP_INFO.info({
        'version': '1.0.0',
        'environment': 'production' if not getattr(settings, 'DEBUG', True) else 'development',
    })


def track_booking_created(country='unknown', status='pending'):
    """Track a booking creation event."""
    if HAS_PROMETHEUS:
        BOOKINGS_CREATED.labels(country=country, status=status).inc()


def track_booking_completed(country='unknown'):
    """Track a booking completion event."""
    if HAS_PROMETHEUS:
        BOOKINGS_COMPLETED.labels(country=country).inc()


def track_payment(provider, status, amount=None):
    """Track a payment event."""
    if HAS_PROMETHEUS:
        PAYMENTS_PROCESSED.labels(provider=provider, status=status).inc()
        if amount is not None:
            PAYMENT_AMOUNT.observe(float(amount))


def track_fraud_check(risk_level, action):
    """Track a fraud check result."""
    if HAS_PROMETHEUS:
        FRAUD_CHECKS.labels(risk_level=risk_level, action=action).inc()


def update_active_users(role, count):
    """Update the active users gauge."""
    if HAS_PROMETHEUS:
        ACTIVE_USERS.labels(role=role).set(count)


def update_active_properties(country, count):
    """Update active properties gauge."""
    if HAS_PROMETHEUS:
        ACTIVE_PROPERTIES.labels(country=country).set(count)


def get_metrics():
    """Generate Prometheus metrics output."""
    if HAS_PROMETHEUS:
        return generate_latest(), CONTENT_TYPE_LATEST
    return b'# Prometheus client not installed\n', 'text/plain'
