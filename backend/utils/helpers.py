"""
Helper functions for StayAfrica backend
"""
from decimal import Decimal
from datetime import datetime, date
import hashlib
import secrets
from django.conf import settings


def calculate_nights(check_in, check_out):
    """Calculate number of nights between two dates"""
    if isinstance(check_in, str):
        check_in = datetime.strptime(check_in, '%Y-%m-%d').date()
    if isinstance(check_out, str):
        check_out = datetime.strptime(check_out, '%Y-%m-%d').date()
    
    return (check_out - check_in).days


def calculate_booking_total(price_per_night, nights, cleaning_fee=0, service_fee=None):
    """Calculate total booking cost"""
    from apps.admin_dashboard.models import SystemConfiguration
    
    config = SystemConfiguration.get_config()
    
    price_per_night = Decimal(str(price_per_night))
    cleaning_fee = Decimal(str(cleaning_fee))
    
    # Use service fee from config if not provided
    if service_fee is None:
        service_fee = config.service_fee
    service_fee = Decimal(str(service_fee))
    
    nightly_total = price_per_night * nights
    commission_rate = Decimal(str(config.commission_rate))
    commission_fee = (nightly_total + service_fee) * commission_rate
    
    grand_total = nightly_total + service_fee + cleaning_fee
    
    return {
        'nightly_total': nightly_total,
        'service_fee': service_fee,
        'cleaning_fee': cleaning_fee,
        'commission_fee': commission_fee,
        'grand_total': grand_total,
        'host_payout': nightly_total + cleaning_fee - commission_fee,
    }


def generate_booking_reference():
    """Generate unique booking reference"""
    import uuid
    return f'BK{uuid.uuid4().hex[:10].upper()}'


def generate_verification_token():
    """Generate secure verification token"""
    return secrets.token_urlsafe(32)


def hash_webhook_signature(payload, secret):
    """Generate webhook signature hash"""
    return hashlib.sha256(f"{payload}{secret}".encode()).hexdigest()


def verify_webhook_signature(payload, signature, secret):
    """Verify webhook signature"""
    expected_signature = hash_webhook_signature(payload, secret)
    return secrets.compare_digest(signature, expected_signature)


def format_currency(amount, currency='USD'):
    """Format currency for display"""
    symbols = {
        'USD': '$',
        'ZWL': 'ZWL',
        'ZAR': 'R',
        'BWP': 'P',
        'NAD': 'N$',
        'ZMW': 'ZK',
    }
    symbol = symbols.get(currency, currency)
    return f"{symbol}{amount:.2f}"


def sanitize_input(text):
    """Sanitize user input to prevent XSS"""
    import html
    if text:
        return html.escape(text.strip())
    return text


def get_client_ip(request):
    """Get client IP address from request"""
    x_forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR')
    if x_forwarded_for:
        ip = x_forwarded_for.split(',')[0]
    else:
        ip = request.META.get('REMOTE_ADDR')
    return ip


def paginate_queryset(queryset, page=1, page_size=20):
    """Helper to paginate querysets"""
    start = (page - 1) * page_size
    end = start + page_size
    return queryset[start:end]


def is_booking_date_available(property_obj, check_in, check_out, exclude_booking_id=None):
    """Check if property is available for given dates"""
    from apps.bookings.models import Booking
    
    # Get all confirmed bookings for this property
    conflicting_bookings = Booking.objects.filter(
        rental_property=property_obj,
        status__in=['confirmed', 'pending']
    ).filter(
        check_in__lt=check_out,
        check_out__gt=check_in
    )
    
    # Exclude current booking if provided (for updates)
    if exclude_booking_id:
        conflicting_bookings = conflicting_bookings.exclude(id=exclude_booking_id)
    
    return not conflicting_bookings.exists()
