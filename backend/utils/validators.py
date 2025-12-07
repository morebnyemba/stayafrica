"""
Custom validators for StayAfrica backend
"""
import re
from datetime import date, timedelta
from django.core.exceptions import ValidationError
from django.utils import timezone


def validate_future_date(value):
    """Validate that a date is in the future"""
    if value < date.today():
        raise ValidationError("Date must be in the future")


def validate_checkout_after_checkin(check_in, check_out):
    """Validate that checkout date is after check-in date"""
    if check_out <= check_in:
        raise ValidationError("Check-out date must be after check-in date")


def validate_booking_dates(check_in, check_out):
    """Validate booking dates are valid"""
    from apps.admin_dashboard.models import SystemConfiguration
    
    today = date.today()
    config = SystemConfiguration.get_config()
    max_advance_days = config.max_advance_booking_days
    max_stay_days = config.max_stay_duration_days
    
    # Check if check-in is not in the past
    if check_in < today:
        raise ValidationError("Check-in date cannot be in the past")
    
    # Check if check-out is after check-in
    if check_out <= check_in:
        raise ValidationError("Check-out date must be after check-in date")
    
    # Check max advance booking
    if (check_in - today).days > max_advance_days:
        raise ValidationError(f"Cannot book more than {max_advance_days} days in advance")
    
    # Check max stay duration
    if (check_out - check_in).days > max_stay_days:
        raise ValidationError(f"Maximum stay duration is {max_stay_days} days")
    
    return True


def validate_phone_number(value):
    """Validate phone number format"""
    pattern = r'^\+?1?\d{9,15}$'
    if not re.match(pattern, value):
        raise ValidationError("Phone number must be between 9 and 15 digits")


def validate_price(value):
    """Validate that price is positive"""
    if value <= 0:
        raise ValidationError("Price must be greater than zero")


def validate_rating(value):
    """Validate rating is between 1 and 5"""
    if not 1 <= value <= 5:
        raise ValidationError("Rating must be between 1 and 5")


def validate_max_guests(value):
    """Validate max guests is reasonable"""
    if value < 1:
        raise ValidationError("Maximum guests must be at least 1")
    if value > 50:
        raise ValidationError("Maximum guests cannot exceed 50")
