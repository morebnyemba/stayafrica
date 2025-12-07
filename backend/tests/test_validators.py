"""
Unit tests for custom validators
"""
import pytest
from datetime import date, timedelta
from django.core.exceptions import ValidationError
from utils.validators import (
    validate_future_date,
    validate_booking_dates,
    validate_price,
    validate_rating,
    validate_max_guests
)


class TestValidators:
    """Test custom validators"""
    
    def test_validate_future_date_valid(self):
        """Test future date validation with valid date"""
        future_date = date.today() + timedelta(days=1)
        # Should not raise exception
        validate_future_date(future_date)
    
    def test_validate_future_date_invalid(self):
        """Test future date validation with past date"""
        past_date = date.today() - timedelta(days=1)
        with pytest.raises(ValidationError):
            validate_future_date(past_date)
    
    def test_validate_booking_dates_valid(self):
        """Test booking dates validation with valid dates"""
        check_in = date.today() + timedelta(days=1)
        check_out = date.today() + timedelta(days=3)
        assert validate_booking_dates(check_in, check_out) is True
    
    def test_validate_booking_dates_past_checkin(self):
        """Test booking dates validation with past check-in"""
        check_in = date.today() - timedelta(days=1)
        check_out = date.today() + timedelta(days=2)
        with pytest.raises(ValidationError, match="cannot be in the past"):
            validate_booking_dates(check_in, check_out)
    
    def test_validate_booking_dates_checkout_before_checkin(self):
        """Test booking dates validation with checkout before check-in"""
        check_in = date.today() + timedelta(days=3)
        check_out = date.today() + timedelta(days=1)
        with pytest.raises(ValidationError, match="after check-in"):
            validate_booking_dates(check_in, check_out)
    
    def test_validate_booking_dates_too_far_advance(self):
        """Test booking dates validation with booking too far in advance"""
        check_in = date.today() + timedelta(days=400)
        check_out = date.today() + timedelta(days=402)
        with pytest.raises(ValidationError, match="365 days in advance"):
            validate_booking_dates(check_in, check_out)
    
    def test_validate_booking_dates_too_long_stay(self):
        """Test booking dates validation with stay too long"""
        check_in = date.today() + timedelta(days=1)
        check_out = date.today() + timedelta(days=100)
        with pytest.raises(ValidationError, match="90 days"):
            validate_booking_dates(check_in, check_out)
    
    def test_validate_price_valid(self):
        """Test price validation with valid price"""
        validate_price(100.50)  # Should not raise
        validate_price(1)  # Should not raise
    
    def test_validate_price_invalid(self):
        """Test price validation with invalid price"""
        with pytest.raises(ValidationError, match="greater than zero"):
            validate_price(0)
        with pytest.raises(ValidationError, match="greater than zero"):
            validate_price(-10)
    
    def test_validate_rating_valid(self):
        """Test rating validation with valid ratings"""
        for rating in range(1, 6):
            validate_rating(rating)  # Should not raise
    
    def test_validate_rating_invalid(self):
        """Test rating validation with invalid ratings"""
        with pytest.raises(ValidationError, match="between 1 and 5"):
            validate_rating(0)
        with pytest.raises(ValidationError, match="between 1 and 5"):
            validate_rating(6)
    
    def test_validate_max_guests_valid(self):
        """Test max guests validation with valid values"""
        validate_max_guests(1)
        validate_max_guests(10)
        validate_max_guests(50)
    
    def test_validate_max_guests_invalid(self):
        """Test max guests validation with invalid values"""
        with pytest.raises(ValidationError, match="at least 1"):
            validate_max_guests(0)
        with pytest.raises(ValidationError, match="cannot exceed 50"):
            validate_max_guests(51)
