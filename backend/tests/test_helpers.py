"""
Unit tests for helper functions
"""
import pytest
from datetime import date, timedelta
from decimal import Decimal
from utils.helpers import (
    calculate_nights,
    calculate_booking_total,
    generate_booking_reference,
    format_currency,
    sanitize_input
)


class TestHelpers:
    """Test helper functions"""
    
    def test_calculate_nights(self):
        """Test nights calculation"""
        check_in = date(2024, 1, 1)
        check_out = date(2024, 1, 5)
        nights = calculate_nights(check_in, check_out)
        assert nights == 4
    
    def test_calculate_nights_with_strings(self):
        """Test nights calculation with string dates"""
        nights = calculate_nights('2024-01-01', '2024-01-05')
        assert nights == 4
    
    def test_calculate_booking_total(self):
        """Test booking total calculation"""
        totals = calculate_booking_total(
            price_per_night=100,
            nights=3,
            cleaning_fee=20,
            service_fee=3
        )
        
        assert totals['nightly_total'] == Decimal('300')
        assert totals['service_fee'] == Decimal('3')
        assert totals['cleaning_fee'] == Decimal('20')
        assert totals['grand_total'] == Decimal('323')
        # Commission = (300 + 3) * 0.07 = 21.21
        assert totals['commission_fee'] == Decimal('21.21')
        # Host payout = 300 + 20 - 21.21 = 298.79
        assert totals['host_payout'] == Decimal('298.79')
    
    def test_generate_booking_reference(self):
        """Test booking reference generation"""
        ref1 = generate_booking_reference()
        ref2 = generate_booking_reference()
        
        assert ref1.startswith('BK')
        assert ref2.startswith('BK')
        assert len(ref1) == 12  # BK + 10 chars
        assert ref1 != ref2  # Should be unique
    
    def test_format_currency_usd(self):
        """Test currency formatting for USD"""
        formatted = format_currency(100.50, 'USD')
        assert formatted == '$100.50'
    
    def test_format_currency_zar(self):
        """Test currency formatting for ZAR"""
        formatted = format_currency(100.50, 'ZAR')
        assert formatted == 'R100.50'
    
    def test_sanitize_input(self):
        """Test input sanitization"""
        # Test XSS prevention
        malicious = '<script>alert("xss")</script>'
        sanitized = sanitize_input(malicious)
        assert '<script>' not in sanitized
        assert '&lt;script&gt;' in sanitized
        
        # Test normal text
        normal = 'This is normal text'
        assert sanitize_input(normal) == normal
        
        # Test None
        assert sanitize_input(None) is None
        
        # Test stripping
        assert sanitize_input('  text  ') == 'text'
