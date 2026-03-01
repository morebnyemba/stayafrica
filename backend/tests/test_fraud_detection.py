"""
Tests for the Fraud Detection Service.
"""
import pytest
from decimal import Decimal
from datetime import date, timedelta
from django.utils import timezone
from services.fraud_detection_service import FraudDetectionService, RISK_LOW, RISK_MEDIUM


@pytest.mark.django_db
class TestFraudDetectionService:
    """Tests for fraud detection risk assessment."""

    def setup_method(self):
        self.service = FraudDetectionService()

    def test_low_risk_normal_booking(self, booking_factory):
        booking = booking_factory()
        result = self.service.assess_booking_risk(booking)
        assert result['risk_score'] >= 0
        assert result['risk_level'] in ['low', 'medium', 'high', 'critical']
        assert result['action'] in ['allow', 'monitor', 'manual_review', 'block']
        assert 'signals' in result

    def test_new_account_high_value_flag(self, user_factory, property_factory, booking_factory):
        new_user = user_factory(email='new@test.com')
        prop = property_factory(price_per_night=Decimal('200.00'))
        check_in = date.today() + timedelta(days=7)
        check_out = check_in + timedelta(days=5)
        booking = booking_factory(
            guest=new_user,
            rental_property=prop,
            check_in=check_in,
            check_out=check_out,
        )
        result = self.service.assess_booking_risk(booking)
        # New account + high value should produce signals
        assert result['risk_score'] > 0

    def test_unverified_account_flag(self, user_factory, property_factory, booking_factory):
        unverified = user_factory(email='unverified@test.com', is_verified=False)
        prop = property_factory(price_per_night=Decimal('100.00'))
        check_in = date.today() + timedelta(days=7)
        check_out = check_in + timedelta(days=5)
        booking = booking_factory(
            guest=unverified,
            rental_property=prop,
            check_in=check_in,
            check_out=check_out,
        )
        result = self.service.assess_booking_risk(booking)
        has_account_signal = any(
            s['category'] == 'account' for s in result['signals']
        )
        assert has_account_signal

    def test_long_stay_flagged(self, booking_factory):
        check_in = date.today() + timedelta(days=7)
        check_out = check_in + timedelta(days=45)
        booking = booking_factory(check_in=check_in, check_out=check_out)
        result = self.service.assess_booking_risk(booking)
        has_pattern_signal = any(
            'long stay' in s['reason'].lower() for s in result['signals']
        )
        assert has_pattern_signal

    def test_zero_value_booking_critical(self, guest_user, property_factory):
        from apps.bookings.models import Booking
        prop = property_factory()
        booking = Booking.objects.create(
            guest=guest_user,
            rental_property=prop,
            check_in=date.today() + timedelta(days=7),
            check_out=date.today() + timedelta(days=10),
            nightly_total=Decimal('0'),
            service_fee=Decimal('0'),
            commission_fee=Decimal('0'),
            grand_total=Decimal('0'),
        )
        result = self.service.assess_booking_risk(booking)
        assert result['risk_score'] >= 50

    def test_risk_score_capped_at_100(self, booking_factory):
        booking = booking_factory()
        result = self.service.assess_booking_risk(booking)
        assert result['risk_score'] <= 100

    def test_result_contains_required_fields(self, booking_factory):
        booking = booking_factory()
        result = self.service.assess_booking_risk(booking)
        assert 'risk_score' in result
        assert 'risk_level' in result
        assert 'action' in result
        assert 'signals' in result
        assert 'assessed_at' in result
