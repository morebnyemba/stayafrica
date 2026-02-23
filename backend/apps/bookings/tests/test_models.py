"""
Tests for Booking model.
"""
import pytest
from datetime import date, timedelta
from decimal import Decimal
from apps.bookings.models import Booking


@pytest.mark.django_db
class TestBookingModel:
    """Tests for the Booking model."""

    def test_create_booking(self, booking_factory):
        booking = booking_factory()
        assert booking.status == 'pending'
        assert booking.booking_ref is not None
        assert booking.booking_ref.startswith('BK')

    def test_auto_generated_booking_ref(self, booking_factory):
        booking = booking_factory()
        assert len(booking.booking_ref) == 12  # 'BK' + 10 hex chars

    def test_unique_booking_refs(self, booking_factory):
        refs = set()
        for _ in range(5):
            booking = booking_factory()
            refs.add(booking.booking_ref)
        assert len(refs) == 5

    def test_nights_property(self, booking_factory):
        check_in = date.today() + timedelta(days=10)
        check_out = check_in + timedelta(days=5)
        booking = booking_factory(check_in=check_in, check_out=check_out)
        assert booking.nights == 5

    def test_single_night_booking(self, booking_factory):
        check_in = date.today() + timedelta(days=10)
        check_out = check_in + timedelta(days=1)
        booking = booking_factory(check_in=check_in, check_out=check_out)
        assert booking.nights == 1

    def test_status_choices(self, booking_factory):
        for status_val in ['pending', 'confirmed', 'cancelled', 'completed']:
            booking = booking_factory(status=status_val)
            assert booking.status == status_val

    def test_pricing_fields(self, booking_factory):
        booking = booking_factory()
        assert booking.nightly_total > 0
        assert booking.service_fee == Decimal('3.00')
        assert booking.commission_fee > 0
        assert booking.grand_total > booking.nightly_total

    def test_str_representation(self, booking_factory, guest_user):
        booking = booking_factory(guest=guest_user)
        assert booking.booking_ref in str(booking)
        assert guest_user.email in str(booking)

    def test_currency_default(self, booking_factory):
        booking = booking_factory()
        assert booking.currency == 'USD'

    def test_special_requests(self, booking_factory):
        booking = booking_factory(special_requests='Early check-in please')
        assert booking.special_requests == 'Early check-in please'

    def test_guest_relationship(self, booking_factory, guest_user):
        booking = booking_factory(guest=guest_user)
        assert booking.guest == guest_user
        assert booking in guest_user.bookings.all()

    def test_property_relationship(self, booking_factory, property_factory):
        prop = property_factory()
        booking = booking_factory(rental_property=prop)
        assert booking.rental_property == prop
        assert booking in prop.bookings.all()

    def test_ordering(self, booking_factory):
        b1 = booking_factory()
        b2 = booking_factory()
        bookings = list(Booking.objects.all())
        # Should be ordered by -created_at (newest first)
        assert bookings[0].created_at >= bookings[1].created_at
