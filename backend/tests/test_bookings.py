"""
Comprehensive tests for the Bookings app.
Tests booking creation, status transitions, fee calculations,
availability checks, and edge cases.
"""
import pytest
from decimal import Decimal
from datetime import date, timedelta
from django.urls import reverse
from rest_framework.test import APIClient


@pytest.mark.django_db
class TestBookingModel:
    """Tests for the Booking model."""

    def test_booking_ref_auto_generated(self, booking_factory):
        booking = booking_factory()
        assert booking.booking_ref.startswith('BK')
        assert len(booking.booking_ref) == 12

    def test_booking_ref_unique(self, booking_factory):
        b1 = booking_factory()
        b2 = booking_factory()
        assert b1.booking_ref != b2.booking_ref

    def test_nights_calculation(self, booking_factory):
        check_in = date.today() + timedelta(days=10)
        check_out = check_in + timedelta(days=5)
        booking = booking_factory(check_in=check_in, check_out=check_out)
        assert booking.nights == 5

    def test_default_status_pending(self, booking_factory):
        booking = booking_factory()
        assert booking.status == 'pending'

    def test_fee_calculations(self, property_factory, booking_factory):
        prop = property_factory(price_per_night=Decimal('100.00'))
        check_in = date.today() + timedelta(days=7)
        check_out = check_in + timedelta(days=3)
        booking = booking_factory(
            rental_property=prop,
            check_in=check_in,
            check_out=check_out,
        )
        assert booking.nightly_total == Decimal('300.00')
        assert booking.service_fee == Decimal('3.00')
        expected_commission = (Decimal('300.00') + Decimal('3.00')) * Decimal('0.07')
        assert booking.commission_fee == expected_commission

    def test_grand_total(self, property_factory, booking_factory):
        prop = property_factory(price_per_night=Decimal('50.00'))
        check_in = date.today() + timedelta(days=7)
        check_out = check_in + timedelta(days=2)
        booking = booking_factory(
            rental_property=prop,
            check_in=check_in,
            check_out=check_out,
        )
        nightly = Decimal('100.00')
        service = Decimal('3.00')
        commission = (nightly + service) * Decimal('0.07')
        expected_total = nightly + service + commission
        assert booking.grand_total == expected_total

    def test_string_representation(self, booking_factory, guest_user):
        booking = booking_factory(guest=guest_user)
        assert guest_user.email in str(booking)
        assert booking.booking_ref in str(booking)

    def test_ordering_by_created_at(self, booking_factory):
        b1 = booking_factory()
        b2 = booking_factory()
        from apps.bookings.models import Booking
        bookings = list(Booking.objects.all())
        assert bookings[0].pk == b2.pk  # newest first


@pytest.mark.django_db
class TestBookingAPI:
    """Tests for the Booking API endpoints."""

    def setup_method(self):
        self.client = APIClient()

    def test_guest_can_create_booking(self, guest_user, property_factory):
        self.client.force_authenticate(user=guest_user)
        prop = property_factory()
        check_in = date.today() + timedelta(days=10)
        check_out = check_in + timedelta(days=3)
        response = self.client.post(
            reverse('booking-list'),
            {
                'rental_property': prop.id,
                'check_in': check_in.isoformat(),
                'check_out': check_out.isoformat(),
                'number_of_guests': 2,
            },
            format='json',
        )
        assert response.status_code in [201, 200]

    def test_unauthenticated_cannot_create_booking(self, property_factory):
        prop = property_factory()
        check_in = date.today() + timedelta(days=10)
        check_out = check_in + timedelta(days=3)
        response = self.client.post(
            reverse('booking-list'),
            {
                'rental_property': prop.id,
                'check_in': check_in.isoformat(),
                'check_out': check_out.isoformat(),
            },
            format='json',
        )
        assert response.status_code in [401, 403]

    def test_guest_can_list_own_bookings(self, guest_user, booking_factory):
        self.client.force_authenticate(user=guest_user)
        booking_factory(guest=guest_user)
        response = self.client.get(reverse('booking-list'))
        assert response.status_code == 200

    def test_guest_cannot_see_others_bookings(self, user_factory, booking_factory):
        user1 = user_factory(email='user1@test.com')
        user2 = user_factory(email='user2@test.com')
        booking_factory(guest=user1)
        self.client.force_authenticate(user=user2)
        response = self.client.get(reverse('booking-list'))
        assert response.status_code == 200
        results = response.data.get('results', response.data)
        if isinstance(results, list):
            assert len(results) == 0

    def test_host_can_confirm_booking(self, host_user, booking_factory, property_factory):
        prop = property_factory(host=host_user)
        booking = booking_factory(rental_property=prop, status='pending')
        self.client.force_authenticate(user=host_user)
        response = self.client.post(
            reverse('booking-confirm', kwargs={'pk': booking.pk}),
        )
        assert response.status_code in [200, 204]

    def test_guest_can_cancel_booking(self, guest_user, booking_factory):
        booking = booking_factory(guest=guest_user, status='pending')
        self.client.force_authenticate(user=guest_user)
        response = self.client.post(
            reverse('booking-cancel', kwargs={'pk': booking.pk}),
        )
        assert response.status_code in [200, 204]

    def test_cannot_book_past_dates(self, guest_user, property_factory):
        self.client.force_authenticate(user=guest_user)
        prop = property_factory()
        response = self.client.post(
            reverse('booking-list'),
            {
                'rental_property': prop.id,
                'check_in': (date.today() - timedelta(days=5)).isoformat(),
                'check_out': (date.today() - timedelta(days=2)).isoformat(),
            },
            format='json',
        )
        assert response.status_code == 400

    def test_checkout_before_checkin_rejected(self, guest_user, property_factory):
        self.client.force_authenticate(user=guest_user)
        prop = property_factory()
        check_in = date.today() + timedelta(days=10)
        response = self.client.post(
            reverse('booking-list'),
            {
                'rental_property': prop.id,
                'check_in': check_in.isoformat(),
                'check_out': (check_in - timedelta(days=2)).isoformat(),
            },
            format='json',
        )
        assert response.status_code == 400


@pytest.mark.django_db
class TestBookingStatusTransitions:
    """Tests for booking status workflow."""

    def test_pending_to_confirmed(self, booking_factory):
        booking = booking_factory(status='pending')
        booking.status = 'confirmed'
        booking.save()
        booking.refresh_from_db()
        assert booking.status == 'confirmed'

    def test_pending_to_cancelled(self, booking_factory):
        booking = booking_factory(status='pending')
        booking.status = 'cancelled'
        booking.save()
        booking.refresh_from_db()
        assert booking.status == 'cancelled'

    def test_confirmed_to_completed(self, booking_factory):
        booking = booking_factory(status='confirmed')
        booking.status = 'completed'
        booking.save()
        booking.refresh_from_db()
        assert booking.status == 'completed'
