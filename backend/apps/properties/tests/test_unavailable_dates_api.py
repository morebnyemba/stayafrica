import pytest
from django.urls import reverse
from rest_framework.test import APIClient


@pytest.mark.django_db
class TestUnavailableDatesAPI:
    def setup_method(self):
        self.client = APIClient()

    def test_unavailable_dates_public_for_active_property(self, property_factory, booking_factory):
        prop = property_factory(status='active')
        booking = booking_factory(rental_property=prop, status='confirmed')

        url = reverse('property-unavailable-dates', args=[prop.id])
        response = self.client.get(url)

        assert response.status_code == 200
        payload = response.json()
        assert payload['property_id'] == prop.id
        assert isinstance(payload['unavailable_dates'], list)
        assert booking.check_in.strftime('%Y-%m-%d') in payload['unavailable_dates']
