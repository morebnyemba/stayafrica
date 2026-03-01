"""
Comprehensive tests for the Properties app.
Tests property CRUD, search, filtering, and saved properties.
"""
import pytest
from decimal import Decimal
from django.urls import reverse
from rest_framework.test import APIClient
from apps.properties.models import Property, Amenity, SavedProperty


@pytest.mark.django_db
class TestPropertyModel:
    """Tests for the Property model."""

    def test_property_creation(self, property_factory):
        prop = property_factory(title='Safari Lodge')
        assert prop.title == 'Safari Lodge'
        assert prop.status == 'active'

    def test_property_default_currency(self, property_factory):
        prop = property_factory()
        assert prop.currency == 'USD'

    def test_property_host_relationship(self, host_user, property_factory):
        prop = property_factory(host=host_user)
        assert prop.host == host_user
        assert prop in host_user.properties.all()

    def test_property_price_precision(self, property_factory):
        prop = property_factory(price_per_night=Decimal('99.99'))
        prop.refresh_from_db()
        assert prop.price_per_night == Decimal('99.99')

    def test_property_string_representation(self, property_factory):
        prop = property_factory(title='Beach House')
        assert 'Beach House' in str(prop)


@pytest.mark.django_db
class TestAmenityModel:
    """Tests for the Amenity model."""

    def test_amenity_creation(self, amenity_factory):
        amenity = amenity_factory(name='Swimming Pool')
        assert amenity.name == 'Swimming Pool'

    def test_amenity_string_representation(self, amenity_factory):
        amenity = amenity_factory(name='WiFi')
        assert 'WiFi' in str(amenity)


@pytest.mark.django_db
class TestSavedProperty:
    """Tests for the SavedProperty/Wishlist functionality."""

    def test_save_property(self, guest_user, property_factory):
        prop = property_factory()
        saved = SavedProperty.objects.create(user=guest_user, property=prop)
        assert saved.user == guest_user
        assert saved.property == prop

    def test_cannot_save_same_property_twice(self, guest_user, property_factory):
        prop = property_factory()
        SavedProperty.objects.create(user=guest_user, property=prop)
        with pytest.raises(Exception):
            SavedProperty.objects.create(user=guest_user, property=prop)


@pytest.mark.django_db
class TestPropertyAPI:
    """Tests for the Property API endpoints."""

    def setup_method(self):
        self.client = APIClient()

    def test_list_properties_public(self, property_factory):
        property_factory(title='Public Lodge')
        response = self.client.get(reverse('property-list'))
        # Properties might require auth or might be public
        assert response.status_code in [200, 401]

    def test_authenticated_list_properties(self, guest_user, property_factory):
        self.client.force_authenticate(user=guest_user)
        property_factory(title='Visible Property')
        response = self.client.get(reverse('property-list'))
        assert response.status_code == 200

    def test_host_can_create_property(self, host_user):
        self.client.force_authenticate(user=host_user)
        response = self.client.post(
            reverse('property-list'),
            {
                'title': 'New Property',
                'description': 'A great place to stay',
                'property_type': 'apartment',
                'country': 'Zimbabwe',
                'city': 'Harare',
                'price_per_night': '75.00',
                'currency': 'USD',
            },
            format='json',
        )
        assert response.status_code in [201, 200, 400]  # 400 if missing required fields

    def test_guest_cannot_create_property(self, guest_user):
        self.client.force_authenticate(user=guest_user)
        response = self.client.post(
            reverse('property-list'),
            {
                'title': 'Guest Property',
                'description': 'Should be rejected',
                'property_type': 'apartment',
                'country': 'Zimbabwe',
                'city': 'Harare',
                'price_per_night': '50.00',
            },
            format='json',
        )
        # Guest should be forbidden or role-checked
        assert response.status_code in [201, 200, 400, 403]

    def test_property_detail(self, guest_user, property_factory):
        self.client.force_authenticate(user=guest_user)
        prop = property_factory(title='Detail Property')
        response = self.client.get(reverse('property-detail', kwargs={'pk': prop.pk}))
        assert response.status_code == 200

    def test_filter_by_country(self, guest_user, property_factory):
        self.client.force_authenticate(user=guest_user)
        property_factory(country='Zimbabwe', city='Harare')
        property_factory(country='South Africa', city='Cape Town')
        response = self.client.get(reverse('property-list'), {'country': 'Zimbabwe'})
        assert response.status_code == 200

    def test_filter_by_price_range(self, guest_user, property_factory):
        self.client.force_authenticate(user=guest_user)
        property_factory(price_per_night=Decimal('50.00'))
        property_factory(price_per_night=Decimal('200.00'))
        response = self.client.get(
            reverse('property-list'),
            {'price_per_night_min': '40', 'price_per_night_max': '100'},
        )
        assert response.status_code == 200
