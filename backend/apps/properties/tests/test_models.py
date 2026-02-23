"""
Tests for Property, Amenity, PropertyImage, and SavedProperty models.
"""
import pytest
from decimal import Decimal
from apps.properties.models import Property, Amenity, PropertyImage, SavedProperty

try:
    from django.contrib.gis.geos import Point
    HAS_GIS = True
except Exception:
    HAS_GIS = False


@pytest.mark.django_db
class TestAmenityModel:
    """Tests for the Amenity model."""

    def test_create_amenity(self, amenity_factory):
        amenity = amenity_factory(name='WiFi', icon='wifi')
        assert amenity.name == 'WiFi'
        assert amenity.icon == 'wifi'

    def test_str_representation(self, amenity_factory):
        amenity = amenity_factory(name='Pool')
        assert str(amenity) == 'Pool'

    def test_unique_name(self, amenity_factory):
        amenity_factory(name='Parking')
        with pytest.raises(Exception):
            amenity_factory(name='Parking')


@pytest.mark.django_db
class TestPropertyModel:
    """Tests for the Property model."""

    def test_create_property(self, property_factory):
        prop = property_factory(title='Sunset Villa')
        assert prop.title == 'Sunset Villa'
        assert prop.status == 'active'

    def test_auto_generated_id(self, property_factory):
        prop = property_factory()
        assert prop.id is not None
        assert len(prop.id) == 10
        assert prop.id.isdigit()

    def test_unique_id_generation(self, property_factory):
        ids = set()
        for _ in range(5):
            prop = property_factory()
            ids.add(prop.id)
        assert len(ids) == 5  # All unique

    @pytest.mark.skipif(not HAS_GIS, reason='GDAL not available')
    def test_geospatial_location(self, property_factory):
        prop = property_factory()
        assert isinstance(prop.location, Point)
        assert prop.location.x == 31.05  # longitude
        assert prop.location.y == -17.83  # latitude

    def test_property_type_choices(self, property_factory):
        for ptype in ['lodge', 'cottage', 'room', 'apartment', 'house', 'villa']:
            prop = property_factory(property_type=ptype)
            assert prop.property_type == ptype

    def test_status_choices(self, property_factory):
        active = property_factory(status='active')
        inactive = property_factory(status='inactive')
        pending = property_factory(status='pending_approval')
        assert active.is_available is True
        assert inactive.is_available is False
        assert pending.is_available is False

    def test_default_values(self, property_factory):
        prop = property_factory()
        assert prop.currency == 'USD'
        assert prop.max_guests == 2
        assert prop.bedrooms == 1
        assert prop.bathrooms == 1
        assert prop.instant_booking_enabled is False
        assert prop.instant_booking_requirements == {}

    def test_str_representation(self, property_factory):
        prop = property_factory(title='Beach House', city='Durban', country='South Africa')
        assert 'Beach House' in str(prop)
        assert 'Durban' in str(prop)

    def test_amenities_m2m(self, property_factory, amenity_factory):
        prop = property_factory()
        wifi = amenity_factory(name='WiFi')
        pool = amenity_factory(name='Pool')
        prop.amenities.add(wifi, pool)
        assert prop.amenities.count() == 2

    def test_host_relationship(self, property_factory, host_user):
        prop = property_factory(host=host_user)
        assert prop.host == host_user
        assert prop in host_user.properties.all()


@pytest.mark.django_db
class TestPropertyImageModel:
    """Tests for the PropertyImage model."""

    def test_create_image(self, property_factory):
        prop = property_factory()
        img = PropertyImage.objects.create(
            property=prop,
            caption='Front view',
            order=1,
        )
        assert img.caption == 'Front view'
        assert img.order == 1

    def test_ordering(self, property_factory):
        prop = property_factory()
        img2 = PropertyImage.objects.create(property=prop, order=2)
        img1 = PropertyImage.objects.create(property=prop, order=1)
        images = list(PropertyImage.objects.filter(property=prop))
        assert images[0].order <= images[1].order


@pytest.mark.django_db
class TestSavedPropertyModel:
    """Tests for the SavedProperty (wishlist) model."""

    def test_save_property(self, user_factory, property_factory):
        user = user_factory()
        prop = property_factory()
        saved = SavedProperty.objects.create(user=user, property=prop)
        assert saved.user == user
        assert saved.property == prop

    def test_unique_together(self, user_factory, property_factory):
        user = user_factory()
        prop = property_factory()
        SavedProperty.objects.create(user=user, property=prop)
        with pytest.raises(Exception):
            SavedProperty.objects.create(user=user, property=prop)

    def test_str_representation(self, user_factory, property_factory):
        user = user_factory(email='saver@test.com')
        prop = property_factory(title='Dream Home')
        saved = SavedProperty.objects.create(user=user, property=prop)
        assert 'saver@test.com' in str(saved)
        assert 'Dream Home' in str(saved)

    def test_cascade_delete_user(self, user_factory, property_factory):
        user = user_factory()
        prop = property_factory()
        SavedProperty.objects.create(user=user, property=prop)
        user.delete()
        assert SavedProperty.objects.count() == 0
