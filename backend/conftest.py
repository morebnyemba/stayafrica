"""
Shared pytest fixtures for StayAfrica backend tests.
"""
import pytest
from decimal import Decimal

# Gracefully handle GDAL not being available (e.g., Windows without OSGeo)
try:
    from django.contrib.gis.geos import Point
    HAS_GIS = True
except Exception:
    HAS_GIS = False
    Point = None


@pytest.fixture
def user_factory(db):
    """Factory for creating User instances."""
    from apps.users.models import User

    def _create_user(
        email=None,
        password='testpass123',
        role='guest',
        country='Zimbabwe',
        is_verified=False,
        **kwargs,
    ):
        from uuid import uuid4
        if email is None:
            email = f'user-{uuid4().hex[:8]}@test.com'
        user = User.objects.create_user(
            username=email.split('@')[0],
            email=email,
            password=password,
            role=role,
            country_of_residence=country,
            is_verified=is_verified,
            **kwargs,
        )
        return user

    return _create_user


@pytest.fixture
def guest_user(user_factory):
    """A default guest user."""
    return user_factory(email='guest@test.com', role='guest')


@pytest.fixture
def host_user(user_factory):
    """A verified host user."""
    return user_factory(email='host@test.com', role='host', is_verified=True)


@pytest.fixture
def admin_user(user_factory):
    """An admin user."""
    return user_factory(email='admin@test.com', role='admin', is_staff=True)


@pytest.fixture
def amenity_factory(db):
    """Factory for creating Amenity instances."""
    from apps.properties.models import Amenity

    def _create_amenity(name=None, icon='wifi'):
        from uuid import uuid4
        if name is None:
            name = f'Amenity-{uuid4().hex[:6]}'
        return Amenity.objects.create(name=name, icon=icon)

    return _create_amenity


@pytest.fixture
def property_factory(db, host_user):
    """Factory for creating Property instances."""
    from apps.properties.models import Property

    def _create_property(
        host=None,
        title='Test Property',
        price_per_night=Decimal('50.00'),
        country='Zimbabwe',
        city='Harare',
        status='active',
        **kwargs,
    ):
        prop = Property(
            host=host or host_user,
            title=title,
            description='A test property description.',
            property_type='apartment',
            country=country,
            city=city,
            price_per_night=price_per_night,
            currency='USD',
            status=status,
            **kwargs,
        )
        # Only set location if GIS is available
        if HAS_GIS and Point is not None:
            prop.location = Point(31.05, -17.83)  # Harare coordinates
        prop.save()
        return prop

    return _create_property


@pytest.fixture
def booking_factory(db, guest_user, property_factory):
    """Factory for creating Booking instances."""
    from apps.bookings.models import Booking
    from datetime import date, timedelta

    def _create_booking(
        guest=None,
        rental_property=None,
        check_in=None,
        check_out=None,
        status='pending',
        **kwargs,
    ):
        if check_in is None:
            check_in = date.today() + timedelta(days=7)
        if check_out is None:
            check_out = check_in + timedelta(days=3)
        if rental_property is None:
            rental_property = property_factory()

        nights = (check_out - check_in).days
        nightly_total = rental_property.price_per_night * nights
        service_fee = Decimal('3.00')
        commission_fee = (nightly_total + service_fee) * Decimal('0.07')
        grand_total = nightly_total + service_fee + commission_fee

        booking = Booking.objects.create(
            guest=guest or guest_user,
            rental_property=rental_property,
            check_in=check_in,
            check_out=check_out,
            nightly_total=nightly_total,
            service_fee=service_fee,
            commission_fee=commission_fee,
            grand_total=grand_total,
            status=status,
            **(kwargs),
        )
        return booking

    return _create_booking
