"""
Tests for User, UserPreference, and UserPropertyInteraction models.
"""
import pytest
from apps.users.models import User, UserPreference, UserPropertyInteraction


@pytest.mark.django_db
class TestUserModel:
    """Tests for the custom User model."""

    def test_create_guest_user(self, user_factory):
        user = user_factory(email='guest@example.com', role='guest')
        assert user.email == 'guest@example.com'
        assert user.role == 'guest'
        assert user.is_verified is False

    def test_create_host_user(self, user_factory):
        user = user_factory(email='host@example.com', role='host', is_verified=True)
        assert user.role == 'host'
        assert user.is_verified is True

    def test_create_admin_user(self, user_factory):
        user = user_factory(email='admin@example.com', role='admin', is_staff=True)
        assert user.role == 'admin'
        assert user.is_staff is True

    def test_email_is_username_field(self):
        assert User.USERNAME_FIELD == 'email'

    def test_is_host_property(self, user_factory):
        host = user_factory(role='host')
        guest = user_factory(role='guest')
        assert host.is_host is True
        assert guest.is_host is False

    def test_is_guest_property(self, user_factory):
        guest = user_factory(role='guest')
        host = user_factory(role='host')
        assert guest.is_guest is True
        assert host.is_guest is False

    def test_is_admin_user_property(self, user_factory):
        admin = user_factory(role='admin')
        staff = user_factory(role='guest', is_staff=True)
        guest = user_factory(role='guest')
        assert admin.is_admin_user is True
        assert staff.is_admin_user is True
        assert guest.is_admin_user is False

    def test_str_representation(self, user_factory):
        user = user_factory(email='test@example.com', role='guest')
        assert 'test@example.com' in str(user)
        assert 'Guest' in str(user)

    def test_email_uniqueness(self, user_factory):
        user_factory(email='unique@example.com')
        with pytest.raises(Exception):
            user_factory(email='unique@example.com')

    def test_two_factor_fields_defaults(self, user_factory):
        user = user_factory()
        assert user.two_factor_enabled is False
        assert user.two_factor_secret is None
        assert user.backup_codes == []

    def test_social_auth_fields(self, user_factory):
        user = user_factory()
        assert user.google_id is None
        assert user.facebook_id is None
        assert user.apple_id is None

    def test_online_status_defaults(self, user_factory):
        user = user_factory()
        assert user.is_online is False
        assert user.last_seen is None


@pytest.mark.django_db
class TestUserPreferenceModel:
    """Tests for UserPreference model."""

    def test_create_preferences(self, user_factory):
        user = user_factory()
        prefs = UserPreference.objects.create(user=user)
        assert prefs.preferred_property_types == []
        assert prefs.preferred_min_price is None
        assert prefs.usual_guest_count == 1

    def test_preference_defaults(self, user_factory):
        user = user_factory()
        prefs = UserPreference.objects.create(user=user)
        assert prefs.preferred_countries == []
        assert prefs.preferred_cities == []
        assert prefs.preferred_amenities == []

    def test_one_to_one_relationship(self, user_factory):
        user = user_factory()
        UserPreference.objects.create(user=user)
        with pytest.raises(Exception):
            UserPreference.objects.create(user=user)

    def test_str_representation(self, user_factory):
        user = user_factory(email='pref@test.com')
        prefs = UserPreference.objects.create(user=user)
        assert 'pref@test.com' in str(prefs)


@pytest.mark.django_db
class TestUserPropertyInteractionModel:
    """Tests for UserPropertyInteraction model."""

    def test_create_view_interaction(self, user_factory):
        user = user_factory()
        interaction = UserPropertyInteraction.objects.create(
            user=user,
            property_id='1234567890',
            interaction_type='view',
        )
        assert interaction.interaction_type == 'view'
        assert interaction.property_id == '1234567890'

    def test_create_booking_interaction(self, user_factory):
        user = user_factory()
        interaction = UserPropertyInteraction.objects.create(
            user=user,
            property_id='1234567890',
            interaction_type='book',
        )
        assert interaction.interaction_type == 'book'

    def test_optional_fields(self, user_factory):
        user = user_factory()
        interaction = UserPropertyInteraction.objects.create(
            user=user,
            property_id='1234567890',
            interaction_type='search',
            search_query='Harare villa',
            viewed_duration_seconds=45,
        )
        assert interaction.search_query == 'Harare villa'
        assert interaction.viewed_duration_seconds == 45

    def test_str_representation(self, user_factory):
        user = user_factory(email='interact@test.com')
        interaction = UserPropertyInteraction.objects.create(
            user=user,
            property_id='1234567890',
            interaction_type='view',
        )
        assert 'interact@test.com' in str(interaction)
