"""
Unit tests for User model
"""
import pytest
from django.core.exceptions import ValidationError
from apps.users.models import User


@pytest.mark.django_db
class TestUserModel:
    """Test User model"""
    
    def test_user_creation(self):
        """Test basic user creation"""
        user = User.objects.create_user(
            username='testuser',
            email='test@example.com',
            password='testpass123',
            role='guest'
        )
        assert user.email == 'test@example.com'
        assert user.role == 'guest'
        assert user.is_verified is False
        assert user.check_password('testpass123')
    
    def test_user_email_uniqueness(self):
        """Test that email must be unique"""
        User.objects.create_user(
            username='user1',
            email='test@example.com',
            password='testpass123'
        )
        
        with pytest.raises(Exception):  # Should raise IntegrityError
            User.objects.create_user(
                username='user2',
                email='test@example.com',
                password='testpass123'
            )
    
    def test_user_is_host_property(self):
        """Test is_host property"""
        host = User.objects.create_user(
            username='host',
            email='host@example.com',
            password='testpass123',
            role='host'
        )
        guest = User.objects.create_user(
            username='guest',
            email='guest@example.com',
            password='testpass123',
            role='guest'
        )
        
        assert host.is_host is True
        assert guest.is_host is False
    
    def test_user_is_guest_property(self):
        """Test is_guest property"""
        guest = User.objects.create_user(
            username='guest',
            email='guest@example.com',
            password='testpass123',
            role='guest'
        )
        host = User.objects.create_user(
            username='host',
            email='host@example.com',
            password='testpass123',
            role='host'
        )
        
        assert guest.is_guest is True
        assert host.is_guest is False
    
    def test_user_is_admin_property(self):
        """Test is_admin_user property"""
        admin = User.objects.create_user(
            username='admin',
            email='admin@example.com',
            password='testpass123',
            role='admin'
        )
        staff = User.objects.create_user(
            username='staff',
            email='staff@example.com',
            password='testpass123',
            is_staff=True
        )
        regular = User.objects.create_user(
            username='regular',
            email='regular@example.com',
            password='testpass123'
        )
        
        assert admin.is_admin_user is True
        assert staff.is_admin_user is True
        assert regular.is_admin_user is False
    
    def test_user_str_representation(self):
        """Test string representation of user"""
        user = User.objects.create_user(
            username='testuser',
            email='test@example.com',
            password='testpass123',
            role='guest'
        )
        
        assert 'test@example.com' in str(user)
        assert 'Guest' in str(user)
