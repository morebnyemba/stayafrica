from django.db import models
from django.contrib.auth.models import AbstractUser
from django.core.validators import RegexValidator

class User(AbstractUser):
    ROLE_CHOICES = [
        ('guest', 'Guest'),
        ('host', 'Host'),
        ('admin', 'Admin'),
    ]
    
    email = models.EmailField(unique=True)
    phone_number = models.CharField(
        max_length=20,
        validators=[
            RegexValidator(
                regex=r'^\+?1?\d{9,15}$',
                message='Phone number must be between 9 and 15 digits.'
            )
        ],
        blank=True,
        null=True
    )
    role = models.CharField(max_length=10, choices=ROLE_CHOICES, default='guest')
    country_of_residence = models.CharField(max_length=100, blank=True, null=True)
    is_verified = models.BooleanField(default=False)
    
    # Two-Factor Authentication fields
    two_factor_enabled = models.BooleanField(default=False)
    two_factor_secret = models.CharField(max_length=32, blank=True, null=True)
    backup_codes = models.JSONField(default=list, blank=True)
    
    # Social Authentication fields
    google_id = models.CharField(max_length=255, blank=True, null=True, unique=True)
    facebook_id = models.CharField(max_length=255, blank=True, null=True, unique=True)
    apple_id = models.CharField(max_length=255, blank=True, null=True, unique=True)
    profile_picture = models.ImageField(upload_to='profile_pictures/%Y/%m/%d/', null=True, blank=True)
    bio = models.TextField(blank=True, null=True)
    is_online = models.BooleanField(default=False)
    last_seen = models.DateTimeField(null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    USERNAME_FIELD = 'email'
    REQUIRED_FIELDS = ['username']  # Required for createsuperuser, not for regular registration
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['email']),
            models.Index(fields=['role']),
            models.Index(fields=['country_of_residence']),
        ]
    
    def __str__(self):
        return f'{self.email} ({self.get_role_display()})'
    
    @property
    def is_host(self):
        return self.role == 'host'
    
    @property
    def is_guest(self):
        return self.role == 'guest'
    
    @property
    def is_admin_user(self):
        return self.role == 'admin' or self.is_staff


class UserPreference(models.Model):
    """Store user preferences for personalized recommendations"""
    user = models.OneToOneField(User, on_delete=models.CASCADE, related_name='preferences')
    
    # Property type preferences
    preferred_property_types = models.JSONField(default=list, blank=True, help_text='List of preferred property types')
    
    # Price range preferences
    preferred_min_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    preferred_max_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    
    # Location preferences
    preferred_countries = models.JSONField(default=list, blank=True, help_text='List of preferred countries')
    preferred_cities = models.JSONField(default=list, blank=True, help_text='List of preferred cities')
    
    # Guest preferences
    usual_guest_count = models.IntegerField(default=1)
    
    # Amenity preferences
    preferred_amenities = models.JSONField(default=list, blank=True, help_text='List of preferred amenity IDs')
    
    # Last known location for nearby recommendations
    last_latitude = models.FloatField(null=True, blank=True)
    last_longitude = models.FloatField(null=True, blank=True)
    
    # Tracking
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        indexes = [
            models.Index(fields=['user']),
        ]
    
    def __str__(self):
        return f'Preferences for {self.user.email}'


class UserPropertyInteraction(models.Model):
    """Track user interactions with properties for better recommendations"""
    INTERACTION_TYPES = [
        ('view', 'Viewed'),
        ('save', 'Saved'),
        ('unsave', 'Unsaved'),
        ('book', 'Booked'),
        ('search', 'Searched'),
    ]
    
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name='property_interactions')
    property_id = models.CharField(max_length=10, db_index=True, help_text='Property ID')
    interaction_type = models.CharField(max_length=20, choices=INTERACTION_TYPES)
    
    # Additional context
    search_query = models.CharField(max_length=255, blank=True, null=True)
    viewed_duration_seconds = models.IntegerField(null=True, blank=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['user', 'interaction_type']),
            models.Index(fields=['property_id']),
            models.Index(fields=['created_at']),
        ]
    
    def __str__(self):
        return f'{self.user.email} - {self.get_interaction_type_display()} - Property {self.property_id}'

