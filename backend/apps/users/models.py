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
    profile_picture = models.ImageField(upload_to='profile_pictures/%Y/%m/%d/', null=True, blank=True)
    bio = models.TextField(blank=True, null=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
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
