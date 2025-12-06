from django.db import models
from django.contrib.gis.db import models as gis_models
from apps.users.models import User

class Amenity(models.Model):
    name = models.CharField(max_length=100, unique=True)
    icon = models.CharField(max_length=50, blank=True)
    description = models.TextField(blank=True)
    
    class Meta:
        verbose_name_plural = 'Amenities'
    
    def __str__(self):
        return self.name

class Property(models.Model):
    PROPERTY_TYPE_CHOICES = [
        ('lodge', 'Lodge'),
        ('cottage', 'Cottage'),
        ('room', 'Room'),
        ('apartment', 'Apartment'),
        ('house', 'House'),
        ('villa', 'Villa'),
    ]
    
    STATUS_CHOICES = [
        ('active', 'Active'),
        ('inactive', 'Inactive'),
        ('pending_approval', 'Pending Approval'),
    ]
    
    host = models.ForeignKey(User, on_delete=models.CASCADE, related_name='properties')
    title = models.CharField(max_length=255)
    description = models.TextField()
    property_type = models.CharField(max_length=20, choices=PROPERTY_TYPE_CHOICES)
    
    # Geospatial
    location = gis_models.PointField(help_text='Latitude and Longitude')
    country = models.CharField(max_length=100)
    city = models.CharField(max_length=100)
    suburb = models.CharField(max_length=100, blank=True)
    address = models.TextField()
    
    # Pricing
    price_per_night = models.DecimalField(max_digits=10, decimal_places=2)
    currency = models.CharField(max_length=3, default='USD')
    
    # Amenities & Status
    amenities = models.ManyToManyField(Amenity, blank=True)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending_approval')
    
    # Media
    main_image = models.ImageField(upload_to='properties/%Y/%m/%d/')
    
    # Metadata
    max_guests = models.IntegerField(default=2)
    bedrooms = models.IntegerField(default=1)
    bathrooms = models.IntegerField(default=1)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['host']),
            models.Index(fields=['status']),
            models.Index(fields=['country']),
            models.Index(fields=['city']),
        ]
    
    def __str__(self):
        return f'{self.title} - {self.city}, {self.country}'
    
    @property
    def is_available(self):
        return self.status == 'active'

class PropertyImage(models.Model):
    property = models.ForeignKey(Property, on_delete=models.CASCADE, related_name='images')
    image = models.ImageField(upload_to='property_images/%Y/%m/%d/')
    caption = models.CharField(max_length=255, blank=True)
    order = models.IntegerField(default=0)
    created_at = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        ordering = ['order', 'created_at']
    
    def __str__(self):
        return f'{self.property.title} - Image {self.order}'
