"""
Local Points of Interest (POI) Models
Display nearby attractions, restaurants, and amenities
"""
from django.contrib.gis.db import models as gis_models
from django.db import models
from apps.properties.models import Property
import uuid


class POICategory(models.Model):
    """Categories for points of interest"""
    name = models.CharField(max_length=100, unique=True)
    icon = models.CharField(max_length=50, help_text='Icon name for display')
    color = models.CharField(max_length=7, default='#3A5C50', help_text='Hex color code')
    description = models.TextField(blank=True)
    display_order = models.IntegerField(default=0)
    
    class Meta:
        ordering = ['display_order', 'name']
        verbose_name_plural = 'POI Categories'
    
    def __str__(self):
        return self.name


class PointOfInterest(models.Model):
    """Points of interest near properties"""
    POI_TYPE_CHOICES = [
        ('restaurant', 'Restaurant'),
        ('cafe', 'Cafe'),
        ('bar', 'Bar'),
        ('grocery', 'Grocery Store'),
        ('pharmacy', 'Pharmacy'),
        ('hospital', 'Hospital'),
        ('attraction', 'Tourist Attraction'),
        ('museum', 'Museum'),
        ('park', 'Park'),
        ('beach', 'Beach'),
        ('transport', 'Transportation'),
        ('shopping', 'Shopping'),
        ('entertainment', 'Entertainment'),
        ('services', 'Services'),
        ('other', 'Other'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    name = models.CharField(max_length=255)
    category = models.ForeignKey(
        POICategory,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='pois'
    )
    poi_type = models.CharField(max_length=50, choices=POI_TYPE_CHOICES)
    description = models.TextField(blank=True)
    
    # Location
    location = gis_models.PointField(help_text='Latitude and Longitude')
    address = models.TextField(blank=True)
    city = models.CharField(max_length=100)
    country = models.CharField(max_length=100)
    
    # Contact
    phone = models.CharField(max_length=50, blank=True)
    website = models.URLField(blank=True)
    
    # Details
    rating = models.DecimalField(
        max_digits=2,
        decimal_places=1,
        null=True,
        blank=True,
        help_text='Average rating (0-5)'
    )
    review_count = models.IntegerField(default=0)
    price_level = models.IntegerField(
        null=True,
        blank=True,
        help_text='Price level 1-4 ($ to $$$$)'
    )
    
    # Hours
    opening_hours = models.JSONField(
        default=dict,
        blank=True,
        help_text='Opening hours by day of week'
    )
    
    # Images
    image_url = models.URLField(blank=True, help_text='Main image URL')
    
    # Data source
    source = models.CharField(
        max_length=50,
        default='manual',
        help_text='Data source (e.g., google_places, manual)'
    )
    external_id = models.CharField(max_length=255, blank=True, help_text='ID from external source')
    
    # Metadata
    is_active = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-rating', 'name']
        indexes = [
            models.Index(fields=['city', 'poi_type']),
            models.Index(fields=['is_active']),
        ]
    
    def __str__(self):
        return f"{self.name} ({self.get_poi_type_display()})"


class PropertyPOI(models.Model):
    """Relationship between properties and nearby POIs with distance"""
    property = models.ForeignKey(Property, on_delete=models.CASCADE, related_name='nearby_pois')
    poi = models.ForeignKey(PointOfInterest, on_delete=models.CASCADE, related_name='nearby_properties')
    
    distance_meters = models.FloatField(help_text='Distance in meters')
    walking_time_minutes = models.IntegerField(
        null=True,
        blank=True,
        help_text='Estimated walking time'
    )
    driving_time_minutes = models.IntegerField(
        null=True,
        blank=True,
        help_text='Estimated driving time'
    )
    
    # Host annotations
    host_notes = models.TextField(
        blank=True,
        help_text='Host notes about this POI'
    )
    is_recommended = models.BooleanField(
        default=False,
        help_text='Host recommends this place'
    )
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['distance_meters']
        unique_together = ('property', 'poi')
        indexes = [
            models.Index(fields=['property', 'distance_meters']),
            models.Index(fields=['property', 'is_recommended']),
        ]
    
    def __str__(self):
        return f"{self.poi.name} - {self.distance_meters}m from {self.property.title}"
    
    @property
    def distance_km(self):
        """Distance in kilometers"""
        return round(self.distance_meters / 1000, 2)
    
    @property
    def distance_display(self):
        """Human-readable distance"""
        if self.distance_meters < 1000:
            return f"{int(self.distance_meters)}m"
        else:
            return f"{self.distance_km}km"
