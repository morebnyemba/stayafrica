from django.db import models
from django.contrib.gis.db import models as gis_models
from apps.users.models import User
import random


class ExperienceCategory(models.Model):
    """Categories for experiences like Adventure, Cultural, Wildlife, etc."""
    name = models.CharField(max_length=100, unique=True)
    description = models.TextField(blank=True)
    icon = models.CharField(max_length=50, blank=True)
    
    class Meta:
        verbose_name_plural = 'Experience Categories'
        ordering = ['name']
    
    def __str__(self):
        return self.name


class Experience(models.Model):
    """Experiences and activities that guests can book"""
    DURATION_CHOICES = [
        ('half_day', 'Half Day (2-4 hours)'),
        ('full_day', 'Full Day (5-8 hours)'),
        ('multi_day', 'Multi-Day'),
        ('hourly', 'Hourly'),
    ]
    
    DIFFICULTY_CHOICES = [
        ('easy', 'Easy'),
        ('moderate', 'Moderate'),
        ('challenging', 'Challenging'),
        ('expert', 'Expert'),
    ]
    
    STATUS_CHOICES = [
        ('active', 'Active'),
        ('inactive', 'Inactive'),
        ('pending_approval', 'Pending Approval'),
    ]
    
    # Basic info
    id = models.CharField(max_length=10, primary_key=True, editable=False)
    host = models.ForeignKey(User, on_delete=models.CASCADE, related_name='experiences')
    title = models.CharField(max_length=255)
    description = models.TextField()
    category = models.ForeignKey(ExperienceCategory, on_delete=models.SET_NULL, null=True, related_name='experiences')
    
    # Location
    location = gis_models.PointField(help_text='Latitude and Longitude')
    country = models.CharField(max_length=100)
    city = models.CharField(max_length=100)
    address = models.TextField(blank=True)
    
    # Pricing and details
    price_per_person = models.DecimalField(max_digits=10, decimal_places=2)
    currency = models.CharField(max_length=3, default='USD')
    duration = models.CharField(max_length=20, choices=DURATION_CHOICES)
    duration_hours = models.DecimalField(max_digits=5, decimal_places=2, help_text='Duration in hours')
    difficulty = models.CharField(max_length=20, choices=DIFFICULTY_CHOICES, default='easy')
    
    # Capacity
    min_participants = models.IntegerField(default=1)
    max_participants = models.IntegerField(default=10)
    
    # Additional info
    included_items = models.TextField(blank=True, help_text='What is included (one per line)')
    requirements = models.TextField(blank=True, help_text='Requirements for participants')
    cancellation_policy = models.TextField(blank=True)
    
    # Media
    main_image = models.ImageField(upload_to='experiences/%Y/%m/%d/', blank=True, null=True)
    
    # Status
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending_approval')
    
    # Metadata
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['host']),
            models.Index(fields=['status']),
            models.Index(fields=['country']),
            models.Index(fields=['city']),
            models.Index(fields=['category']),
        ]
    
    def __str__(self):
        return f'{self.title} - {self.city}, {self.country}'
    
    def save(self, *args, **kwargs):
        if not self.id:
            # Generate 10-digit unique ID
            while True:
                new_id = ''.join([str(random.randint(0, 9)) for _ in range(10)])
                if not Experience.objects.filter(id=new_id).exists():
                    self.id = new_id
                    break
        super().save(*args, **kwargs)
    
    @property
    def is_available(self):
        return self.status == 'active'


class ExperienceImage(models.Model):
    """Additional images for experiences"""
    experience = models.ForeignKey(Experience, on_delete=models.CASCADE, related_name='images')
    image = models.ImageField(upload_to='experience_images/%Y/%m/%d/')
    caption = models.CharField(max_length=255, blank=True)
    order = models.IntegerField(default=0)
    created_at = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        ordering = ['order', 'created_at']
    
    def __str__(self):
        return f'{self.experience.title} - Image {self.order}'


class ExperienceBooking(models.Model):
    """Bookings for experiences"""
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('confirmed', 'Confirmed'),
        ('cancelled', 'Cancelled'),
        ('completed', 'Completed'),
    ]
    
    booking_ref = models.CharField(max_length=50, unique=True, db_index=True)
    guest = models.ForeignKey(User, on_delete=models.CASCADE, related_name='experience_bookings')
    experience = models.ForeignKey(Experience, on_delete=models.CASCADE, related_name='bookings')
    
    # Booking details
    booking_date = models.DateField(help_text='Date of the experience')
    booking_time = models.TimeField(blank=True, null=True, help_text='Start time if applicable')
    num_participants = models.IntegerField(default=1)
    
    # Pricing
    price_per_person = models.DecimalField(max_digits=10, decimal_places=2)
    service_fee = models.DecimalField(max_digits=10, decimal_places=2, default=3.00)
    total_amount = models.DecimalField(max_digits=10, decimal_places=2)
    currency = models.CharField(max_length=3, default='USD')
    
    # Status
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    special_requests = models.TextField(blank=True, null=True)
    
    # Metadata
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['guest']),
            models.Index(fields=['experience']),
            models.Index(fields=['status']),
            models.Index(fields=['booking_ref']),
            models.Index(fields=['booking_date']),
        ]
    
    def save(self, *args, **kwargs):
        if not self.booking_ref:
            import uuid
            self.booking_ref = f'EX{uuid.uuid4().hex[:10].upper()}'
        
        # Calculate total
        if not self.total_amount:
            base_price = self.price_per_person * self.num_participants
            self.total_amount = base_price + self.service_fee
        
        super().save(*args, **kwargs)
    
    def __str__(self):
        return f'{self.booking_ref} - {self.guest.email}'


class ExperienceAvailability(models.Model):
    """Define available dates/times for experiences"""
    WEEKDAY_CHOICES = [
        (0, 'Monday'),
        (1, 'Tuesday'),
        (2, 'Wednesday'),
        (3, 'Thursday'),
        (4, 'Friday'),
        (5, 'Saturday'),
        (6, 'Sunday'),
    ]
    
    experience = models.ForeignKey(Experience, on_delete=models.CASCADE, related_name='availability')
    weekday = models.IntegerField(choices=WEEKDAY_CHOICES, null=True, blank=True)
    specific_date = models.DateField(null=True, blank=True, help_text='For one-time availability')
    start_time = models.TimeField()
    end_time = models.TimeField()
    is_available = models.BooleanField(default=True)
    
    class Meta:
        ordering = ['weekday', 'start_time']
        verbose_name_plural = 'Experience Availabilities'
    
    def __str__(self):
        if self.specific_date:
            return f'{self.experience.title} - {self.specific_date}'
        return f'{self.experience.title} - {self.get_weekday_display()}'
