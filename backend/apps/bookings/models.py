from django.db import models
from django.utils.timezone import now
from apps.users.models import User
from apps.properties.models import Property
import uuid

class Booking(models.Model):
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('confirmed', 'Confirmed'),
        ('cancelled', 'Cancelled'),
        ('completed', 'Completed'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    booking_ref = models.CharField(max_length=50, unique=True, db_index=True)
    guest = models.ForeignKey(User, on_delete=models.CASCADE, related_name='bookings')
    rental_property = models.ForeignKey(Property, on_delete=models.CASCADE, related_name='bookings')
    
    check_in = models.DateField()
    check_out = models.DateField()
    number_of_guests = models.PositiveIntegerField(default=1)
    
    nightly_total = models.DecimalField(max_digits=10, decimal_places=2)
    service_fee = models.DecimalField(max_digits=10, decimal_places=2, default=3.00)
    commission_fee = models.DecimalField(max_digits=10, decimal_places=2)
    cleaning_fee = models.DecimalField(max_digits=10, decimal_places=2, default=0, blank=True)
    taxes = models.DecimalField(max_digits=10, decimal_places=2, default=0, blank=True, help_text='Total taxes applied')
    grand_total = models.DecimalField(max_digits=10, decimal_places=2)
    
    currency = models.CharField(max_length=3, default='USD')
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    
    special_requests = models.TextField(blank=True, null=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['guest']),
            models.Index(fields=['rental_property']),
            models.Index(fields=['status']),
            models.Index(fields=['booking_ref']),
        ]
    
    def save(self, *args, **kwargs):
        if not self.booking_ref:
            self.booking_ref = f'BK{uuid.uuid4().hex[:10].upper()}'
        super().save(*args, **kwargs)
    
    def __str__(self):
        return f'{self.booking_ref} - {self.guest.email}'
    
    @property
    def nights(self):
        return (self.check_out - self.check_in).days
