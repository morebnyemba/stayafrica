from django.db import models
from apps.bookings.models import Booking
import uuid

class Payment(models.Model):
    PROVIDER_CHOICES = [
        ('paynow', 'Paynow'),
        ('payfast', 'PayFast'),
        ('ozow', 'Ozow'),
        ('stripe', 'Stripe'),
        ('cash_on_arrival', 'Cash on Arrival'),
    ]
    
    STATUS_CHOICES = [
        ('initiated', 'Initiated'),
        ('success', 'Success'),
        ('failed', 'Failed'),
        ('pending', 'Pending'),
    ]
    
    booking = models.OneToOneField(Booking, on_delete=models.CASCADE, related_name='payment')
    provider = models.CharField(max_length=20, choices=PROVIDER_CHOICES)
    gateway_ref = models.CharField(max_length=255, unique=True, db_index=True)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='initiated')
    
    amount = models.DecimalField(max_digits=10, decimal_places=2)
    currency = models.CharField(max_length=3, default='USD')
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['booking']),
            models.Index(fields=['provider']),
            models.Index(fields=['status']),
        ]
    
    def __str__(self):
        return f'{self.booking.booking_ref} - {self.provider} ({self.status})'
