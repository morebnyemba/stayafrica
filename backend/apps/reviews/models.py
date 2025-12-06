from django.db import models
from apps.users.models import User
from apps.bookings.models import Booking

class Review(models.Model):
    RATING_CHOICES = [(i, str(i)) for i in range(1, 6)]
    
    booking = models.OneToOneField(Booking, on_delete=models.CASCADE, related_name='review')
    guest = models.ForeignKey(User, on_delete=models.CASCADE, related_name='reviews_given')
    host = models.ForeignKey(User, on_delete=models.CASCADE, related_name='reviews_received')
    
    rating = models.IntegerField(choices=RATING_CHOICES, default=5)
    text = models.TextField()
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['guest']),
            models.Index(fields=['host']),
            models.Index(fields=['booking']),
        ]
    
    def __str__(self):
        return f'Review by {self.guest.email} for {self.host.email}'
