from django.db import models
from apps.users.models import User
from apps.bookings.models import Booking

class Review(models.Model):
    RATING_CHOICES = [(i, str(i)) for i in range(1, 6)]
    
    REVIEW_TYPE_CHOICES = [
        ('property', 'Property Review'),
        ('experience', 'Experience Review'),
    ]
    
    # Link to booking (for property reviews)
    booking = models.OneToOneField(Booking, on_delete=models.CASCADE, related_name='review', null=True, blank=True)
    
    # Direct links for flexibility
    guest = models.ForeignKey(User, on_delete=models.CASCADE, related_name='reviews_given')
    host = models.ForeignKey(User, on_delete=models.CASCADE, related_name='reviews_received')
    
    # Support for different review types
    review_type = models.CharField(max_length=20, choices=REVIEW_TYPE_CHOICES, default='property')
    
    # Property reference (optional, for direct property reviews)
    property_id = models.CharField(max_length=10, null=True, blank=True, help_text='Direct property reference')
    
    # Experience reference (optional, for experience reviews)
    experience_id = models.CharField(max_length=10, null=True, blank=True, help_text='Direct experience reference')
    
    rating = models.IntegerField(choices=RATING_CHOICES, default=5)
    text = models.TextField()
    
    # Response from host
    host_response = models.TextField(blank=True, null=True)
    host_response_date = models.DateTimeField(null=True, blank=True)
    
    # Helpful votes
    helpful_count = models.IntegerField(default=0)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['guest']),
            models.Index(fields=['host']),
            models.Index(fields=['booking']),
            models.Index(fields=['review_type']),
            models.Index(fields=['property_id']),
            models.Index(fields=['experience_id']),
        ]
    
    def __str__(self):
        return f'Review by {self.guest.email} for {self.host.email}'


class ReviewVote(models.Model):
    """Track helpful/unhelpful votes on reviews"""
    VOTE_CHOICES = [
        ('helpful', 'Helpful'),
        ('unhelpful', 'Unhelpful'),
    ]
    
    review = models.ForeignKey(Review, on_delete=models.CASCADE, related_name='votes')
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name='review_votes')
    vote_type = models.CharField(max_length=10, choices=VOTE_CHOICES)
    created_at = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        unique_together = ('review', 'user')
        indexes = [
            models.Index(fields=['review', 'vote_type']),
        ]
    
    def __str__(self):
        return f'{self.user.email} - {self.vote_type} - Review {self.review.id}'
