from rest_framework import viewsets, status
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from apps.reviews.models import Review
from apps.reviews.serializers import ReviewSerializer
from apps.bookings.models import Booking
from datetime import date

class ReviewViewSet(viewsets.ModelViewSet):
    serializer_class = ReviewSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return reviews for properties of current user or reviews by user"""
        user = self.request.user
        return Review.objects.filter(host=user) | Review.objects.filter(guest=user)
    
    def perform_create(self, serializer):
        """Create review after booking checkout"""
        booking_id = self.request.data.get('booking_id')
        
        try:
            booking = Booking.objects.get(id=booking_id, guest=self.request.user)
        except Booking.DoesNotExist:
            raise ValidationError('Booking not found')
        
        # Check if checkout date has passed
        if booking.check_out > date.today():
            raise ValidationError('Can only review after checkout date')
        
        # Check if review already exists
        if Review.objects.filter(booking=booking).exists():
            raise ValidationError('Review already exists for this booking')
        
        serializer.save(
            booking=booking,
            guest=self.request.user,
            host=booking.property.host
        )
