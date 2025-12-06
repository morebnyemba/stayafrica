from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from apps.bookings.models import Booking
from apps.bookings.serializers import BookingSerializer
from services.payment_gateway import PaymentGatewayService

class BookingViewSet(viewsets.ModelViewSet):
    serializer_class = BookingSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return bookings for current user"""
        user = self.request.user
        if user.is_host:
            return Booking.objects.filter(property__host=user)
        return Booking.objects.filter(guest=user)
    
    def perform_create(self, serializer):
        """Create booking and calculate fees"""
        booking = serializer.save(guest=self.request.user)
        
        # Calculate fees
        payment_service = PaymentGatewayService()
        fees = payment_service.calculate_fees(
            booking.nightly_total,
            booking.cleaning_fee
        )
        
        booking.commission_fee = fees['commission_fee']
        booking.grand_total = booking.nightly_total + booking.service_fee + booking.cleaning_fee
        booking.save()
    
    @action(detail=True, methods=['post'])
    def confirm(self, request, pk=None):
        """Confirm a pending booking"""
        booking = self.get_object()
        if booking.status == 'pending':
            booking.status = 'confirmed'
            booking.save()
            return Response({'status': 'confirmed'})
        return Response({'error': 'Can only confirm pending bookings'}, status=status.HTTP_400_BAD_REQUEST)
    
    @action(detail=True, methods=['post'])
    def cancel(self, request, pk=None):
        """Cancel a booking"""
        booking = self.get_object()
        if booking.status in ['pending', 'confirmed']:
            booking.status = 'cancelled'
            booking.save()
            return Response({'status': 'cancelled'})
        return Response({'error': 'Cannot cancel completed or cancelled bookings'}, status=status.HTTP_400_BAD_REQUEST)
