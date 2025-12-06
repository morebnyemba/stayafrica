from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from apps.payments.models import Payment
from apps.payments.serializers import PaymentSerializer
from services.payment_gateway import PaymentGatewayService

class PaymentViewSet(viewsets.ModelViewSet):
    serializer_class = PaymentSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return payments for current user"""
        user = self.request.user
        return Payment.objects.filter(booking__guest=user)
    
    @action(detail=False, methods=['post'])
    def initiate(self, request):
        """Initiate a payment"""
        booking_id = request.data.get('booking_id')
        provider = request.data.get('provider')
        
        try:
            from apps.bookings.models import Booking
            booking = Booking.objects.get(id=booking_id, guest=request.user)
        except Booking.DoesNotExist:
            return Response({'error': 'Booking not found'}, status=status.HTTP_404_NOT_FOUND)
        
        # Check if payment provider is available for user's country
        payment_service = PaymentGatewayService()
        available_providers = payment_service.get_available_providers(request.user.country_of_residence)
        
        if provider not in available_providers:
            return Response(
                {'error': f'Payment provider not available for your country'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Create payment record
        payment = Payment.objects.create(
            booking=booking,
            provider=provider,
            gateway_ref=f'{booking.booking_ref}-{provider}',
            amount=booking.grand_total,
            currency=booking.currency,
            status='initiated'
        )
        
        serializer = PaymentSerializer(payment)
        return Response(serializer.data, status=status.HTTP_201_CREATED)
    
    @action(detail=False, methods=['post'], permission_classes=[])
    def webhook(self, request):
        """Handle payment provider webhooks"""
        provider = request.data.get('provider')
        # Implementation for handling webhooks
        return Response({'status': 'received'})
