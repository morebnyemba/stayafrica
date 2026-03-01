from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAdminUser, IsAuthenticated, AllowAny
from apps.admin_dashboard.models import AuditLog, AdminStats, SystemConfiguration
from apps.admin_dashboard.serializers import AuditLogSerializer, AdminStatsSerializer, SystemConfigurationSerializer
from django.db.models import Sum, Count, Q, F
from django.db.models.functions import TruncDay, TruncWeek, TruncMonth, TruncYear
from apps.bookings.models import Booking
from apps.users.models import User
from apps.properties.models import Property
from apps.payments.models import Payment, Wallet, WalletTransaction
import uuid
import logging

logger = logging.getLogger(__name__)


class AuditLogViewSet(viewsets.ReadOnlyModelViewSet):
    queryset = AuditLog.objects.all()
    serializer_class = AuditLogSerializer
    permission_classes = [IsAdminUser]
    filterset_fields = ['action', 'user']
    ordering_fields = ['timestamp']
    ordering = ['-timestamp']

class AdminStatsViewSet(viewsets.ViewSet):
    permission_classes = [IsAdminUser]
    
    @action(detail=False, methods=['get'])
    def dashboard(self, request):
        """Get dashboard statistics"""
        stats, _ = AdminStats.objects.get_or_create(pk=1)
        
        # Calculate real-time stats
        stats.total_revenue = Booking.objects.filter(
            status='completed'
        ).aggregate(Sum('grand_total'))['grand_total__sum'] or 0
        
        stats.total_bookings = Booking.objects.count()
        stats.total_users = User.objects.count()
        stats.active_hosts = User.objects.filter(role='host', is_verified=True).count()
        stats.total_properties = Property.objects.count()
        stats.save()
        
        serializer = AdminStatsSerializer(stats)
        return Response(serializer.data)
    
    @action(detail=False, methods=['post'])
    def bulk_approve_properties(self, request):
        """Approve multiple properties"""
        property_ids = request.data.get('property_ids', [])
        
        updated = Property.objects.filter(
            id__in=property_ids,
            status='pending_approval'
        ).update(status='active')
        
        return Response({'updated': updated})
    
    @action(detail=False, methods=['post'])
    def bulk_payout(self, request):
        """Process bulk payouts to hosts for completed bookings."""
        from django.db import transaction as db_transaction
        
        host_ids = request.data.get('host_ids', [])
        
        # Find completed bookings that haven't been paid out yet
        paid_booking_ids = WalletTransaction.objects.filter(
            metadata__type='host_payout',
            status='completed',
        ).values_list('booking_id', flat=True)
        
        completed_bookings = Booking.objects.filter(
            status='completed',
        ).exclude(
            id__in=paid_booking_ids,
        ).select_related('rental_property__host')
        
        if host_ids:
            completed_bookings = completed_bookings.filter(
                rental_property__host_id__in=host_ids,
            )
        
        if not completed_bookings.exists():
            return Response({'status': 'no_pending_payouts', 'processed': 0})
        
        processed = 0
        errors = []
        
        for booking in completed_bookings:
            try:
                with db_transaction.atomic():
                    host = booking.rental_property.host
                    wallet, _ = Wallet.objects.get_or_create(user=host)
                    
                    # host_payout = grand_total - commission_fee
                    payout_amount = booking.grand_total - booking.commission_fee
                    
                    WalletTransaction.objects.create(
                        wallet=wallet,
                        booking=booking,
                        txn_type='credit',
                        status='completed',
                        amount=payout_amount,
                        currency=booking.currency,
                        reference=f'PAYOUT-{booking.booking_ref}-{uuid.uuid4().hex[:8]}',
                        metadata={'type': 'host_payout', 'booking_ref': booking.booking_ref},
                    )
                    
                    wallet.balance = F('balance') + payout_amount
                    wallet.save(update_fields=['balance', 'updated_at'])
                    
                    processed += 1
            except Exception as e:
                logger.error(f"Payout failed for booking {booking.booking_ref}: {e}")
                errors.append({'booking': booking.booking_ref, 'error': str(e)})
        
        AuditLog.objects.create(
            user=request.user,
            action='bulk_payout',
            changes={'processed': processed, 'errors': len(errors)},
        )
        
        return Response({
            'status': 'completed',
            'processed': processed,
            'errors': errors,
        })
    
    def _get_trunc_func(self, period):
        return {
            'daily': TruncDay,
            'weekly': TruncWeek,
            'monthly': TruncMonth,
            'yearly': TruncYear,
        }.get(period, TruncMonth)
    
    @action(detail=False, methods=['get'])
    def revenue_analytics(self, request):
        """Revenue analytics with time-series data."""
        period = request.query_params.get('period', 'monthly')
        trunc = self._get_trunc_func(period)
        
        data = (
            Payment.objects.filter(status='success')
            .annotate(period=trunc('created_at'))
            .values('period')
            .annotate(
                total=Sum('amount'),
                count=Count('id'),
            )
            .order_by('period')
        )
        
        total_revenue = Payment.objects.filter(status='success').aggregate(Sum('amount'))['amount__sum'] or 0
        total_commission = Booking.objects.filter(status='completed').aggregate(Sum('commission'))['commission__sum'] or 0
        
        return Response({
            'data': list(data),
            'summary': {
                'total_revenue': float(total_revenue),
                'total_commission': float(total_commission),
                'net_revenue': float(total_revenue - total_commission),
            },
        })
    
    @action(detail=False, methods=['get'])
    def booking_analytics(self, request):
        """Booking analytics with time-series data."""
        period = request.query_params.get('period', 'monthly')
        trunc = self._get_trunc_func(period)
        
        data = (
            Booking.objects.all()
            .annotate(period=trunc('created_at'))
            .values('period')
            .annotate(
                total=Count('id'),
                confirmed=Count('id', filter=Q(status='confirmed')),
                completed=Count('id', filter=Q(status='completed')),
                cancelled=Count('id', filter=Q(status='cancelled')),
            )
            .order_by('period')
        )
        
        return Response({
            'data': list(data),
            'summary': {
                'total': Booking.objects.count(),
                'confirmed': Booking.objects.filter(status='confirmed').count(),
                'completed': Booking.objects.filter(status='completed').count(),
                'cancelled': Booking.objects.filter(status='cancelled').count(),
                'avg_stay': Booking.objects.filter(status='completed').count(),
            },
        })
    
    @action(detail=False, methods=['get'])
    def user_analytics(self, request):
        """User growth and engagement analytics."""
        period = request.query_params.get('period', 'monthly')
        trunc = self._get_trunc_func(period)
        
        data = (
            User.objects.all()
            .annotate(period=trunc('date_joined'))
            .values('period')
            .annotate(
                new_users=Count('id'),
                guests=Count('id', filter=Q(role='guest')),
                hosts=Count('id', filter=Q(role='host')),
            )
            .order_by('period')
        )
        
        return Response({
            'data': list(data),
            'summary': {
                'total_users': User.objects.count(),
                'total_guests': User.objects.filter(role='guest').count(),
                'total_hosts': User.objects.filter(role='host').count(),
                'verified': User.objects.filter(is_verified=True).count(),
            },
        })

class SystemConfigurationViewSet(viewsets.ViewSet):
    """Public endpoint for system configuration"""
    permission_classes = [AllowAny]
    
    @action(detail=False, methods=['get'])
    def fees(self, request):
        """Get fee configuration for booking calculations"""
        config = SystemConfiguration.get_config()
        serializer = SystemConfigurationSerializer(config)
        return Response(serializer.data)
