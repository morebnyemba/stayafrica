from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAdminUser, IsAuthenticated
from apps.admin_dashboard.models import AuditLog, AdminStats
from apps.admin_dashboard.serializers import AuditLogSerializer, AdminStatsSerializer
from django.db.models import Sum, Count, Q
from apps.bookings.models import Booking
from apps.users.models import User
from apps.properties.models import Property

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
        """Process bulk payouts to hosts"""
        # TODO: Implement bulk payout logic
        return Response({'status': 'payout processing'})
