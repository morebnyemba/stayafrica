"""
Views for Host Analytics Dashboard
"""
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from django.utils import timezone
from django.db import models
from datetime import timedelta, date
from dateutil.relativedelta import relativedelta
from apps.properties.analytics_models import (
    PropertyAnalytics,
    HostAnalyticsSummary,
    RevenueProjection,
    PerformanceBenchmark
)
from apps.properties.analytics_serializers import (
    PropertyAnalyticsSerializer,
    HostAnalyticsSummarySerializer,
    RevenueProjectionSerializer,
    PerformanceBenchmarkSerializer,
    AnalyticsDashboardSerializer
)
from services.host_analytics_service import HostAnalyticsService
import logging

logger = logging.getLogger(__name__)


class HostAnalyticsViewSet(viewsets.ViewSet):
    """ViewSet for host analytics dashboard"""
    permission_classes = [IsAuthenticated]
    
    def list(self, request):
        """
        List available analytics endpoints
        """
        endpoints = {
            'dashboard': request.build_absolute_uri('dashboard/'),
            'revenue_chart': request.build_absolute_uri('revenue_chart/'),
            'occupancy_trend': request.build_absolute_uri('occupancy_trend/'),
            'generate_projections': request.build_absolute_uri('generate_projections/'),
        }
        return Response({
            'message': 'Host Analytics API',
            'available_endpoints': endpoints
        })
    
    @action(detail=False, methods=['get'])
    def dashboard(self, request):
        """
        Get complete analytics dashboard for host
        
        Query params:
        - period: 'daily', 'weekly', 'monthly', 'yearly' (default: 'monthly')
        - start_date: Start date (default: first day of current month)
        - end_date: End date (default: today)
        """
        host = request.user
        period = request.query_params.get('period', 'monthly')
        
        # Determine date range
        today = timezone.now().date()
        if period == 'monthly':
            start_date = date(today.year, today.month, 1)
            end_date = today
        elif period == 'weekly':
            start_date = today - timedelta(days=7)
            end_date = today
        elif period == 'daily':
            start_date = today
            end_date = today
        elif period == 'yearly':
            start_date = date(today.year, 1, 1)
            end_date = today
        else:
            # Custom dates
            start_date = request.query_params.get('start_date', str(today))
            end_date = request.query_params.get('end_date', str(today))
            start_date = date.fromisoformat(start_date)
            end_date = date.fromisoformat(end_date)
        
        # Generate summary
        summary = HostAnalyticsService.generate_host_summary(
            host, start_date, end_date, period
        )
        
        # Get property analytics
        property_analytics = PropertyAnalytics.objects.filter(
            property__host=host,
            date__gte=start_date,
            date__lte=end_date
        ).order_by('-date')[:30]  # Last 30 days
        
        # Get projections (next 3 months)
        next_month = today + relativedelta(months=1)
        projections = RevenueProjection.objects.filter(
            host=host,
            target_month__gte=next_month
        ).order_by('target_month')[:3]
        
        # If no projections exist, generate them
        if not projections.exists():
            try:
                for i in range(1, 4):
                    target = today + relativedelta(months=i)
                    HostAnalyticsService.project_revenue(host, target)
                
                projections = RevenueProjection.objects.filter(
                    host=host,
                    target_month__gte=next_month
                ).order_by('target_month')[:3]
            except Exception as e:
                logger.error(f"Error generating projections: {e}")
        
        # Get insights
        insights = HostAnalyticsService.get_performance_insights(host)
        
        # Prepare response
        dashboard_data = {
            'summary': HostAnalyticsSummarySerializer(summary).data,
            'property_analytics': PropertyAnalyticsSerializer(property_analytics, many=True).data,
            'projections': RevenueProjectionSerializer(projections, many=True).data,
            'insights': insights
        }
        
        serializer = AnalyticsDashboardSerializer(dashboard_data)
        return Response(serializer.data)
    
    @action(detail=False, methods=['get'])
    def revenue_chart(self, request):
        """
        Get revenue data for charting
        
        Query params:
        - period: 'week', 'month', 'year' (default: 'month')
        """
        host = request.user
        period = request.query_params.get('period', 'month')
        
        today = timezone.now().date()
        
        if period == 'week':
            start_date = today - timedelta(days=7)
        elif period == 'month':
            start_date = today - timedelta(days=30)
        elif period == 'year':
            start_date = today - timedelta(days=365)
        else:
            start_date = today - timedelta(days=30)
        
        # Get daily analytics
        analytics = PropertyAnalytics.objects.filter(
            property__host=host,
            date__gte=start_date,
            date__lte=today
        ).values('date').annotate(
            revenue=models.Sum('total_revenue'),
            bookings=models.Sum('bookings_count'),
            occupancy=models.Avg('occupancy_rate')
        ).order_by('date')
        
        # Format for charting
        chart_data = {
            'labels': [str(item['date']) for item in analytics],
            'revenue': [float(item['revenue']) for item in analytics],
            'bookings': [item['bookings'] for item in analytics],
            'occupancy': [float(item['occupancy']) for item in analytics]
        }
        
        return Response(chart_data)
    
    @action(detail=False, methods=['get'])
    def occupancy_trend(self, request):
        """Get occupancy trend over time"""
        host = request.user
        days = int(request.query_params.get('days', 30))
        
        today = timezone.now().date()
        start_date = today - timedelta(days=days)
        
        analytics = PropertyAnalytics.objects.filter(
            property__host=host,
            date__gte=start_date,
            date__lte=today
        ).values('date').annotate(
            avg_occupancy=models.Avg('occupancy_rate')
        ).order_by('date')
        
        trend_data = {
            'dates': [str(item['date']) for item in analytics],
            'occupancy_rates': [float(item['avg_occupancy']) for item in analytics]
        }
        
        return Response(trend_data)
    
    @action(detail=False, methods=['post'])
    def generate_projections(self, request):
        """Generate revenue projections for next N months"""
        host = request.user
        months = int(request.data.get('months', 3))
        property_id = request.data.get('property_id')
        
        property_obj = None
        if property_id:
            from apps.properties.models import Property
            try:
                property_obj = Property.objects.get(id=property_id, host=host)
            except Property.DoesNotExist:
                return Response(
                    {'error': 'Property not found'},
                    status=status.HTTP_404_NOT_FOUND
                )
        
        projections = []
        today = timezone.now().date()
        
        for i in range(1, months + 1):
            target_month = today + relativedelta(months=i)
            try:
                projection = HostAnalyticsService.project_revenue(
                    host, target_month, property_obj
                )
                projections.append(projection)
            except Exception as e:
                logger.error(f"Error generating projection for {target_month}: {e}")
        
        serializer = RevenueProjectionSerializer(projections, many=True)
        return Response(serializer.data)


class PropertyAnalyticsViewSet(viewsets.ReadOnlyModelViewSet):
    """ViewSet for property-specific analytics"""
    serializer_class = PropertyAnalyticsSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return analytics for host's properties"""
        return PropertyAnalytics.objects.filter(
            property__host=self.request.user
        )


class PerformanceBenchmarkViewSet(viewsets.ReadOnlyModelViewSet):
    """ViewSet for performance benchmarks"""
    serializer_class = PerformanceBenchmarkSerializer
    permission_classes = [IsAuthenticated]
    queryset = PerformanceBenchmark.objects.all()
    
    def get_queryset(self):
        """Filter benchmarks by region and property type"""
        queryset = PerformanceBenchmark.objects.all()
        
        region = self.request.query_params.get('region')
        property_type = self.request.query_params.get('property_type')
        
        if region:
            queryset = queryset.filter(region=region)
        if property_type:
            queryset = queryset.filter(property_type=property_type)
        
        return queryset.order_by('-month')[:12]  # Last 12 months
