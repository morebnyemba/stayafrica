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
    
    # Period to days mapping (includes aliases for backward compatibility)
    PERIOD_DAYS = {
        'daily': 7,
        'weekly': 28,
        'monthly': 30,
        'yearly': 365,
        # Aliases for backward compatibility
        'week': 7,
        'month': 30,
        'year': 365,
    }
    
    def _get_period_days(self, period: str, default: int = 30) -> int:
        """Get the number of days for a given period string."""
        return self.PERIOD_DAYS.get(period, default)
    
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
        
        # Generate booking timeline data for BookingTimelineChart
        booking_timeline = self._generate_booking_timeline(host, start_date, end_date)
        
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
        
        # Prepare response with all fields expected by frontend
        dashboard_data = {
            'summary': HostAnalyticsSummarySerializer(summary).data,
            'property_analytics': PropertyAnalyticsSerializer(property_analytics, many=True).data,
            'projections': RevenueProjectionSerializer(projections, many=True).data,
            'booking_timeline': booking_timeline,
            'insights': insights,
            'period': period,
            'date_range': {
                'start_date': str(start_date),
                'end_date': str(end_date),
            }
        }
        
        return Response(dashboard_data)
    
    def _generate_booking_timeline(self, host, start_date, end_date):
        """
        Generate booking timeline data for the BookingTimelineChart.
        Returns array of BookingDataPoint objects:
        [{ date, count, confirmed, pending, cancelled }, ...]
        """
        from apps.bookings.models import Booking
        
        # Get all bookings for host's properties in the date range
        bookings = Booking.objects.filter(
            property__host=host,
            created_at__date__gte=start_date,
            created_at__date__lte=end_date
        ).values('created_at__date', 'status').annotate(
            count=models.Count('id')
        ).order_by('created_at__date')
        
        # Group by date
        timeline_dict = {}
        for item in bookings:
            date_str = str(item['created_at__date'])
            if date_str not in timeline_dict:
                timeline_dict[date_str] = {
                    'date': date_str,
                    'count': 0,
                    'confirmed': 0,
                    'pending': 0,
                    'cancelled': 0,
                }
            
            timeline_dict[date_str]['count'] += item['count']
            
            # Map booking status to frontend expected fields
            status = item['status']
            if status in ['confirmed', 'completed', 'active']:
                timeline_dict[date_str]['confirmed'] += item['count']
            elif status in ['pending', 'pending_approval']:
                timeline_dict[date_str]['pending'] += item['count']
            elif status in ['cancelled', 'rejected']:
                timeline_dict[date_str]['cancelled'] += item['count']
        
        # Convert dict to sorted list
        timeline = sorted(timeline_dict.values(), key=lambda x: x['date'])
        return timeline
    
    @action(detail=False, methods=['get'])
    def revenue_chart(self, request):
        """
        Get revenue data for charting
        
        Query params:
        - period: 'daily', 'weekly', 'month', 'year' (default: 'monthly')
        
        Returns array of RevenueDataPoint objects:
        [{ date, revenue, bookings, label }, ...]
        """
        host = request.user
        period = request.query_params.get('period', 'monthly')
        
        today = timezone.now().date()
        
        # Map period names to day ranges (includes aliases for backward compatibility)
        days = self._get_period_days(period)
        start_date = today - timedelta(days=days)
        
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
        
        # Format for frontend charting - return array of objects
        chart_data = [
            {
                'date': str(item['date']),
                'revenue': float(item['revenue'] or 0),
                'bookings': item['bookings'] or 0,
                'label': str(item['date']),
            }
            for item in analytics
        ]
        
        return Response(chart_data)
    
    @action(detail=False, methods=['get'])
    def occupancy_trend(self, request):
        """
        Get occupancy trend over time
        
        Query params:
        - period: 'daily', 'weekly', 'monthly', 'yearly' (default: 'monthly')
        
        Returns array of OccupancyDataPoint objects:
        [{ date, occupancy_rate, booked_nights, available_nights }, ...]
        """
        host = request.user
        period = request.query_params.get('period', 'monthly')
        
        # Use common period days mapping
        days = self._get_period_days(period)
        
        today = timezone.now().date()
        start_date = today - timedelta(days=days)
        
        analytics = PropertyAnalytics.objects.filter(
            property__host=host,
            date__gte=start_date,
            date__lte=today
        ).values('date').annotate(
            avg_occupancy=models.Avg('occupancy_rate'),
            total_booked=models.Sum('nights_booked'),
            total_available=models.Sum('nights_available')
        ).order_by('date')
        
        # Format for frontend charting - return array of objects
        trend_data = [
            {
                'date': str(item['date']),
                'occupancy_rate': float(item['avg_occupancy'] or 0),
                'booked_nights': item['total_booked'] or 0,
                'available_nights': item['total_available'] or 0,
            }
            for item in analytics
        ]
        
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
