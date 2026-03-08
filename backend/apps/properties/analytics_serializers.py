"""
Serializers for Host Analytics Dashboard
"""
from rest_framework import serializers
from apps.properties.analytics_models import (
    PropertyAnalytics,
    HostAnalyticsSummary,
    RevenueProjection,
    PerformanceBenchmark
)


class PropertyAnalyticsSerializer(serializers.ModelSerializer):
    """Serializer for property analytics"""
    property_name = serializers.CharField(source='property.title', read_only=True)
    property_id = serializers.CharField(source='property.id', read_only=True)
    
    class Meta:
        model = PropertyAnalytics
        fields = [
            'id', 'property', 'property_id', 'property_name', 'date',
            'total_revenue', 'bookings_count', 'nights_booked',
            'avg_nightly_rate', 'occupancy_rate', 'nights_available',
            'views_count', 'inquiries_count', 'conversion_rate',
            'unique_guests', 'returning_guests', 'created_at'
        ]
        read_only_fields = fields


class HostAnalyticsSummarySerializer(serializers.ModelSerializer):
    """Serializer for host analytics summary"""
    period_display = serializers.CharField(source='get_period_display', read_only=True)
    
    class Meta:
        model = HostAnalyticsSummary
        fields = [
            'id', 'period', 'period_display', 'start_date', 'end_date',
            'total_revenue', 'total_bookings', 'total_nights_booked',
            'avg_booking_value', 'avg_occupancy_rate', 'properties_count',
            'total_views', 'total_inquiries', 'avg_conversion_rate',
            'total_unique_guests', 'total_returning_guests',
            'guest_retention_rate', 'avg_rating', 'total_reviews',
            'created_at', 'updated_at'
        ]
        read_only_fields = fields


class RevenueProjectionSerializer(serializers.ModelSerializer):
    """Serializer for revenue projections"""
    property_name = serializers.CharField(
        source='property.title',
        read_only=True,
        allow_null=True
    )
    
    class Meta:
        model = RevenueProjection
        fields = [
            'id', 'property', 'property_name', 'projection_date',
            'target_month', 'projected_revenue', 'projected_bookings',
            'projected_occupancy', 'confidence_level',
            'seasonal_factor', 'trend_factor', 'created_at'
        ]
        read_only_fields = fields


class PerformanceBenchmarkSerializer(serializers.ModelSerializer):
    """Serializer for performance benchmarks"""
    
    class Meta:
        model = PerformanceBenchmark
        fields = [
            'id', 'region', 'property_type', 'month',
            'avg_occupancy_rate', 'avg_nightly_rate',
            'avg_revenue_per_available_night',
            'avg_booking_lead_time_days', 'avg_length_of_stay',
            'occupancy_75th_percentile', 'occupancy_90th_percentile',
            'rate_75th_percentile', 'rate_90th_percentile',
            'sample_size', 'created_at'
        ]
        read_only_fields = fields


class AnalyticsDashboardSerializer(serializers.Serializer):
    """Serializer for complete analytics dashboard data"""
    summary = HostAnalyticsSummarySerializer()
    property_analytics = PropertyAnalyticsSerializer(many=True)
    projections = RevenueProjectionSerializer(many=True)
    insights = serializers.DictField()
    benchmarks = PerformanceBenchmarkSerializer(many=True, required=False)


# ── New serializers for v2 dashboard (additive) ──────────

class DashboardSummaryV2Serializer(serializers.Serializer):
    """Maps HostAnalyticsSummary model fields to frontend DashboardSummary shape."""
    total_revenue = serializers.FloatField(default=0)
    average_occupancy = serializers.FloatField(default=0)
    total_bookings = serializers.IntegerField(default=0)
    average_rating = serializers.FloatField(default=0)
    revenue_change = serializers.FloatField(default=0)
    occupancy_change = serializers.FloatField(default=0)
    bookings_change = serializers.FloatField(default=0)
    rating_change = serializers.FloatField(default=0)
    period = serializers.CharField(default='monthly')


class RevenueDataPointSerializer(serializers.Serializer):
    """Matches frontend RevenueDataPoint type."""
    date = serializers.CharField()
    revenue = serializers.FloatField(default=0)
    bookings = serializers.IntegerField(default=0)
    label = serializers.CharField(required=False, allow_blank=True)


class OccupancyDataPointSerializer(serializers.Serializer):
    """Matches frontend OccupancyDataPoint type."""
    date = serializers.CharField()
    occupancy_rate = serializers.FloatField(default=0)
    booked_nights = serializers.IntegerField(default=0)
    available_nights = serializers.IntegerField(default=0)


class BookingTimelinePointSerializer(serializers.Serializer):
    """Matches frontend BookingDataPoint type."""
    date = serializers.CharField()
    count = serializers.IntegerField(default=0)
    confirmed = serializers.IntegerField(default=0)
    pending = serializers.IntegerField(default=0)
    cancelled = serializers.IntegerField(default=0)


class PropertyPerformanceV2Serializer(serializers.Serializer):
    """Matches frontend PropertyPerformance type."""
    property_id = serializers.CharField()
    property_name = serializers.CharField()
    property_image = serializers.CharField(required=False, allow_null=True)
    revenue = serializers.FloatField(default=0)
    occupancy_rate = serializers.FloatField(default=0)
    bookings = serializers.IntegerField(default=0)
    average_rating = serializers.FloatField(default=0)
    revenue_per_night = serializers.FloatField(default=0)
    days_available = serializers.IntegerField(default=30)


class InsightItemSerializer(serializers.Serializer):
    """Matches frontend Insight type."""
    id = serializers.CharField()
    type = serializers.ChoiceField(
        choices=['success', 'warning', 'info', 'danger'],
        default='info',
    )
    title = serializers.CharField()
    description = serializers.CharField()
    recommendation = serializers.CharField(required=False, allow_blank=True)
    action_url = serializers.CharField(required=False, allow_blank=True, allow_null=True)
    action_label = serializers.CharField(required=False, allow_blank=True, allow_null=True)
    metric = serializers.CharField(required=False, allow_blank=True, allow_null=True)
    value = serializers.CharField(required=False, allow_blank=True, allow_null=True)


class DateRangeSerializer(serializers.Serializer):
    """Matches frontend DateRange type."""
    start_date = serializers.CharField()
    end_date = serializers.CharField()


class FullDashboardSerializer(serializers.Serializer):
    """
    Complete analytics dashboard matching frontend AnalyticsDashboardData.
    Used by the dashboard_full endpoint.
    """
    summary = DashboardSummaryV2Serializer()
    revenue_chart = RevenueDataPointSerializer(many=True)
    occupancy_chart = OccupancyDataPointSerializer(many=True)
    booking_timeline = BookingTimelinePointSerializer(many=True)
    property_performance = PropertyPerformanceV2Serializer(many=True)
    insights = InsightItemSerializer(many=True)
    period = serializers.CharField()
    date_range = DateRangeSerializer()

