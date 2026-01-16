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
