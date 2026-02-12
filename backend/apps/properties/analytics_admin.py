from django.contrib import admin
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.properties.analytics_models import (
    PropertyAnalytics, HostAnalyticsSummary, 
    RevenueProjection, PerformanceBenchmark
)


@admin.register(PropertyAnalytics)
class PropertyAnalyticsAdmin(UnfoldModelAdmin):
    """Admin interface for property analytics"""
    
    list_display = ['property', 'date', 'views_count', 'bookings_count', 
                    'occupancy_rate_display', 'total_revenue_display']
    list_filter = ['date']
    search_fields = ['property__title', 'property__host__email']
    readonly_fields = ['property', 'date', 'views_count', 'bookings_count',
                       'total_revenue', 'avg_nightly_rate', 'occupancy_rate',
                       'nights_booked', 'nights_available', 'inquiries_count',
                       'conversion_rate', 'unique_guests', 'returning_guests',
                       'created_at', 'updated_at']
    list_select_related = ['property', 'property__host']
    list_per_page = 25
    date_hierarchy = 'date'
    
    fieldsets = (
        (_('Property & Date'), {
            'fields': ('property', 'date'),
        }),
        (_('Views & Bookings'), {
            'fields': ('views_count', 'bookings_count', 'inquiries_count', 'conversion_rate'),
        }),
        (_('Revenue'), {
            'fields': ('total_revenue', 'avg_nightly_rate'),
        }),
        (_('Occupancy'), {
            'fields': ('occupancy_rate', 'nights_booked', 'nights_available'),
        }),
        (_('Guests'), {
            'fields': ('unique_guests', 'returning_guests'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Occupancy Rate'))
    def occupancy_rate_display(self, obj):
        if obj.occupancy_rate:
            return f"{obj.occupancy_rate:.1f}%"
        return '-'
    
    @display(description=_('Revenue'))
    def total_revenue_display(self, obj):
        if obj.total_revenue:
            return f"${obj.total_revenue:,.2f}"
        return '-'
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False


@admin.register(HostAnalyticsSummary)
class HostAnalyticsSummaryAdmin(UnfoldModelAdmin):
    """Admin interface for host analytics summary"""
    
    list_display = ['host', 'properties_count', 'total_views', 'total_bookings',
                    'total_revenue_display', 'avg_rating_display', 'updated_at']
    list_filter = ['period', 'updated_at']
    search_fields = ['host__email', 'host__username']
    readonly_fields = ['host', 'period', 'start_date', 'end_date',
                       'properties_count', 'total_views', 'total_inquiries',
                       'total_bookings', 'total_revenue', 'avg_booking_value',
                       'total_nights_booked', 'avg_occupancy_rate',
                       'avg_conversion_rate', 'total_unique_guests',
                       'total_returning_guests', 'guest_retention_rate',
                       'avg_rating', 'total_reviews',
                       'created_at', 'updated_at']
    list_select_related = ['host']
    list_per_page = 25
    
    fieldsets = (
        (_('Host'), {
            'fields': ('host',),
        }),
        (_('Period'), {
            'fields': ('period', 'start_date', 'end_date'),
        }),
        (_('Properties'), {
            'fields': ('properties_count',),
        }),
        (_('Performance'), {
            'fields': ('total_views', 'total_inquiries', 'total_bookings',
                       'total_revenue', 'avg_booking_value'),
        }),
        (_('Occupancy'), {
            'fields': ('total_nights_booked', 'avg_occupancy_rate', 'avg_conversion_rate'),
        }),
        (_('Guests'), {
            'fields': ('total_unique_guests', 'total_returning_guests', 'guest_retention_rate'),
        }),
        (_('Quality Metrics'), {
            'fields': ('avg_rating', 'total_reviews'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Total Revenue'))
    def total_revenue_display(self, obj):
        return f"${obj.total_revenue:,.2f}" if obj.total_revenue else '-'
    
    @display(description=_('Avg Rating'))
    def avg_rating_display(self, obj):
        if obj.avg_rating:
            return f"{obj.avg_rating:.1f}/5"
        return '-'
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False


@admin.register(RevenueProjection)
class RevenueProjectionAdmin(UnfoldModelAdmin):
    """Admin interface for revenue projections"""
    
    list_display = ['property', 'projection_date', 'target_month', 'projected_bookings', 
                    'projected_revenue_display', 'confidence_level_display']
    list_filter = ['projection_date', 'target_month']
    search_fields = ['property__title', 'property__host__email']
    readonly_fields = ['property', 'host', 'projection_date', 'target_month',
                       'projected_bookings', 'projected_revenue',
                       'projected_occupancy', 'confidence_level',
                       'seasonal_factor', 'trend_factor', 'created_at']
    list_select_related = ['property', 'property__host', 'host']
    list_per_page = 25
    date_hierarchy = 'projection_date'
    
    fieldsets = (
        (_('Property & Date'), {
            'fields': ('property', 'host', 'projection_date', 'target_month'),
        }),
        (_('Projections'), {
            'fields': ('projected_bookings', 'projected_revenue', 'projected_occupancy'),
        }),
        (_('Confidence & Factors'), {
            'fields': ('confidence_level', 'seasonal_factor', 'trend_factor'),
        }),
        (_('Timestamp'), {
            'fields': ('created_at',),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Projected Revenue'))
    def projected_revenue_display(self, obj):
        return f"${obj.projected_revenue:,.2f}" if obj.projected_revenue else '-'
    
    @display(description=_('Confidence'))
    def confidence_level_display(self, obj):
        if obj.confidence_level:
            return f"{obj.confidence_level:.0f}%"
        return '-'
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False


@admin.register(PerformanceBenchmark)
class PerformanceBenchmarkAdmin(UnfoldModelAdmin):
    """Admin interface for performance benchmarks"""
    
    list_display = ['region', 'property_type', 'month',
                    'avg_occupancy_rate_display', 'avg_nightly_rate_display']
    list_filter = ['property_type', 'region']
    search_fields = ['region']
    readonly_fields = ['region', 'property_type', 'month',
                       'avg_occupancy_rate', 'avg_nightly_rate',
                       'avg_revenue_per_available_night',
                       'avg_booking_lead_time_days', 'avg_length_of_stay',
                       'occupancy_75th_percentile', 'occupancy_90th_percentile',
                       'rate_75th_percentile', 'rate_90th_percentile',
                       'sample_size', 'created_at', 'updated_at']
    list_per_page = 25
    date_hierarchy = 'month'
    
    fieldsets = (
        (_('Location & Type'), {
            'fields': ('region', 'property_type'),
        }),
        (_('Period'), {
            'fields': ('month',),
        }),
        (_('Averages'), {
            'fields': ('avg_occupancy_rate', 'avg_nightly_rate',
                       'avg_revenue_per_available_night',
                       'avg_booking_lead_time_days', 'avg_length_of_stay'),
        }),
        (_('Percentiles'), {
            'fields': ('occupancy_75th_percentile', 'occupancy_90th_percentile',
                       'rate_75th_percentile', 'rate_90th_percentile'),
        }),
        (_('Sample'), {
            'fields': ('sample_size',),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Avg Occupancy'))
    def avg_occupancy_rate_display(self, obj):
        if obj.avg_occupancy_rate:
            return f"{obj.avg_occupancy_rate:.1f}%"
        return '-'
    
    @display(description=_('Avg Nightly Rate'))
    def avg_nightly_rate_display(self, obj):
        if obj.avg_nightly_rate:
            return f"${obj.avg_nightly_rate:,.2f}"
        return '-'
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False
