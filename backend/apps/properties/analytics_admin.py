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
    
    list_display = ['property', 'period_display', 'total_views', 'total_bookings', 
                    'occupancy_rate_display', 'avg_rating_display']
    list_filter = ['period_type', 'year', 'month']
    search_fields = ['property__title', 'property__host__email']
    readonly_fields = ['property', 'period_type', 'year', 'month', 'quarter', 'week',
                       'total_views', 'total_bookings', 'total_revenue', 'avg_nightly_rate',
                       'avg_rating', 'total_reviews', 'occupancy_rate', 'cancellation_rate',
                       'response_rate', 'avg_response_time_minutes', 'created_at', 'updated_at']
    list_select_related = ['property', 'property__host']
    list_per_page = 25
    
    fieldsets = (
        (_('Property & Period'), {
            'fields': ('property', 'period_type', 'year', 'month', 'quarter', 'week'),
        }),
        (_('Views & Bookings'), {
            'fields': ('total_views', 'total_bookings', 'conversion_rate'),
        }),
        (_('Revenue'), {
            'fields': ('total_revenue', 'avg_nightly_rate'),
        }),
        (_('Reviews'), {
            'fields': ('total_reviews', 'avg_rating'),
        }),
        (_('Rates'), {
            'fields': ('occupancy_rate', 'cancellation_rate', 'response_rate', 'avg_response_time_minutes'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Period'))
    def period_display(self, obj):
        if obj.period_type == 'monthly':
            return f"{obj.year}-{obj.month:02d}"
        elif obj.period_type == 'quarterly':
            return f"{obj.year} Q{obj.quarter}"
        elif obj.period_type == 'weekly':
            return f"{obj.year} W{obj.week}"
        return f"{obj.year}"
    
    @display(description=_('Occupancy Rate'))
    def occupancy_rate_display(self, obj):
        if obj.occupancy_rate:
            return f"{obj.occupancy_rate:.1f}%"
        return '-'
    
    @display(description=_('Avg Rating'))
    def avg_rating_display(self, obj):
        if obj.avg_rating:
            return f"{obj.avg_rating:.1f}/5"
        return '-'
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False


@admin.register(HostAnalyticsSummary)
class HostAnalyticsSummaryAdmin(UnfoldModelAdmin):
    """Admin interface for host analytics summary"""
    
    list_display = ['host', 'total_properties', 'total_views', 'total_bookings',
                    'total_revenue_display', 'avg_rating_display', 'updated_at']
    list_filter = ['updated_at']
    search_fields = ['host__email', 'host__username']
    readonly_fields = ['host', 'total_properties', 'active_properties', 'total_views',
                       'total_bookings', 'total_revenue', 'avg_nightly_rate',
                       'avg_occupancy_rate', 'avg_rating', 'total_reviews',
                       'avg_response_rate', 'avg_response_time_minutes',
                       'last_booking_date', 'created_at', 'updated_at']
    list_select_related = ['host']
    list_per_page = 25
    
    fieldsets = (
        (_('Host'), {
            'fields': ('host',),
        }),
        (_('Properties'), {
            'fields': ('total_properties', 'active_properties'),
        }),
        (_('Performance'), {
            'fields': ('total_views', 'total_bookings', 'total_revenue', 'avg_nightly_rate'),
        }),
        (_('Quality Metrics'), {
            'fields': ('avg_rating', 'total_reviews', 'avg_response_rate', 'avg_response_time_minutes'),
        }),
        (_('Activity'), {
            'fields': ('avg_occupancy_rate', 'last_booking_date'),
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
    
    list_display = ['property', 'projection_date', 'projected_bookings', 
                    'projected_revenue_display', 'confidence_score_display']
    list_filter = ['projection_date']
    search_fields = ['property__title', 'property__host__email']
    readonly_fields = ['property', 'projection_date', 'projected_bookings',
                       'projected_revenue', 'projected_occupancy_rate',
                       'confidence_score', 'created_at']
    list_select_related = ['property', 'property__host']
    list_per_page = 25
    date_hierarchy = 'projection_date'
    
    fieldsets = (
        (_('Property & Date'), {
            'fields': ('property', 'projection_date'),
        }),
        (_('Projections'), {
            'fields': ('projected_bookings', 'projected_revenue', 'projected_occupancy_rate'),
        }),
        (_('Confidence'), {
            'fields': ('confidence_score',),
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
    def confidence_score_display(self, obj):
        if obj.confidence_score:
            return f"{obj.confidence_score:.0f}%"
        return '-'
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False


@admin.register(PerformanceBenchmark)
class PerformanceBenchmarkAdmin(UnfoldModelAdmin):
    """Admin interface for performance benchmarks"""
    
    list_display = ['location_display', 'property_type', 'period_display',
                    'avg_occupancy_rate_display', 'avg_daily_rate_display']
    list_filter = ['property_type', 'year', 'month']
    search_fields = ['city', 'country']
    readonly_fields = ['country', 'city', 'property_type', 'year', 'month',
                       'avg_occupancy_rate', 'avg_daily_rate', 'avg_revenue_per_listing',
                       'avg_rating', 'total_properties_sampled', 'created_at', 'updated_at']
    list_per_page = 25
    
    fieldsets = (
        (_('Location & Type'), {
            'fields': ('country', 'city', 'property_type'),
        }),
        (_('Period'), {
            'fields': ('year', 'month'),
        }),
        (_('Metrics'), {
            'fields': ('avg_occupancy_rate', 'avg_daily_rate', 'avg_revenue_per_listing', 'avg_rating'),
        }),
        (_('Sample'), {
            'fields': ('total_properties_sampled',),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Location'))
    def location_display(self, obj):
        return f"{obj.city}, {obj.country}"
    
    @display(description=_('Period'))
    def period_display(self, obj):
        return f"{obj.year}-{obj.month:02d}"
    
    @display(description=_('Avg Occupancy'))
    def avg_occupancy_rate_display(self, obj):
        if obj.avg_occupancy_rate:
            return f"{obj.avg_occupancy_rate:.1f}%"
        return '-'
    
    @display(description=_('Avg Daily Rate'))
    def avg_daily_rate_display(self, obj):
        if obj.avg_daily_rate:
            return f"${obj.avg_daily_rate:,.2f}"
        return '-'
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False
