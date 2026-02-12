from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.properties.poi_models import POICategory, PointOfInterest, PropertyPOI


@admin.register(POICategory)
class POICategoryAdmin(UnfoldModelAdmin):
    """Admin interface for POI categories"""
    
    list_display = ['name', 'icon', 'description_short']
    search_fields = ['name', 'description']
    list_per_page = 50
    
    fieldsets = (
        (_('Category Details'), {
            'fields': ('name', 'icon', 'description'),
        }),
    )
    
    @display(description=_('Description'))
    def description_short(self, obj):
        if obj.description:
            return obj.description[:60] + '...' if len(obj.description) > 60 else obj.description
        return '-'


@admin.register(PointOfInterest)
class PointOfInterestAdmin(UnfoldModelAdmin):
    """Admin interface for points of interest"""
    
    list_display = ['name', 'category', 'city', 'country', 'poi_type', 'active_badge']
    list_filter = ['category', 'poi_type', 'is_active', 'country']
    search_fields = ['name', 'city', 'country', 'address']
    readonly_fields = ['created_at', 'updated_at']
    list_select_related = ['category']
    list_per_page = 25
    actions = ['activate_pois', 'deactivate_pois']
    
    fieldsets = (
        (_('Basic Information'), {
            'fields': ('name', 'category', 'poi_type', 'is_active'),
        }),
        (_('Location'), {
            'fields': ('location', 'address', 'city', 'country'),
        }),
        (_('Details'), {
            'fields': ('description', 'website', 'phone', 'opening_hours',
                       'rating', 'review_count', 'price_level', 'image_url'),
            'classes': ['collapse'],
        }),
        (_('Metadata'), {
            'fields': ('source', 'external_id', 'created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Active'), label=True)
    def active_badge(self, obj):
        return {
            'value': 'Active' if obj.is_active else 'Inactive',
            'color': 'success' if obj.is_active else 'warning',
        }
    
    @admin.action(description=_('Activate selected POIs'))
    def activate_pois(self, request, queryset):
        updated = queryset.update(is_active=True)
        self.message_user(request, f'{updated} POI(s) activated.')
    
    @admin.action(description=_('Deactivate selected POIs'))
    def deactivate_pois(self, request, queryset):
        updated = queryset.update(is_active=False)
        self.message_user(request, f'{updated} POI(s) deactivated.')


@admin.register(PropertyPOI)
class PropertyPOIAdmin(UnfoldModelAdmin):
    """Admin interface for property-POI relationships"""
    
    list_display = ['property_display', 'poi_display', 'distance_display', 
                    'walking_time_minutes', 'driving_time_minutes', 'created_at']
    list_filter = ['is_recommended', 'created_at']
    search_fields = ['linked_property__title', 'poi__name']
    readonly_fields = ['linked_property', 'poi', 'created_at', 'updated_at']
    list_select_related = ['linked_property', 'poi', 'poi__category']
    list_per_page = 25
    
    fieldsets = (
        (_('Relationship'), {
            'fields': ('linked_property', 'poi'),
        }),
        (_('Distance & Travel'), {
            'fields': ('distance_meters', 'walking_time_minutes', 'driving_time_minutes'),
        }),
        (_('Host Info'), {
            'fields': ('host_notes', 'is_recommended'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Property'))
    def property_display(self, obj):
        return obj.linked_property.title
    
    @display(description=_('POI'))
    def poi_display(self, obj):
        return f"{obj.poi.name} ({obj.poi.category.name})"
    
    @display(description=_('Distance'))
    def distance_display(self, obj):
        if obj.distance_meters:
            if obj.distance_meters >= 1000:
                return f"{obj.distance_meters / 1000:.1f} km"
            return f"{obj.distance_meters:.0f} m"
        return '-'
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False
