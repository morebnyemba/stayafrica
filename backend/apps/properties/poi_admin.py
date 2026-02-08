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
    
    list_display = ['name', 'category', 'city', 'country', 'poi_type', 'verified_badge']
    list_filter = ['category', 'poi_type', 'is_verified', 'country']
    search_fields = ['name', 'city', 'country', 'address']
    readonly_fields = ['created_at', 'updated_at']
    list_select_related = ['category']
    list_per_page = 25
    actions = ['verify_pois', 'unverify_pois']
    
    fieldsets = (
        (_('Basic Information'), {
            'fields': ('name', 'category', 'poi_type', 'is_verified'),
        }),
        (_('Location'), {
            'fields': ('latitude', 'longitude', 'address', 'city', 'country'),
        }),
        (_('Details'), {
            'fields': ('description', 'website', 'phone', 'opening_hours'),
            'classes': ['collapse'],
        }),
        (_('Metadata'), {
            'fields': ('google_place_id', 'created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Verified'), label=True)
    def verified_badge(self, obj):
        return {
            'value': 'Verified' if obj.is_verified else 'Unverified',
            'color': 'success' if obj.is_verified else 'warning',
        }
    
    @admin.action(description=_('Mark selected POIs as verified'))
    def verify_pois(self, request, queryset):
        updated = queryset.update(is_verified=True)
        self.message_user(request, f'{updated} POI(s) verified.')
    
    @admin.action(description=_('Mark selected POIs as unverified'))
    def unverify_pois(self, request, queryset):
        updated = queryset.update(is_verified=False)
        self.message_user(request, f'{updated} POI(s) unverified.')


@admin.register(PropertyPOI)
class PropertyPOIAdmin(UnfoldModelAdmin):
    """Admin interface for property-POI relationships"""
    
    list_display = ['property_display', 'poi_display', 'distance_display', 'travel_time', 'created_at']
    list_filter = ['transport_mode', 'created_at']
    search_fields = ['property__title', 'poi__name']
    readonly_fields = ['property', 'poi', 'distance_km', 'created_at', 'updated_at']
    list_select_related = ['property', 'poi', 'poi__category']
    list_per_page = 25
    
    fieldsets = (
        (_('Relationship'), {
            'fields': ('property', 'poi'),
        }),
        (_('Distance & Travel'), {
            'fields': ('distance_km', 'travel_time', 'transport_mode'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Property'))
    def property_display(self, obj):
        return obj.property.title
    
    @display(description=_('POI'))
    def poi_display(self, obj):
        return f"{obj.poi.name} ({obj.poi.category.name})"
    
    @display(description=_('Distance'))
    def distance_display(self, obj):
        return f"{obj.distance_km:.2f} km"
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False
