from django.contrib import admin
from apps.properties.models import Property, Amenity, PropertyImage

@admin.register(Amenity)
class AmenityAdmin(admin.ModelAdmin):
    list_display = ['name', 'icon']
    search_fields = ['name']

class PropertyImageInline(admin.StackedInline):
    model = PropertyImage
    extra = 1

@admin.register(Property)
class PropertyAdmin(admin.ModelAdmin):
    list_display = ['title', 'host', 'city', 'country', 'price_per_night', 'status', 'created_at']
    list_filter = ['status', 'country', 'city', 'property_type', 'created_at']
    search_fields = ['title', 'description', 'address', 'host__email']
    readonly_fields = ['created_at', 'updated_at']
    inlines = [PropertyImageInline]
    fieldsets = (
        ('Basic Info', {'fields': ('host', 'title', 'description', 'property_type')}),
        ('Location', {'fields': ('location', 'country', 'city', 'suburb', 'address')}),
        ('Pricing', {'fields': ('price_per_night', 'currency')}),
        ('Details', {'fields': ('max_guests', 'bedrooms', 'bathrooms', 'amenities')}),
        ('Media', {'fields': ('main_image',)}),
        ('Status', {'fields': ('status',)}),
        ('Timestamps', {'fields': ('created_at', 'updated_at')}),
    )

@admin.register(PropertyImage)
class PropertyImageAdmin(admin.ModelAdmin):
    list_display = ['property', 'order', 'created_at']
    list_filter = ['property', 'created_at']
    ordering = ['property', 'order']
