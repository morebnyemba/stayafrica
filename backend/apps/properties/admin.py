from django.contrib import admin
from django.utils.html import format_html
from unfold.admin import ModelAdmin as UnfoldModelAdmin, StackedInline as UnfoldStackedInline
from apps.properties.models import Property, Amenity, PropertyImage, SavedProperty


@admin.register(Amenity)
class AmenityAdmin(UnfoldModelAdmin):
    list_display = ['name', 'icon']
    search_fields = ['name', 'description']
    list_filter = ['name']


class PropertyImageInline(UnfoldStackedInline):
    model = PropertyImage
    extra = 1
    readonly_fields = ['created_at']


@admin.register(Property)
class PropertyAdmin(UnfoldModelAdmin):
    list_display = [
        'title', 'host', 'city', 'country', 'price_per_night', 'status_badge', 'created_at'
    ]
    list_filter = ['status', 'country', 'city', 'property_type', 'created_at']
    search_fields = ['title', 'description', 'address', 'host__email']
    list_select_related = ['host']
    readonly_fields = ['created_at', 'updated_at', 'id']
    inlines = [PropertyImageInline]
    actions = ['mark_active', 'mark_inactive', 'approve_property', 'reject_property', 'publish_property', 'unpublish_property']
    fieldsets = (
        ('Basic Info', {'fields': ('host', 'title', 'description', 'property_type')}),
        ('Location', {'fields': ('location', 'country', 'city', 'suburb', 'address')}),
        ('Pricing', {'fields': ('price_per_night', 'currency')}),
        ('Details', {'fields': ('max_guests', 'bedrooms', 'bathrooms', 'amenities')}),
        ('Media', {'fields': ('main_image',)}),
        ('Status', {'fields': ('status',)}),
        ('Timestamps', {'fields': ('created_at', 'updated_at', 'id')}),
    )

    @admin.display(description='Status')
    def status_badge(self, obj):
        colors = {
            'active': 'bg-green-100 text-green-700',
            'inactive': 'bg-red-100 text-red-700',
            'pending_approval': 'bg-amber-100 text-amber-700',
        }
        klass = colors.get(obj.status, 'bg-gray-100 text-gray-700')
        return format_html('<span class="{} px-2 py-1 rounded">{}</span>', klass, obj.get_status_display())

    def mark_active(self, request, queryset):
        queryset.update(status='active')
    mark_active.short_description = 'Mark selected properties as active'

    def mark_inactive(self, request, queryset):
        queryset.update(status='inactive')
    mark_inactive.short_description = 'Mark selected properties as inactive'

    def approve_property(self, request, queryset):
        queryset.update(status='active')
    approve_property.short_description = 'Approve property'

    def reject_property(self, request, queryset):
        queryset.update(status='inactive')
    reject_property.short_description = 'Reject property'

    def publish_property(self, request, queryset):
        queryset.update(status='active')
    publish_property.short_description = 'Publish property'

    def unpublish_property(self, request, queryset):
        queryset.update(status='inactive')
    unpublish_property.short_description = 'Unpublish property'


@admin.register(PropertyImage)
class PropertyImageAdmin(UnfoldModelAdmin):
    list_display = ['property', 'order', 'created_at']
    list_filter = ['created_at']
    search_fields = ['property__title']
    ordering = ['property', 'order']
    readonly_fields = ['created_at']


@admin.register(SavedProperty)
class SavedPropertyAdmin(UnfoldModelAdmin):
    list_display = ['user', 'property', 'created_at']
    list_filter = ['created_at']
    search_fields = ['user__email', 'property__title']
    readonly_fields = ['created_at']
    ordering = ['-created_at']
