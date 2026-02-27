from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin, TabularInline as UnfoldTabularInline
from unfold.decorators import display
from apps.properties.models import Property, Amenity, PropertyImage, SavedProperty


@admin.register(Amenity)
class AmenityAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Amenity management"""
    
    list_display = ['name', 'icon', 'description_short']
    search_fields = ['name', 'description']
    list_filter = ['name']
    list_per_page = 50
    
    fieldsets = (
        (_('Amenity Details'), {
            'fields': ('name', 'icon', 'description'),
        }),
    )

    @display(description=_('Description'))
    def description_short(self, obj):
        if obj.description:
            return obj.description[:50] + '...' if len(obj.description) > 50 else obj.description
        return '-'


class PropertyImageInline(UnfoldTabularInline):
    """Inline for property images with better display"""
    model = PropertyImage
    extra = 1
    readonly_fields = ['image_preview', 'created_at']
    fields = ['image', 'image_preview', 'order', 'created_at']
    
    @display(description=_('Preview'))
    def image_preview(self, obj):
        if obj.image:
            from django.utils.html import escape
            # Escape the URL to prevent XSS
            safe_url = escape(obj.image.url)
            return format_html(
                '<img src="{}" style="max-height: 100px; max-width: 150px; border-radius: 4px; '
                'border: 2px solid #D9B168;" />',
                safe_url
            )
        return '-'


@admin.register(Property)
class PropertyAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Property management"""
    
    list_display = [
        'title', 'host_display', 'location_display', 'price_display', 
        'capacity_display', 'status_badge', 'created_at'
    ]
    list_filter = ['status', 'country', 'city', 'property_type', 'created_at']
    search_fields = ['title', 'description', 'address', 'host__email', 'host__username']
    list_select_related = ['host']
    readonly_fields = ['created_at', 'updated_at', 'id', 'property_summary']
    inlines = [PropertyImageInline]
    list_per_page = 25
    
    actions = [
        'mark_active', 'mark_inactive', 'approve_property', 
        'reject_property', 'publish_property', 'unpublish_property'
    ]
    
    fieldsets = (
        (_('Basic Information'), {
            'fields': ('title', 'host', 'property_summary', 'description', 'property_type', 'status'),
            'classes': ['tab'],
        }),
        (_('Location'), {
            'fields': ('location', 'country', 'city', 'suburb', 'address'),
            'classes': ['tab'],
            'description': 'Specify the property location details',
        }),
        (_('Pricing'), {
            'fields': ('price_per_night', 'currency'),
            'classes': ['tab'],
        }),
        (_('Capacity & Features'), {
            'fields': ('max_guests', 'bedrooms', 'bathrooms', 'amenities'),
            'classes': ['tab'],
        }),
        (_('Media'), {
            'fields': ('main_image',),
            'classes': ['tab'],
        }),
        (_('Metadata'), {
            'fields': ('id', 'created_at', 'updated_at'),
            'classes': ['collapse', 'tab'],
        }),
    )
    
    filter_horizontal = ['amenities']

    # Custom action methods
    @admin.action(description=_('Mark selected properties as active'))
    def mark_active(self, request, queryset):
        updated = queryset.update(status='active')
        self.message_user(request, f'{updated} property(ies) marked as active.')

    @admin.action(description=_('Mark selected properties as inactive'))
    def mark_inactive(self, request, queryset):
        updated = queryset.update(status='inactive')
        self.message_user(request, f'{updated} property(ies) marked as inactive.')

    @admin.action(description=_('Approve property'))
    def approve_property(self, request, queryset):
        updated = queryset.update(status='active')
        self.message_user(request, f'{updated} property(ies) approved.')

    @admin.action(description=_('Reject property'))
    def reject_property(self, request, queryset):
        updated = queryset.update(status='inactive')
        self.message_user(request, f'{updated} property(ies) rejected.')

    @admin.action(description=_('Publish property'))
    def publish_property(self, request, queryset):
        updated = queryset.update(status='active')
        self.message_user(request, f'{updated} property(ies) published.')

    @admin.action(description=_('Unpublish property'))
    def unpublish_property(self, request, queryset):
        updated = queryset.update(status='inactive')
        self.message_user(request, f'{updated} property(ies) unpublished.')

    # Custom display methods
    @display(description=_('Status'), ordering='status', label=True)
    def status_badge(self, obj):
        colors = {
            'active': 'success',
            'inactive': 'danger',
            'pending_approval': 'warning',
        }
        return {
            'value': obj.get_status_display(),
            'color': colors.get(obj.status, 'secondary'),
        }

    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.get_full_name() or obj.host.email

    @display(description=_('Location'))
    def location_display(self, obj):
        return f"{obj.city}, {obj.country}"

    @display(description=_('Price'), ordering='price_per_night')
    def price_display(self, obj):
        return f"{obj.currency} {obj.price_per_night:.2f}/night"

    @display(description=_('Capacity'))
    def capacity_display(self, obj):
        return f"👥 {obj.max_guests} | 🛏️ {obj.bedrooms} | 🚿 {obj.bathrooms}"

    @display(description=_('Summary'))
    def property_summary(self, obj):
        if obj.id:
            amenities_count = obj.amenities.count()
            images_count = obj.images.count()
            summary = f"""
            <div style="padding: 10px; background: #F4F1EA; border-left: 3px solid #D9B168; border-radius: 4px;">
                <strong style="color: #122F26;">Property Summary</strong><br/>
                <span style="color: #3A5C50;">
                    Type: {obj.get_property_type_display()}<br/>
                    Location: {obj.city}, {obj.country}<br/>
                    Price: {obj.currency} {obj.price_per_night:.2f} per night<br/>
                    Capacity: {obj.max_guests} guests, {obj.bedrooms} bedrooms, {obj.bathrooms} bathrooms<br/>
                    Amenities: {amenities_count} | Images: {images_count}<br/>
                    Status: {obj.get_status_display()}
                </span>
            </div>
            """
            return format_html(summary)
        return "Save to see summary"


@admin.register(PropertyImage)
class PropertyImageAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Property Image management"""
    
    list_display = ['property_display', 'image_preview', 'order', 'created_at']
    list_filter = ['property', 'created_at', 'property__city', 'property__country']
    search_fields = ['property__title', 'property__host__email']
    ordering = ['property', 'order']
    readonly_fields = ['created_at', 'image_preview_large']
    list_select_related = ['property']
    
    fieldsets = (
        (_('Image Details'), {
            'fields': ('property', 'image', 'image_preview_large', 'order'),
        }),
        (_('Metadata'), {
            'fields': ('created_at',),
            'classes': ['collapse'],
        }),
    )

    @display(description=_('Property'))
    def property_display(self, obj):
        return obj.property.title

    @display(description=_('Preview'))
    def image_preview(self, obj):
        if obj.image:
            from django.utils.html import escape
            safe_url = escape(obj.image.url)
            return format_html(
                '<img src="{}" style="max-height: 50px; max-width: 75px; border-radius: 4px;" />',
                safe_url
            )
        return '-'

    @display(description=_('Image Preview'))
    def image_preview_large(self, obj):
        if obj.image:
            from django.utils.html import escape
            safe_url = escape(obj.image.url)
            return format_html(
                '<img src="{}" style="max-width: 400px; border-radius: 8px; '
                'border: 3px solid #D9B168; box-shadow: 0 4px 6px rgba(0,0,0,0.1);" />',
                safe_url
            )
        return 'No image uploaded'


@admin.register(SavedProperty)
class SavedPropertyAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Saved Property management"""
    
    list_display = ['user_display', 'property_display', 'created_at']
    list_filter = ['created_at', 'property__city', 'property__country']
    search_fields = ['user__email', 'user__username', 'property__title']
    readonly_fields = ['created_at']
    ordering = ['-created_at']
    list_select_related = ['user', 'property']
    
    fieldsets = (
        (_('Saved Property Details'), {
            'fields': ('user', 'property', 'created_at'),
        }),
    )

    @display(description=_('User'))
    def user_display(self, obj):
        return obj.user.get_full_name() or obj.user.email

    @display(description=_('Property'))
    def property_display(self, obj):
        return f"{obj.property.title} ({obj.property.city})"
