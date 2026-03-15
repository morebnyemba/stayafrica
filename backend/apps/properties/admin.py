from django.contrib import admin
from django import forms
from django.contrib import messages
from django.template.response import TemplateResponse
from django.urls import path, reverse
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin, TabularInline as UnfoldTabularInline
from unfold.decorators import display
from apps.properties.models import Property, Amenity, PropertyImage, SavedProperty


class AmenityBulkAddForm(forms.Form):
    amenities = forms.CharField(
        label=_('Amenities'),
        widget=forms.Textarea(
            attrs={
                'rows': 16,
                'placeholder': 'WiFi\nPool\nAir Conditioning\nKitchen\nWasher',
            }
        ),
        help_text=_('Enter one amenity per line. Commas and semicolons are also supported.'),
    )


@admin.register(Amenity)
class AmenityAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Amenity management"""

    change_list_template = 'admin/properties/amenity/change_list.html'
    
    list_display = ['name', 'icon', 'description_short']
    search_fields = ['name', 'description']
    list_filter = ['name']
    list_per_page = 50
    
    fieldsets = (
        (_('Amenity Details'), {
            'fields': ('name', 'icon', 'description'),
        }),
    )

    def get_urls(self):
        urls = super().get_urls()
        custom_urls = [
            path(
                'bulk-add/',
                self.admin_site.admin_view(self.bulk_add_view),
                name='properties_amenity_bulk_add',
            ),
        ]
        return custom_urls + urls

    def bulk_add_view(self, request):
        form = AmenityBulkAddForm(request.POST or None)

        if request.method == 'POST' and form.is_valid():
            raw_value = form.cleaned_data['amenities']
            parsed_names = []
            seen_names = set()

            normalized_value = raw_value.replace(';', '\n').replace(',', '\n')
            for line in normalized_value.splitlines():
                name = line.strip()
                if not name:
                    continue

                lookup_key = name.casefold()
                if lookup_key in seen_names:
                    continue

                seen_names.add(lookup_key)
                parsed_names.append(name)

            created_count = 0
            existing_count = 0

            for name in parsed_names:
                _, created = Amenity.objects.get_or_create(name=name)
                if created:
                    created_count += 1
                else:
                    existing_count += 1

            if created_count:
                self.message_user(
                    request,
                    _(f'{created_count} amenity(s) created successfully.'),
                    level=messages.SUCCESS,
                )

            if existing_count:
                self.message_user(
                    request,
                    _(f'{existing_count} amenity(s) already existed and were skipped.'),
                    level=messages.INFO,
                )

            return admin.shortcuts.redirect(reverse('admin:properties_amenity_changelist'))

        context = {
            **self.admin_site.each_context(request),
            'opts': self.model._meta,
            'title': _('Bulk add amenities'),
            'form': form,
        }
        return TemplateResponse(request, 'admin/properties/amenity/bulk_add.html', context)

    def changelist_view(self, request, extra_context=None):
        extra_context = extra_context or {}
        extra_context['bulk_add_url'] = reverse('admin:properties_amenity_bulk_add')
        return super().changelist_view(request, extra_context=extra_context)

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
    @display(description=_('Status'), ordering='status', label={"Active": "success", "Inactive": "danger", "Pending Approval": "warning"})
    def status_badge(self, obj):
        return obj.get_status_display()

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


# ── POI Admin ─────────────────────────────────────────────────────────────

from apps.properties.poi_models import POICategory, PointOfInterest, PropertyPOI


@admin.register(POICategory)
class POICategoryAdmin(UnfoldModelAdmin):
    list_display = ['name', 'icon', 'color_swatch', 'display_order', 'poi_count']
    search_fields = ['name']
    list_editable = ['display_order']
    ordering = ['display_order', 'name']

    @display(description=_('Color'))
    def color_swatch(self, obj):
        return format_html(
            '<span style="display:inline-block;width:18px;height:18px;border-radius:50%;'
            'background:{};border:1px solid #ccc;vertical-align:middle;"></span> {}',
            obj.color, obj.color
        )

    @display(description=_('POIs'))
    def poi_count(self, obj):
        return obj.pois.count()


@admin.register(PointOfInterest)
class PointOfInterestAdmin(UnfoldModelAdmin):
    list_display = [
        'name', 'poi_type_badge', 'city', 'country',
        'rating_display', 'source_badge', 'active_badge', 'created_at'
    ]
    list_filter = ['poi_type', 'source', 'is_active', 'city', 'country']
    search_fields = ['name', 'address', 'city', 'external_id']
    readonly_fields = ['id', 'created_at', 'updated_at']
    list_per_page = 50
    date_hierarchy = 'created_at'

    fieldsets = (
        (_('Basic Info'), {
            'fields': ('id', 'name', 'category', 'poi_type', 'description'),
        }),
        (_('Location'), {
            'fields': ('location', 'address', 'city', 'country'),
        }),
        (_('Contact & Details'), {
            'fields': ('phone', 'website', 'rating', 'review_count', 'price_level', 'image_url'),
        }),
        (_('Hours & Source'), {
            'fields': ('opening_hours', 'source', 'external_id', 'is_active'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )

    @display(description=_('Type'), label={
        "Restaurant": "info", "Cafe": "info", "Bar": "info",
        "Hospital": "danger", "Pharmacy": "danger",
        "Attraction": "success", "Museum": "success", "Park": "success", "Beach": "success",
        "Transport": "warning", "Shopping": "secondary",
    })
    def poi_type_badge(self, obj):
        return obj.get_poi_type_display()

    @display(description=_('Rating'))
    def rating_display(self, obj):
        if obj.rating:
            return f"⭐ {obj.rating}"
        return '-'

    @display(description=_('Source'), label={"manual": "secondary", "google_places": "info", "openstreetmap": "success"})
    def source_badge(self, obj):
        return obj.source

    @display(description=_('Active'), label={"Active": "success", "Inactive": "danger"})
    def active_badge(self, obj):
        return "Active" if obj.is_active else "Inactive"


@admin.register(PropertyPOI)
class PropertyPOIAdmin(UnfoldModelAdmin):
    list_display = [
        'property_display', 'poi_display', 'distance_display',
        'walking_time', 'recommended_badge', 'created_at'
    ]
    list_filter = ['is_recommended', 'created_at']
    search_fields = ['linked_property__title', 'poi__name']
    readonly_fields = ['created_at', 'updated_at']
    list_select_related = ['linked_property', 'poi']
    raw_id_fields = ['linked_property', 'poi']
    list_per_page = 50

    @display(description=_('Property'))
    def property_display(self, obj):
        return obj.linked_property.title

    @display(description=_('POI'))
    def poi_display(self, obj):
        return f"{obj.poi.name} ({obj.poi.get_poi_type_display()})"

    @display(description=_('Distance'))
    def distance_display(self, obj):
        return obj.distance_display

    @display(description=_('Walk'))
    def walking_time(self, obj):
        return f"{obj.walking_time_minutes} min" if obj.walking_time_minutes else '-'

    @display(description=_('Recommended'), label={"⭐ Recommended": "success", "No": "secondary"})
    def recommended_badge(self, obj):
        return "⭐ Recommended" if obj.is_recommended else "No"


# ── Analytics Admin ───────────────────────────────────────────────────────

from apps.properties.analytics_models import (
    PropertyAnalytics, HostAnalyticsSummary,
    RevenueProjection, PerformanceBenchmark
)


@admin.register(PropertyAnalytics)
class PropertyAnalyticsAdmin(UnfoldModelAdmin):
    list_display = [
        'property_display', 'date', 'revenue_display', 'bookings_count',
        'occupancy_display', 'views_count', 'conversion_display'
    ]
    list_filter = ['date', 'property__city']
    search_fields = ['property__title', 'property__host__email']
    readonly_fields = ['created_at', 'updated_at']
    date_hierarchy = 'date'
    list_per_page = 50
    ordering = ['-date']

    @display(description=_('Property'))
    def property_display(self, obj):
        return obj.property.title

    @display(description=_('Revenue'))
    def revenue_display(self, obj):
        return f"${obj.total_revenue:,.2f}"

    @display(description=_('Occupancy'))
    def occupancy_display(self, obj):
        return f"{obj.occupancy_rate}%"

    @display(description=_('Conversion'))
    def conversion_display(self, obj):
        return f"{obj.conversion_rate}%"

    def has_add_permission(self, request):
        return False


@admin.register(HostAnalyticsSummary)
class HostAnalyticsSummaryAdmin(UnfoldModelAdmin):
    list_display = [
        'host_display', 'period_badge', 'date_range', 'revenue_display',
        'total_bookings', 'occupancy_display', 'avg_rating_display'
    ]
    list_filter = ['period', 'start_date']
    search_fields = ['host__email', 'host__first_name', 'host__last_name']
    readonly_fields = ['created_at', 'updated_at']
    date_hierarchy = 'start_date'
    ordering = ['-start_date']

    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.get_full_name() or obj.host.email

    @display(description=_('Period'), label={"Daily": "secondary", "Weekly": "info", "Monthly": "success", "Yearly": "warning"})
    def period_badge(self, obj):
        return obj.get_period_display()

    @display(description=_('Date Range'))
    def date_range(self, obj):
        return f"{obj.start_date} → {obj.end_date}"

    @display(description=_('Revenue'))
    def revenue_display(self, obj):
        return f"${obj.total_revenue:,.2f}"

    @display(description=_('Occupancy'))
    def occupancy_display(self, obj):
        return f"{obj.avg_occupancy_rate}%"

    @display(description=_('Rating'))
    def avg_rating_display(self, obj):
        return f"⭐ {obj.avg_rating}" if obj.avg_rating else '-'

    def has_add_permission(self, request):
        return False


@admin.register(RevenueProjection)
class RevenueProjectionAdmin(UnfoldModelAdmin):
    list_display = [
        'host_display', 'property_display', 'target_month',
        'revenue_display', 'confidence_display', 'projection_date'
    ]
    list_filter = ['target_month', 'projection_date']
    search_fields = ['host__email', 'property__title']
    readonly_fields = ['created_at']
    ordering = ['-target_month']

    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.email

    @display(description=_('Property'))
    def property_display(self, obj):
        return obj.property.title if obj.property else 'All Properties'

    @display(description=_('Projected Revenue'))
    def revenue_display(self, obj):
        return f"${obj.projected_revenue:,.2f}"

    @display(description=_('Confidence'), label=True)
    def confidence_display(self, obj):
        return f"{obj.confidence_level}%"

    def has_add_permission(self, request):
        return False


@admin.register(PerformanceBenchmark)
class PerformanceBenchmarkAdmin(UnfoldModelAdmin):
    list_display = [
        'region', 'property_type', 'month', 'occupancy_display',
        'rate_display', 'sample_size'
    ]
    list_filter = ['region', 'property_type', 'month']
    search_fields = ['region', 'property_type']
    readonly_fields = ['created_at', 'updated_at']
    date_hierarchy = 'month'
    ordering = ['-month']

    @display(description=_('Avg Occupancy'))
    def occupancy_display(self, obj):
        return f"{obj.avg_occupancy_rate}%"

    @display(description=_('Avg Nightly Rate'))
    def rate_display(self, obj):
        return f"${obj.avg_nightly_rate:,.2f}"

    def has_add_permission(self, request):
        return False


# ── Wishlist Admin ────────────────────────────────────────────────────────

from apps.properties.wishlist_models import Wishlist, WishlistItem, WishlistVote, WishlistComment


@admin.register(Wishlist)
class WishlistAdmin(UnfoldModelAdmin):
    list_display = [
        'name', 'owner_display', 'privacy_badge', 'item_count',
        'collaborator_count', 'updated_at'
    ]
    list_filter = ['privacy', 'created_at']
    search_fields = ['name', 'owner__email', 'description']
    readonly_fields = ['id', 'share_token', 'created_at', 'updated_at']
    filter_horizontal = ['collaborators']
    list_select_related = ['owner']

    @display(description=_('Owner'))
    def owner_display(self, obj):
        return obj.owner.get_full_name() or obj.owner.email

    @display(description=_('Privacy'), label={"Private": "secondary", "Shared with Collaborators": "info", "Public": "success"})
    def privacy_badge(self, obj):
        return obj.get_privacy_display()

    @display(description=_('Items'))
    def item_count(self, obj):
        return obj.items.count()

    @display(description=_('Collaborators'))
    def collaborator_count(self, obj):
        return obj.collaborators.count()


@admin.register(WishlistItem)
class WishlistItemAdmin(UnfoldModelAdmin):
    list_display = [
        'property_display', 'wishlist_display', 'added_by_display',
        'votes', 'date_range', 'added_at'
    ]
    list_filter = ['added_at', 'wishlist__privacy']
    search_fields = ['property__title', 'wishlist__name', 'added_by__email']
    readonly_fields = ['added_at', 'updated_at', 'votes']
    list_select_related = ['property', 'wishlist', 'added_by']

    @display(description=_('Property'))
    def property_display(self, obj):
        return obj.property.title

    @display(description=_('Wishlist'))
    def wishlist_display(self, obj):
        return obj.wishlist.name

    @display(description=_('Added By'))
    def added_by_display(self, obj):
        return obj.added_by.email if obj.added_by else '-'

    @display(description=_('Dates'))
    def date_range(self, obj):
        if obj.preferred_check_in and obj.preferred_check_out:
            return f"{obj.preferred_check_in} → {obj.preferred_check_out}"
        return '-'


@admin.register(WishlistVote)
class WishlistVoteAdmin(UnfoldModelAdmin):
    list_display = ['user_display', 'item_display', 'vote_badge', 'created_at']
    list_filter = ['vote', 'created_at']
    search_fields = ['user__email', 'wishlist_item__property__title']
    readonly_fields = ['user', 'wishlist_item', 'vote', 'created_at']
    list_select_related = ['user', 'wishlist_item__property']

    @display(description=_('User'))
    def user_display(self, obj):
        return obj.user.email

    @display(description=_('Item'))
    def item_display(self, obj):
        return obj.wishlist_item.property.title

    @display(description=_('Vote'), label={"👍 Upvote": "success", "👎 Downvote": "danger"})
    def vote_badge(self, obj):
        return "👍 Upvote" if obj.vote == 1 else "👎 Downvote"

    def has_add_permission(self, request):
        return False

    def has_change_permission(self, request, obj=None):
        return False


@admin.register(WishlistComment)
class WishlistCommentAdmin(UnfoldModelAdmin):
    list_display = ['user_display', 'item_display', 'text_preview', 'created_at']
    list_filter = ['created_at']
    search_fields = ['user__email', 'text', 'wishlist_item__property__title']
    readonly_fields = ['created_at', 'updated_at']
    list_select_related = ['user', 'wishlist_item__property']

    @display(description=_('User'))
    def user_display(self, obj):
        return obj.user.email

    @display(description=_('Item'))
    def item_display(self, obj):
        return obj.wishlist_item.property.title

    @display(description=_('Comment'))
    def text_preview(self, obj):
        return obj.text[:80] + '...' if len(obj.text) > 80 else obj.text
