from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.bookings.models import Booking


@admin.register(Booking)
class BookingAdmin(UnfoldModelAdmin):
    """
    Enhanced admin interface for Booking management with StayAfrica theme
    """
    
    # Display configuration
    list_display = [
        'booking_ref', 'guest_display', 'property_display', 'check_in', 'check_out', 
        'nights_count', 'total_display', 'status_badge', 'created_at'
    ]
    list_filter = ['status', 'created_at', 'check_in', 'check_out', 'currency']
    search_fields = ['booking_ref', 'guest__email', 'guest__username', 'rental_property__title']
    readonly_fields = [
        'booking_ref', 'commission_fee', 'grand_total', 'created_at', 'updated_at',
        'nights_count', 'booking_summary'
    ]
    
    list_select_related = ['guest', 'rental_property']
    list_per_page = 25
    date_hierarchy = 'created_at'
    
    # Fieldsets for better organization
    fieldsets = (
        (_('Booking Information'), {
            'fields': ('booking_ref', 'booking_summary', 'status'),
            'classes': ['tab'],
        }),
        (_('Guest & Property'), {
            'fields': ('guest', 'rental_property'),
            'classes': ['tab'],
        }),
        (_('Booking Details'), {
            'fields': ('check_in', 'check_out', 'nights_count', 'number_of_guests'),
            'classes': ['tab'],
        }),
        (_('Pricing'), {
            'fields': ('base_price', 'commission_fee', 'grand_total', 'currency'),
            'classes': ['tab'],
        }),
        (_('Additional Information'), {
            'fields': ('special_requests',),
            'classes': ['collapse', 'tab'],
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse', 'tab'],
        }),
    )
    
    # Actions
    actions = ['mark_confirmed', 'mark_cancelled', 'mark_completed', 'mark_paid']
    actions_selection_counter = True

    # Custom action methods
    @admin.action(description=_('Mark selected bookings as confirmed'))
    def mark_confirmed(self, request, queryset):
        updated = queryset.update(status='confirmed')
        self.message_user(request, f'{updated} booking(s) confirmed successfully.')

    @admin.action(description=_('Mark selected bookings as cancelled'))
    def mark_cancelled(self, request, queryset):
        updated = queryset.update(status='cancelled')
        self.message_user(request, f'{updated} booking(s) cancelled successfully.')

    @admin.action(description=_('Mark selected bookings as completed'))
    def mark_completed(self, request, queryset):
        updated = queryset.update(status='completed')
        self.message_user(request, f'{updated} booking(s) marked as completed.')

    @admin.action(description=_('Mark selected bookings as paid/confirmed'))
    def mark_paid(self, request, queryset):
        updated = queryset.update(status='confirmed')
        self.message_user(request, f'{updated} booking(s) marked as paid.')

    # Custom display methods
    @display(description=_('Status'), ordering='status', label=True)
    def status_badge(self, obj):
        """Display status with StayAfrica themed badges"""
        colors = {
            'pending': 'warning',      # Safari Gold theme
            'confirmed': 'info',       # Blue/Moss theme
            'completed': 'success',    # Green theme
            'cancelled': 'danger',     # Red theme
        }
        return {
            'value': obj.get_status_display(),
            'color': colors.get(obj.status, 'secondary'),
        }

    @display(description=_('Guest'), ordering='guest__email')
    def guest_display(self, obj):
        return f"{obj.guest.get_full_name() or obj.guest.email}"

    @display(description=_('Property'), ordering='rental_property__title')
    def property_display(self, obj):
        return obj.rental_property.title

    @display(description=_('Total'), ordering='grand_total')
    def total_display(self, obj):
        return f"{obj.currency} {obj.grand_total:.2f}"

    @display(description=_('Nights'))
    def nights_count(self, obj):
        if obj.check_in and obj.check_out:
            return (obj.check_out - obj.check_in).days
        return 'N/A'

    @display(description=_('Summary'))
    def booking_summary(self, obj):
        """Display a formatted summary of the booking"""
        if obj.id:
            nights = (obj.check_out - obj.check_in).days if obj.check_in and obj.check_out else 0
            summary = f"""
            <div style="padding: 10px; background: #F4F1EA; border-left: 3px solid #D9B168; border-radius: 4px;">
                <strong style="color: #122F26;">Booking Summary</strong><br/>
                <span style="color: #3A5C50;">
                    {obj.guest.get_full_name() or obj.guest.email} booking {obj.rental_property.title}<br/>
                    {obj.check_in.strftime('%B %d, %Y')} - {obj.check_out.strftime('%B %d, %Y')} ({nights} nights)<br/>
                    Total: {obj.currency} {obj.grand_total:.2f}
                </span>
            </div>
            """
            return format_html(summary)
        return "Save to see summary"
