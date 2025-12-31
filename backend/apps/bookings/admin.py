from django.contrib import admin
from django.utils.html import format_html
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from apps.bookings.models import Booking


@admin.register(Booking)
class BookingAdmin(UnfoldModelAdmin):
    list_display = [
        'booking_ref', 'guest', 'rental_property', 'check_in', 'check_out', 'grand_total', 'status_badge', 'created_at'
    ]
    list_filter = ['status', 'created_at', 'check_in', 'check_out', 'currency']
    search_fields = ['booking_ref', 'guest__email', 'rental_property__title']
    readonly_fields = ['booking_ref', 'commission_fee', 'grand_total', 'created_at', 'updated_at']
    raw_id_fields = ['rental_property', 'guest']
    date_hierarchy = 'created_at'

    list_select_related = ['guest', 'rental_property']
    actions = ['mark_confirmed', 'mark_cancelled', 'process_refund', 'mark_paid']

    def mark_confirmed(self, request, queryset):
        queryset.update(status='confirmed')
    mark_confirmed.short_description = 'Mark selected bookings as confirmed'

    def mark_cancelled(self, request, queryset):
        queryset.update(status='cancelled')
    mark_cancelled.short_description = 'Mark selected bookings as cancelled'

    def process_refund(self, request, queryset):
        queryset.update(status='cancelled')
    process_refund.short_description = 'Process refund (mark cancelled)'

    def mark_paid(self, request, queryset):
        queryset.update(status='confirmed')
    mark_paid.short_description = 'Mark selected bookings as paid/confirmed'

    @admin.display(description='Status')
    def status_badge(self, obj):
        colors = {
            'pending': 'bg-amber-100 text-amber-700',
            'confirmed': 'bg-blue-100 text-blue-700',
            'completed': 'bg-green-100 text-green-700',
            'cancelled': 'bg-red-100 text-red-700',
        }
        klass = colors.get(obj.status, 'bg-gray-100 text-gray-700')
        return format_html('<span class="{} px-2 py-1 rounded">{}</span>', klass, obj.get_status_display())
