from django.contrib import admin
from apps.bookings.models import Booking

@admin.register(Booking)
class BookingAdmin(admin.ModelAdmin):
    list_display = ['booking_ref', 'guest', 'rental_property', 'check_in', 'check_out', 'grand_total', 'status']
    list_filter = ['status', 'created_at', 'check_in']
    search_fields = ['booking_ref', 'guest__email', 'rental_property__title']
    readonly_fields = ['booking_ref', 'commission_fee', 'grand_total', 'created_at', 'updated_at']
