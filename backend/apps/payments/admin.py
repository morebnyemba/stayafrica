from django.contrib import admin
from apps.payments.models import Payment

@admin.register(Payment)
class PaymentAdmin(admin.ModelAdmin):
    list_display = ['gateway_ref', 'booking', 'provider', 'status', 'amount', 'created_at']
    list_filter = ['provider', 'status', 'created_at']
    search_fields = ['gateway_ref', 'booking__booking_ref']
    readonly_fields = ['gateway_ref', 'created_at', 'updated_at']
