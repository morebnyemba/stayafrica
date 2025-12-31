from django.contrib import admin
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from apps.admin_dashboard.models import SystemConfiguration, AuditLog, AdminStats


@admin.register(SystemConfiguration)
class SystemConfigurationAdmin(UnfoldModelAdmin):
    """Admin interface for system configuration (singleton)"""
    
    fieldsets = (
        ('Pricing Configuration', {
            'fields': ('commission_rate', 'service_fee', 'default_currency'),
            'description': 'Configure booking fees and default currency'
        }),
        ('Paynow (Zimbabwe)', {
            'fields': ('paynow_integration_id', 'paynow_integration_key', 'paynow_webhook_secret'),
            'classes': ('collapse',),
            'description': 'Paynow payment gateway configuration'
        }),
        ('PayFast (South Africa)', {
            'fields': ('payfast_merchant_id', 'payfast_merchant_key', 'payfast_passphrase', 'payfast_webhook_secret'),
            'classes': ('collapse',),
            'description': 'PayFast payment gateway configuration'
        }),
        ('Stripe (International)', {
            'fields': ('stripe_secret_key', 'stripe_publishable_key', 'stripe_webhook_secret'),
            'classes': ('collapse',),
            'description': 'Stripe payment gateway configuration'
        }),
        ('Business Rules', {
            'fields': ('max_advance_booking_days', 'max_stay_duration_days', 'review_window_days', 'review_edit_window_days'),
            'description': 'Configure booking and review time limits'
        }),
        ('Email Settings', {
            'fields': ('admin_email', 'support_email'),
            'classes': ('collapse',),
        }),
        ('Maintenance', {
            'fields': ('maintenance_mode', 'maintenance_message'),
            'classes': ('collapse',),
        }),
    )
    
    readonly_fields = ('created_at', 'updated_at')
    
    def has_add_permission(self, request):
        """Prevent adding more than one configuration"""
        return not SystemConfiguration.objects.exists()
    
    def has_delete_permission(self, request, obj=None):
        """Prevent deletion of configuration"""
        return False


@admin.register(AuditLog)
class AuditLogAdmin(UnfoldModelAdmin):
    list_display = ['user', 'action', 'timestamp']
    list_filter = ['action', 'timestamp']
    search_fields = ['user__email', 'action']
    readonly_fields = ['timestamp', 'changes', 'content_type', 'object_id']
    date_hierarchy = 'timestamp'

    def has_add_permission(self, request):
        return False

    def has_delete_permission(self, request, obj=None):
        return False


@admin.register(AdminStats)
class AdminStatsAdmin(UnfoldModelAdmin):
    list_display = ['total_revenue', 'total_bookings', 'total_users', 'active_hosts', 'last_updated']
    readonly_fields = ['last_updated']
