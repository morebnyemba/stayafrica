from django.contrib import admin
from apps.admin_dashboard.models import AuditLog, AdminStats

@admin.register(AuditLog)
class AuditLogAdmin(admin.ModelAdmin):
    list_display = ['user', 'action', 'timestamp']
    list_filter = ['action', 'timestamp']
    search_fields = ['user__email', 'action']
    readonly_fields = ['timestamp']

@admin.register(AdminStats)
class AdminStatsAdmin(admin.ModelAdmin):
    list_display = ['total_revenue', 'total_bookings', 'total_users', 'active_hosts', 'last_updated']
    readonly_fields = ['last_updated']
