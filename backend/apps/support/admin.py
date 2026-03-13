from django.contrib import admin
from .models import SupportTicket, SupportTicketEvent, CannedResponse, BugReport

class SupportTicketEventInline(admin.TabularInline):
    model = SupportTicketEvent
    extra = 0
    readonly_fields = ('created_at',)

@admin.register(SupportTicket)
class SupportTicketAdmin(admin.ModelAdmin):
    list_display = ('id', 'subject', 'requester', 'assigned_agent', 'category', 'priority', 'status', 'created_at')
    list_filter = ('status', 'priority', 'category')
    search_fields = ('subject', 'requester__email', 'assigned_agent__email')
    inlines = [SupportTicketEventInline]

@admin.register(CannedResponse)
class CannedResponseAdmin(admin.ModelAdmin):
    list_display = ('title', 'category', 'is_active')
    list_filter = ('category', 'is_active')
    search_fields = ('title', 'body')

@admin.register(BugReport)
class BugReportAdmin(admin.ModelAdmin):
    list_display = ('id', 'title', 'reporter', 'severity', 'status', 'created_at')
    list_filter = ('severity', 'status')
    search_fields = ('title', 'description', 'reporter__email')
    readonly_fields = ('browser_info', 'console_logs', 'network_errors', 'created_at', 'updated_at')
