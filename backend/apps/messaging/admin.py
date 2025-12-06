from django.contrib import admin
from apps.messaging.models import Message

@admin.register(Message)
class MessageAdmin(admin.ModelAdmin):
    list_display = ['sender', 'receiver', 'is_read', 'created_at']
    list_filter = ['is_read', 'created_at']
    search_fields = ['sender__email', 'receiver__email', 'text']
    readonly_fields = ['created_at']
