from django.contrib import admin
from apps.reviews.models import Review

@admin.register(Review)
class ReviewAdmin(admin.ModelAdmin):
    list_display = ['guest', 'host', 'rating', 'booking', 'created_at']
    list_filter = ['rating', 'created_at']
    search_fields = ['guest__email', 'host__email', 'text']
    readonly_fields = ['created_at', 'updated_at']
