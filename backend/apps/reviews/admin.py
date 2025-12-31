from django.contrib import admin
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from apps.reviews.models import Review


@admin.register(Review)
class ReviewAdmin(UnfoldModelAdmin):
    list_display = ['guest', 'host', 'rating', 'booking', 'created_at']
    list_filter = ['rating', 'created_at']
    search_fields = ['guest__email', 'host__email', 'text']
    readonly_fields = ['created_at', 'updated_at']
    raw_id_fields = ['booking', 'guest', 'host']
    date_hierarchy = 'created_at'
    actions = ['moderate_reviews']

    def moderate_reviews(self, request, queryset):
        queryset.update(text='[MODERATED]')
    moderate_reviews.short_description = 'Hide/Moderate selected reviews'
