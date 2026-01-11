from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.reviews.models import Review


@admin.register(Review)
class ReviewAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Review management with StayAfrica theme"""
    
    list_display = [
        'id', 'guest_display', 'host_display', 'rating_display', 
        'booking_display', 'created_at'
    ]
    list_filter = ['rating', 'created_at', 'updated_at']
    search_fields = ['guest__email', 'guest__username', 'host__email', 'host__username', 'text']
    readonly_fields = ['created_at', 'updated_at', 'review_summary']
    date_hierarchy = 'created_at'
    list_per_page = 25
    actions = ['moderate_reviews', 'feature_reviews', 'hide_reviews']
    list_select_related = ['guest', 'host', 'booking']
    
    fieldsets = (
        (_('Review Information'), {
            'fields': ('review_summary', 'rating'),
        }),
        (_('Participants'), {
            'fields': ('guest', 'host', 'booking'),
        }),
        (_('Review Content'), {
            'fields': ('text',),
            'description': 'The review text written by the guest',
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )

    @display(description=_('Guest'))
    def guest_display(self, obj):
        return obj.guest.get_full_name() or obj.guest.email

    @display(description=_('Host'))
    def host_display(self, obj):
        return obj.host.get_full_name() or obj.host.email

    @display(description=_('Rating'), ordering='rating')
    def rating_display(self, obj):
        """Display rating with star emojis and color coding"""
        stars = '⭐' * obj.rating
        # Color code based on rating
        if obj.rating >= 4:
            color = '#D9B168'  # Safari Gold for good ratings
        elif obj.rating >= 3:
            color = '#3A5C50'  # Moss Green for average
        else:
            color = '#B91C1C'  # Red for poor ratings
        
        return format_html(
            '<span style="color: {}; font-size: 16px;">{}</span> <span style="color: #122F26;">({}/5)</span>',
            color, stars, obj.rating
        )

    @display(description=_('Booking'))
    def booking_display(self, obj):
        if obj.booking:
            return obj.booking.booking_ref
        return '-'

    @display(description=_('Review Summary'))
    def review_summary(self, obj):
        if obj.id:
            # Get star representation
            stars = '⭐' * obj.rating
            
            # Get review excerpt
            review_text = obj.text[:150] + '...' if len(obj.text) > 150 else obj.text
            
            # Determine color based on rating
            if obj.rating >= 4:
                border_color = '#D9B168'  # Safari Gold
                rating_color = '#D9B168'
            elif obj.rating >= 3:
                border_color = '#3A5C50'  # Moss Green
                rating_color = '#3A5C50'
            else:
                border_color = '#B91C1C'  # Red
                rating_color = '#B91C1C'
            
            summary = f"""
            <div style="padding: 12px; background: #F4F1EA; border-left: 4px solid {border_color}; border-radius: 4px;">
                <div style="margin-bottom: 8px;">
                    <strong style="color: #122F26; font-size: 16px;">Review by {obj.guest.get_full_name() or obj.guest.email}</strong>
                </div>
                <div style="margin-bottom: 8px;">
                    <span style="color: {rating_color}; font-size: 18px;">{stars}</span>
                    <strong style="color: #122F26; margin-left: 8px;">{obj.rating}/5</strong>
                </div>
                <div style="color: #3A5C50; margin-bottom: 8px;">
                    <strong>For:</strong> {obj.host.get_full_name() or obj.host.email}<br/>
                    <strong>Booking:</strong> {obj.booking.booking_ref if obj.booking else 'N/A'}<br/>
                    <strong>Date:</strong> {obj.created_at.strftime('%B %d, %Y')}
                </div>
                <div style="padding: 8px; background: white; border-radius: 4px; margin-top: 8px;">
                    <em style="color: #3A5C50;">{review_text}</em>
                </div>
            </div>
            """
            return format_html(summary)
        return "Save to see summary"

    @admin.action(description=_('Hide/Moderate selected reviews'))
    def moderate_reviews(self, request, queryset):
        updated = queryset.update(text='[MODERATED - This review has been hidden by administrators]')
        self.message_user(request, f'{updated} review(s) moderated.')

    @admin.action(description=_('Feature selected reviews'))
    def feature_reviews(self, request, queryset):
        # This would require a 'featured' field in the model
        # For now, just show a message
        count = queryset.count()
        self.message_user(
            request, 
            f'{count} review(s) selected. Note: To feature reviews, add a "featured" field to the Review model.',
            level='warning'
        )

    @admin.action(description=_('Hide selected reviews'))
    def hide_reviews(self, request, queryset):
        updated = queryset.update(text='[HIDDEN]')
        self.message_user(request, f'{updated} review(s) hidden.')
