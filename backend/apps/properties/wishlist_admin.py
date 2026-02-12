from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.properties.wishlist_models import Wishlist, WishlistItem, WishlistVote, WishlistComment


@admin.register(Wishlist)
class WishlistAdmin(UnfoldModelAdmin):
    """Admin interface for wishlists"""
    
    list_display = ['name', 'owner_display', 'privacy_badge', 'created_at']
    list_filter = ['privacy', 'created_at']
    search_fields = ['name', 'description', 'owner__email']
    readonly_fields = ['owner', 'share_token', 'created_at', 'updated_at']
    list_select_related = ['owner']
    list_per_page = 25
    
    fieldsets = (
        (_('Wishlist Details'), {
            'fields': ('owner', 'name', 'description', 'privacy'),
        }),
        (_('Sharing'), {
            'fields': ('share_token',),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Owner'))
    def owner_display(self, obj):
        return obj.owner.get_full_name() or obj.owner.email
    
    @display(description=_('Privacy'), label=True)
    def privacy_badge(self, obj):
        colors = {
            'private': 'secondary',
            'public': 'success',
            'shared': 'info',
        }
        return {
            'value': obj.get_privacy_display(),
            'color': colors.get(obj.privacy, 'secondary'),
        }
    
    def has_add_permission(self, request):
        return False


@admin.register(WishlistItem)
class WishlistItemAdmin(UnfoldModelAdmin):
    """Admin interface for wishlist items"""
    
    list_display = ['wishlist_display', 'property_display', 'added_by_display',
                    'votes', 'notes_short', 'added_at']
    list_filter = ['added_at']
    search_fields = ['wishlist__name', 'property__title', 'notes']
    readonly_fields = ['wishlist', 'property', 'added_by', 'added_at']
    list_select_related = ['wishlist', 'wishlist__owner', 'property', 'added_by']
    list_per_page = 25
    date_hierarchy = 'added_at'
    
    fieldsets = (
        (_('Item Details'), {
            'fields': ('wishlist', 'property', 'added_by'),
        }),
        (_('Preferences'), {
            'fields': ('preferred_check_in', 'preferred_check_out'),
        }),
        (_('Notes'), {
            'fields': ('notes',),
            'classes': ['collapse'],
        }),
        (_('Stats'), {
            'fields': ('votes',),
        }),
        (_('Timestamp'), {
            'fields': ('added_at',),
        }),
    )
    
    @display(description=_('Wishlist'))
    def wishlist_display(self, obj):
        return f"{obj.wishlist.name} ({obj.wishlist.owner.email})"
    
    @display(description=_('Property'))
    def property_display(self, obj):
        return obj.property.title
    
    @display(description=_('Added By'))
    def added_by_display(self, obj):
        if obj.added_by:
            return obj.added_by.get_full_name() or obj.added_by.email
        return '-'
    
    @display(description=_('Notes'))
    def notes_short(self, obj):
        if obj.notes:
            return obj.notes[:50] + '...' if len(obj.notes) > 50 else obj.notes
        return '-'
    
    def has_add_permission(self, request):
        return False


@admin.register(WishlistVote)
class WishlistVoteAdmin(UnfoldModelAdmin):
    """Admin interface for wishlist votes"""
    
    list_display = ['wishlist_item_display', 'user_display', 'vote_badge', 'created_at']
    list_filter = ['vote', 'created_at']
    search_fields = ['wishlist_item__wishlist__name', 'user__email']
    readonly_fields = ['wishlist_item', 'user', 'vote', 'created_at', 'updated_at']
    list_select_related = ['wishlist_item', 'wishlist_item__wishlist', 'user']
    list_per_page = 25
    date_hierarchy = 'created_at'
    
    fieldsets = (
        (_('Vote Details'), {
            'fields': ('wishlist_item', 'user', 'vote'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Wishlist Item'))
    def wishlist_item_display(self, obj):
        return f"{obj.wishlist_item.wishlist.name}"
    
    @display(description=_('Voter'))
    def user_display(self, obj):
        return obj.user.get_full_name() or obj.user.email
    
    @display(description=_('Vote'), label=True)
    def vote_badge(self, obj):
        if obj.vote == 1:
            return {'value': 'Upvote', 'color': 'success'}
        elif obj.vote == -1:
            return {'value': 'Downvote', 'color': 'danger'}
        return {'value': str(obj.vote), 'color': 'secondary'}
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False


@admin.register(WishlistComment)
class WishlistCommentAdmin(UnfoldModelAdmin):
    """Admin interface for wishlist comments"""
    
    list_display = ['wishlist_item_display', 'user_display', 'comment_preview', 'created_at']
    list_filter = ['created_at']
    search_fields = ['wishlist_item__wishlist__name', 'user__email', 'text']
    readonly_fields = ['wishlist_item', 'user', 'created_at', 'updated_at']
    list_select_related = ['wishlist_item', 'wishlist_item__wishlist', 'user']
    list_per_page = 25
    date_hierarchy = 'created_at'
    actions = ['delete_comments']
    
    fieldsets = (
        (_('Comment Details'), {
            'fields': ('wishlist_item', 'user'),
        }),
        (_('Content'), {
            'fields': ('text',),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Wishlist Item'))
    def wishlist_item_display(self, obj):
        return f"{obj.wishlist_item.wishlist.name}"
    
    @display(description=_('User'))
    def user_display(self, obj):
        return obj.user.get_full_name() or obj.user.email
    
    @display(description=_('Comment'))
    def comment_preview(self, obj):
        return obj.text[:80] + '...' if len(obj.text) > 80 else obj.text
    
    @admin.action(description=_('Delete selected comments'))
    def delete_comments(self, request, queryset):
        count = queryset.count()
        queryset.delete()
        self.message_user(request, f'{count} comment(s) deleted.')
