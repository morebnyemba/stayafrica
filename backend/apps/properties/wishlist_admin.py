from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.properties.wishlist_models import Wishlist, WishlistItem, WishlistVote, WishlistComment


@admin.register(Wishlist)
class WishlistAdmin(UnfoldModelAdmin):
    """Admin interface for wishlists"""
    
    list_display = ['name', 'user_display', 'visibility_badge', 'item_count', 
                    'vote_count', 'created_at']
    list_filter = ['visibility', 'created_at']
    search_fields = ['name', 'description', 'user__email']
    readonly_fields = ['user', 'slug', 'item_count', 'vote_count', 'created_at', 'updated_at']
    list_select_related = ['user']
    list_per_page = 25
    
    fieldsets = (
        (_('Wishlist Details'), {
            'fields': ('user', 'name', 'slug', 'description', 'visibility'),
        }),
        (_('Statistics'), {
            'fields': ('item_count', 'vote_count'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Owner'))
    def user_display(self, obj):
        return obj.user.get_full_name() or obj.user.email
    
    @display(description=_('Visibility'), label=True)
    def visibility_badge(self, obj):
        colors = {
            'private': 'secondary',
            'public': 'success',
            'friends': 'info',
        }
        return {
            'value': obj.get_visibility_display(),
            'color': colors.get(obj.visibility, 'secondary'),
        }
    
    def has_add_permission(self, request):
        return False


@admin.register(WishlistItem)
class WishlistItemAdmin(UnfoldModelAdmin):
    """Admin interface for wishlist items"""
    
    list_display = ['wishlist_display', 'property_display', 'priority_badge', 
                    'notes_short', 'added_at']
    list_filter = ['priority', 'added_at']
    search_fields = ['wishlist__name', 'property__title', 'notes']
    readonly_fields = ['wishlist', 'property', 'added_at']
    list_select_related = ['wishlist', 'wishlist__user', 'property']
    list_per_page = 25
    date_hierarchy = 'added_at'
    
    fieldsets = (
        (_('Item Details'), {
            'fields': ('wishlist', 'property', 'priority'),
        }),
        (_('Notes'), {
            'fields': ('notes',),
            'classes': ['collapse'],
        }),
        (_('Timestamp'), {
            'fields': ('added_at',),
        }),
    )
    
    @display(description=_('Wishlist'))
    def wishlist_display(self, obj):
        return f"{obj.wishlist.name} ({obj.wishlist.user.email})"
    
    @display(description=_('Property'))
    def property_display(self, obj):
        return obj.property.title
    
    @display(description=_('Priority'), label=True)
    def priority_badge(self, obj):
        colors = {
            'low': 'secondary',
            'medium': 'info',
            'high': 'warning',
        }
        return {
            'value': obj.get_priority_display(),
            'color': colors.get(obj.priority, 'secondary'),
        }
    
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
    
    list_display = ['wishlist_display', 'user_display', 'vote_type_badge', 'voted_at']
    list_filter = ['vote_type', 'voted_at']
    search_fields = ['wishlist__name', 'user__email']
    readonly_fields = ['wishlist', 'user', 'vote_type', 'voted_at']
    list_select_related = ['wishlist', 'wishlist__user', 'user']
    list_per_page = 25
    date_hierarchy = 'voted_at'
    
    fieldsets = (
        (_('Vote Details'), {
            'fields': ('wishlist', 'user', 'vote_type'),
        }),
        (_('Timestamp'), {
            'fields': ('voted_at',),
        }),
    )
    
    @display(description=_('Wishlist'))
    def wishlist_display(self, obj):
        return obj.wishlist.name
    
    @display(description=_('Voter'))
    def user_display(self, obj):
        return obj.user.get_full_name() or obj.user.email
    
    @display(description=_('Vote'), label=True)
    def vote_type_badge(self, obj):
        colors = {
            'like': 'success',
            'love': 'danger',
            'save': 'info',
        }
        return {
            'value': obj.get_vote_type_display(),
            'color': colors.get(obj.vote_type, 'secondary'),
        }
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False


@admin.register(WishlistComment)
class WishlistCommentAdmin(UnfoldModelAdmin):
    """Admin interface for wishlist comments"""
    
    list_display = ['wishlist_display', 'user_display', 'comment_preview', 
                    'reply_count', 'created_at']
    list_filter = ['created_at']
    search_fields = ['wishlist__name', 'user__email', 'comment']
    readonly_fields = ['wishlist', 'user', 'parent_comment', 'reply_count', 'created_at', 'updated_at']
    list_select_related = ['wishlist', 'user', 'parent_comment']
    list_per_page = 25
    date_hierarchy = 'created_at'
    actions = ['delete_comments']
    
    fieldsets = (
        (_('Comment Details'), {
            'fields': ('wishlist', 'user', 'parent_comment'),
        }),
        (_('Content'), {
            'fields': ('comment',),
        }),
        (_('Statistics'), {
            'fields': ('reply_count',),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Wishlist'))
    def wishlist_display(self, obj):
        return obj.wishlist.name
    
    @display(description=_('User'))
    def user_display(self, obj):
        return obj.user.get_full_name() or obj.user.email
    
    @display(description=_('Comment'))
    def comment_preview(self, obj):
        return obj.comment[:80] + '...' if len(obj.comment) > 80 else obj.comment
    
    @admin.action(description=_('Delete selected comments'))
    def delete_comments(self, request, queryset):
        count = queryset.count()
        queryset.delete()
        self.message_user(request, f'{count} comment(s) deleted.')
