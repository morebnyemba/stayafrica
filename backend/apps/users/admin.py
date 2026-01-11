from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.users.models import User


@admin.register(User)
class UserAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for User management with StayAfrica theme"""
    
    list_display = [
        'email', 'username', 'full_name_display', 'role_badge', 
        'verified_badge', 'active_badge', 'country_of_residence', 'date_joined'
    ]
    list_filter = [
        'role', 'is_verified', 'is_active', 'is_staff', 
        'is_superuser', 'country_of_residence', 'created_at'
    ]
    search_fields = ['email', 'username', 'phone_number', 'first_name', 'last_name']
    readonly_fields = ['created_at', 'updated_at', 'last_login', 'user_summary', 'profile_image_preview']
    list_per_page = 25
    date_hierarchy = 'created_at'
    
    actions = [
        'activate_users', 'deactivate_users', 'verify_users', 
        'unverify_users', 'verify_identity', 'ban_user'
    ]
    
    fieldsets = (
        (_('Account Information'), {
            'fields': ('email', 'username', 'user_summary', 'password'),
            'classes': ['tab'],
        }),
        (_('Personal Information'), {
            'fields': (
                'first_name', 'last_name', 'phone_number', 
                'profile_picture', 'profile_image_preview'
            ),
            'classes': ['tab'],
        }),
        (_('Profile & Verification'), {
            'fields': ('role', 'country_of_residence', 'bio', 'is_verified'),
            'classes': ['tab'],
        }),
        (_('Permissions & Access'), {
            'fields': ('is_active', 'is_staff', 'is_superuser', 'groups', 'user_permissions'),
            'classes': ['tab'],
            'description': 'Manage user permissions and access levels',
        }),
        (_('Activity Timestamps'), {
            'fields': ('last_login', 'date_joined', 'created_at', 'updated_at'),
            'classes': ['collapse', 'tab'],
        }),
    )
    
    filter_horizontal = ['groups', 'user_permissions']

    # Custom action methods
    @admin.action(description=_('Mark selected users as active'))
    def activate_users(self, request, queryset):
        updated = queryset.update(is_active=True)
        self.message_user(request, f'{updated} user(s) activated.')

    @admin.action(description=_('Mark selected users as inactive'))
    def deactivate_users(self, request, queryset):
        updated = queryset.update(is_active=False)
        self.message_user(request, f'{updated} user(s) deactivated.')

    @admin.action(description=_('Mark selected users as verified'))
    def verify_users(self, request, queryset):
        updated = queryset.update(is_verified=True)
        self.message_user(request, f'{updated} user(s) verified.')

    @admin.action(description=_('Mark selected users as unverified'))
    def unverify_users(self, request, queryset):
        updated = queryset.update(is_verified=False)
        self.message_user(request, f'{updated} user(s) unverified.')

    @admin.action(description=_('Verify identity (KYC)'))
    def verify_identity(self, request, queryset):
        updated = queryset.update(is_verified=True)
        self.message_user(request, f'{updated} user(s) identity verified.')

    @admin.action(description=_('Ban user (deactivate)'))
    def ban_user(self, request, queryset):
        updated = queryset.update(is_active=False)
        self.message_user(request, f'{updated} user(s) banned.')

    # Custom display methods
    @display(description=_('Role'), ordering='role', label=True)
    def role_badge(self, obj):
        colors = {
            'admin': 'success',
            'host': 'info',
            'guest': 'secondary'
        }
        return {
            'value': obj.get_role_display(),
            'color': colors.get(obj.role, 'secondary'),
        }

    @display(description=_('Verified'), label=True, boolean=True)
    def verified_badge(self, obj):
        if obj.is_verified:
            return {'value': 'Verified', 'color': 'success'}
        return {'value': 'Unverified', 'color': 'warning'}

    @display(description=_('Active'), label=True, boolean=True)
    def active_badge(self, obj):
        if obj.is_active:
            return {'value': 'Active', 'color': 'success'}
        return {'value': 'Inactive', 'color': 'danger'}

    @display(description=_('Full Name'))
    def full_name_display(self, obj):
        name = obj.get_full_name()
        return name if name else '-'

    @display(description=_('Profile Picture'))
    def profile_image_preview(self, obj):
        if obj.profile_picture:
            from django.utils.html import escape
            safe_url = escape(obj.profile_picture.url)
            return format_html(
                '<img src="{}" style="max-width: 150px; max-height: 150px; '
                'border-radius: 50%; border: 3px solid #D9B168; '
                'box-shadow: 0 4px 6px rgba(0,0,0,0.1);" />',
                safe_url
            )
        return format_html(
            '<div style="width: 150px; height: 150px; border-radius: 50%; '
            'background: #F4F1EA; display: flex; align-items: center; '
            'justify-content: center; border: 3px solid #3A5C50;">'
            '<span style="color: #3A5C50; font-size: 48px;">👤</span></div>'
        )

    @display(description=_('User Summary'))
    def user_summary(self, obj):
        if obj.id:
            # Note: For better performance with many users, consider:
            # 1. Using annotations in get_queryset() to precompute stats
            # 2. Caching these values
            # 3. Computing stats asynchronously
            
            stats = "N/A"
            if obj.role == 'host':
                # These queries should be optimized with annotations if used frequently
                from apps.properties.models import Property
                from apps.bookings.models import Booking
                properties_count = Property.objects.filter(host=obj).count()
                bookings_received = Booking.objects.filter(rental_property__host=obj).count()
                stats = f"Properties: {properties_count} | Bookings Received: {bookings_received}"
            elif obj.role == 'guest':
                from apps.bookings.models import Booking
                bookings_made = Booking.objects.filter(guest=obj).count()
                stats = f"Bookings Made: {bookings_made}"
            else:
                stats = "Admin User"
            
            summary = f"""
            <div style="padding: 12px; background: #F4F1EA; border-left: 4px solid #D9B168; border-radius: 4px;">
                <strong style="color: #122F26; font-size: 16px;">User Profile Summary</strong><br/>
                <div style="margin-top: 8px; color: #3A5C50;">
                    <strong>Name:</strong> {obj.get_full_name() or 'Not provided'}<br/>
                    <strong>Email:</strong> {obj.email}<br/>
                    <strong>Username:</strong> {obj.username}<br/>
                    <strong>Role:</strong> {obj.get_role_display()}<br/>
                    <strong>Country:</strong> {obj.country_of_residence or 'Not specified'}<br/>
                    <strong>Phone:</strong> {obj.phone_number or 'Not provided'}<br/>
                    <strong>Verified:</strong> {'✓ Yes' if obj.is_verified else '✗ No'}<br/>
                    <strong>Active:</strong> {'✓ Yes' if obj.is_active else '✗ No'}<br/>
                    <strong>Stats:</strong> {stats}
                </div>
            </div>
            """
            return format_html(summary)
        return "Save to see summary"
