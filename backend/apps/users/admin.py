from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.users.models import User, UserPreference, UserPropertyInteraction


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


@admin.register(UserPreference)
class UserPreferenceAdmin(UnfoldModelAdmin):
    list_display = ['user', 'usual_guest_count', 'has_location', 'updated_at']
    search_fields = ['user__email', 'user__username']
    readonly_fields = ['created_at', 'updated_at']
    
    fieldsets = (
        ('User', {
            'fields': ('user',)
        }),
        ('Property Preferences', {
            'fields': ('preferred_property_types', 'preferred_min_price', 'preferred_max_price')
        }),
        ('Location Preferences', {
            'fields': ('preferred_countries', 'preferred_cities', 'last_latitude', 'last_longitude')
        }),
        ('Other Preferences', {
            'fields': ('usual_guest_count', 'preferred_amenities')
        }),
        ('Metadata', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )
    
    def has_location(self, obj):
        return bool(obj.last_latitude and obj.last_longitude)
    has_location.boolean = True
    has_location.short_description = 'Has Location'


@admin.register(UserPropertyInteraction)
class UserPropertyInteractionAdmin(UnfoldModelAdmin):
    list_display = ['user', 'property_id', 'interaction_type', 'created_at']
    list_filter = ['interaction_type', 'created_at']
    search_fields = ['user__email', 'property_id', 'search_query']
    readonly_fields = ['created_at']
    
    fieldsets = (
        ('Interaction', {
            'fields': ('user', 'property_id', 'interaction_type')
        }),
        ('Details', {
            'fields': ('search_query', 'viewed_duration_seconds')
        }),
        ('Metadata', {
            'fields': ('created_at',),
            'classes': ('collapse',)
        }),
    )


# Register verification models
from apps.users.verification_models import IdentityVerification, VerificationAttempt, VerificationSettings


@admin.register(IdentityVerification)
class IdentityVerificationAdmin(UnfoldModelAdmin):
    """Admin interface for identity verification"""
    
    list_display = [
        'user_email', 'document_type_display', 'status_badge',
        'submitted_at', 'reviewed_by_name', 'reviewed_at'
    ]
    list_filter = ['status', 'document_type', 'document_country', 'submitted_at', 'verification_method']
    search_fields = ['user__email', 'document_number', 'document_country']
    readonly_fields = [
        'id', 'submitted_at', 'reviewed_at', 'reviewed_by',
        'expires_at', 'created_at', 'updated_at', 'document_preview',
        'selfie_preview'
    ]
    list_select_related = ['user', 'reviewed_by']
    date_hierarchy = 'submitted_at'
    list_per_page = 25
    
    actions = ['approve_verifications', 'mark_under_review']
    
    fieldsets = (
        (_('User Information'), {
            'fields': ('id', 'user'),
        }),
        (_('Document Information'), {
            'fields': (
                'document_type', 'document_number', 'document_country',
                'document_expiry_date'
            ),
        }),
        (_('Uploaded Documents'), {
            'fields': (
                'document_front_image', 'document_preview',
                'document_back_image', 'selfie_image', 'selfie_preview'
            ),
        }),
        (_('Verification Status'), {
            'fields': ('status', 'verification_method'),
        }),
        (_('Review Details'), {
            'fields': (
                'reviewed_at', 'reviewed_by', 'rejection_reason',
                'admin_notes', 'expires_at'
            ),
            'classes': ['collapse'],
        }),
        (_('Metadata'), {
            'fields': ('metadata', 'submitted_at', 'created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('User'))
    def user_email(self, obj):
        return obj.user.email
    
    @display(description=_('Document Type'))
    def document_type_display(self, obj):
        return obj.get_document_type_display()
    
    @display(description=_('Status'), label=True)
    def status_badge(self, obj):
        colors = {
            'pending': 'warning',
            'under_review': 'info',
            'approved': 'success',
            'rejected': 'danger',
            'expired': 'secondary',
        }
        return {
            'value': obj.get_status_display(),
            'color': colors.get(obj.status, 'secondary'),
        }
    
    @display(description=_('Reviewed By'))
    def reviewed_by_name(self, obj):
        return obj.reviewed_by.email if obj.reviewed_by else '-'
    
    @display(description=_('Document Preview'))
    def document_preview(self, obj):
        if obj.document_front_image:
            return format_html(
                '<img src="{}" width="300" style="border: 2px solid #3A5C50; border-radius: 8px;" />',
                obj.document_front_image.url
            )
        return "No image"
    
    @display(description=_('Selfie Preview'))
    def selfie_preview(self, obj):
        if obj.selfie_image:
            return format_html(
                '<img src="{}" width="200" style="border: 2px solid #3A5C50; border-radius: 50%;" />',
                obj.selfie_image.url
            )
        return "No image"
    
    @admin.action(description=_('Approve selected verifications'))
    def approve_verifications(self, request, queryset):
        for verification in queryset.filter(status__in=['pending', 'under_review']):
            verification.approve(request.user, notes='Bulk approved by admin')
        self.message_user(request, f'{queryset.count()} verification(s) approved.')
    
    @admin.action(description=_('Mark as under review'))
    def mark_under_review(self, request, queryset):
        updated = queryset.filter(status='pending').update(status='under_review')
        self.message_user(request, f'{updated} verification(s) marked as under review.')


@admin.register(VerificationAttempt)
class VerificationAttemptAdmin(UnfoldModelAdmin):
    """Admin interface for verification attempts"""
    
    list_display = ['user_email', 'attempted_at', 'success_badge', 'ip_address']
    list_filter = ['success', 'attempted_at']
    search_fields = ['user__email', 'ip_address']
    readonly_fields = ['user', 'verification', 'ip_address', 'user_agent', 'success', 'error_message', 'attempted_at']
    list_select_related = ['user']
    date_hierarchy = 'attempted_at'
    
    @display(description=_('User'))
    def user_email(self, obj):
        return obj.user.email
    
    @display(description=_('Success'), label=True)
    def success_badge(self, obj):
        return {
            'value': 'Success' if obj.success else 'Failed',
            'color': 'success' if obj.success else 'danger',
        }
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False


@admin.register(VerificationSettings)
class VerificationSettingsAdmin(UnfoldModelAdmin):
    """Admin interface for verification settings"""
    
    list_display = ['id', 'max_attempts_per_day', 'verification_valid_years', 'updated_at']
    readonly_fields = ['updated_at']
    
    fieldsets = (
        (_('Rate Limiting'), {
            'fields': ('max_attempts_per_day', 'max_attempts_per_month'),
        }),
        (_('Document Requirements'), {
            'fields': ('require_document_back', 'require_selfie'),
        }),
        (_('Image Requirements'), {
            'fields': ('min_image_width', 'min_image_height', 'max_image_size_mb'),
        }),
        (_('Verification Policy'), {
            'fields': (
                'verification_valid_years',
                'require_verification_for_hosting',
                'require_verification_for_booking'
            ),
        }),
        (_('Third-Party Integration'), {
            'fields': ('use_third_party_service', 'third_party_api_key'),
            'classes': ['collapse'],
        }),
        (_('Metadata'), {
            'fields': ('updated_at',),
            'classes': ['collapse'],
        }),
    )
    
    def has_add_permission(self, request):
        # Only allow one settings instance
        return not VerificationSettings.objects.exists()
    
    def has_delete_permission(self, request, obj=None):
        # Don't allow deleting settings
        return False

