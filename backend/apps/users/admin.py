from django.contrib import admin
from django.utils.html import format_html
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from apps.users.models import User


@admin.register(User)
class UserAdmin(UnfoldModelAdmin):
    list_display = [
        'email', 'username', 'role_badge', 'is_verified', 'is_active_badge', 'country_of_residence', 'date_joined'
    ]
    list_filter = ['role', 'is_verified', 'is_active', 'is_staff', 'is_superuser', 'country_of_residence', 'created_at']
    search_fields = ['email', 'username', 'phone_number', 'first_name', 'last_name']
    readonly_fields = ['created_at', 'updated_at', 'last_login']
    actions = ['activate_users', 'deactivate_users', 'verify_users', 'unverify_users', 'verify_identity', 'ban_user']
    fieldsets = (
        ('Personal Info', {'fields': ('email', 'username', 'first_name', 'last_name', 'phone_number', 'profile_picture')}),
        ('Profile', {'fields': ('role', 'country_of_residence', 'bio', 'is_verified')}),
        ('Permissions', {'fields': ('is_active', 'is_staff', 'is_superuser', 'groups', 'user_permissions')}),
        ('Timestamps', {'fields': ('last_login', 'date_joined', 'created_at', 'updated_at')}),
    )

    @admin.display(description='Role')
    def role_badge(self, obj):
        colors = {'admin': 'bg-green-100 text-green-700', 'host': 'bg-blue-100 text-blue-700', 'guest': 'bg-gray-100 text-gray-700'}
        klass = colors.get(obj.role, 'bg-gray-100 text-gray-700')
        return format_html('<span class="{} px-2 py-1 rounded">{}</span>', klass, obj.get_role_display())

    @admin.display(description='Active')
    def is_active_badge(self, obj):
        klass = 'bg-green-100 text-green-700' if obj.is_active else 'bg-red-100 text-red-700'
        label = 'Active' if obj.is_active else 'Inactive'
        return format_html('<span class="{} px-2 py-1 rounded">{}</span>', klass, label)

    def activate_users(self, request, queryset):
        queryset.update(is_active=True)
    activate_users.short_description = 'Mark selected users as active'

    def deactivate_users(self, request, queryset):
        queryset.update(is_active=False)
    deactivate_users.short_description = 'Mark selected users as inactive'

    def verify_users(self, request, queryset):
        queryset.update(is_verified=True)
    verify_users.short_description = 'Mark selected users as verified'

    def unverify_users(self, request, queryset):
        queryset.update(is_verified=False)
    unverify_users.short_description = 'Mark selected users as unverified'

    def verify_identity(self, request, queryset):
        queryset.update(is_verified=True)
    verify_identity.short_description = 'Verify identity (KYC)'

    def ban_user(self, request, queryset):
        queryset.update(is_active=False)
    ban_user.short_description = 'Ban user (deactivate)'
