from django.contrib import admin
from apps.users.models import User

@admin.register(User)
class UserAdmin(admin.ModelAdmin):
    list_display = ['email', 'username', 'role', 'is_staff', 'is_superuser', 'is_verified', 'created_at']
    list_filter = ['role', 'is_verified', 'is_staff', 'is_superuser', 'country_of_residence', 'created_at']
    search_fields = ['email', 'username', 'phone_number']
    readonly_fields = ['created_at', 'updated_at']
    fieldsets = (
        ('Personal Info', {'fields': ('email', 'username', 'first_name', 'last_name', 'phone_number')}),
        ('Profile', {'fields': ('role', 'country_of_residence', 'bio', 'profile_picture')}),
        ('Permissions', {'fields': ('is_active', 'is_staff', 'is_superuser', 'is_verified')}),
        ('Timestamps', {'fields': ('created_at', 'updated_at')}),
    )
