from django.contrib import admin
from apps.users.models import User

@admin.register(User)
class UserAdmin(admin.ModelAdmin):
    list_display = ['email', 'username', 'role', 'country_of_residence', 'is_verified', 'created_at']
    list_filter = ['role', 'is_verified', 'country_of_residence', 'created_at']
    search_fields = ['email', 'username', 'phone_number']
    readonly_fields = ['created_at', 'updated_at']
    fieldsets = (
        ('Personal Info', {'fields': ('email', 'username', 'first_name', 'last_name', 'phone_number')}),
        ('Profile', {'fields': ('role', 'country_of_residence', 'bio', 'profile_picture')}),
        ('Status', {'fields': ('is_verified', 'is_active')}),
        ('Timestamps', {'fields': ('created_at', 'updated_at')}),
    )
