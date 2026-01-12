from django.contrib import admin
from unfold.admin import ModelAdmin
from apps.experiences.models import (
    Experience, ExperienceCategory, ExperienceImage,
    ExperienceBooking, ExperienceAvailability
)


@admin.register(ExperienceCategory)
class ExperienceCategoryAdmin(ModelAdmin):
    list_display = ['name', 'icon']
    search_fields = ['name']


class ExperienceImageInline(admin.TabularInline):
    model = ExperienceImage
    extra = 1
    fields = ['image', 'caption', 'order']


@admin.register(Experience)
class ExperienceAdmin(ModelAdmin):
    list_display = ['id', 'title', 'host', 'city', 'country', 'price_per_person', 'status', 'created_at']
    list_filter = ['status', 'difficulty', 'duration', 'country', 'category']
    search_fields = ['title', 'description', 'city', 'country', 'host__email']
    readonly_fields = ['id', 'created_at', 'updated_at']
    inlines = [ExperienceImageInline]
    
    fieldsets = (
        ('Basic Information', {
            'fields': ('id', 'host', 'title', 'description', 'category', 'status')
        }),
        ('Location', {
            'fields': ('location', 'country', 'city', 'address')
        }),
        ('Details', {
            'fields': ('price_per_person', 'currency', 'duration', 'duration_hours', 
                      'difficulty', 'min_participants', 'max_participants')
        }),
        ('Additional Information', {
            'fields': ('included_items', 'requirements', 'cancellation_policy', 'main_image')
        }),
        ('Metadata', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(ExperienceBooking)
class ExperienceBookingAdmin(ModelAdmin):
    list_display = ['booking_ref', 'guest', 'experience', 'booking_date', 'num_participants', 
                   'total_amount', 'status', 'created_at']
    list_filter = ['status', 'booking_date', 'created_at']
    search_fields = ['booking_ref', 'guest__email', 'experience__title']
    readonly_fields = ['booking_ref', 'created_at', 'updated_at']
    
    fieldsets = (
        ('Booking Information', {
            'fields': ('booking_ref', 'guest', 'experience', 'status')
        }),
        ('Details', {
            'fields': ('booking_date', 'booking_time', 'num_participants', 'special_requests')
        }),
        ('Pricing', {
            'fields': ('price_per_person', 'service_fee', 'total_amount', 'currency')
        }),
        ('Metadata', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(ExperienceAvailability)
class ExperienceAvailabilityAdmin(ModelAdmin):
    list_display = ['experience', 'weekday', 'specific_date', 'start_time', 'end_time', 'is_available']
    list_filter = ['weekday', 'is_available', 'experience']
    search_fields = ['experience__title']
