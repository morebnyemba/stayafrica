"""
Management command to initialize system configuration with defaults
"""
from django.core.management.base import BaseCommand
from apps.admin_dashboard.models import SystemConfiguration


class Command(BaseCommand):
    help = 'Initialize system configuration with default values'

    def handle(self, *args, **options):
        config, created = SystemConfiguration.objects.get_or_create(pk=1)
        
        if created:
            self.stdout.write(
                self.style.SUCCESS('Successfully created system configuration with defaults')
            )
        else:
            self.stdout.write(
                self.style.WARNING('System configuration already exists')
            )
        
        self.stdout.write('\nCurrent Configuration:')
        self.stdout.write(f'  Commission Rate: {config.commission_rate}')
        self.stdout.write(f'  Service Fee: ${config.service_fee}')
        self.stdout.write(f'  Default Currency: {config.default_currency}')
        self.stdout.write(f'  Max Advance Booking: {config.max_advance_booking_days} days')
        self.stdout.write(f'  Max Stay Duration: {config.max_stay_duration_days} days')
        self.stdout.write(f'  Review Window: {config.review_window_days} days')
        self.stdout.write(f'  Review Edit Window: {config.review_edit_window_days} days')
        
        self.stdout.write('\n' + self.style.SUCCESS('Configuration can be modified in Django Admin'))
