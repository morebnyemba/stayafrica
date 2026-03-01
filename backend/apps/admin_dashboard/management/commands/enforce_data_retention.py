"""
Data retention management command for GDPR/POPIA compliance.
Handles anonymization and deletion of expired user data.
"""
from django.core.management.base import BaseCommand, CommandError
from django.utils import timezone
from datetime import timedelta
from django.db import transaction
import logging

logger = logging.getLogger(__name__)


class Command(BaseCommand):
    help = 'Enforce data retention policies (GDPR/POPIA compliance)'

    def add_arguments(self, parser):
        parser.add_argument(
            '--dry-run',
            action='store_true',
            help='Show what would be deleted without making changes',
        )
        parser.add_argument(
            '--inactive-days',
            type=int,
            default=730,
            help='Days of inactivity before anonymizing user data (default: 730 = 2 years)',
        )
        parser.add_argument(
            '--delete-unverified-days',
            type=int,
            default=30,
            help='Days before deleting unverified accounts (default: 30)',
        )
        parser.add_argument(
            '--log-retention-days',
            type=int,
            default=90,
            help='Days to retain audit logs (default: 90)',
        )

    def handle(self, *args, **options):
        dry_run = options['dry_run']
        now = timezone.now()

        if dry_run:
            self.stdout.write(self.style.WARNING('=== DRY RUN MODE ==='))

        # 1. Remove unverified accounts
        self._cleanup_unverified(now, options['delete_unverified_days'], dry_run)

        # 2. Anonymize inactive users
        self._anonymize_inactive(now, options['inactive_days'], dry_run)

        # 3. Clean old audit logs
        self._cleanup_audit_logs(now, options['log_retention_days'], dry_run)

        # 4. Clean expired tokens/sessions
        self._cleanup_tokens(now, dry_run)

        # 5. Clean old notification data
        self._cleanup_notifications(now, dry_run)

        self.stdout.write(self.style.SUCCESS('Data retention enforcement complete'))

    def _cleanup_unverified(self, now, days, dry_run):
        from apps.users.models import User

        cutoff = now - timedelta(days=days)
        unverified = User.objects.filter(
            is_verified=False,
            date_joined__lt=cutoff,
        ).exclude(role='admin')

        count = unverified.count()
        self.stdout.write(f'Unverified accounts older than {days} days: {count}')

        if not dry_run and count > 0:
            with transaction.atomic():
                deleted, _ = unverified.delete()
                logger.info(f'Deleted {deleted} unverified accounts')
                self.stdout.write(self.style.SUCCESS(f'  Deleted {deleted} accounts'))

    def _anonymize_inactive(self, now, days, dry_run):
        from apps.users.models import User

        cutoff = now - timedelta(days=days)
        inactive = User.objects.filter(
            last_login__lt=cutoff,
            is_active=True,
        ).exclude(role='admin').exclude(email__startswith='anon_')

        count = inactive.count()
        self.stdout.write(f'Inactive accounts (>{days} days since login): {count}')

        if not dry_run and count > 0:
            with transaction.atomic():
                for user in inactive:
                    anon_id = f'anon_{user.id.hex[:8]}'
                    user.email = f'{anon_id}@anonymized.stayafrica.app'
                    user.first_name = 'Anonymized'
                    user.last_name = 'User'
                    user.phone_number = ''
                    user.profile_picture = ''
                    user.bio = ''
                    user.is_active = False
                    user.set_unusable_password()
                    user.save()
                logger.info(f'Anonymized {count} inactive users')
                self.stdout.write(self.style.SUCCESS(f'  Anonymized {count} users'))

    def _cleanup_audit_logs(self, now, days, dry_run):
        from apps.admin_dashboard.models import AuditLog

        cutoff = now - timedelta(days=days)
        old_logs = AuditLog.objects.filter(timestamp__lt=cutoff)

        count = old_logs.count()
        self.stdout.write(f'Audit logs older than {days} days: {count}')

        if not dry_run and count > 0:
            deleted, _ = old_logs.delete()
            logger.info(f'Deleted {deleted} audit logs')
            self.stdout.write(self.style.SUCCESS(f'  Deleted {deleted} audit logs'))

    def _cleanup_tokens(self, now, dry_run):
        from django.core.cache import cache

        self.stdout.write('Expired cache tokens: cleaned by Redis TTL (automatic)')

    def _cleanup_notifications(self, now, dry_run):
        try:
            from apps.notifications.models import Notification
            cutoff = now - timedelta(days=180)
            old = Notification.objects.filter(created_at__lt=cutoff, is_read=True)
            count = old.count()
            self.stdout.write(f'Read notifications older than 180 days: {count}')

            if not dry_run and count > 0:
                deleted, _ = old.delete()
                self.stdout.write(self.style.SUCCESS(f'  Deleted {deleted} notifications'))
        except Exception:
            self.stdout.write('  Notifications cleanup skipped (model not available)')
