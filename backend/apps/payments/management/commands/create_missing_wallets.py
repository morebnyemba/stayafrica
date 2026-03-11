from django.core.management.base import BaseCommand
from django.contrib.auth import get_user_model
from apps.payments.models import Wallet

User = get_user_model()


class Command(BaseCommand):
    help = 'Create wallets for existing users who do not have one yet'

    def add_arguments(self, parser):
        parser.add_argument(
            '--dry-run',
            action='store_true',
            help='Show how many wallets would be created without actually creating them',
        )

    def handle(self, *args, **options):
        users_without_wallet = User.objects.exclude(
            id__in=Wallet.objects.values_list('user_id', flat=True)
        )
        count = users_without_wallet.count()

        if count == 0:
            self.stdout.write(self.style.SUCCESS('All users already have wallets.'))
            return

        if options['dry_run']:
            self.stdout.write(self.style.WARNING(
                f'{count} user(s) without wallets. Run without --dry-run to create them.'
            ))
            return

        created = 0
        for user in users_without_wallet.iterator():
            _, was_created = Wallet.objects.get_or_create(user=user)
            if was_created:
                created += 1

        self.stdout.write(self.style.SUCCESS(
            f'Created {created} wallet(s) for existing users.'
        ))
