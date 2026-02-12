"""
Data migration: seed SocialApp rows from environment variables so that
OAuth credentials live in the database (managed via Django Admin) rather
than in settings.py.

After this migration runs, manage credentials at:
  /admin/socialaccount/socialapp/
"""

import os
from django.db import migrations


def seed_social_apps(apps, schema_editor):
    """Create SocialApp rows for each provider if env vars are set."""
    SocialApp = apps.get_model('socialaccount', 'SocialApp')
    Site = apps.get_model('sites', 'Site')

    # Ensure at least the default site exists
    site, _ = Site.objects.get_or_create(
        id=1,
        defaults={'domain': 'stayafrica.app', 'name': 'StayAfrica'},
    )

    providers = [
        {
            'provider': 'google',
            'name': 'Google',
            'client_id': os.getenv('GOOGLE_CLIENT_ID', ''),
            'secret': os.getenv('GOOGLE_CLIENT_SECRET', ''),
        },
        {
            'provider': 'facebook',
            'name': 'Facebook',
            'client_id': os.getenv('FACEBOOK_APP_ID', ''),
            'secret': os.getenv('FACEBOOK_APP_SECRET', ''),
        },
        {
            'provider': 'apple',
            'name': 'Apple',
            'client_id': os.getenv('APPLE_SERVICE_ID', ''),
            'secret': os.getenv('APPLE_KEY_ID', ''),
            'key': os.getenv('APPLE_TEAM_ID', ''),
        },
    ]

    for p in providers:
        # Skip if no client_id configured
        if not p.get('client_id'):
            continue

        app, created = SocialApp.objects.get_or_create(
            provider=p['provider'],
            defaults={
                'name': p['name'],
                'client_id': p['client_id'],
                'secret': p.get('secret', ''),
                'key': p.get('key', ''),
            },
        )
        if created:
            app.sites.add(site)


def reverse_seed(apps, schema_editor):
    """Remove seeded social apps (optional - doesn't delete manually added ones)."""
    pass


class Migration(migrations.Migration):

    dependencies = [
        ('users', '0002_add_2fa_and_social_auth'),
        ('socialaccount', '0001_initial'),
        ('sites', '0002_alter_domain_unique'),
    ]

    operations = [
        migrations.RunPython(seed_social_apps, reverse_seed),
    ]
