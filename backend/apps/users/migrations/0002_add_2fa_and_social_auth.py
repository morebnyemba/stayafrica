# Generated migration for 2FA and Social Auth fields

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('users', '0001_initial'),  # Adjust this to your latest migration
    ]

    operations = [
        migrations.AddField(
            model_name='user',
            name='two_factor_enabled',
            field=models.BooleanField(default=False),
        ),
        migrations.AddField(
            model_name='user',
            name='two_factor_secret',
            field=models.CharField(blank=True, max_length=32, null=True),
        ),
        migrations.AddField(
            model_name='user',
            name='backup_codes',
            field=models.JSONField(blank=True, default=list),
        ),
        migrations.AddField(
            model_name='user',
            name='google_id',
            field=models.CharField(blank=True, max_length=255, null=True, unique=True),
        ),
        migrations.AddField(
            model_name='user',
            name='facebook_id',
            field=models.CharField(blank=True, max_length=255, null=True, unique=True),
        ),
        migrations.AddField(
            model_name='user',
            name='apple_id',
            field=models.CharField(blank=True, max_length=255, null=True, unique=True),
        ),
    ]
