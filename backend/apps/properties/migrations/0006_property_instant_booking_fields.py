# Generated migration for adding instant booking fields to Property model
from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('properties', '0005_alter_property_address_alter_property_main_image'),
    ]

    operations = [
        migrations.AddField(
            model_name='property',
            name='instant_booking_enabled',
            field=models.BooleanField(default=False, help_text='Auto-confirm bookings from qualified guests'),
        ),
        migrations.AddField(
            model_name='property',
            name='instant_booking_requirements',
            field=models.JSONField(blank=True, default=dict, help_text='Requirements for instant booking (e.g., verified guests only, min reviews)'),
        ),
    ]
