# Generated migration for adding taxes field to Booking model
from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('bookings', '0001_initial'),
    ]

    operations = [
        migrations.AddField(
            model_name='booking',
            name='taxes',
            field=models.DecimalField(blank=True, decimal_places=2, default=0, help_text='Total taxes applied', max_digits=10),
        ),
    ]
