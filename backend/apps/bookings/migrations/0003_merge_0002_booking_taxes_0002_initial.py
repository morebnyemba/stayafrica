# Generated merge migration to resolve conflicting migrations
from django.db import migrations


class Migration(migrations.Migration):

    dependencies = [
        ('bookings', '0002_booking_taxes'),
        ('bookings', '0002_initial'),
    ]

    operations = [
    ]
