"""
Enhanced Booking Model Migration for Comprehensive Pricing
"""
from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    dependencies = [
        ('bookings', '0002_auto_previous'),  # Replace with your last migration
    ]

    operations = [
        migrations.AddField(
            model_name='booking',
            name='pricing_breakdown',
            field=models.JSONField(
                default=dict,
                blank=True,
                help_text='Detailed pricing calculation including fees, taxes, discounts'
            ),
        ),
        migrations.AddField(
            model_name='booking',
            name='base_price',
            field=models.DecimalField(
                max_digits=10,
                decimal_places=2,
                null=True,
                blank=True,
                help_text='Base nightly rate × nights'
            ),
        ),
        migrations.AddField(
            model_name='booking',
            name='total_fees',
            field=models.DecimalField(
                max_digits=10,
                decimal_places=2,
                default=0,
                help_text='Sum of all additional fees'
            ),
        ),
        migrations.AddField(
            model_name='booking',
            name='total_tax',
            field=models.DecimalField(
                max_digits=10,
                decimal_places=2,
                default=0,
                help_text='Sum of all taxes'
            ),
        ),
        migrations.AddField(
            model_name='booking',
            name='pricing_adjustments',
            field=models.DecimalField(
                max_digits=10,
                decimal_places=2,
                default=0,
                help_text='Discounts or premiums applied'
            ),
        ),
    ]
