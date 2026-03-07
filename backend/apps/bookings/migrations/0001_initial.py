"""
Initial migration for bookings app.
Matches the existing DB schema so --fake-initial will skip table creation.
"""
from django.conf import settings
from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    initial = True

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
        ('properties', '0001_initial'),
    ]

    operations = [
        migrations.CreateModel(
            name='Booking',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('booking_ref', models.CharField(db_index=True, max_length=50, unique=True)),
                ('check_in', models.DateField()),
                ('check_out', models.DateField()),
                ('number_of_guests', models.PositiveIntegerField(default=1)),
                ('nightly_total', models.DecimalField(decimal_places=2, max_digits=10)),
                ('service_fee', models.DecimalField(decimal_places=2, default=3.00, max_digits=10)),
                ('commission_fee', models.DecimalField(decimal_places=2, max_digits=10)),
                ('cleaning_fee', models.DecimalField(blank=True, decimal_places=2, default=0, max_digits=10)),
                ('taxes', models.DecimalField(blank=True, decimal_places=2, default=0, help_text='Total taxes applied', max_digits=10)),
                ('grand_total', models.DecimalField(decimal_places=2, max_digits=10)),
                ('currency', models.CharField(default='USD', max_length=3)),
                ('status', models.CharField(choices=[('pending', 'Pending'), ('confirmed', 'Confirmed'), ('cancelled', 'Cancelled'), ('completed', 'Completed')], default='pending', max_length=20)),
                ('special_requests', models.TextField(blank=True, null=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
                ('guest', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='bookings', to=settings.AUTH_USER_MODEL)),
                ('rental_property', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='bookings', to='properties.property')),
            ],
            options={
                'ordering': ['-created_at'],
            },
        ),
        migrations.AddIndex(
            model_name='booking',
            index=models.Index(fields=['guest'], name='bookings_bo_guest_i_idx'),
        ),
        migrations.AddIndex(
            model_name='booking',
            index=models.Index(fields=['rental_property'], name='bookings_bo_rental__idx'),
        ),
        migrations.AddIndex(
            model_name='booking',
            index=models.Index(fields=['status'], name='bookings_bo_status_idx'),
        ),
        migrations.AddIndex(
            model_name='booking',
            index=models.Index(fields=['booking_ref'], name='bookings_bo_booking_idx'),
        ),
        migrations.AddIndex(
            model_name='booking',
            index=models.Index(fields=['check_in', 'status'], name='bookings_bo_check_i_idx'),
        ),
    ]
