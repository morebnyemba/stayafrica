# Generated migration for admin_dashboard app

from django.conf import settings
from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    initial = True

    dependencies = [
        ('contenttypes', '0002_remove_content_type_name'),
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
    ]

    operations = [
        migrations.CreateModel(
            name='SystemConfiguration',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('commission_rate', models.DecimalField(decimal_places=4, default=0.07, help_text='Commission rate (e.g., 0.07 for 7%)', max_digits=5)),
                ('service_fee', models.DecimalField(decimal_places=2, default=3.0, help_text='Service fee per booking', max_digits=10)),
                ('default_currency', models.CharField(default='USD', help_text='Default currency code (e.g., USD, ZAR)', max_length=3)),
                ('paynow_integration_id', models.CharField(blank=True, help_text='Paynow Integration ID', max_length=255)),
                ('paynow_integration_key', models.CharField(blank=True, help_text='Paynow Integration Key', max_length=255)),
                ('paynow_webhook_secret', models.CharField(blank=True, help_text='Paynow Webhook Secret', max_length=255)),
                ('payfast_merchant_id', models.CharField(blank=True, help_text='PayFast Merchant ID', max_length=255)),
                ('payfast_merchant_key', models.CharField(blank=True, help_text='PayFast Merchant Key', max_length=255)),
                ('payfast_passphrase', models.CharField(blank=True, help_text='PayFast Passphrase', max_length=255)),
                ('payfast_webhook_secret', models.CharField(blank=True, help_text='PayFast Webhook Secret', max_length=255)),
                ('stripe_secret_key', models.CharField(blank=True, help_text='Stripe Secret Key', max_length=255)),
                ('stripe_publishable_key', models.CharField(blank=True, help_text='Stripe Publishable Key', max_length=255)),
                ('stripe_webhook_secret', models.CharField(blank=True, help_text='Stripe Webhook Secret', max_length=255)),
                ('max_advance_booking_days', models.IntegerField(default=365, help_text='Maximum days in advance for bookings')),
                ('max_stay_duration_days', models.IntegerField(default=90, help_text='Maximum stay duration in days')),
                ('review_window_days', models.IntegerField(default=30, help_text='Days after checkout to submit review')),
                ('review_edit_window_days', models.IntegerField(default=7, help_text='Days to edit review after submission')),
                ('admin_email', models.EmailField(blank=True, help_text='Admin notification email', max_length=254)),
                ('support_email', models.EmailField(blank=True, help_text='Support email for user inquiries', max_length=254)),
                ('maintenance_mode', models.BooleanField(default=False, help_text='Enable maintenance mode (API read-only)')),
                ('maintenance_message', models.TextField(blank=True, help_text='Message displayed during maintenance')),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
            ],
            options={
                'verbose_name': 'System Configuration',
                'verbose_name_plural': 'System Configuration',
            },
        ),
        migrations.CreateModel(
            name='AdminStats',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('total_revenue', models.DecimalField(decimal_places=2, default=0, max_digits=12)),
                ('total_bookings', models.IntegerField(default=0)),
                ('total_users', models.IntegerField(default=0)),
                ('active_hosts', models.IntegerField(default=0)),
                ('total_properties', models.IntegerField(default=0)),
                ('last_updated', models.DateTimeField(auto_now=True)),
            ],
            options={
                'verbose_name_plural': 'Admin Stats',
            },
        ),
        migrations.CreateModel(
            name='AuditLog',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('action', models.CharField(max_length=100)),
                ('object_id', models.PositiveIntegerField(blank=True, null=True)),
                ('changes', models.JSONField(blank=True, default=dict)),
                ('timestamp', models.DateTimeField(auto_now_add=True, db_index=True)),
                ('content_type', models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.SET_NULL, to='contenttypes.contenttype')),
                ('user', models.ForeignKey(null=True, on_delete=django.db.models.deletion.SET_NULL, to=settings.AUTH_USER_MODEL)),
            ],
            options={
                'ordering': ['-timestamp'],
            },
        ),
        migrations.AddIndex(
            model_name='auditlog',
            index=models.Index(fields=['user'], name='admin_dashb_user_id_idx'),
        ),
        migrations.AddIndex(
            model_name='auditlog',
            index=models.Index(fields=['action'], name='admin_dashb_action_idx'),
        ),
        migrations.AddIndex(
            model_name='auditlog',
            index=models.Index(fields=['timestamp'], name='admin_dashb_timestamp_idx'),
        ),
    ]
