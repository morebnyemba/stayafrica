# Generated migration for adding payment gateway configuration fields

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('admin_dashboard', '0002_rename_admin_dashb_user_id_idx_admin_dashb_user_id_cb4b17_idx_and_more'),
    ]

    operations = [
        migrations.AddField(
            model_name='systemconfiguration',
            name='flutterwave_secret_key',
            field=models.CharField(
                blank=True,
                help_text='Flutterwave Secret Key',
                max_length=255
            ),
        ),
        migrations.AddField(
            model_name='systemconfiguration',
            name='flutterwave_webhook_secret',
            field=models.CharField(
                blank=True,
                help_text='Flutterwave Webhook Hash',
                max_length=255
            ),
        ),
        migrations.AddField(
            model_name='systemconfiguration',
            name='paystack_secret_key',
            field=models.CharField(
                blank=True,
                help_text='Paystack Secret Key',
                max_length=255
            ),
        ),
        migrations.AddField(
            model_name='systemconfiguration',
            name='paystack_webhook_secret',
            field=models.CharField(
                blank=True,
                help_text='Paystack Webhook Secret',
                max_length=255
            ),
        ),
        migrations.AddField(
            model_name='systemconfiguration',
            name='paypal_client_id',
            field=models.CharField(
                blank=True,
                help_text='PayPal Client ID',
                max_length=255
            ),
        ),
        migrations.AddField(
            model_name='systemconfiguration',
            name='paypal_client_secret',
            field=models.CharField(
                blank=True,
                help_text='PayPal Client Secret',
                max_length=255
            ),
        ),
        migrations.AddField(
            model_name='systemconfiguration',
            name='paypal_mode',
            field=models.CharField(
                choices=[('sandbox', 'Sandbox'), ('live', 'Live')],
                default='sandbox',
                help_text='PayPal Environment',
                max_length=20
            ),
        ),
        migrations.AddField(
            model_name='systemconfiguration',
            name='paypal_webhook_id',
            field=models.CharField(
                blank=True,
                help_text='PayPal Webhook ID (for verification)',
                max_length=255
            ),
        ),
    ]
