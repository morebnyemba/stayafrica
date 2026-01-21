# Generated manually
from django.db import migrations, models
import django.core.validators


class Migration(migrations.Migration):

    dependencies = [
        ('notifications', '0001_initial'),
    ]

    operations = [
        migrations.CreateModel(
            name='EmailConfiguration',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(default='Primary Email', help_text='Friendly name for this email configuration', max_length=100)),
                ('backend', models.CharField(choices=[('django.core.mail.backends.smtp.EmailBackend', 'SMTP'), ('django.core.mail.backends.console.EmailBackend', 'Console (Development)'), ('django_ses.SESBackend', 'Amazon SES'), ('anymail.backends.mailgun.EmailBackend', 'Mailgun'), ('anymail.backends.sendgrid.EmailBackend', 'SendGrid'), ('anymail.backends.mailjet.EmailBackend', 'Mailjet')], default='django.core.mail.backends.smtp.EmailBackend', help_text='Email backend to use', max_length=100)),
                ('host', models.CharField(default='smtp.gmail.com', help_text='SMTP server hostname', max_length=255)),
                ('port', models.PositiveIntegerField(default=587, help_text='SMTP server port (587 for TLS, 465 for SSL)', validators=[django.core.validators.MinValueValidator(1), django.core.validators.MaxValueValidator(65535)])),
                ('encryption', models.CharField(choices=[('none', 'None'), ('tls', 'TLS (Port 587)'), ('ssl', 'SSL (Port 465)')], default='tls', help_text='Connection encryption type', max_length=10)),
                ('username', models.CharField(blank=True, help_text='SMTP username (usually your email address)', max_length=255)),
                ('password', models.CharField(blank=True, help_text='SMTP password or app-specific password', max_length=255)),
                ('default_from_email', models.EmailField(default='noreply@stayafrica.com', help_text='Default sender email address', max_length=254)),
                ('default_from_name', models.CharField(default='StayAfrica', help_text='Default sender name', max_length=100)),
                ('timeout', models.PositiveIntegerField(default=30, help_text='Connection timeout in seconds')),
                ('fail_silently', models.BooleanField(default=False, help_text='If True, errors during sending will be silently ignored')),
                ('is_active', models.BooleanField(default=True, help_text='Whether this email configuration is active')),
                ('last_tested_at', models.DateTimeField(blank=True, help_text='Last time a test email was sent', null=True)),
                ('last_test_success', models.BooleanField(default=False, help_text='Whether the last test email was successful')),
                ('last_test_error', models.TextField(blank=True, help_text='Error message from last test if it failed')),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
            ],
            options={
                'verbose_name': 'Email Configuration',
                'verbose_name_plural': 'Email Configuration',
            },
        ),
    ]
