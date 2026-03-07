from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('bookings', '0001_initial'),
    ]

    operations = [
        migrations.AddField(
            model_name='booking',
            name='checked_in_at',
            field=models.DateTimeField(blank=True, help_text='Actual check-in timestamp', null=True),
        ),
        migrations.AddField(
            model_name='booking',
            name='checked_out_at',
            field=models.DateTimeField(blank=True, help_text='Actual check-out timestamp', null=True),
        ),
        migrations.AddField(
            model_name='booking',
            name='check_in_instructions',
            field=models.TextField(blank=True, default='', help_text='Directions, access codes, or arrival info sent to guest'),
        ),
        migrations.AddField(
            model_name='booking',
            name='access_code',
            field=models.CharField(blank=True, default='', help_text='Lockbox, keypad, or smart-lock code', max_length=100),
        ),
        migrations.AlterField(
            model_name='booking',
            name='status',
            field=models.CharField(
                choices=[
                    ('pending', 'Pending'),
                    ('confirmed', 'Confirmed'),
                    ('checked_in', 'Checked In'),
                    ('checked_out', 'Checked Out'),
                    ('cancelled', 'Cancelled'),
                    ('completed', 'Completed'),
                ],
                default='pending',
                max_length=20,
            ),
        ),
        migrations.AddIndex(
            model_name='booking',
            index=models.Index(fields=['check_in', 'status'], name='bookings_bo_check_i_idx'),
        ),
    ]
