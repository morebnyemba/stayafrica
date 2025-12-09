# Generated migration file for SavedProperty model

from django.conf import settings
from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
        ('properties', '0001_initial'),  # Adjust this based on your last migration
    ]

    operations = [
        migrations.CreateModel(
            name='SavedProperty',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('property', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='saved_by_users', to='properties.property')),
                ('user', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='saved_properties', to=settings.AUTH_USER_MODEL)),
            ],
            options={
                'ordering': ['-created_at'],
            },
        ),
        migrations.AddIndex(
            model_name='savedproperty',
            index=models.Index(fields=['user', 'property'], name='properties_s_user_id_5a3b4c_idx'),
        ),
        migrations.AddIndex(
            model_name='savedproperty',
            index=models.Index(fields=['user'], name='properties_s_user_id_9d6e2f_idx'),
        ),
        migrations.AlterUniqueTogether(
            name='savedproperty',
            unique_together={('user', 'property')},
        ),
    ]
