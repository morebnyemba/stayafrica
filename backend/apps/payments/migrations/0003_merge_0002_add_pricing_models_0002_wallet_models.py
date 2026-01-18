# Generated merge migration to resolve conflicting migrations
from django.db import migrations


class Migration(migrations.Migration):

    dependencies = [
        ('payments', '0002_add_pricing_models'),
        ('payments', '0002_wallet_models'),
    ]

    operations = [
    ]
