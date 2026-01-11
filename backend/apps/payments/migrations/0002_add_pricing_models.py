"""
Pricing Models Migration
"""
from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    dependencies = [
        ('payments', '0001_initial'),
        ('properties', '0001_initial'),
    ]

    operations = [
        migrations.CreateModel(
            name='PricingRule',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(max_length=200)),
                ('rule_type', models.CharField(choices=[('seasonal', 'Seasonal Pricing'), ('weekend', 'Weekend Premium'), ('length_discount', 'Length of Stay Discount'), ('early_bird', 'Early Bird Discount'), ('last_minute', 'Last Minute Discount')], max_length=50)),
                ('is_active', models.BooleanField(default=True)),
                ('priority', models.IntegerField(default=0, help_text='Higher priority rules apply first')),
                ('start_date', models.DateField(blank=True, null=True)),
                ('end_date', models.DateField(blank=True, null=True)),
                ('adjustment_type', models.CharField(choices=[('percentage', 'Percentage'), ('fixed', 'Fixed Amount')], default='percentage', max_length=20)),
                ('adjustment_value', models.DecimalField(decimal_places=2, help_text='Percentage (e.g., 20 for 20%) or fixed amount', max_digits=10)),
                ('min_nights', models.IntegerField(blank=True, null=True)),
                ('max_nights', models.IntegerField(blank=True, null=True)),
                ('min_days_advance', models.IntegerField(blank=True, help_text='Days before check-in', null=True)),
                ('max_days_advance', models.IntegerField(blank=True, null=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
                ('property', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='pricing_rules', to='properties.property')),
            ],
            options={
                'ordering': ['-priority', 'start_date'],
            },
        ),
        migrations.CreateModel(
            name='PropertyFee',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('fee_type', models.CharField(choices=[('cleaning', 'Cleaning Fee'), ('service', 'Service Fee'), ('pet', 'Pet Fee'), ('extra_guest', 'Extra Guest Fee'), ('resort', 'Resort Fee'), ('parking', 'Parking Fee'), ('linen', 'Linen Fee')], max_length=50)),
                ('name', models.CharField(max_length=200)),
                ('amount', models.DecimalField(decimal_places=2, max_digits=10)),
                ('charge_type', models.CharField(choices=[('per_booking', 'Per Booking'), ('per_night', 'Per Night'), ('per_guest', 'Per Guest')], default='per_booking', max_length=20)),
                ('is_mandatory', models.BooleanField(default=True)),
                ('is_active', models.BooleanField(default=True)),
                ('applies_after_guests', models.IntegerField(blank=True, help_text='Fee applies after this many guests', null=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
                ('property', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='fees', to='properties.property')),
            ],
            options={
                'ordering': ['fee_type'],
            },
        ),
        migrations.CreateModel(
            name='PropertyTax',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('tax_type', models.CharField(choices=[('vat', 'VAT/GST'), ('occupancy', 'Occupancy Tax'), ('tourism', 'Tourism Tax'), ('city', 'City Tax')], max_length=50)),
                ('name', models.CharField(max_length=200)),
                ('rate', models.DecimalField(decimal_places=2, help_text='Percentage rate (e.g., 15 for 15%)', max_digits=5)),
                ('is_active', models.BooleanField(default=True)),
                ('applies_to_base_price', models.BooleanField(default=True)),
                ('applies_to_fees', models.BooleanField(default=False)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
                ('property', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='taxes', to='properties.property')),
            ],
            options={
                'verbose_name_plural': 'Property Taxes',
                'ordering': ['tax_type'],
            },
        ),
        migrations.CreateModel(
            name='CurrencyExchangeRate',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('from_currency', models.CharField(max_length=3)),
                ('to_currency', models.CharField(max_length=3)),
                ('rate', models.DecimalField(decimal_places=6, max_digits=12)),
                ('last_updated', models.DateTimeField(auto_now=True)),
                ('is_active', models.BooleanField(default=True)),
            ],
            options={
                'unique_together': {('from_currency', 'to_currency')},
                'ordering': ['from_currency', 'to_currency'],
            },
        ),
        migrations.AddIndex(
            model_name='pricingrule',
            index=models.Index(fields=['property', 'is_active'], name='payments_pr_propert_idx'),
        ),
        migrations.AddIndex(
            model_name='pricingrule',
            index=models.Index(fields=['start_date', 'end_date'], name='payments_pr_start_d_idx'),
        ),
        migrations.AddIndex(
            model_name='propertyfee',
            index=models.Index(fields=['property', 'is_active'], name='payments_pf_propert_idx'),
        ),
        migrations.AddIndex(
            model_name='propertytax',
            index=models.Index(fields=['property', 'is_active'], name='payments_pt_propert_idx'),
        ),
        migrations.AddIndex(
            model_name='currencyexchangerate',
            index=models.Index(fields=['from_currency', 'to_currency', 'is_active'], name='payments_ce_from_cu_idx'),
        ),
    ]
