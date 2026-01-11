"""
Enhanced Property Pricing Model
Supports dynamic pricing, seasonal rates, discounts, taxes
"""
from django.db import models
from apps.properties.models import Property
from decimal import Decimal


class PricingRule(models.Model):
    """Base pricing rule for properties"""
    RULE_TYPE_CHOICES = [
        ('seasonal', 'Seasonal Pricing'),
        ('weekend', 'Weekend Premium'),
        ('length_discount', 'Length of Stay Discount'),
        ('early_bird', 'Early Bird Discount'),
        ('last_minute', 'Last Minute Discount'),
    ]
    
    property = models.ForeignKey(Property, on_delete=models.CASCADE, related_name='pricing_rules')
    name = models.CharField(max_length=200)
    rule_type = models.CharField(max_length=50, choices=RULE_TYPE_CHOICES)
    is_active = models.BooleanField(default=True)
    priority = models.IntegerField(default=0, help_text='Higher priority rules apply first')
    
    # Date range for seasonal/time-based rules
    start_date = models.DateField(null=True, blank=True)
    end_date = models.DateField(null=True, blank=True)
    
    # Adjustment (percentage or fixed amount)
    adjustment_type = models.CharField(max_length=20, choices=[('percentage', 'Percentage'), ('fixed', 'Fixed Amount')], default='percentage')
    adjustment_value = models.DecimalField(max_digits=10, decimal_places=2, help_text='Percentage (e.g., 20 for 20%) or fixed amount')
    
    # Minimum stay requirement (for length discounts)
    min_nights = models.IntegerField(null=True, blank=True)
    max_nights = models.IntegerField(null=True, blank=True)
    
    # Advance booking (for early bird)
    min_days_advance = models.IntegerField(null=True, blank=True, help_text='Days before check-in')
    max_days_advance = models.IntegerField(null=True, blank=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-priority', 'start_date']
        indexes = [
            models.Index(fields=['property', 'is_active']),
            models.Index(fields=['start_date', 'end_date']),
        ]
    
    def __str__(self):
        return f'{self.property.title} - {self.name}'
    
    def applies_to_booking(self, check_in, check_out, booking_date):
        """Check if rule applies to a booking"""
        if not self.is_active:
            return False
        
        # Date range check
        if self.start_date and self.end_date:
            if not (self.start_date <= check_in <= self.end_date):
                return False
        
        # Length of stay check
        nights = (check_out - check_in).days
        if self.min_nights and nights < self.min_nights:
            return False
        if self.max_nights and nights > self.max_nights:
            return False
        
        # Advance booking check
        if self.min_days_advance or self.max_days_advance:
            days_advance = (check_in - booking_date).days
            if self.min_days_advance and days_advance < self.min_days_advance:
                return False
            if self.max_days_advance and days_advance > self.max_days_advance:
                return False
        
        return True
    
    def calculate_adjustment(self, base_price):
        """Calculate the adjusted price"""
        if self.adjustment_type == 'percentage':
            return base_price * (Decimal(str(self.adjustment_value)) / Decimal('100'))
        else:
            return Decimal(str(self.adjustment_value))


class PropertyFee(models.Model):
    """Additional fees for properties (cleaning, service, etc.)"""
    FEE_TYPE_CHOICES = [
        ('cleaning', 'Cleaning Fee'),
        ('service', 'Service Fee'),
        ('pet', 'Pet Fee'),
        ('extra_guest', 'Extra Guest Fee'),
        ('resort', 'Resort Fee'),
        ('parking', 'Parking Fee'),
        ('linen', 'Linen Fee'),
    ]
    
    CHARGE_TYPE_CHOICES = [
        ('per_booking', 'Per Booking'),
        ('per_night', 'Per Night'),
        ('per_guest', 'Per Guest'),
    ]
    
    property = models.ForeignKey(Property, on_delete=models.CASCADE, related_name='fees')
    fee_type = models.CharField(max_length=50, choices=FEE_TYPE_CHOICES)
    name = models.CharField(max_length=200)
    amount = models.DecimalField(max_digits=10, decimal_places=2)
    charge_type = models.CharField(max_length=20, choices=CHARGE_TYPE_CHOICES, default='per_booking')
    is_mandatory = models.BooleanField(default=True)
    is_active = models.BooleanField(default=True)
    
    # Conditions
    applies_after_guests = models.IntegerField(null=True, blank=True, help_text='Fee applies after this many guests')
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['fee_type']
        indexes = [
            models.Index(fields=['property', 'is_active']),
        ]
    
    def __str__(self):
        return f'{self.property.title} - {self.name} ({self.get_charge_type_display()})'
    
    def calculate_fee(self, nights, guests):
        """Calculate fee amount based on charge type"""
        if not self.is_active:
            return Decimal('0')
        
        if self.charge_type == 'per_night':
            return self.amount * Decimal(str(nights))
        elif self.charge_type == 'per_guest':
            if self.applies_after_guests:
                extra_guests = max(0, guests - self.applies_after_guests)
                return self.amount * Decimal(str(extra_guests))
            return self.amount * Decimal(str(guests))
        else:  # per_booking
            return self.amount


class PropertyTax(models.Model):
    """Tax configuration for properties"""
    TAX_TYPE_CHOICES = [
        ('vat', 'VAT/GST'),
        ('occupancy', 'Occupancy Tax'),
        ('tourism', 'Tourism Tax'),
        ('city', 'City Tax'),
    ]
    
    property = models.ForeignKey(Property, on_delete=models.CASCADE, related_name='taxes')
    tax_type = models.CharField(max_length=50, choices=TAX_TYPE_CHOICES)
    name = models.CharField(max_length=200)
    rate = models.DecimalField(max_digits=5, decimal_places=2, help_text='Percentage rate (e.g., 15 for 15%)')
    is_active = models.BooleanField(default=True)
    
    # Tax calculation basis
    applies_to_base_price = models.BooleanField(default=True)
    applies_to_fees = models.BooleanField(default=False)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['tax_type']
        verbose_name_plural = 'Property Taxes'
        indexes = [
            models.Index(fields=['property', 'is_active']),
        ]
    
    def __str__(self):
        return f'{self.property.title} - {self.name} ({self.rate}%)'
    
    def calculate_tax(self, base_amount, fees_amount=Decimal('0')):
        """Calculate tax amount"""
        if not self.is_active:
            return Decimal('0')
        
        taxable_amount = Decimal('0')
        if self.applies_to_base_price:
            taxable_amount += base_amount
        if self.applies_to_fees:
            taxable_amount += fees_amount
        
        return taxable_amount * (self.rate / Decimal('100'))


class CurrencyExchangeRate(models.Model):
    """Currency exchange rates for multi-currency support"""
    from_currency = models.CharField(max_length=3)
    to_currency = models.CharField(max_length=3)
    rate = models.DecimalField(max_digits=12, decimal_places=6)
    last_updated = models.DateTimeField(auto_now=True)
    is_active = models.BooleanField(default=True)
    
    class Meta:
        unique_together = ('from_currency', 'to_currency')
        ordering = ['from_currency', 'to_currency']
        indexes = [
            models.Index(fields=['from_currency', 'to_currency', 'is_active']),
        ]
    
    def __str__(self):
        return f'{self.from_currency} to {self.to_currency}: {self.rate}'
