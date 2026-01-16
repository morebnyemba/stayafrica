"""
Tax Collection Models
Handle tax calculation, collection, and remittance for bookings
"""
from django.db import models
from django.utils import timezone
from apps.users.models import User
from apps.properties.models import Property
from apps.bookings.models import Booking
from decimal import Decimal


class TaxJurisdiction(models.Model):
    """
    Tax jurisdictions (country, state, city levels)
    """
    JURISDICTION_TYPES = [
        ('country', 'Country'),
        ('state', 'State/Province'),
        ('city', 'City/Municipality'),
        ('special', 'Special District'),
    ]
    
    name = models.CharField(max_length=200)
    jurisdiction_type = models.CharField(max_length=20, choices=JURISDICTION_TYPES)
    code = models.CharField(max_length=50, unique=True, help_text='Jurisdiction code (e.g., US-CA-SF)')
    
    # Geographic identifiers
    country_code = models.CharField(max_length=2, help_text='ISO 3166-1 alpha-2')
    state_province_code = models.CharField(max_length=10, blank=True)
    city_name = models.CharField(max_length=100, blank=True)
    
    # Parent jurisdiction (hierarchical)
    parent_jurisdiction = models.ForeignKey(
        'self',
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='sub_jurisdictions'
    )
    
    is_active = models.BooleanField(default=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['country_code', 'state_province_code', 'city_name']
        indexes = [
            models.Index(fields=['code']),
            models.Index(fields=['country_code', 'state_province_code']),
        ]
    
    def __str__(self):
        return f"{self.name} ({self.code})"


class TaxRate(models.Model):
    """
    Tax rates for different jurisdictions
    """
    TAX_TYPES = [
        ('vat', 'Value Added Tax (VAT)'),
        ('sales', 'Sales Tax'),
        ('occupancy', 'Occupancy Tax / Hotel Tax'),
        ('tourism', 'Tourism Tax'),
        ('service', 'Service Tax'),
        ('other', 'Other'),
    ]
    
    jurisdiction = models.ForeignKey(
        TaxJurisdiction,
        on_delete=models.CASCADE,
        related_name='tax_rates'
    )
    
    name = models.CharField(max_length=200, help_text='Tax name')
    tax_type = models.CharField(max_length=20, choices=TAX_TYPES)
    
    # Rate configuration
    rate_percentage = models.DecimalField(
        max_digits=5,
        decimal_places=2,
        help_text='Tax rate as percentage (e.g., 12.50 for 12.5%)'
    )
    
    # Optional flat fee
    flat_fee = models.DecimalField(
        max_digits=10,
        decimal_places=2,
        default=0,
        help_text='Flat fee per booking (if applicable)'
    )
    
    # Applicability
    applies_to_base_price = models.BooleanField(
        default=True,
        help_text='Apply tax to base booking price'
    )
    applies_to_cleaning_fee = models.BooleanField(
        default=False,
        help_text='Apply tax to cleaning fee'
    )
    applies_to_service_fee = models.BooleanField(
        default=False,
        help_text='Apply tax to service fee'
    )
    
    # Effective dates
    effective_from = models.DateField()
    effective_to = models.DateField(null=True, blank=True)
    
    # Collection responsibility
    collected_by_platform = models.BooleanField(
        default=True,
        help_text='Platform collects and remits tax'
    )
    
    is_active = models.BooleanField(default=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['jurisdiction', '-effective_from']
        indexes = [
            models.Index(fields=['jurisdiction', 'is_active']),
            models.Index(fields=['effective_from', 'effective_to']),
        ]
    
    def __str__(self):
        return f"{self.name} - {self.rate_percentage}% ({self.jurisdiction})"
    
    def is_effective(self, date=None):
        """Check if tax rate is effective on a given date"""
        if not date:
            date = timezone.now().date()
        
        if not self.is_active:
            return False
        
        if date < self.effective_from:
            return False
        
        if self.effective_to and date > self.effective_to:
            return False
        
        return True


class BookingTax(models.Model):
    """
    Taxes applied to a specific booking
    """
    booking = models.ForeignKey(
        Booking,
        on_delete=models.CASCADE,
        related_name='booking_taxes'
    )
    
    tax_rate = models.ForeignKey(
        TaxRate,
        on_delete=models.PROTECT,
        related_name='bookings'
    )
    
    # Calculated amounts
    taxable_amount = models.DecimalField(
        max_digits=10,
        decimal_places=2,
        help_text='Amount subject to tax'
    )
    tax_amount = models.DecimalField(
        max_digits=10,
        decimal_places=2,
        help_text='Calculated tax amount'
    )
    
    # Metadata
    calculation_date = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        ordering = ['booking', 'tax_rate']
        indexes = [
            models.Index(fields=['booking']),
            models.Index(fields=['tax_rate']),
        ]
    
    def __str__(self):
        return f"{self.tax_rate.name} on {self.booking.booking_ref}: ${self.tax_amount}"


class TaxRemittance(models.Model):
    """
    Tax remittance records for compliance
    """
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('processing', 'Processing'),
        ('completed', 'Completed'),
        ('failed', 'Failed'),
    ]
    
    jurisdiction = models.ForeignKey(
        TaxJurisdiction,
        on_delete=models.PROTECT,
        related_name='remittances'
    )
    
    period_start = models.DateField(help_text='Reporting period start')
    period_end = models.DateField(help_text='Reporting period end')
    
    # Amounts
    total_tax_collected = models.DecimalField(
        max_digits=12,
        decimal_places=2,
        help_text='Total tax collected in period'
    )
    total_bookings = models.IntegerField(help_text='Number of bookings')
    
    # Remittance details
    remittance_date = models.DateField(null=True, blank=True)
    remittance_reference = models.CharField(
        max_length=200,
        blank=True,
        help_text='External reference number'
    )
    
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    
    # Files
    report_file_url = models.URLField(
        blank=True,
        help_text='URL to generated tax report'
    )
    confirmation_file_url = models.URLField(
        blank=True,
        help_text='URL to remittance confirmation'
    )
    
    notes = models.TextField(blank=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-period_end']
        unique_together = ('jurisdiction', 'period_start', 'period_end')
        indexes = [
            models.Index(fields=['jurisdiction', '-period_end']),
            models.Index(fields=['status', '-period_end']),
        ]
    
    def __str__(self):
        return f"{self.jurisdiction} - {self.period_start} to {self.period_end}"


class TaxExemption(models.Model):
    """
    Tax exemptions for specific properties or hosts
    """
    EXEMPTION_TYPES = [
        ('property', 'Property Exemption'),
        ('host', 'Host Exemption'),
        ('duration', 'Long Stay Exemption'),
    ]
    
    exemption_type = models.CharField(max_length=20, choices=EXEMPTION_TYPES)
    
    property = models.ForeignKey(
        Property,
        on_delete=models.CASCADE,
        null=True,
        blank=True,
        related_name='tax_exemptions'
    )
    host = models.ForeignKey(
        User,
        on_delete=models.CASCADE,
        null=True,
        blank=True,
        related_name='tax_exemptions'
    )
    
    jurisdiction = models.ForeignKey(
        TaxJurisdiction,
        on_delete=models.CASCADE,
        related_name='exemptions'
    )
    
    tax_rate = models.ForeignKey(
        TaxRate,
        on_delete=models.CASCADE,
        null=True,
        blank=True,
        related_name='exemptions',
        help_text='Specific tax rate (or null for all)'
    )
    
    # Exemption details
    exemption_certificate_number = models.CharField(
        max_length=100,
        blank=True
    )
    reason = models.TextField()
    
    # Validity
    valid_from = models.DateField()
    valid_to = models.DateField(null=True, blank=True)
    
    is_active = models.BooleanField(default=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-valid_from']
        indexes = [
            models.Index(fields=['property', 'is_active']),
            models.Index(fields=['host', 'is_active']),
            models.Index(fields=['jurisdiction', 'is_active']),
        ]
    
    def __str__(self):
        target = self.property or self.host
        return f"Exemption for {target} - {self.jurisdiction}"
    
    def is_valid(self, date=None):
        """Check if exemption is valid on a given date"""
        if not date:
            date = timezone.now().date()
        
        if not self.is_active:
            return False
        
        if date < self.valid_from:
            return False
        
        if self.valid_to and date > self.valid_to:
            return False
        
        return True
