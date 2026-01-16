"""
Host Analytics Models
Track revenue, occupancy, performance metrics for hosts
"""
from django.db import models
from django.utils import timezone
from apps.users.models import User
from apps.properties.models import Property
from datetime import timedelta


class PropertyAnalytics(models.Model):
    """
    Daily analytics aggregation for properties
    """
    property = models.ForeignKey(
        Property,
        on_delete=models.CASCADE,
        related_name='analytics'
    )
    date = models.DateField(help_text='Date for these analytics')
    
    # Revenue metrics
    total_revenue = models.DecimalField(
        max_digits=10,
        decimal_places=2,
        default=0,
        help_text='Total revenue earned'
    )
    bookings_count = models.IntegerField(default=0)
    nights_booked = models.IntegerField(default=0)
    avg_nightly_rate = models.DecimalField(
        max_digits=10,
        decimal_places=2,
        null=True,
        blank=True
    )
    
    # Occupancy metrics
    occupancy_rate = models.DecimalField(
        max_digits=5,
        decimal_places=2,
        default=0,
        help_text='Percentage of nights booked'
    )
    nights_available = models.IntegerField(default=1)
    
    # Performance metrics
    views_count = models.IntegerField(default=0, help_text='Property views')
    inquiries_count = models.IntegerField(default=0, help_text='Booking inquiries')
    conversion_rate = models.DecimalField(
        max_digits=5,
        decimal_places=2,
        default=0,
        help_text='Inquiry to booking conversion %'
    )
    
    # Guest metrics
    unique_guests = models.IntegerField(default=0)
    returning_guests = models.IntegerField(default=0)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-date']
        unique_together = ('property', 'date')
        indexes = [
            models.Index(fields=['property', '-date']),
            models.Index(fields=['date']),
        ]
        verbose_name_plural = 'Property Analytics'
    
    def __str__(self):
        return f"{self.property.title} - {self.date}"


class HostAnalyticsSummary(models.Model):
    """
    Aggregated analytics summary for hosts across all properties
    """
    PERIOD_CHOICES = [
        ('daily', 'Daily'),
        ('weekly', 'Weekly'),
        ('monthly', 'Monthly'),
        ('yearly', 'Yearly'),
    ]
    
    host = models.ForeignKey(
        User,
        on_delete=models.CASCADE,
        related_name='analytics_summaries'
    )
    period = models.CharField(max_length=20, choices=PERIOD_CHOICES)
    start_date = models.DateField()
    end_date = models.DateField()
    
    # Revenue summary
    total_revenue = models.DecimalField(max_digits=12, decimal_places=2, default=0)
    total_bookings = models.IntegerField(default=0)
    total_nights_booked = models.IntegerField(default=0)
    avg_booking_value = models.DecimalField(
        max_digits=10,
        decimal_places=2,
        null=True,
        blank=True
    )
    
    # Occupancy summary
    avg_occupancy_rate = models.DecimalField(
        max_digits=5,
        decimal_places=2,
        default=0
    )
    properties_count = models.IntegerField(default=0)
    
    # Performance summary
    total_views = models.IntegerField(default=0)
    total_inquiries = models.IntegerField(default=0)
    avg_conversion_rate = models.DecimalField(
        max_digits=5,
        decimal_places=2,
        default=0
    )
    
    # Guest summary
    total_unique_guests = models.IntegerField(default=0)
    total_returning_guests = models.IntegerField(default=0)
    guest_retention_rate = models.DecimalField(
        max_digits=5,
        decimal_places=2,
        default=0
    )
    
    # Rating metrics
    avg_rating = models.DecimalField(
        max_digits=3,
        decimal_places=2,
        null=True,
        blank=True
    )
    total_reviews = models.IntegerField(default=0)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-start_date']
        indexes = [
            models.Index(fields=['host', '-start_date']),
            models.Index(fields=['period', '-start_date']),
        ]
        verbose_name_plural = 'Host Analytics Summaries'
    
    def __str__(self):
        return f"{self.host.email} - {self.period} ({self.start_date} to {self.end_date})"


class RevenueProjection(models.Model):
    """
    Revenue projections based on historical data
    """
    host = models.ForeignKey(
        User,
        on_delete=models.CASCADE,
        related_name='revenue_projections'
    )
    property = models.ForeignKey(
        Property,
        on_delete=models.CASCADE,
        null=True,
        blank=True,
        related_name='revenue_projections',
        help_text='Specific property or null for all properties'
    )
    
    projection_date = models.DateField(help_text='Date of projection')
    target_month = models.DateField(help_text='Month being projected')
    
    projected_revenue = models.DecimalField(max_digits=12, decimal_places=2)
    projected_bookings = models.IntegerField()
    projected_occupancy = models.DecimalField(max_digits=5, decimal_places=2)
    
    confidence_level = models.DecimalField(
        max_digits=5,
        decimal_places=2,
        help_text='Confidence percentage (0-100)'
    )
    
    # Factors
    seasonal_factor = models.DecimalField(
        max_digits=5,
        decimal_places=2,
        default=1.0,
        help_text='Seasonal adjustment multiplier'
    )
    trend_factor = models.DecimalField(
        max_digits=5,
        decimal_places=2,
        default=1.0,
        help_text='Trend adjustment multiplier'
    )
    
    created_at = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        ordering = ['-target_month']
        unique_together = ('host', 'property', 'target_month', 'projection_date')
        indexes = [
            models.Index(fields=['host', '-target_month']),
            models.Index(fields=['property', '-target_month']),
        ]
    
    def __str__(self):
        prop_name = self.property.title if self.property else "All Properties"
        return f"{self.host.email} - {prop_name} - {self.target_month}"


class PerformanceBenchmark(models.Model):
    """
    Benchmark metrics for comparing host performance
    """
    region = models.CharField(
        max_length=100,
        help_text='Geographic region for benchmark'
    )
    property_type = models.CharField(
        max_length=50,
        help_text='Type of property (house, apartment, etc.)'
    )
    month = models.DateField(help_text='Month for benchmark data')
    
    # Benchmark metrics
    avg_occupancy_rate = models.DecimalField(max_digits=5, decimal_places=2)
    avg_nightly_rate = models.DecimalField(max_digits=10, decimal_places=2)
    avg_revenue_per_available_night = models.DecimalField(
        max_digits=10,
        decimal_places=2
    )
    avg_booking_lead_time_days = models.IntegerField(
        help_text='Average days between booking and check-in'
    )
    avg_length_of_stay = models.DecimalField(max_digits=5, decimal_places=2)
    
    # Percentiles
    occupancy_75th_percentile = models.DecimalField(max_digits=5, decimal_places=2)
    occupancy_90th_percentile = models.DecimalField(max_digits=5, decimal_places=2)
    rate_75th_percentile = models.DecimalField(max_digits=10, decimal_places=2)
    rate_90th_percentile = models.DecimalField(max_digits=10, decimal_places=2)
    
    sample_size = models.IntegerField(help_text='Number of properties in sample')
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-month']
        unique_together = ('region', 'property_type', 'month')
        indexes = [
            models.Index(fields=['region', 'property_type', '-month']),
        ]
    
    def __str__(self):
        return f"{self.region} - {self.property_type} - {self.month}"
