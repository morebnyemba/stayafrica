"""
Tax Collection Service
Calculate, collect, and report taxes for bookings
"""
from django.db.models import Sum, Count
from django.utils import timezone
from decimal import Decimal
from datetime import timedelta
import logging

logger = logging.getLogger(__name__)


class TaxCollectionService:
    """Service for tax calculation and collection"""
    
    @staticmethod
    def calculate_booking_taxes(booking, check_date=None):
        """
        Calculate all applicable taxes for a booking
        
        Args:
            booking: Booking instance
            check_date: Date to check tax rates (defaults to booking creation date)
            
        Returns:
            List of (TaxRate, tax_amount) tuples
        """
        from apps.payments.tax_models import TaxRate, TaxJurisdiction, TaxExemption, BookingTax
        
        if not check_date:
            check_date = booking.created_at.date()
        
        property = booking.rental_property
        
        # Determine jurisdiction based on property location
        jurisdiction = TaxCollectionService._get_property_jurisdiction(property)
        
        if not jurisdiction:
            logger.warning(f"No jurisdiction found for property {property.id}")
            return []
        
        # Check for exemptions
        exemptions = TaxExemption.objects.filter(
            jurisdiction=jurisdiction,
            is_active=True
        ).filter(
            models.Q(property=property) | models.Q(host=property.host)
        )
        
        active_exemptions = [e for e in exemptions if e.is_valid(check_date)]
        
        # Get applicable tax rates
        tax_rates = TaxRate.objects.filter(
            jurisdiction=jurisdiction,
            is_active=True
        )
        
        # Filter by effective dates
        effective_rates = [rate for rate in tax_rates if rate.is_effective(check_date)]
        
        taxes = []
        for tax_rate in effective_rates:
            # Check if exempt
            is_exempt = any(
                e.tax_rate is None or e.tax_rate == tax_rate
                for e in active_exemptions
            )
            
            if is_exempt:
                logger.info(f"Booking {booking.booking_ref} exempt from {tax_rate.name}")
                continue
            
            # Calculate taxable amount
            taxable_amount = Decimal('0')
            
            if tax_rate.applies_to_base_price:
                taxable_amount += booking.nightly_total
            
            if tax_rate.applies_to_cleaning_fee:
                taxable_amount += booking.cleaning_fee
            
            if tax_rate.applies_to_service_fee:
                taxable_amount += booking.service_fee
            
            # Calculate tax
            tax_amount = (taxable_amount * tax_rate.rate_percentage / 100) + tax_rate.flat_fee
            tax_amount = tax_amount.quantize(Decimal('0.01'))
            
            taxes.append((tax_rate, tax_amount))
            
            # Create BookingTax record
            BookingTax.objects.get_or_create(
                booking=booking,
                tax_rate=tax_rate,
                defaults={
                    'taxable_amount': taxable_amount,
                    'tax_amount': tax_amount
                }
            )
        
        return taxes
    
    @staticmethod
    def _get_property_jurisdiction(property):
        """Determine tax jurisdiction for a property"""
        from apps.payments.tax_models import TaxJurisdiction
        
        # Try to find jurisdiction by property location
        # This is simplified - in production, use geocoding or property.location
        
        # For now, try to match by country code from property
        # Assuming property has a country field or address
        country_code = getattr(property, 'country_code', 'US')
        
        # Try to find most specific jurisdiction
        jurisdiction = TaxJurisdiction.objects.filter(
            country_code=country_code,
            is_active=True
        ).order_by('-jurisdiction_type').first()
        
        return jurisdiction
    
    @staticmethod
    def generate_tax_report(jurisdiction, start_date, end_date):
        """
        Generate tax report for a jurisdiction and period
        
        Args:
            jurisdiction: TaxJurisdiction instance
            start_date: Period start date
            end_date: Period end date
            
        Returns:
            Dict with report data
        """
        from apps.payments.tax_models import BookingTax, TaxRate
        from apps.bookings.models import Booking
        
        # Get all booking taxes in period
        booking_taxes = BookingTax.objects.filter(
            tax_rate__jurisdiction=jurisdiction,
            booking__created_at__date__gte=start_date,
            booking__created_at__date__lte=end_date,
            booking__status__in=['confirmed', 'completed']
        ).select_related('tax_rate', 'booking')
        
        # Aggregate by tax type
        by_tax_type = {}
        total_collected = Decimal('0')
        
        for booking_tax in booking_taxes:
            tax_type = booking_tax.tax_rate.tax_type
            
            if tax_type not in by_tax_type:
                by_tax_type[tax_type] = {
                    'tax_name': booking_tax.tax_rate.name,
                    'rate_percentage': booking_tax.tax_rate.rate_percentage,
                    'total_taxable': Decimal('0'),
                    'total_tax': Decimal('0'),
                    'booking_count': 0
                }
            
            by_tax_type[tax_type]['total_taxable'] += booking_tax.taxable_amount
            by_tax_type[tax_type]['total_tax'] += booking_tax.tax_amount
            by_tax_type[tax_type]['booking_count'] += 1
            total_collected += booking_tax.tax_amount
        
        report = {
            'jurisdiction': {
                'name': jurisdiction.name,
                'code': jurisdiction.code
            },
            'period': {
                'start': start_date,
                'end': end_date
            },
            'summary': {
                'total_tax_collected': total_collected,
                'total_bookings': booking_taxes.count(),
                'total_properties': booking_taxes.values('booking__rental_property').distinct().count()
            },
            'by_tax_type': by_tax_type
        }
        
        return report
    
    @staticmethod
    def create_remittance(jurisdiction, start_date, end_date):
        """
        Create tax remittance record
        
        Args:
            jurisdiction: TaxJurisdiction instance
            start_date: Period start date
            end_date: Period end date
            
        Returns:
            TaxRemittance instance
        """
        from apps.payments.tax_models import TaxRemittance, BookingTax
        
        # Generate report
        report = TaxCollectionService.generate_tax_report(
            jurisdiction, start_date, end_date
        )
        
        # Create remittance record
        remittance = TaxRemittance.objects.create(
            jurisdiction=jurisdiction,
            period_start=start_date,
            period_end=end_date,
            total_tax_collected=report['summary']['total_tax_collected'],
            total_bookings=report['summary']['total_bookings'],
            status='pending'
        )
        
        logger.info(f"Created remittance {remittance.id} for {jurisdiction.name}")
        
        return remittance
    
    @staticmethod
    def process_remittance(remittance_id, reference_number=None):
        """
        Mark remittance as completed
        
        Args:
            remittance_id: TaxRemittance ID
            reference_number: Optional external reference
            
        Returns:
            TaxRemittance instance
        """
        from apps.payments.tax_models import TaxRemittance
        
        try:
            remittance = TaxRemittance.objects.get(id=remittance_id)
            
            remittance.status = 'completed'
            remittance.remittance_date = timezone.now().date()
            
            if reference_number:
                remittance.remittance_reference = reference_number
            
            remittance.save()
            
            logger.info(f"Processed remittance {remittance_id}")
            return remittance
            
        except TaxRemittance.DoesNotExist:
            logger.error(f"Remittance {remittance_id} not found")
            return None
    
    @staticmethod
    def get_tax_summary_for_host(host, start_date=None, end_date=None):
        """
        Get tax summary for a host
        
        Args:
            host: User instance (host)
            start_date: Optional start date
            end_date: Optional end date
            
        Returns:
            Dict with tax summary
        """
        from apps.payments.tax_models import BookingTax
        from apps.properties.models import Property
        
        if not start_date:
            start_date = (timezone.now() - timedelta(days=365)).date()
        if not end_date:
            end_date = timezone.now().date()
        
        # Get host's properties
        properties = Property.objects.filter(host=host)
        
        # Get booking taxes
        booking_taxes = BookingTax.objects.filter(
            booking__rental_property__in=properties,
            booking__created_at__date__gte=start_date,
            booking__created_at__date__lte=end_date
        ).select_related('tax_rate', 'booking')
        
        # Aggregate
        summary = booking_taxes.aggregate(
            total_tax_collected=Sum('tax_amount'),
            total_bookings=Count('booking', distinct=True)
        )
        
        # By jurisdiction
        by_jurisdiction = {}
        for booking_tax in booking_taxes:
            jurisdiction_code = booking_tax.tax_rate.jurisdiction.code
            
            if jurisdiction_code not in by_jurisdiction:
                by_jurisdiction[jurisdiction_code] = {
                    'name': booking_tax.tax_rate.jurisdiction.name,
                    'total_tax': Decimal('0'),
                    'booking_count': 0
                }
            
            by_jurisdiction[jurisdiction_code]['total_tax'] += booking_tax.tax_amount
            by_jurisdiction[jurisdiction_code]['booking_count'] += 1
        
        return {
            'period': {'start': start_date, 'end': end_date},
            'summary': summary,
            'by_jurisdiction': by_jurisdiction
        }
