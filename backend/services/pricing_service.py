"""
Dynamic Pricing Service
Integrates PricingRule model with booking calculations
"""
from decimal import Decimal
from datetime import date, datetime
from django.db.models import Q
import logging

logger = logging.getLogger(__name__)


class PricingService:
    """Service for calculating dynamic property pricing"""
    
    @staticmethod
    def calculate_price_for_booking(property_obj, check_in, check_out, booking_date=None):
        """
        Calculate total price for a booking considering dynamic pricing rules
        
        Args:
            property_obj: Property instance
            check_in: Check-in date
            check_out: Check-out date
            booking_date: Date booking was made (defaults to today)
            
        Returns:
            dict with pricing breakdown per night and total
        """
        from apps.payments.pricing_models import PricingRule, PropertyFee, PropertyTax
        
        if booking_date is None:
            booking_date = date.today()
        
        # Convert strings to dates if needed
        if isinstance(check_in, str):
            check_in = datetime.strptime(check_in, '%Y-%m-%d').date()
        if isinstance(check_out, str):
            check_out = datetime.strptime(check_out, '%Y-%m-%d').date()
        if isinstance(booking_date, str):
            booking_date = datetime.strptime(booking_date, '%Y-%m-%d').date()
        
        nights = (check_out - check_in).days
        base_price_per_night = Decimal(str(property_obj.price_per_night))
        
        # Get applicable pricing rules
        applicable_rules = PricingRule.objects.filter(
            property=property_obj,
            is_active=True
        ).order_by('-priority')
        
        # Calculate adjusted price per night
        total_adjustment = Decimal('0')
        applied_rules = []
        
        for rule in applicable_rules:
            if rule.applies_to_booking(check_in, check_out, booking_date):
                adjustment = rule.calculate_adjustment(base_price_per_night)
                total_adjustment += adjustment
                applied_rules.append({
                    'name': rule.name,
                    'type': rule.rule_type,
                    'adjustment': float(adjustment)
                })
        
        adjusted_price_per_night = base_price_per_night + total_adjustment
        # Ensure price doesn't go negative
        if adjusted_price_per_night < Decimal('0'):
            adjusted_price_per_night = base_price_per_night
        
        nightly_total = adjusted_price_per_night * nights
        
        # Calculate fees
        total_fees = Decimal('0')
        fee_breakdown = []
        
        fees = PropertyFee.objects.filter(property=property_obj, is_active=True)
        for fee in fees:
            fee_amount = fee.calculate_fee(nights, property_obj.max_guests or 2)
            total_fees += fee_amount
            if fee_amount > 0:
                fee_breakdown.append({
                    'name': fee.name,
                    'type': fee.fee_type,
                    'amount': float(fee_amount)
                })
        
        # Calculate taxes
        total_taxes = Decimal('0')
        tax_breakdown = []
        
        taxes = PropertyTax.objects.filter(property=property_obj, is_active=True)
        for tax in taxes:
            tax_amount = tax.calculate_tax(nightly_total, total_fees)
            total_taxes += tax_amount
            if tax_amount > 0:
                tax_breakdown.append({
                    'name': tax.name,
                    'type': tax.tax_type,
                    'rate': float(tax.rate),
                    'amount': float(tax_amount)
                })
        
        return {
            'base_price_per_night': float(base_price_per_night),
            'adjusted_price_per_night': float(adjusted_price_per_night),
            'nights': nights,
            'nightly_total': float(nightly_total),
            'total_fees': float(total_fees),
            'fee_breakdown': fee_breakdown,
            'total_taxes': float(total_taxes),
            'tax_breakdown': tax_breakdown,
            'subtotal': float(nightly_total + total_fees),
            'total_adjustments': float(total_adjustment),
            'applied_rules': applied_rules,
        }
    
    @staticmethod
    def get_price_calendar(property_obj, start_date, end_date):
        """
        Get pricing for each day in a date range
        Useful for displaying pricing calendar to hosts
        
        Args:
            property_obj: Property instance
            start_date: Start date for calendar
            end_date: End date for calendar
            
        Returns:
            list of dicts with date and price for each day
        """
        from datetime import timedelta
        
        if isinstance(start_date, str):
            start_date = datetime.strptime(start_date, '%Y-%m-%d').date()
        if isinstance(end_date, str):
            end_date = datetime.strptime(end_date, '%Y-%m-%d').date()
        
        calendar = []
        current_date = start_date
        
        while current_date <= end_date:
            # Calculate price for a one-night stay
            next_date = current_date + timedelta(days=1)
            pricing = PricingService.calculate_price_for_booking(
                property_obj,
                current_date,
                next_date,
                booking_date=date.today()
            )
            
            calendar.append({
                'date': current_date.isoformat(),
                'base_price': pricing['base_price_per_night'],
                'adjusted_price': pricing['adjusted_price_per_night'],
                'has_rules': len(pricing['applied_rules']) > 0,
                'rules': pricing['applied_rules']
            })
            
            current_date = next_date
        
        return calendar
    
    @staticmethod
    def get_pricing_summary(property_obj, check_in, check_out):
        """
        Get user-friendly pricing summary for display
        
        Args:
            property_obj: Property instance
            check_in: Check-in date
            check_out: Check-out date
            
        Returns:
            dict with formatted pricing information
        """
        pricing = PricingService.calculate_price_for_booking(
            property_obj, check_in, check_out
        )
        
        # Calculate price range if variable pricing
        price_range_text = f"${pricing['adjusted_price_per_night']:.2f}/night"
        if pricing['total_adjustments'] != 0:
            price_range_text = f"${pricing['adjusted_price_per_night']:.2f}/night (adjusted)"
        
        return {
            'display_text': price_range_text,
            'nightly_total': pricing['nightly_total'],
            'fees': pricing['total_fees'],
            'taxes': pricing['total_taxes'],
            'subtotal': pricing['subtotal'],
            'has_dynamic_pricing': len(pricing['applied_rules']) > 0,
            'breakdown': pricing
        }
