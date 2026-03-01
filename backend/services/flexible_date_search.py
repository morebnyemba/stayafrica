"""
Flexible Date Search Service
Supports searching properties with date flexibility (±3 days, weekends, month-long)
"""
from datetime import datetime, timedelta, date
from decimal import Decimal
from django.db.models import Q
import logging

logger = logging.getLogger(__name__)


class FlexibleDateSearch:
    """Service for flexible date searching"""
    
    @staticmethod
    def parse_date(date_str):
        """Parse date string to date object"""
        if isinstance(date_str, date):
            return date_str
        try:
            return datetime.strptime(date_str, '%Y-%m-%d').date()
        except (ValueError, TypeError):
            return None
    
    @staticmethod
    def get_date_ranges(check_in, check_out, flexibility_type='exact', flexibility_days=3):
        """
        Generate date ranges based on flexibility type
        
        Args:
            check_in: Check-in date
            check_out: Check-out date
            flexibility_type: 'exact', 'flexible_days', 'weekend', 'month'
            flexibility_days: Number of days flexibility (for flexible_days type)
            
        Returns:
            list of (check_in, check_out) tuples
        """
        check_in = FlexibleDateSearch.parse_date(check_in)
        check_out = FlexibleDateSearch.parse_date(check_out)
        
        if not check_in or not check_out:
            return []
        
        nights = (check_out - check_in).days
        
        if flexibility_type == 'exact':
            return [(check_in, check_out)]
        
        elif flexibility_type == 'flexible_days':
            # Generate date ranges ±flexibility_days around the original dates
            ranges = []
            for offset in range(-flexibility_days, flexibility_days + 1):
                new_check_in = check_in + timedelta(days=offset)
                new_check_out = new_check_in + timedelta(days=nights)
                ranges.append((new_check_in, new_check_out))
            return ranges
        
        elif flexibility_type == 'weekend':
            # Find weekends around the check-in date
            ranges = []
            # Search within ±2 weeks
            for week_offset in range(-2, 3):
                # Find the nearest Friday (start of weekend)
                days_until_friday = (4 - check_in.weekday()) % 7
                friday = check_in + timedelta(days=days_until_friday) + timedelta(weeks=week_offset)
                
                # Weekend options: Fri-Sun (2 nights) or Fri-Mon (3 nights)
                ranges.append((friday, friday + timedelta(days=2)))  # Fri-Sun
                ranges.append((friday, friday + timedelta(days=3)))  # Fri-Mon
            return ranges
        
        elif flexibility_type == 'month':
            # Generate date ranges throughout the month
            ranges = []
            # Start from beginning of the month
            month_start = check_in.replace(day=1)
            current_date = month_start
            
            # Generate multiple date ranges throughout the month
            while current_date.month == check_in.month:
                end_date = current_date + timedelta(days=nights)
                if end_date.month == check_in.month or end_date <= check_in + timedelta(days=31):
                    ranges.append((current_date, end_date))
                current_date += timedelta(days=7)  # Weekly intervals
            
            return ranges
        
        return [(check_in, check_out)]
    
    @staticmethod
    def search_properties_with_flexible_dates(
        check_in,
        check_out,
        flexibility_type='exact',
        flexibility_days=3,
        filters=None
    ):
        """
        Search for available properties with flexible dates
        
        Args:
            check_in: Desired check-in date
            check_out: Desired check-out date
            flexibility_type: 'exact', 'flexible_days', 'weekend', 'month'
            flexibility_days: Number of days flexibility
            filters: Additional property filters (dict)
            
        Returns:
            dict with available properties grouped by date ranges
        """
        from apps.properties.models import Property
        from apps.bookings.models import Booking
        from utils.helpers import is_booking_date_available
        
        date_ranges = FlexibleDateSearch.get_date_ranges(
            check_in, check_out, flexibility_type, flexibility_days
        )
        
        # Start with active properties
        properties = Property.objects.filter(status='active')
        
        # Apply additional filters if provided
        if filters:
            if 'property_type' in filters:
                properties = properties.filter(property_type=filters['property_type'])
            if 'min_price' in filters:
                properties = properties.filter(price_per_night__gte=filters['min_price'])
            if 'max_price' in filters:
                properties = properties.filter(price_per_night__lte=filters['max_price'])
            if 'guests' in filters:
                properties = properties.filter(max_guests__gte=filters['guests'])
            if 'bedrooms' in filters:
                properties = properties.filter(bedrooms__gte=filters['bedrooms'])
            if 'city' in filters:
                properties = properties.filter(city__icontains=filters['city'])
            if 'country' in filters:
                properties = properties.filter(country=filters['country'])
        
        # Check availability for each date range
        results = []
        for date_range in date_ranges:
            range_check_in, range_check_out = date_range
            available_properties = []
            
            for property_obj in properties:
                if is_booking_date_available(property_obj, range_check_in, range_check_out):
                    # Calculate pricing for this date range
                    try:
                        from services.pricing_service import PricingService
                        pricing = PricingService.calculate_price_for_booking(
                            property_obj, range_check_in, range_check_out
                        )
                        
                        available_properties.append({
                            'property_id': property_obj.id,
                            'title': property_obj.title,
                            'check_in': range_check_in.isoformat(),
                            'check_out': range_check_out.isoformat(),
                            'nights': pricing['nights'],
                            'base_price_per_night': pricing['base_price_per_night'],
                            'adjusted_price_per_night': pricing['adjusted_price_per_night'],
                            'total_price': pricing['nightly_total'] + pricing['total_fees'] + pricing['total_taxes'],
                            'has_dynamic_pricing': pricing['total_adjustments'] != 0,
                        })
                    except Exception as e:
                        logger.warning(f"Error calculating pricing for property {property_obj.id}: {e}")
                        continue
            
            if available_properties:
                results.append({
                    'check_in': range_check_in.isoformat(),
                    'check_out': range_check_out.isoformat(),
                    'nights': (range_check_out - range_check_in).days,
                    'available_count': len(available_properties),
                    'properties': available_properties,
                })
        
        return {
            'flexibility_type': flexibility_type,
            'original_check_in': check_in.isoformat() if isinstance(check_in, date) else check_in,
            'original_check_out': check_out.isoformat() if isinstance(check_out, date) else check_out,
            'date_ranges_checked': len(date_ranges),
            'results': results,
            'total_available_options': len(results),
        }
    
    @staticmethod
    def get_price_range_summary(search_results):
        """
        Get min and max prices from search results
        
        Args:
            search_results: Results from search_properties_with_flexible_dates
            
        Returns:
            dict with min_price, max_price, avg_price
        """
        all_prices = []
        
        for date_range in search_results.get('results', []):
            for prop in date_range.get('properties', []):
                all_prices.append(float(prop['total_price']))
        
        if not all_prices:
            return {'min_price': 0, 'max_price': 0, 'avg_price': 0}
        
        return {
            'min_price': min(all_prices),
            'max_price': max(all_prices),
            'avg_price': sum(all_prices) / len(all_prices),
            'currency': FlexibleDateSearch._get_dominant_currency(search_results),
        }
    
    @staticmethod
    def _get_dominant_currency(search_results):
        """Determine the most common currency from search results."""
        from collections import Counter
        currencies = []
        for date_range in search_results.get('results', []):
            for prop in date_range.get('properties', []):
                currencies.append(prop.get('currency', 'USD'))
        if not currencies:
            from apps.admin_dashboard.models import SystemConfiguration
            config = SystemConfiguration.get_config()
            return config.default_currency
        return Counter(currencies).most_common(1)[0][0]
