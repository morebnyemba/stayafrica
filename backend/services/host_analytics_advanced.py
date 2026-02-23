"""
Host Analytics Service
Generate insights, projections, and performance metrics for hosts
"""
from django.db.models import Sum, Avg, Count, Q, F
from django.utils import timezone
from datetime import timedelta, date
from decimal import Decimal
import logging

logger = logging.getLogger(__name__)


class HostAnalyticsService:
    """Service for generating host analytics and insights"""
    
    @staticmethod
    def calculate_property_analytics(property, target_date=None):
        """
        Calculate daily analytics for a property
        
        Args:
            property: Property instance
            target_date: Date to calculate (defaults to yesterday)
            
        Returns:
            PropertyAnalytics instance
        """
        from apps.properties.analytics_models import PropertyAnalytics
        from apps.bookings.models import Booking
        
        if not target_date:
            target_date = (timezone.now() - timedelta(days=1)).date()
        
        # Get bookings for this date
        bookings = Booking.objects.filter(
            rental_property=property,
            status__in=['confirmed', 'completed'],
            check_in__lte=target_date,
            check_out__gt=target_date
        )
        
        # Calculate metrics
        bookings_count = bookings.count()
        total_revenue = bookings.aggregate(
            total=Sum('grand_total')
        )['total'] or Decimal('0')
        
        # Calculate nights booked
        nights_booked = 0
        for booking in bookings:
            # If booking spans target_date, count it as 1 night
            if booking.check_in <= target_date < booking.check_out:
                nights_booked += 1
        
        # Occupancy rate (1 night available per day for single property)
        nights_available = 1
        occupancy_rate = (nights_booked / nights_available * 100) if nights_available > 0 else 0
        
        # Average nightly rate
        avg_nightly_rate = None
        if bookings_count > 0:
            avg_nightly_rate = total_revenue / bookings_count
        
        # Get or create analytics record
        analytics, created = PropertyAnalytics.objects.get_or_create(
            property=property,
            date=target_date,
            defaults={
                'total_revenue': total_revenue,
                'bookings_count': bookings_count,
                'nights_booked': nights_booked,
                'avg_nightly_rate': avg_nightly_rate,
                'occupancy_rate': occupancy_rate,
                'nights_available': nights_available,
            }
        )
        
        if not created:
            # Update existing record
            analytics.total_revenue = total_revenue
            analytics.bookings_count = bookings_count
            analytics.nights_booked = nights_booked
            analytics.avg_nightly_rate = avg_nightly_rate
            analytics.occupancy_rate = occupancy_rate
            analytics.save()
        
        return analytics
    
    @staticmethod
    def generate_host_summary(host, start_date, end_date, period='monthly'):
        """
        Generate analytics summary for a host
        
        Args:
            host: User instance (host)
            start_date: Start date for period
            end_date: End date for period
            period: Period type ('daily', 'weekly', 'monthly', 'yearly')
            
        Returns:
            HostAnalyticsSummary instance
        """
        from apps.properties.analytics_models import HostAnalyticsSummary
        from apps.bookings.models import Booking
        from apps.properties.models import Property
        from apps.reviews.models import Review
        
        # Get host's properties
        properties = Property.objects.filter(host=host)
        
        # Get bookings in period
        bookings = Booking.objects.filter(
            rental_property__in=properties,
            status__in=['confirmed', 'completed'],
            created_at__date__gte=start_date,
            created_at__date__lte=end_date
        )
        
        # Calculate revenue metrics
        revenue_data = bookings.aggregate(
            total_revenue=Sum('grand_total'),
            total_bookings=Count('id'),
            total_nights=Sum(
                F('check_out') - F('check_in')
            ),
            avg_booking_value=Avg('grand_total')
        )
        
        # Get property analytics for occupancy
        property_analytics = []
        for prop in properties:
            analytics = HostAnalyticsService.calculate_property_analytics(prop, end_date)
            property_analytics.append(analytics)
        
        avg_occupancy = Decimal('0')
        if property_analytics:
            total_occupancy = sum(a.occupancy_rate for a in property_analytics)
            avg_occupancy = total_occupancy / len(property_analytics)
        
        # Guest metrics
        guest_bookings = bookings.values('guest').annotate(
            booking_count=Count('id')
        )
        total_unique_guests = guest_bookings.count()
        total_returning_guests = guest_bookings.filter(booking_count__gt=1).count()
        
        guest_retention_rate = Decimal('0')
        if total_unique_guests > 0:
            guest_retention_rate = (Decimal(total_returning_guests) / Decimal(total_unique_guests)) * 100
        
        # Review metrics
        # Get property IDs to filter reviews (property_id is CharField, not ForeignKey)
        property_ids = properties.values_list('id', flat=True)
        reviews = Review.objects.filter(
            property_id__in=property_ids,
            created_at__gte=start_date,
            created_at__lte=end_date
        )
        avg_rating = reviews.aggregate(avg=Avg('rating'))['avg']
        
        # Create or update summary
        summary, created = HostAnalyticsSummary.objects.get_or_create(
            host=host,
            period=period,
            start_date=start_date,
            end_date=end_date,
            defaults={
                'total_revenue': revenue_data['total_revenue'] or Decimal('0'),
                'total_bookings': revenue_data['total_bookings'] or 0,
                'total_nights_booked': revenue_data['total_nights'] or 0,
                'avg_booking_value': revenue_data['avg_booking_value'],
                'avg_occupancy_rate': avg_occupancy,
                'properties_count': properties.count(),
                'total_unique_guests': total_unique_guests,
                'total_returning_guests': total_returning_guests,
                'guest_retention_rate': guest_retention_rate,
                'avg_rating': avg_rating,
                'total_reviews': reviews.count(),
            }
        )
        
        if not created:
            # Update existing
            summary.total_revenue = revenue_data['total_revenue'] or Decimal('0')
            summary.total_bookings = revenue_data['total_bookings'] or 0
            summary.total_nights_booked = revenue_data['total_nights'] or 0
            summary.avg_booking_value = revenue_data['avg_booking_value']
            summary.avg_occupancy_rate = avg_occupancy
            summary.properties_count = properties.count()
            summary.total_unique_guests = total_unique_guests
            summary.total_returning_guests = total_returning_guests
            summary.guest_retention_rate = guest_retention_rate
            summary.avg_rating = avg_rating
            summary.total_reviews = reviews.count()
            summary.save()
        
        return summary
    
    @staticmethod
    def project_revenue(host, target_month, property=None):
        """
        Project revenue for future month based on historical data
        
        Args:
            host: User instance (host)
            target_month: Date of month to project
            property: Optional specific property
            
        Returns:
            RevenueProjection instance
        """
        from apps.properties.analytics_models import RevenueProjection
        from apps.bookings.models import Booking
        from apps.properties.models import Property
        
        # Get historical data (last 6 months)
        six_months_ago = (timezone.now() - timedelta(days=180)).date()
        
        if property:
            properties = [property]
        else:
            properties = Property.objects.filter(host=host)
        
        # Get historical bookings
        historical_bookings = Booking.objects.filter(
            rental_property__in=properties,
            status__in=['confirmed', 'completed'],
            created_at__date__gte=six_months_ago
        )
        
        # Calculate average metrics
        avg_data = historical_bookings.aggregate(
            avg_revenue=Avg('grand_total'),
            avg_bookings=Count('id')
        )
        
        # Simple projection (can be enhanced with ML)
        months_in_history = 6
        avg_monthly_bookings = (avg_data['avg_bookings'] or 0) / months_in_history
        avg_booking_value = avg_data['avg_revenue'] or Decimal('0')
        
        projected_bookings = int(avg_monthly_bookings)
        projected_revenue = avg_booking_value * projected_bookings
        
        # Seasonal factor (simplified - can be enhanced)
        target_month_num = target_month.month
        seasonal_factors = {
            12: 1.3, 1: 1.2, 2: 1.1,  # Winter (high season)
            6: 1.4, 7: 1.5, 8: 1.4,    # Summer (peak season)
        }
        seasonal_factor = Decimal(str(seasonal_factors.get(target_month_num, 1.0)))
        
        # Apply seasonal adjustment
        projected_revenue = projected_revenue * seasonal_factor
        projected_bookings = int(projected_bookings * seasonal_factor)
        
        # Estimate occupancy
        total_nights_in_month = 30  # Simplified
        projected_nights_booked = projected_bookings * 3  # Assume avg 3 nights per booking
        
        # Prevent division by zero when host has no properties
        if len(properties) > 0:
            projected_occupancy = min(
                (projected_nights_booked / (total_nights_in_month * len(properties))) * 100,
                100
            )
        else:
            projected_occupancy = 0
        
        # Confidence level (simplified)
        confidence = Decimal('70.0')  # 70% confidence
        
        # Create projection
        projection = RevenueProjection.objects.create(
            host=host,
            property=property,
            projection_date=timezone.now().date(),
            target_month=target_month,
            projected_revenue=projected_revenue,
            projected_bookings=projected_bookings,
            projected_occupancy=Decimal(str(projected_occupancy)),
            confidence_level=confidence,
            seasonal_factor=seasonal_factor,
            trend_factor=Decimal('1.0')
        )
        
        return projection
    
    @staticmethod
    def get_performance_insights(host):
        """
        Generate performance insights and recommendations
        
        Args:
            host: User instance
            
        Returns:
            Dict with insights and recommendations
        """
        from apps.properties.models import Property
        from apps.bookings.models import Booking
        
        # Get host's properties
        properties = Property.objects.filter(host=host)
        
        # Get recent performance (last 30 days)
        thirty_days_ago = (timezone.now() - timedelta(days=30)).date()
        recent_bookings = Booking.objects.filter(
            rental_property__in=properties,
            status__in=['confirmed', 'completed'],
            created_at__date__gte=thirty_days_ago
        )
        
        insights = {
            'total_properties': properties.count(),
            'bookings_last_30_days': recent_bookings.count(),
            'revenue_last_30_days': recent_bookings.aggregate(
                total=Sum('grand_total')
            )['total'] or Decimal('0'),
            'recommendations': []
        }
        
        # Generate recommendations
        if insights['bookings_last_30_days'] == 0:
            insights['recommendations'].append({
                'type': 'low_bookings',
                'message': 'No bookings in the last 30 days. Consider adjusting your pricing or improving your listing photos.',
                'priority': 'high'
            })
        
        # Check for properties without reviews
        from apps.reviews.models import Review
        property_ids_with_reviews = Review.objects.filter(
            property_id__in=properties.values_list('id', flat=True)
        ).values_list('property_id', flat=True).distinct()
        properties_without_reviews = properties.exclude(id__in=property_ids_with_reviews).count()
        if properties_without_reviews > 0:
            insights['recommendations'].append({
                'type': 'no_reviews',
                'message': f'{properties_without_reviews} property(ies) have no reviews. Encourage guests to leave reviews.',
                'priority': 'medium'
            })
        
        return insights
