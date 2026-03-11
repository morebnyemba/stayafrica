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
        # Sum of date diffs returns timedelta; convert to integer days
        total_nights_val = revenue_data.get('total_nights')
        if hasattr(total_nights_val, 'days'):
            revenue_data['total_nights'] = total_nights_val.days
        elif total_nights_val is None:
            revenue_data['total_nights'] = 0
        
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

    # ── New helpers for the analytics dashboard (additive) ──────────

    @staticmethod
    def get_revenue_chart_data(host, start_date, end_date):
        """
        Generate RevenueDataPoint[] for the revenue chart.
        Returns list of {date, revenue, bookings, label}.
        """
        from apps.properties.analytics_models import PropertyAnalytics
        from django.db import models as db_models

        analytics = (
            PropertyAnalytics.objects.filter(
                property__host=host,
                date__gte=start_date,
                date__lte=end_date,
            )
            .values('date')
            .annotate(
                revenue=db_models.Sum('total_revenue'),
                bookings=db_models.Sum('bookings_count'),
            )
            .order_by('date')
        )

        return [
            {
                'date': str(item['date']),
                'revenue': float(item['revenue'] or 0),
                'bookings': item['bookings'] or 0,
                'label': item['date'].strftime('%b %d'),
            }
            for item in analytics
        ]

    @staticmethod
    def get_occupancy_chart_data(host, start_date, end_date):
        """
        Generate OccupancyDataPoint[] for the occupancy trend chart.
        Returns list of {date, occupancy_rate, booked_nights, available_nights}.
        """
        from apps.properties.analytics_models import PropertyAnalytics
        from django.db import models as db_models

        analytics = (
            PropertyAnalytics.objects.filter(
                property__host=host,
                date__gte=start_date,
                date__lte=end_date,
            )
            .values('date')
            .annotate(
                occupancy_rate=db_models.Avg('occupancy_rate'),
                booked_nights=db_models.Sum('nights_booked'),
                available_nights=db_models.Sum('nights_available'),
            )
            .order_by('date')
        )

        return [
            {
                'date': str(item['date']),
                'occupancy_rate': float(item['occupancy_rate'] or 0),
                'booked_nights': item['booked_nights'] or 0,
                'available_nights': item['available_nights'] or 0,
            }
            for item in analytics
        ]

    @staticmethod
    def get_booking_timeline_data(host, start_date, end_date):
        """
        Generate BookingDataPoint[] for the booking timeline chart.
        Returns list of {date, count, confirmed, pending, cancelled}.
        """
        from apps.bookings.models import Booking
        from django.db.models.functions import TruncDate
        from django.db import models as db_models

        bookings = (
            Booking.objects.filter(
                rental_property__host=host,
                created_at__date__gte=start_date,
                created_at__date__lte=end_date,
            )
            .annotate(booking_date=TruncDate('created_at'))
            .values('booking_date')
            .annotate(
                count=db_models.Count('id'),
                confirmed=db_models.Count('id', filter=db_models.Q(status='confirmed')),
                pending=db_models.Count('id', filter=db_models.Q(status='pending')),
                cancelled=db_models.Count('id', filter=db_models.Q(status='cancelled')),
            )
            .order_by('booking_date')
        )

        return [
            {
                'date': str(item['booking_date']),
                'count': item['count'],
                'confirmed': item['confirmed'],
                'pending': item['pending'],
                'cancelled': item['cancelled'],
            }
            for item in bookings
        ]

    @staticmethod
    def get_property_performance_data(host):
        """
        Generate PropertyPerformance[] matching frontend type.
        Returns list of {property_id, property_name, property_image,
        revenue, occupancy_rate, bookings, average_rating, revenue_per_night, days_available}.
        """
        from apps.properties.models import Property
        from apps.bookings.models import Booking
        from apps.reviews.models import Review
        from django.db.models import Sum, Avg, Count, F
        from decimal import Decimal

        properties = Property.objects.filter(host=host)
        result = []

        for prop in properties:
            completed = Booking.objects.filter(
                rental_property=prop, status='completed'
            )
            earnings = completed.aggregate(
                total=Sum(F('nightly_total') + F('cleaning_fee') - F('commission_fee'))
            )['total'] or Decimal('0.00')

            total_bookings = Booking.objects.filter(rental_property=prop).count()

            reviews = Review.objects.filter(property_id=prop.id)
            avg_rating = reviews.aggregate(avg=Avg('rating'))['avg'] or 0.0

            # Get image
            image_url = None
            first_image = prop.images.first() if hasattr(prop, 'images') else None
            if first_image:
                image_url = getattr(first_image, 'image_url', None) or (
                    first_image.image.url if hasattr(first_image, 'image') and first_image.image else None
                )

            # Revenue per night
            total_nights = completed.aggregate(
                nights=Sum(F('check_out') - F('check_in'))
            )['nights']
            nights_count = total_nights.days if total_nights else 0
            rev_per_night = float(earnings) / nights_count if nights_count > 0 else 0

            result.append({
                'property_id': str(prop.id),
                'property_name': prop.title,
                'property_image': image_url,
                'revenue': float(earnings),
                'occupancy_rate': 0,  # calculated separately if analytics exist
                'bookings': total_bookings,
                'average_rating': round(avg_rating, 2),
                'revenue_per_night': round(rev_per_night, 2),
                'days_available': 30,
            })

        return result

    @staticmethod
    def get_insights_list(host):
        """
        Generate Insight[] matching frontend type.
        Returns list of {id, type, title, description, recommendation}.
        """
        raw = HostAnalyticsService.get_performance_insights(host)

        insights = []
        recommendations = raw.get('recommendations', [])

        for i, rec in enumerate(recommendations):
            priority_to_type = {
                'high': 'danger',
                'medium': 'warning',
                'low': 'info',
            }
            insights.append({
                'id': f'insight-{i}',
                'type': priority_to_type.get(rec.get('priority', 'low'), 'info'),
                'title': rec.get('type', 'Insight').replace('_', ' ').title(),
                'description': rec.get('message', ''),
                'recommendation': rec.get('message', ''),
            })

        # Add summary-based insights
        if raw.get('bookings_last_30_days', 0) > 0:
            revenue = raw.get('revenue_last_30_days', Decimal('0'))
            insights.append({
                'id': 'insight-revenue-30d',
                'type': 'success',
                'title': 'Recent Revenue',
                'description': f'You earned ${float(revenue):,.2f} from {raw["bookings_last_30_days"]} bookings in the last 30 days.',
            })

        return insights

    @staticmethod
    def compute_period_changes(host, start_date, end_date, period='monthly'):
        """
        Compute period-over-period percentage changes for summary cards.
        Returns {revenue_change, occupancy_change, bookings_change, rating_change}.
        """
        from apps.bookings.models import Booking
        from apps.reviews.models import Review
        from apps.properties.models import Property
        from django.db.models import Sum, Avg, Count

        # Current period
        period_length = (end_date - start_date).days or 1
        prev_start = start_date - timedelta(days=period_length)
        prev_end = start_date - timedelta(days=1)

        properties = Property.objects.filter(host=host)

        def _get_metrics(s, e):
            bookings = Booking.objects.filter(
                rental_property__in=properties,
                status__in=['confirmed', 'completed'],
                created_at__date__gte=s,
                created_at__date__lte=e,
            )
            agg = bookings.aggregate(
                revenue=Sum('grand_total'),
                count=Count('id'),
            )
            property_ids = properties.values_list('id', flat=True)
            rating = Review.objects.filter(
                property_id__in=property_ids,
                created_at__gte=s,
                created_at__lte=e,
            ).aggregate(avg=Avg('rating'))['avg'] or 0
            return {
                'revenue': float(agg['revenue'] or 0),
                'bookings': agg['count'] or 0,
                'rating': float(rating),
            }

        current = _get_metrics(start_date, end_date)
        previous = _get_metrics(prev_start, prev_end)

        def _pct_change(cur, prev):
            if prev == 0:
                return 0
            return round(((cur - prev) / prev) * 100, 1)

        return {
            'revenue_change': _pct_change(current['revenue'], previous['revenue']),
            'occupancy_change': 0,  # simplified — would need PropertyAnalytics
            'bookings_change': _pct_change(current['bookings'], previous['bookings']),
            'rating_change': _pct_change(current['rating'], previous['rating']),
        }
