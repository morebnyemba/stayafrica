"""
Host Analytics Service
Provides comprehensive analytics and statistics for hosts
"""
from django.db.models import Count, Sum, Avg, Q, F
from django.db.models.functions import TruncMonth, TruncWeek
from django.utils import timezone
from datetime import timedelta
from decimal import Decimal
from typing import Dict, List
import logging

logger = logging.getLogger(__name__)


class HostAnalyticsService:
    """
    Service for generating host analytics and performance metrics
    """
    
    @classmethod
    def get_host_overview(cls, host_user) -> Dict:
        """
        Get comprehensive host overview with key metrics
        
        Args:
            host_user: User instance (host)
            
        Returns:
            Dict with overview statistics
        """
        from apps.properties.models import Property
        from apps.bookings.models import Booking
        from apps.reviews.models import Review
        
        # Get all host properties
        properties = Property.objects.filter(host=host_user)
        property_ids = properties.values_list('id', flat=True)
        
        # Get all bookings for host's properties
        bookings = Booking.objects.filter(rental_property__host=host_user)
        
        # Calculate statistics
        total_properties = properties.count()
        active_properties = properties.filter(status='active').count()
        pending_properties = properties.filter(status='pending_approval').count()
        
        total_bookings = bookings.count()
        confirmed_bookings = bookings.filter(status='confirmed').count()
        pending_bookings = bookings.filter(status='pending').count()
        completed_bookings = bookings.filter(status='completed').count()
        
        # Calculate total earnings (completed bookings only)
        # Host gets: (nightly_total + cleaning_fee) - commission_fee
        total_earnings = bookings.filter(status='completed').aggregate(
            total=Sum(F('nightly_total') + F('cleaning_fee') - F('commission_fee'))
        )['total'] or Decimal('0.00')
        
        # Pending earnings (confirmed but not completed)
        pending_earnings = bookings.filter(status='confirmed').aggregate(
            total=Sum(F('nightly_total') + F('cleaning_fee') - F('commission_fee'))
        )['total'] or Decimal('0.00')
        
        # Average rating across all properties
        avg_rating = Review.objects.filter(
            booking__rental_property__host=host_user
        ).aggregate(avg=Avg('rating'))['avg'] or 0.0
        
        # Total reviews received
        total_reviews = Review.objects.filter(
            booking__rental_property__host=host_user
        ).count()
        
        # Response rate (percentage of bookings confirmed vs pending)
        response_rate = 0
        if total_bookings > 0:
            responded_bookings = bookings.exclude(status='pending').count()
            response_rate = (responded_bookings / total_bookings) * 100
        
        # Occupancy rate (last 30 days)
        thirty_days_ago = timezone.now().date() - timedelta(days=30)
        recent_bookings = bookings.filter(
            check_in__gte=thirty_days_ago,
            status__in=['confirmed', 'completed']
        )
        
        # Calculate total available nights vs booked nights
        total_nights_booked = sum([
            (booking.check_out - booking.check_in).days
            for booking in recent_bookings
        ])
        
        # Assuming all properties are available (simplified calculation)
        total_available_nights = active_properties * 30
        occupancy_rate = 0
        if total_available_nights > 0:
            occupancy_rate = (total_nights_booked / total_available_nights) * 100
        
        return {
            'total_properties': total_properties,
            'active_properties': active_properties,
            'pending_properties': pending_properties,
            'total_bookings': total_bookings,
            'confirmed_bookings': confirmed_bookings,
            'pending_bookings': pending_bookings,
            'completed_bookings': completed_bookings,
            'total_earnings': float(total_earnings),
            'pending_earnings': float(pending_earnings),
            'average_rating': round(avg_rating, 2),
            'total_reviews': total_reviews,
            'response_rate': round(response_rate, 1),
            'occupancy_rate': round(occupancy_rate, 1),
        }
    
    @classmethod
    def get_earnings_breakdown(cls, host_user, period: str = 'month') -> List[Dict]:
        """
        Get earnings breakdown by time period
        
        Args:
            host_user: User instance (host)
            period: 'week', 'month', or 'year'
            
        Returns:
            List of earnings by period
        """
        from apps.bookings.models import Booking
        
        # Only count completed bookings
        bookings = Booking.objects.filter(
            rental_property__host=host_user,
            status='completed'
        )
        
        # Aggregate by period
        if period == 'week':
            trunc_func = TruncWeek
        elif period == 'month':
            trunc_func = TruncMonth
        else:
            trunc_func = TruncMonth  # Default to month
        
        earnings_by_period = bookings.annotate(
            period=trunc_func('created_at')
        ).values('period').annotate(
            total_earnings=Sum(F('nightly_total') + F('cleaning_fee') - F('commission_fee')),
            booking_count=Count('id')
        ).order_by('period')
        
        return [
            {
                'period': item['period'].strftime('%Y-%m' if period == 'month' else '%Y-W%W'),
                'total': float(item['total_earnings'] or 0),
                'bookings': item['booking_count']
            }
            for item in earnings_by_period
        ]
    
    @classmethod
    def get_property_performance(cls, host_user) -> List[Dict]:
        """
        Get performance metrics for each property
        
        Args:
            host_user: User instance (host)
            
        Returns:
            List of property performance metrics
        """
        from apps.properties.models import Property
        from apps.bookings.models import Booking
        from apps.reviews.models import Review
        
        properties = Property.objects.filter(host=host_user)
        performance_data = []
        
        for prop in properties:
            bookings = Booking.objects.filter(rental_property=prop)
            completed = bookings.filter(status='completed')
            
            # Calculate earnings
            earnings = completed.aggregate(
                total=Sum(F('nightly_total') + F('cleaning_fee') - F('commission_fee'))
            )['total'] or Decimal('0.00')
            
            # Get reviews
            reviews = Review.objects.filter(booking__rental_property=prop)
            avg_rating = reviews.aggregate(avg=Avg('rating'))['avg'] or 0.0
            
            performance_data.append({
                'property_id': prop.id,
                'property_title': prop.title,
                'status': prop.status,
                'total_bookings': bookings.count(),
                'completed_bookings': completed.count(),
                'total_earnings': float(earnings),
                'average_rating': round(avg_rating, 2),
                'review_count': reviews.count(),
            })
        
        return performance_data
    
    @classmethod
    def get_booking_calendar(cls, property_id: int, start_date=None, end_date=None) -> Dict:
        """
        Get booking calendar for a property
        
        Args:
            property_id: Property ID
            start_date: Start date for calendar (default: today)
            end_date: End date for calendar (default: 3 months from start)
            
        Returns:
            Dict with booked dates and availability
        """
        from apps.bookings.models import Booking
        from datetime import date
        
        if not start_date:
            start_date = timezone.now().date()
        if not end_date:
            end_date = start_date + timedelta(days=90)  # 3 months
        
        # Get all bookings that overlap with the date range
        bookings = Booking.objects.filter(
            rental_property_id=property_id,
            status__in=['confirmed', 'completed'],
            check_out__gte=start_date,
            check_in__lte=end_date
        ).order_by('check_in')
        
        # Build calendar data
        booked_dates = []
        for booking in bookings:
            current_date = booking.check_in
            while current_date < booking.check_out:
                booked_dates.append({
                    'date': current_date.strftime('%Y-%m-%d'),
                    'booking_id': booking.id,
                    'booking_ref': booking.booking_ref,
                    'guest_email': booking.guest.email,
                    'status': booking.status,
                })
                current_date += timedelta(days=1)
        
        return {
            'start_date': start_date.strftime('%Y-%m-%d'),
            'end_date': end_date.strftime('%Y-%m-%d'),
            'booked_dates': booked_dates,
        }
    
    @classmethod
    def get_upcoming_check_ins(cls, host_user, days: int = 7) -> List[Dict]:
        """
        Get upcoming check-ins for host's properties
        
        Args:
            host_user: User instance (host)
            days: Number of days to look ahead (default: 7)
            
        Returns:
            List of upcoming check-ins
        """
        from apps.bookings.models import Booking
        
        today = timezone.now().date()
        end_date = today + timedelta(days=days)
        
        upcoming = Booking.objects.filter(
            rental_property__host=host_user,
            status='confirmed',
            check_in__gte=today,
            check_in__lte=end_date
        ).select_related('guest', 'rental_property').order_by('check_in')
        
        return [
            {
                'booking_id': booking.id,
                'booking_ref': booking.booking_ref,
                'property_title': booking.rental_property.title,
                'guest_name': f"{booking.guest.first_name} {booking.guest.last_name}",
                'guest_email': booking.guest.email,
                'check_in': booking.check_in.strftime('%Y-%m-%d'),
                'check_out': booking.check_out.strftime('%Y-%m-%d'),
                'guests': getattr(booking, 'number_of_guests', 1),
            }
            for booking in upcoming
        ]
    
    @classmethod
    def get_pending_actions(cls, host_user) -> Dict:
        """
        Get pending actions that require host attention
        
        Args:
            host_user: User instance (host)
            
        Returns:
            Dict with counts of pending items
        """
        from apps.bookings.models import Booking
        from apps.properties.models import Property
        from apps.messaging.models import Message
        
        # Pending booking requests
        pending_bookings = Booking.objects.filter(
            rental_property__host=host_user,
            status='pending'
        ).count()
        
        # Properties pending approval
        pending_properties = Property.objects.filter(
            host=host_user,
            status='pending_approval'
        ).count()
        
        # Unread messages
        unread_messages = Message.objects.filter(
            receiver=host_user,
            is_read=False
        ).count()
        
        # Bookings needing completion (checkout date passed)
        today = timezone.now().date()
        needs_completion = Booking.objects.filter(
            rental_property__host=host_user,
            status='confirmed',
            check_out__lt=today
        ).count()
        
        return {
            'pending_bookings': pending_bookings,
            'pending_properties': pending_properties,
            'unread_messages': unread_messages,
            'needs_completion': needs_completion,
            'total_pending': pending_bookings + pending_properties + unread_messages + needs_completion,
        }
