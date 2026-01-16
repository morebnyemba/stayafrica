"""
Instant Booking Service
Handles automatic booking confirmation for qualified guests
"""
from decimal import Decimal
from django.utils import timezone
import logging

logger = logging.getLogger(__name__)


class InstantBookingService:
    """Service for instant booking eligibility and auto-confirmation"""
    
    @staticmethod
    def is_guest_qualified(guest, property_obj):
        """
        Check if guest meets instant booking requirements
        
        Args:
            guest: User instance (guest)
            property_obj: Property instance
            
        Returns:
            tuple: (is_qualified: bool, reason: str)
        """
        requirements = property_obj.instant_booking_requirements or {}
        
        # Check if instant booking is enabled
        if not property_obj.instant_booking_enabled:
            return False, "Instant booking not enabled for this property"
        
        # Check if guest is verified (required by default)
        require_verified = requirements.get('require_verified', True)
        if require_verified and not guest.is_verified:
            return False, "Guest must be verified for instant booking"
        
        # Check minimum reviews
        min_reviews = requirements.get('min_reviews', 0)
        if min_reviews > 0:
            from apps.reviews.models import Review
            guest_reviews = Review.objects.filter(
                booking__guest=guest,
                booking__status='completed'
            ).count()
            
            if guest_reviews < min_reviews:
                return False, f"Guest needs at least {min_reviews} review(s) for instant booking"
        
        # Check minimum rating
        min_rating = requirements.get('min_rating', 0)
        if min_rating > 0:
            from apps.reviews.models import Review
            from django.db.models import Avg
            
            avg_rating = Review.objects.filter(
                booking__guest=guest,
                booking__status='completed'
            ).aggregate(Avg('rating'))['rating__avg']
            
            if not avg_rating or avg_rating < min_rating:
                return False, f"Guest needs a minimum rating of {min_rating} stars for instant booking"
        
        # Check if guest has completed bookings
        require_completed_bookings = requirements.get('require_completed_bookings', False)
        if require_completed_bookings:
            from apps.bookings.models import Booking
            completed_count = Booking.objects.filter(
                guest=guest,
                status='completed'
            ).count()
            
            if completed_count == 0:
                return False, "Guest must have at least one completed booking for instant booking"
        
        # Check if guest has payment method on file
        require_payment_method = requirements.get('require_payment_method', True)
        if require_payment_method:
            # Check if guest has valid payment method
            # This is a placeholder - implement based on payment system
            # For now, we assume payment method is checked during booking creation
            pass
        
        # Check if guest is blocked by host
        if hasattr(property_obj.host, 'blocked_guests'):
            if guest in property_obj.host.blocked_guests.all():
                return False, "Guest is blocked by host"
        
        # All checks passed
        return True, "Guest qualified for instant booking"
    
    @staticmethod
    def auto_confirm_booking(booking):
        """
        Automatically confirm a booking if eligible
        
        Args:
            booking: Booking instance
            
        Returns:
            tuple: (confirmed: bool, reason: str)
        """
        property_obj = booking.rental_property
        guest = booking.guest
        
        # Check if property has instant booking enabled
        if not property_obj.instant_booking_enabled:
            return False, "Property does not have instant booking enabled"
        
        # Check guest qualification
        is_qualified, reason = InstantBookingService.is_guest_qualified(guest, property_obj)
        
        if not is_qualified:
            logger.info(f"Booking {booking.booking_ref} not auto-confirmed: {reason}")
            return False, reason
        
        # Auto-confirm the booking
        booking.status = 'confirmed'
        booking.save(update_fields=['status', 'updated_at'])
        
        logger.info(f"Booking {booking.booking_ref} auto-confirmed via instant booking")
        
        # Send confirmation notification
        try:
            from services.notification_service import NotificationService
            NotificationService.send_booking_confirmation(booking)
        except Exception as e:
            logger.warning(f"Failed to send auto-confirmation notification: {e}")
        
        return True, "Booking automatically confirmed"
    
    @staticmethod
    def get_instant_booking_info(property_obj, guest=None):
        """
        Get instant booking information for a property
        
        Args:
            property_obj: Property instance
            guest: Optional User instance to check qualification
            
        Returns:
            dict with instant booking info
        """
        if not property_obj.instant_booking_enabled:
            return {
                'enabled': False,
                'message': 'Instant booking not available for this property'
            }
        
        requirements = property_obj.instant_booking_requirements or {}
        
        info = {
            'enabled': True,
            'requirements': {
                'verified_guest': requirements.get('require_verified', True),
                'min_reviews': requirements.get('min_reviews', 0),
                'min_rating': requirements.get('min_rating', 0),
                'completed_bookings': requirements.get('require_completed_bookings', False),
                'payment_method': requirements.get('require_payment_method', True),
            }
        }
        
        # Check guest qualification if provided
        if guest:
            is_qualified, reason = InstantBookingService.is_guest_qualified(guest, property_obj)
            info['guest_qualified'] = is_qualified
            info['qualification_message'] = reason
        
        return info
    
    @staticmethod
    def set_instant_booking_requirements(property_obj, requirements):
        """
        Set instant booking requirements for a property
        
        Args:
            property_obj: Property instance
            requirements: dict with requirements
            
        Returns:
            Property instance
        """
        valid_keys = [
            'require_verified',
            'min_reviews',
            'min_rating',
            'require_completed_bookings',
            'require_payment_method'
        ]
        
        # Filter to only valid keys
        filtered_requirements = {
            k: v for k, v in requirements.items() if k in valid_keys
        }
        
        property_obj.instant_booking_requirements = filtered_requirements
        property_obj.save(update_fields=['instant_booking_requirements'])
        
        logger.info(f"Instant booking requirements updated for property {property_obj.id}")
        
        return property_obj
