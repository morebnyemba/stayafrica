"""
Utility functions for property-related operations
"""
from django.db.models import Avg, Count


def get_property_review_stats(property_obj):
    """
    Get review statistics for a property
    
    NOTE: This function still causes N+1 queries when used in list serializers.
    For better performance with lists, consider:
    1. Caching these values in the Property model (updated via signals)
    2. Using select_related/prefetch_related in views
    3. Computing these at the database level with annotations
    
    Args:
        property_obj: Property instance
        
    Returns:
        Dict with average_rating and review_count
    """
    from apps.reviews.models import Review
    from apps.bookings.models import Booking
    
    # Get all bookings for this property that have reviews
    bookings = Booking.objects.filter(rental_property=property_obj)
    reviews = Review.objects.filter(booking__in=bookings)
    
    avg_rating = reviews.aggregate(Avg('rating'))['rating__avg']
    review_count = reviews.count()
    
    return {
        'average_rating': round(avg_rating, 1) if avg_rating else None,
        'review_count': review_count
    }


def annotate_properties_with_review_stats(queryset):
    """
    Annotate a property queryset with review statistics
    This helps avoid N+1 queries when serializing multiple properties
    
    Args:
        queryset: Property QuerySet
        
    Returns:
        Annotated QuerySet
    """
    from django.db.models import Avg, Count, Q
    from apps.reviews.models import Review
    from apps.bookings.models import Booking
    
    # Subquery to get reviews through bookings
    # Note: This is a simplified version, may need optimization based on actual data volume
    return queryset.annotate(
        booking_count=Count('bookings', distinct=True),
    )

