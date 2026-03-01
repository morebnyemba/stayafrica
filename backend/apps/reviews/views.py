from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated, AllowAny
from django.core.exceptions import ValidationError
from django.db import transaction
from django.db.models import Q, Avg
from apps.reviews.models import Review
from apps.reviews.serializers import ReviewSerializer
from apps.bookings.models import Booking
from utils.validators import validate_rating
from utils.decorators import api_ratelimit, log_action
from utils.helpers import sanitize_input
from services.audit_logger import AuditLoggerService
from datetime import date, timedelta
import logging

logger = logging.getLogger(__name__)

class ReviewViewSet(viewsets.ModelViewSet):
    serializer_class = ReviewSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return reviews - admins see all, users see their own"""
        user = self.request.user
        
        # Admin users can see all reviews
        if user.is_staff:
            qs = Review.objects.all().select_related('guest', 'host', 'booking__rental_property')
            search = self.request.query_params.get('search')
            if search:
                qs = qs.filter(Q(text__icontains=search) | Q(guest__email__icontains=search) | Q(host__email__icontains=search))
            rating = self.request.query_params.get('rating')
            if rating:
                qs = qs.filter(rating=int(rating))
            return qs
        
        # Allow filtering by property or user
        property_id = self.request.query_params.get('property_id')
        host_id = self.request.query_params.get('host_id')
        
        if property_id:
            # Public: get reviews for a specific property
            from apps.properties.models import Property
            try:
                property_obj = Property.objects.get(id=property_id)
                return Review.objects.filter(host=property_obj.host).select_related('guest', 'booking')
            except Property.DoesNotExist:
                return Review.objects.none()
        
        if host_id:
            # Public: get reviews for a specific host
            return Review.objects.filter(host_id=host_id).select_related('guest', 'booking')
        
        # Private: user's own reviews (given or received)
        return Review.objects.filter(
            Q(host=user) | Q(guest=user)
        ).select_related('guest', 'host', 'booking__rental_property')
    
    def get_permissions(self):
        """Allow reading reviews without authentication"""
        if self.action in ['list', 'retrieve', 'property_reviews', 'host_stats']:
            return [AllowAny()]
        return super().get_permissions()
    
    @transaction.atomic
    @api_ratelimit(rate='5/h')
    @log_action('create_review')
    def perform_create(self, serializer):
        """Create review after booking checkout with validation"""
        booking_id = self.request.data.get('booking_id')
        rating = self.request.data.get('rating')
        text = self.request.data.get('text', '')
        
        # Validate rating
        try:
            validate_rating(int(rating))
        except (ValueError, ValidationError) as e:
            logger.error(f"Invalid rating: {rating}")
            raise ValidationError(f"Invalid rating: {str(e)}")
        
        # Sanitize text input
        sanitized_text = sanitize_input(text)
        
        try:
            booking = Booking.objects.select_related('property__host').get(
                id=booking_id,
                guest=self.request.user
            )
        except Booking.DoesNotExist:
            logger.warning(f"Booking {booking_id} not found for user {self.request.user.id}")
            raise ValidationError('Booking not found or you do not have permission to review it')
        
        # Check if booking is completed
        if booking.status != 'completed':
            raise ValidationError('Can only review completed bookings')
        
        # Check if checkout date has passed
        if booking.check_out > date.today():
            raise ValidationError('Can only review after checkout date')
        
        # Check review window (configurable via admin)
        from apps.admin_dashboard.models import SystemConfiguration
        config = SystemConfiguration.get_config()
        max_review_days = config.review_window_days
        
        if (date.today() - booking.check_out).days > max_review_days:
            raise ValidationError(f'Review period has expired. Reviews must be submitted within {max_review_days} days of checkout')
        
        # Check if review already exists
        if hasattr(booking, 'review'):
            raise ValidationError('Review already exists for this booking')
        
        # Create review
        review = serializer.save(
            booking=booking,
            guest=self.request.user,
            host=booking.rental_property.host,
            text=sanitized_text
        )
        
        # Log the action
        AuditLoggerService.log_action(
            user=self.request.user,
            action='create',
            model=Review,
            object_id=review.id,
            changes={
                'booking_ref': booking.booking_ref,
                'rating': rating,
                'host': booking.rental_property.host.email
            }
        )
        
        logger.info(f"Review created: {review.id} for booking {booking.booking_ref}")
    
    @action(detail=True, methods=['put', 'patch'])
    @api_ratelimit(rate='10/h')
    @log_action('update_review')
    def update_review(self, request, pk=None):
        """Update review (only by original reviewer within 7 days)"""
        review = self.get_object()
        
        # Check permissions
        if request.user != review.guest:
            return Response(
                {'error': 'You can only update your own reviews'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Check if within update window (configurable via admin)
        from apps.admin_dashboard.models import SystemConfiguration
        config = SystemConfiguration.get_config()
        update_window_days = config.review_edit_window_days
        
        if (date.today() - review.created_at.date()).days > update_window_days:
            return Response(
                {'error': f'Reviews can only be updated within {update_window_days} days of creation'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Sanitize text
        if 'text' in request.data:
            request.data['text'] = sanitize_input(request.data['text'])
        
        # Validate rating if provided
        if 'rating' in request.data:
            try:
                validate_rating(int(request.data['rating']))
            except (ValueError, ValidationError) as e:
                return Response(
                    {'rating': str(e)},
                    status=status.HTTP_400_BAD_REQUEST
                )
        
        serializer = ReviewSerializer(review, data=request.data, partial=True)
        if serializer.is_valid():
            with transaction.atomic():
                updated_review = serializer.save()
                
                # Log the action
                AuditLoggerService.log_action(
                    user=request.user,
                    action='update',
                    model=Review,
                    object_id=review.id,
                    changes={'updated_fields': list(request.data.keys())}
                )
            
            logger.info(f"Review updated: {review.id}")
            return Response(serializer.data)
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
    
    @action(detail=False, methods=['get'], permission_classes=[AllowAny])
    def property_reviews(self, request):
        """Get all reviews for a specific property"""
        property_id = request.query_params.get('property_id')
        
        if not property_id:
            return Response(
                {'error': 'property_id is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        from apps.properties.models import Property
        try:
            property_obj = Property.objects.get(id=property_id)
            reviews = Review.objects.filter(
                host=property_obj.host
            ).select_related('guest').order_by('-created_at')
            
            serializer = ReviewSerializer(reviews, many=True)
            
            # Calculate average rating
            avg_rating = reviews.aggregate(Avg('rating'))['rating__avg']
            
            return Response({
                'reviews': serializer.data,
                'count': reviews.count(),
                'average_rating': round(avg_rating, 2) if avg_rating else None
            })
        except Property.DoesNotExist:
            return Response(
                {'error': 'Property not found'},
                status=status.HTTP_404_NOT_FOUND
            )
    
    @action(detail=False, methods=['get'], permission_classes=[AllowAny])
    def host_stats(self, request):
        """Get review statistics for a host"""
        host_id = request.query_params.get('host_id')
        
        if not host_id:
            return Response(
                {'error': 'host_id is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        reviews = Review.objects.filter(host_id=host_id)
        
        if not reviews.exists():
            return Response({
                'host_id': host_id,
                'total_reviews': 0,
                'average_rating': None,
                'rating_distribution': {}
            })
        
        # Calculate statistics
        avg_rating = reviews.aggregate(Avg('rating'))['rating__avg']
        rating_distribution = {}
        for i in range(1, 6):
            rating_distribution[str(i)] = reviews.filter(rating=i).count()
        
        return Response({
            'host_id': host_id,
            'total_reviews': reviews.count(),
            'average_rating': round(avg_rating, 2) if avg_rating else None,
            'rating_distribution': rating_distribution
        })
    
    @action(detail=True, methods=['post'])
    @api_ratelimit(rate='10/h')
    def vote(self, request, pk=None):
        """Vote a review as helpful or unhelpful"""
        from apps.reviews.models import ReviewVote
        
        review = self.get_object()
        vote_type = request.data.get('vote_type')
        
        if vote_type not in ['helpful', 'unhelpful']:
            return Response(
                {'error': 'vote_type must be either "helpful" or "unhelpful"'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Check if user already voted
        existing_vote = ReviewVote.objects.filter(review=review, user=request.user).first()
        
        if existing_vote:
            if existing_vote.vote_type == vote_type:
                # Remove vote if same type
                existing_vote.delete()
                if vote_type == 'helpful':
                    review.helpful_count = max(0, review.helpful_count - 1)
                    review.save()
                return Response({'message': 'Vote removed'})
            else:
                # Change vote type
                existing_vote.vote_type = vote_type
                existing_vote.save()
                if vote_type == 'helpful':
                    review.helpful_count += 1
                else:
                    review.helpful_count = max(0, review.helpful_count - 1)
                review.save()
                return Response({'message': 'Vote updated', 'vote_type': vote_type})
        else:
            # Create new vote
            ReviewVote.objects.create(
                review=review,
                user=request.user,
                vote_type=vote_type
            )
            if vote_type == 'helpful':
                review.helpful_count += 1
                review.save()
            return Response({'message': 'Vote recorded', 'vote_type': vote_type})
    
    @action(detail=True, methods=['post'])
    @api_ratelimit(rate='10/h')
    @log_action('respond_to_review')
    def respond(self, request, pk=None):
        """Host responds to a review"""
        review = self.get_object()
        
        # Check permissions - only the host can respond
        if request.user != review.host:
            return Response(
                {'error': 'Only the host can respond to this review'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        response_text = request.data.get('response')
        if not response_text:
            return Response(
                {'error': 'Response text is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Sanitize response
        from utils.helpers import sanitize_input
        sanitized_response = sanitize_input(response_text)
        
        # Update review
        review.host_response = sanitized_response
        from datetime import datetime
        review.host_response_date = datetime.now()
        review.save()
        
        # Log the action
        AuditLoggerService.log_action(
            user=request.user,
            action='respond',
            model=Review,
            object_id=review.id,
            changes={'response_length': len(sanitized_response)}
        )
        
        serializer = self.get_serializer(review)
        return Response(serializer.data)
