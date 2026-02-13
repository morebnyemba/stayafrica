from rest_framework import viewsets, status, filters, serializers
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated, AllowAny, IsAuthenticatedOrReadOnly
from django.contrib.gis.geos import Point
from django.contrib.gis.measure import D
from django.db.models import Q, Avg
from apps.experiences.models import (
    Experience, ExperienceCategory, ExperienceBooking, ExperienceAvailability
)
from apps.experiences.serializers import (
    ExperienceSerializer, ExperienceCategorySerializer,
    ExperienceBookingSerializer, ExperienceAvailabilitySerializer
)
import logging

logger = logging.getLogger(__name__)


class ExperienceCategoryViewSet(viewsets.ReadOnlyModelViewSet):
    """ViewSet for experience categories"""
    queryset = ExperienceCategory.objects.all()
    serializer_class = ExperienceCategorySerializer
    permission_classes = [AllowAny]


class ExperienceViewSet(viewsets.ModelViewSet):
    """ViewSet for experiences"""
    serializer_class = ExperienceSerializer
    permission_classes = [IsAuthenticatedOrReadOnly]
    filter_backends = [filters.SearchFilter, filters.OrderingFilter]
    search_fields = ['title', 'description', 'city', 'country']
    ordering_fields = ['created_at', 'price_per_person', 'title']
    ordering = ['-created_at']
    
    def get_queryset(self):
        """Return experiences based on filters"""
        queryset = Experience.objects.filter(status='active').select_related('host', 'category')
        
        # Filter by category
        category = self.request.query_params.get('category')
        if category:
            queryset = queryset.filter(category_id=category)
        
        # Filter by country
        country = self.request.query_params.get('country')
        if country:
            queryset = queryset.filter(country__icontains=country)
        
        # Filter by city
        city = self.request.query_params.get('city')
        if city:
            queryset = queryset.filter(city__icontains=city)
        
        # Filter by difficulty
        difficulty = self.request.query_params.get('difficulty')
        if difficulty:
            queryset = queryset.filter(difficulty=difficulty)
        
        # Filter by price range
        min_price = self.request.query_params.get('min_price')
        max_price = self.request.query_params.get('max_price')
        if min_price:
            queryset = queryset.filter(price_per_person__gte=min_price)
        if max_price:
            queryset = queryset.filter(price_per_person__lte=max_price)
        
        # Nearby search (if lat/lng provided)
        lat = self.request.query_params.get('lat')
        lng = self.request.query_params.get('lng')
        radius = self.request.query_params.get('radius', 50)  # Default 50km
        
        if lat and lng:
            try:
                user_location = Point(float(lng), float(lat), srid=4326)
                queryset = queryset.filter(
                    location__distance_lte=(user_location, D(km=float(radius)))
                )
            except (ValueError, TypeError) as e:
                logger.warning(f"Invalid location parameters: {e}")
        
        return queryset
    
    def get_permissions(self):
        """Allow reading without authentication, but require auth for create/update/delete"""
        if self.action in ['list', 'retrieve', 'nearby', 'categories']:
            return [AllowAny()]
        return [IsAuthenticated()]
    
    def perform_create(self, serializer):
        """Create experience with current user as host"""
        # Get location from request
        lat = self.request.data.get('latitude')
        lng = self.request.data.get('longitude')
        
        if not lat or not lng:
            raise serializers.ValidationError('Latitude and longitude are required')
        
        try:
            location = Point(float(lng), float(lat), srid=4326)
            serializer.save(host=self.request.user, location=location)
        except (ValueError, TypeError) as e:
            raise serializers.ValidationError(f'Invalid location coordinates: {str(e)}')
    
    @action(detail=False, methods=['get'], permission_classes=[AllowAny])
    def nearby(self, request):
        """Get experiences near a specific location"""
        lat = request.query_params.get('lat')
        lng = request.query_params.get('lng')
        radius = request.query_params.get('radius', 50)
        
        if not lat or not lng:
            return Response(
                {'error': 'Latitude and longitude are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            user_location = Point(float(lng), float(lat), srid=4326)
            experiences = Experience.objects.filter(
                status='active',
                location__distance_lte=(user_location, D(km=float(radius)))
            ).select_related('host', 'category')
            
            serializer = self.get_serializer(experiences, many=True)
            return Response({
                'count': experiences.count(),
                'results': serializer.data
            })
        except (ValueError, TypeError) as e:
            return Response(
                {'error': f'Invalid parameters: {str(e)}'},
                status=status.HTTP_400_BAD_REQUEST
            )
    
    @action(detail=True, methods=['get'], permission_classes=[AllowAny])
    def availability(self, request, pk=None):
        """Get availability for an experience"""
        experience = self.get_object()
        availability = ExperienceAvailability.objects.filter(
            experience=experience,
            is_available=True
        )
        serializer = ExperienceAvailabilitySerializer(availability, many=True)
        return Response(serializer.data)

    @action(detail=False, methods=['get'], permission_classes=[IsAuthenticated])
    def mine(self, request):
        """Get all experiences owned by the current host (all statuses)"""
        queryset = Experience.objects.filter(
            host=request.user
        ).select_related('category').order_by('-created_at')

        status_filter = request.query_params.get('status')
        if status_filter:
            queryset = queryset.filter(status=status_filter)

        search = request.query_params.get('search')
        if search:
            queryset = queryset.filter(
                Q(title__icontains=search) | Q(description__icontains=search)
            )

        serializer = self.get_serializer(queryset, many=True)
        return Response({
            'count': queryset.count(),
            'results': serializer.data
        })


class ExperienceBookingViewSet(viewsets.ModelViewSet):
    """ViewSet for experience bookings"""
    serializer_class = ExperienceBookingSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return bookings for current user (as guest or host)"""
        user = self.request.user
        
        # Allow filtering by status
        status_filter = self.request.query_params.get('status')
        # Allow host to view bookings on their experiences
        role = self.request.query_params.get('role', 'guest')
        
        if role == 'host':
            queryset = ExperienceBooking.objects.filter(
                experience__host=user
            ).select_related('experience', 'guest')
        else:
            queryset = ExperienceBooking.objects.filter(
                guest=user
            ).select_related('experience', 'experience__host')
        
        if status_filter:
            queryset = queryset.filter(status=status_filter)
        
        return queryset.order_by('-created_at')
    
    def perform_create(self, serializer):
        """Create experience booking for current user"""
        experience_id = self.request.data.get('experience')
        
        try:
            experience = Experience.objects.get(id=experience_id)
            
            # Validate participant count
            num_participants = int(self.request.data.get('num_participants', 1))
            if num_participants < experience.min_participants:
                raise serializers.ValidationError(
                    f'Minimum {experience.min_participants} participants required'
                )
            if num_participants > experience.max_participants:
                raise serializers.ValidationError(
                    f'Maximum {experience.max_participants} participants allowed'
                )
            
            # Set pricing
            price_per_person = experience.price_per_person
            
            serializer.save(
                guest=self.request.user,
                price_per_person=price_per_person,
                currency=experience.currency
            )
        except Experience.DoesNotExist:
            raise serializers.ValidationError('Experience not found')
        except ValueError as e:
            raise serializers.ValidationError(f'Invalid data: {str(e)}')
    
    @action(detail=True, methods=['post'])
    def cancel(self, request, pk=None):
        """Cancel a booking"""
        booking = self.get_object()
        
        # Check permissions - guest or host can cancel
        if request.user != booking.guest and request.user != booking.experience.host:
            return Response(
                {'error': 'You can only cancel your own bookings or bookings on your experiences'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Check if already cancelled
        if booking.status == 'cancelled':
            return Response(
                {'error': 'Booking is already cancelled'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Update status
        booking.status = 'cancelled'
        booking.save()
        
        serializer = self.get_serializer(booking)
        return Response(serializer.data)

    @action(detail=True, methods=['post'])
    def confirm(self, request, pk=None):
        """Confirm a booking (host only)"""
        booking = self.get_object()
        
        if request.user != booking.experience.host:
            return Response(
                {'error': 'Only the experience host can confirm bookings'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        if booking.status != 'pending':
            return Response(
                {'error': f'Cannot confirm a booking with status: {booking.status}'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        booking.status = 'confirmed'
        booking.save()
        
        serializer = self.get_serializer(booking)
        return Response(serializer.data)

    @action(detail=True, methods=['post'])
    def complete(self, request, pk=None):
        """Mark a booking as completed (host only)"""
        booking = self.get_object()
        
        if request.user != booking.experience.host:
            return Response(
                {'error': 'Only the experience host can complete bookings'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        if booking.status != 'confirmed':
            return Response(
                {'error': f'Cannot complete a booking with status: {booking.status}'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        booking.status = 'completed'
        booking.save()
        
        serializer = self.get_serializer(booking)
        return Response(serializer.data)
