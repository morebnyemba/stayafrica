from rest_framework import viewsets, status, filters
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated, AllowAny, BasePermission
from django_filters.rest_framework import DjangoFilterBackend
from django.contrib.gis.db.models.functions import Distance
from django.contrib.gis.geos import Point
from django.db import IntegrityError
from apps.properties.models import Property, Amenity, PropertyImage, SavedProperty
from apps.properties.serializers import (
    PropertySerializer,
    PropertyDetailSerializer,
    PropertyListSerializer,
    AmenitySerializer,
    SavedPropertySerializer
)
from services.geocoding_service import GeocodingService
from services.host_analytics import HostAnalyticsService
import logging

logger = logging.getLogger(__name__)


class IsHostOrReadOnly(BasePermission):
    """
    Permission to allow hosts to create/edit/delete properties.
    Everyone can read.
    """
    def has_permission(self, request, view):
        # Allow GET requests to anyone
        if request.method in ['GET', 'HEAD', 'OPTIONS']:
            return True
        # Only authenticated users can create, update, delete
        return request.user and request.user.is_authenticated
    
    def has_object_permission(self, request, view, obj):
        # Allow GET requests to anyone
        if request.method in ['GET', 'HEAD', 'OPTIONS']:
            return True
        # Only the property host can modify
        return obj.host == request.user

class AmenityViewSet(viewsets.ReadOnlyModelViewSet):
    queryset = Amenity.objects.all()
    serializer_class = AmenitySerializer
    permission_classes = [AllowAny]

class PropertyViewSet(viewsets.ModelViewSet):
    queryset = Property.objects.all()
    serializer_class = PropertySerializer
    permission_classes = [IsHostOrReadOnly]
    filter_backends = [DjangoFilterBackend, filters.SearchFilter, filters.OrderingFilter]
    filterset_fields = ['country', 'city', 'property_type', 'status']
    search_fields = ['title', 'description', 'city', 'suburb']
    ordering_fields = ['price_per_night', 'created_at']
    ordering = ['-created_at']
    
    def get_serializer_class(self):
        if self.action == 'retrieve':
            return PropertyDetailSerializer
        elif self.action == 'list':
            return PropertyListSerializer
        return PropertySerializer
    
    def create(self, request, *args, **kwargs):
        """Override create to ensure user is a host and log validation errors"""
        if not request.user.is_host:
            return Response(
                {'error': 'Only hosts can create properties. Please upgrade to host first.'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        # Log incoming data for debugging
        logger.info(f"Property creation attempt by user {request.user.id}: {request.data}")
        
        try:
            return super().create(request, *args, **kwargs)
        except Exception as e:
            logger.error(f"Property creation failed for user {request.user.id}: {str(e)}", exc_info=True)
            raise
    
    def perform_create(self, serializer):
        """Set host to current user"""
        try:
            serializer.save(host=self.request.user)
            logger.info(f"Property created successfully by user {self.request.user.id}")
        except Exception as e:
            logger.error(f"Error in perform_create: {str(e)}", exc_info=True)
            raise
    
    def perform_update(self, serializer):
        """Ensure only the host can update their own property"""
        if serializer.instance.host != self.request.user:
            raise PermissionError('You can only update your own properties')
        serializer.save()
    
    def perform_destroy(self, instance):
        """Ensure only the host can delete their own property"""
        if instance.host != self.request.user:
            raise PermissionError('You can only delete your own properties')
        instance.delete()
    
    def get_queryset(self):
        """
        Return all properties for list/retrieve.
        Filtering by ownership is handled by object-level permissions.
        """
        return Property.objects.all()
    
    @action(detail=True, methods=['post'], permission_classes=[IsAuthenticated])
    def upload_images(self, request, pk=None):
        """Upload multiple images for a property"""
        property_obj = self.get_object()
        
        # Check if user is the owner
        if property_obj.host_id != request.user.id:
            return Response(
                {'error': 'You can only upload images for your own properties'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        files = request.FILES.getlist('images')
        if not files:
            return Response(
                {'error': 'No images provided'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if len(files) > 10:
            return Response(
                {'error': 'Maximum 10 images per property'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        created_images = []
        try:
            for idx, file in enumerate(files):
                # Validate file size (5MB max)
                if file.size > 5 * 1024 * 1024:
                    return Response(
                        {'error': f'Image {idx + 1} exceeds 5MB limit'},
                        status=status.HTTP_400_BAD_REQUEST
                    )
                
                # Validate file type
                if not file.content_type.startswith('image/'):
                    return Response(
                        {'error': f'File {idx + 1} is not an image'},
                        status=status.HTTP_400_BAD_REQUEST
                    )
                
                property_image = PropertyImage.objects.create(
                    property=property_obj,
                    image=file,
                    order=idx
                )
                created_images.append(property_image)
            
            # Queue image processing task
            from tasks.image_tasks import process_property_images
            process_property_images.delay(property_obj.id)
            
            serializer = PropertyImageSerializer(created_images, many=True)
            return Response(
                {'images': serializer.data, 'message': f'{len(created_images)} images uploaded successfully'},
                status=status.HTTP_201_CREATED
            )
        except Exception as e:
            logger.error(f'Error uploading images for property {pk}: {str(e)}')
            return Response(
                {'error': 'Error uploading images'},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
    
    @action(detail=False, methods=['get'], permission_classes=[AllowAny])
    def search_nearby(self, request):
        """Search for properties within a radius"""
        lat = request.query_params.get('lat')
        lon = request.query_params.get('lon')
        radius = request.query_params.get('radius', 10)  # km
        
        if not lat or not lon:
            return Response(
                {'error': 'lat and lon parameters are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            point = Point(float(lon), float(lat))
            properties = Property.objects.filter(
                status='active',
                location__distance_lte=(point, int(radius) * 1000)
            ).annotate(
                distance=Distance('location', point)
            ).order_by('distance')
            
            serializer = PropertyListSerializer(properties, many=True)
            return Response(serializer.data)
        except ValueError:
            return Response(
                {'error': 'Invalid lat/lon values'},
                status=status.HTTP_400_BAD_REQUEST
            )
    
    @action(detail=True, methods=['get'], permission_classes=[AllowAny])
    def availability(self, request, pk=None):
        """Check property availability for given dates"""
        property_obj = self.get_object()
        check_in = request.query_params.get('check_in')
        check_out = request.query_params.get('check_out')
        
        if not check_in or not check_out:
            return Response(
                {'error': 'check_in and check_out dates are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            from datetime import datetime
            check_in_date = datetime.strptime(check_in, '%Y-%m-%d').date()
            check_out_date = datetime.strptime(check_out, '%Y-%m-%d').date()
        except ValueError:
            return Response(
                {'error': 'Invalid date format. Use YYYY-MM-DD'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Check if property is active
        if property_obj.status != 'active':
            return Response({
                'available': False,
                'reason': 'Property is not active'
            })
        
        # Check booking availability
        from utils.helpers import is_booking_date_available
        available = is_booking_date_available(property_obj, check_in_date, check_out_date)
        
        response_data = {
            'available': available,
            'property_id': property_obj.id,
            'check_in': check_in,
            'check_out': check_out,
        }
        
        if available:
            # Calculate pricing
            from utils.helpers import calculate_nights, calculate_booking_total
            nights = calculate_nights(check_in_date, check_out_date)
            totals = calculate_booking_total(property_obj.price_per_night, nights)
            response_data.update({
                'nights': nights,
                'price_per_night': property_obj.price_per_night,
                'pricing': {
                    'nightly_total': str(totals['nightly_total']),
                    'service_fee': str(totals['service_fee']),
                    'commission_fee': str(totals['commission_fee']),
                    'grand_total': str(totals['grand_total']),
                }
            })
        else:
            response_data['reason'] = 'Property is already booked for these dates'
        
        return Response(response_data)
    
    @action(detail=False, methods=['get', 'post'], permission_classes=[IsAuthenticated])
    def saved(self, request):
        """Get or save user's wishlist properties"""
        if request.method == 'GET':
            # Get saved properties
            saved = SavedProperty.objects.filter(user=request.user).select_related('property')
            serializer = SavedPropertySerializer(saved, many=True)
            return Response({'results': serializer.data, 'count': saved.count()})
        
        elif request.method == 'POST':
            # Save a property
            serializer = SavedPropertySerializer(data=request.data, context={'request': request})
            if serializer.is_valid():
                try:
                    serializer.save()
                    return Response(serializer.data, status=status.HTTP_201_CREATED)
                except IntegrityError:
                    return Response(
                        {'error': 'Property already saved'},
                        status=status.HTTP_400_BAD_REQUEST
                    )
            return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
    
    @action(detail=True, methods=['delete'], permission_classes=[IsAuthenticated], url_path='saved')
    def unsave_property(self, request, pk=None):
        """Remove property from wishlist"""
        try:
            saved = SavedProperty.objects.get(user=request.user, property_id=pk)
            saved.delete()
            return Response(
                {'message': 'Property removed from wishlist'},
                status=status.HTTP_204_NO_CONTENT
            )
        except SavedProperty.DoesNotExist:
            return Response(
                {'error': 'Property not in wishlist'},
                status=status.HTTP_404_NOT_FOUND
            )
    
    @action(detail=True, methods=['get'], permission_classes=[IsAuthenticated])
    def is_saved(self, request, pk=None):
        """Check if property is saved by user"""
        is_saved = SavedProperty.objects.filter(
            user=request.user,
            property_id=pk
        ).exists()
        return Response({'is_saved': is_saved})
    
    @action(detail=False, methods=['post'], permission_classes=[AllowAny])
    def geocode(self, request):
        """
        Geocode an address to get coordinates
        POST /api/v1/properties/geocode/
        Body: {"address": "123 Main St, Harare, Zimbabwe", "country": "ZW"}
        """
        address = request.data.get('address')
        country = request.data.get('country')
        
        if not address:
            return Response(
                {'error': 'Address is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        result = GeocodingService.geocode_address(address, country)
        
        if result:
            return Response(result)
        else:
            return Response(
                {'error': 'Could not geocode address'},
                status=status.HTTP_404_NOT_FOUND
            )
    
    @action(detail=False, methods=['post'], permission_classes=[AllowAny])
    def reverse_geocode(self, request):
        """
        Reverse geocode coordinates to get address
        POST /api/v1/properties/reverse_geocode/
        Body: {"latitude": -17.8252, "longitude": 31.0335}
        """
        lat = request.data.get('latitude')
        lon = request.data.get('longitude')
        
        if lat is None or lon is None:
            return Response(
                {'error': 'Both latitude and longitude are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            lat = float(lat)
            lon = float(lon)
        except (TypeError, ValueError):
            return Response(
                {'error': 'Invalid coordinate values'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if not GeocodingService.validate_coordinates(lat, lon):
            return Response(
                {'error': 'Coordinates out of valid range'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        result = GeocodingService.reverse_geocode(lat, lon)
        
        if result:
            return Response(result)
        else:
            return Response(
                {'error': 'Could not reverse geocode coordinates'},
                status=status.HTTP_404_NOT_FOUND
            )
    
    @action(detail=False, methods=['get'], permission_classes=[AllowAny])
    def location_suggestions(self, request):
        """
        Get location autocomplete suggestions
        GET /api/v1/properties/location_suggestions/?q=harare&country=ZW&limit=5
        """
        query = request.query_params.get('q', '')
        country = request.query_params.get('country')
        limit = int(request.query_params.get('limit', 5))
        
        if not query or len(query) < 2:
            return Response(
                {'error': 'Query must be at least 2 characters'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        suggestions = GeocodingService.get_location_suggestions(query, country, limit)
        return Response({'suggestions': suggestions})
    
    @action(detail=False, methods=['get'], permission_classes=[IsAuthenticated])
    def host_properties(self, request):
        """
        Get all properties for the authenticated host
        GET /api/v1/properties/host_properties/
        """
        if not request.user.is_host:
            return Response(
                {'error': 'Only hosts can access this endpoint'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        properties = Property.objects.filter(host=request.user).order_by('-created_at')
        serializer = PropertyListSerializer(properties, many=True)
        return Response({
            'results': serializer.data,
            'count': properties.count()
        })
    
    @action(detail=False, methods=['get'], permission_classes=[IsAuthenticated])
    def host_analytics(self, request):
        """
        Get comprehensive analytics for host
        GET /api/v1/properties/host_analytics/
        """
        if not request.user.is_host:
            return Response(
                {'error': 'Only hosts can access this endpoint'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        overview = HostAnalyticsService.get_host_overview(request.user)
        return Response(overview)
    
    @action(detail=False, methods=['get'], permission_classes=[IsAuthenticated])
    def host_earnings(self, request):
        """
        Get earnings breakdown by period
        GET /api/v1/properties/host_earnings/?period=month
        """
        if not request.user.is_host:
            return Response(
                {'error': 'Only hosts can access this endpoint'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        period = request.query_params.get('period', 'month')
        earnings = HostAnalyticsService.get_earnings_breakdown(request.user, period)
        return Response({'earnings': earnings})
    
    @action(detail=False, methods=['get'], permission_classes=[IsAuthenticated])
    def property_performance(self, request):
        """
        Get performance metrics for all host properties
        GET /api/v1/properties/property_performance/
        """
        if not request.user.is_host:
            return Response(
                {'error': 'Only hosts can access this endpoint'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        performance = HostAnalyticsService.get_property_performance(request.user)
        return Response({'properties': performance})
    
    @action(detail=True, methods=['get'], permission_classes=[IsAuthenticated])
    def booking_calendar(self, request, pk=None):
        """
        Get booking calendar for a specific property
        GET /api/v1/properties/{id}/booking_calendar/?start=2024-01-01&end=2024-03-31
        """
        property_obj = self.get_object()
        
        # Only host or admin can view calendar
        if not (request.user == property_obj.host or request.user.is_admin_user):
            return Response(
                {'error': 'You do not have permission to view this calendar'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        from datetime import datetime
        start_date_str = request.query_params.get('start')
        end_date_str = request.query_params.get('end')
        
        start_date = None
        end_date = None
        
        if start_date_str:
            try:
                start_date = datetime.strptime(start_date_str, '%Y-%m-%d').date()
            except ValueError:
                return Response(
                    {'error': 'Invalid start date format. Use YYYY-MM-DD'},
                    status=status.HTTP_400_BAD_REQUEST
                )
        
        if end_date_str:
            try:
                end_date = datetime.strptime(end_date_str, '%Y-%m-%d').date()
            except ValueError:
                return Response(
                    {'error': 'Invalid end date format. Use YYYY-MM-DD'},
                    status=status.HTTP_400_BAD_REQUEST
                )
        
        calendar_data = HostAnalyticsService.get_booking_calendar(
            property_obj.id, start_date, end_date
        )
        return Response(calendar_data)
    
    @action(detail=False, methods=['get'], permission_classes=[IsAuthenticated])
    def upcoming_checkins(self, request):
        """
        Get upcoming check-ins for host
        GET /api/v1/properties/upcoming_checkins/?days=7
        """
        if not request.user.is_host:
            return Response(
                {'error': 'Only hosts can access this endpoint'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        days = int(request.query_params.get('days', 7))
        check_ins = HostAnalyticsService.get_upcoming_check_ins(request.user, days)
        return Response({'upcoming_checkins': check_ins})
    
    @action(detail=False, methods=['get'], permission_classes=[IsAuthenticated])
    def pending_actions(self, request):
        """
        Get pending actions requiring host attention
        GET /api/v1/properties/pending_actions/
        """
        if not request.user.is_host:
            return Response(
                {'error': 'Only hosts can access this endpoint'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        pending = HostAnalyticsService.get_pending_actions(request.user)
        return Response(pending)
