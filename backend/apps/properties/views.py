from rest_framework import viewsets, status, filters
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated, AllowAny
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

class AmenityViewSet(viewsets.ReadOnlyModelViewSet):
    queryset = Amenity.objects.all()
    serializer_class = AmenitySerializer
    permission_classes = [AllowAny]

class PropertyViewSet(viewsets.ModelViewSet):
    queryset = Property.objects.all()
    serializer_class = PropertySerializer
    permission_classes = [IsAuthenticated]
    filter_backends = [DjangoFilterBackend, filters.SearchFilter, filters.OrderingFilter]
    filterset_fields = ['country', 'city', 'property_type', 'status']
    search_fields = ['title', 'description', 'city', 'suburb']
    ordering_fields = ['price_per_night', 'created_at']
    ordering = ['-created_at']
    
    def get_permissions(self):
        if self.action == 'list' or self.action == 'retrieve':
            return [AllowAny()]
        return super().get_permissions()
    
    def get_serializer_class(self):
        if self.action == 'retrieve':
            return PropertyDetailSerializer
        elif self.action == 'list':
            return PropertyListSerializer
        return PropertySerializer
    
    def perform_create(self, serializer):
        """Set host to current user"""
        serializer.save(host=self.request.user)
    
    def get_queryset(self):
        """Filter properties by status"""
        if self.request.user.is_authenticated and self.request.user.is_host:
            return Property.objects.filter(host=self.request.user)
        return Property.objects.filter(status='active')
    
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
