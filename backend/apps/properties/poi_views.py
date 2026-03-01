from rest_framework import viewsets, filters
from rest_framework.permissions import IsAuthenticated, IsAdminUser
from django_filters.rest_framework import DjangoFilterBackend
from apps.properties.poi_models import POICategory, PointOfInterest
from apps.properties.poi_serializers import POICategorySerializer, PointOfInterestSerializer


class POICategoryViewSet(viewsets.ModelViewSet):
    queryset = POICategory.objects.all()
    serializer_class = POICategorySerializer
    permission_classes = [IsAdminUser]
    filter_backends = [filters.SearchFilter, filters.OrderingFilter]
    search_fields = ['name']
    ordering_fields = ['display_order', 'name']


class PointOfInterestViewSet(viewsets.ModelViewSet):
    queryset = PointOfInterest.objects.select_related('category').all()
    serializer_class = PointOfInterestSerializer
    permission_classes = [IsAdminUser]
    filter_backends = [filters.SearchFilter, filters.OrderingFilter, DjangoFilterBackend]
    search_fields = ['name', 'city', 'country']
    filterset_fields = ['poi_type', 'is_active', 'city']
    ordering_fields = ['name', 'rating', 'created_at']
