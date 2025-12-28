from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.properties.views import (
    PropertyViewSet,
    AmenityViewSet,
    PropertySearchView,
    PropertyFilterView,
    FeaturedPropertiesView,
)

app_name = 'properties'

router = DefaultRouter()
router.register(r'properties', PropertyViewSet, basename='property')
router.register(r'amenities', AmenityViewSet, basename='amenity')

urlpatterns = [
    path('', include(router.urls)),
    # Advanced search endpoints
    path('search/', PropertySearchView.as_view(), name='property_search'),
    path('filter/', PropertyFilterView.as_view(), name='property_filter'),
    path('featured/', FeaturedPropertiesView.as_view(), name='featured_properties'),
]
