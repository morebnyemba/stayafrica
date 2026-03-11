from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.properties.views import (
    PropertyViewSet,
    AmenityViewSet,
    PropertyImageViewSet,
    SavedPropertyViewSet,
    GlobalPropertyAnalyticsViewSet,
    GlobalHostAnalyticsSummaryViewSet,
    PropertySearchView,
    PropertyFilterView,
    FeaturedPropertiesView,
)

app_name = 'properties'

router = DefaultRouter()
router.register(r'properties', PropertyViewSet, basename='property')
router.register(r'amenities', AmenityViewSet, basename='amenity')
router.register(r'property-images', PropertyImageViewSet, basename='property-image')
router.register(r'saved-properties', SavedPropertyViewSet, basename='saved-property')
router.register(r'global-property-analytics', GlobalPropertyAnalyticsViewSet, basename='global-property-analytics')
router.register(r'global-host-analytics', GlobalHostAnalyticsSummaryViewSet, basename='global-host-analytics')

urlpatterns = [
    path('', include(router.urls)),
    # Advanced search endpoints
    path('search/', PropertySearchView.as_view(), name='property_search'),
    path('filter/', PropertyFilterView.as_view(), name='property_filter'),
    path('featured/', FeaturedPropertiesView.as_view(), name='featured_properties'),
]
