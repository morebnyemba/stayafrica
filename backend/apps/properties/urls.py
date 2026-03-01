from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.properties.views import PropertyViewSet, AmenityViewSet
from apps.properties.wishlist_views import WishlistViewSet
from apps.properties.poi_views import PointOfInterestViewSet, POICategoryViewSet
from apps.properties.analytics_views import (
    HostAnalyticsViewSet,
    PropertyAnalyticsViewSet,
    PerformanceBenchmarkViewSet
)

app_name = 'properties'

router = DefaultRouter()
router.register(r'properties', PropertyViewSet, basename='property')
router.register(r'amenities', AmenityViewSet, basename='amenity')
router.register(r'wishlists', WishlistViewSet, basename='wishlist')
router.register(r'pois', PointOfInterestViewSet, basename='poi')
router.register(r'poi-categories', POICategoryViewSet, basename='poi-category')
router.register(r'analytics/host', HostAnalyticsViewSet, basename='host-analytics')
router.register(r'analytics/properties', PropertyAnalyticsViewSet, basename='property-analytics')
router.register(r'analytics/benchmarks', PerformanceBenchmarkViewSet, basename='benchmarks')

urlpatterns = [
    path('', include(router.urls)),
]
