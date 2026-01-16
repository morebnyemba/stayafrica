from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.properties.views import PropertyViewSet, AmenityViewSet
from apps.properties.wishlist_views import WishlistViewSet

app_name = 'properties'

router = DefaultRouter()
router.register(r'properties', PropertyViewSet, basename='property')
router.register(r'amenities', AmenityViewSet, basename='amenity')
router.register(r'wishlists', WishlistViewSet, basename='wishlist')

urlpatterns = [
    path('', include(router.urls)),
]
