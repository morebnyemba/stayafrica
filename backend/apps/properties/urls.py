from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.properties.views import PropertyViewSet, AmenityViewSet

app_name = 'properties'

router = DefaultRouter()
router.register(r'properties', PropertyViewSet, basename='property')
router.register(r'amenities', AmenityViewSet, basename='amenity')

urlpatterns = [
    path('', include(router.urls)),
]
