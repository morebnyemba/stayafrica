from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.experiences.views import (
    ExperienceViewSet, ExperienceCategoryViewSet, ExperienceBookingViewSet
)

app_name = 'experiences'

router = DefaultRouter()
router.register(r'categories', ExperienceCategoryViewSet, basename='experience-category')
router.register(r'experiences', ExperienceViewSet, basename='experience')
router.register(r'bookings', ExperienceBookingViewSet, basename='experience-booking')

urlpatterns = [
    path('', include(router.urls)),
]
