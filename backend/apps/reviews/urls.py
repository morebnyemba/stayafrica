from django.urls import path, include
from rest_framework.routers import DefaultRouter
from apps.reviews.views import ReviewViewSet, AdminReviewVoteViewSet

app_name = 'reviews'

router = DefaultRouter()
router.register(r'reviews', ReviewViewSet, basename='review')
router.register(r'review-votes', AdminReviewVoteViewSet, basename='review-vote')

urlpatterns = [
    path('', include(router.urls)),
]
