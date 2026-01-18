from django.urls import path, include
from rest_framework.routers import DefaultRouter
from rest_framework_simplejwt.views import TokenRefreshView
from apps.users.views import (
    UserViewSet,
    CustomTokenObtainPairView,
)
from apps.users.verification_views import (
    IdentityVerificationViewSet,
    VerificationSettingsViewSet
)

app_name = 'users'

router = DefaultRouter()
router.register(r'users', UserViewSet, basename='user')
# Register verification under users/ prefix to match frontend expectations
# DRF correctly handles this as: /api/v1/users/verification/ vs /api/v1/users/{id}/
# The literal 'verification' path is matched before the numeric ID pattern
router.register(r'users/verification', IdentityVerificationViewSet, basename='verification')
router.register(r'users/verification-settings', VerificationSettingsViewSet, basename='verification-settings')

urlpatterns = [
    path('', include(router.urls)),
    # Authentication endpoints
    path('auth/login/', CustomTokenObtainPairView.as_view(), name='token_obtain_pair'),
    path('auth/refresh/', TokenRefreshView.as_view(), name='token_refresh'),
]
