from django.urls import path, include
from rest_framework.routers import DefaultRouter
from rest_framework_simplejwt.views import TokenRefreshView
from apps.users.views import (
    UserViewSet,
    CustomTokenObtainPairView,
    RegisterView,
    ProfileUpdateView,
    UserSearchView,
)
from apps.users.verification_views import (
    IdentityVerificationViewSet,
    VerificationSettingsViewSet
)

app_name = 'users'

router = DefaultRouter()
router.register(r'users', UserViewSet, basename='user')
router.register(r'verification', IdentityVerificationViewSet, basename='verification')
router.register(r'verification-settings', VerificationSettingsViewSet, basename='verification-settings')

urlpatterns = [
    path('', include(router.urls)),
    # Authentication endpoints
    path('auth/login/', CustomTokenObtainPairView.as_view(), name='token_obtain_pair'),
    path('auth/refresh/', TokenRefreshView.as_view(), name='token_refresh'),
    path('auth/register/', RegisterView.as_view(), name='register'),
    # Profile endpoints
    path('profile/update/', ProfileUpdateView.as_view(), name='profile_update'),
    # User search endpoint
    path('search/', UserSearchView.as_view(), name='user_search'),
]
