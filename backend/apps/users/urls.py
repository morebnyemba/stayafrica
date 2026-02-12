from django.urls import path, include
from rest_framework.routers import DefaultRouter
from rest_framework_simplejwt.views import TokenRefreshView
from apps.users.views import UserViewSet, CustomTokenObtainPairView, UserPreferenceViewSet, UserPropertyInteractionViewSet
from apps.users import views_2fa

app_name = 'users'

router = DefaultRouter()
router.register(r'users', UserViewSet, basename='user')
router.register(r'preferences', UserPreferenceViewSet, basename='user-preference')
router.register(r'interactions', UserPropertyInteractionViewSet, basename='user-interaction')

urlpatterns = [
    path('', include(router.urls)),
    path('auth/login/', CustomTokenObtainPairView.as_view(), name='token_obtain_pair'),
    path('auth/login/2fa/', views_2fa.login_with_2fa, name='login_with_2fa'),
    path('auth/refresh/', TokenRefreshView.as_view(), name='token_refresh'),
    
    # Two-Factor Authentication endpoints
    path('2fa/setup/', views_2fa.setup_2fa, name='2fa_setup'),
    path('2fa/enable/', views_2fa.enable_2fa, name='2fa_enable'),
    path('2fa/disable/', views_2fa.disable_2fa, name='2fa_disable'),
    path('2fa/status/', views_2fa.get_2fa_status, name='2fa_status'),
    path('2fa/verify/', views_2fa.verify_token, name='2fa_verify'),
    path('2fa/backup-codes/regenerate/', views_2fa.regenerate_backup_codes, name='2fa_backup_codes_regenerate'),
]
