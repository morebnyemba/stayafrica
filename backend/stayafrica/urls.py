from django.contrib import admin
from django.urls import path, include, re_path
from django.conf import settings
from django.conf.urls.static import static
from drf_spectacular.views import SpectacularAPIView, SpectacularSwaggerView, SpectacularRedocView

urlpatterns = [
    # Admin
    path('admin/', admin.site.urls),
    
    # Health Checks
    path('api/health/', include('apps.health.urls')),
    
    # API Documentation
    path('api/schema/', SpectacularAPIView.as_view(), name='schema'),
    path('api/docs/', SpectacularSwaggerView.as_view(url_name='schema'), name='swagger-ui'),
    path('api/redoc/', SpectacularRedocView.as_view(url_name='schema'), name='redoc'),
    
    # API v1
    path('api/v1/', include('apps.users.urls', namespace='users')),
    path('api/v1/', include('apps.properties.urls', namespace='properties')),
    path('api/v1/', include('apps.bookings.urls', namespace='bookings')),
    path('api/v1/', include('apps.payments.urls', namespace='payments')),
    path('api/v1/', include('apps.reviews.urls', namespace='reviews')),
    path('api/v1/', include('apps.messaging.urls', namespace='messaging')),
    path('api/v1/admin/', include('apps.admin_dashboard.urls', namespace='admin_dashboard')),
]

if settings.DEBUG:
    urlpatterns += static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
    urlpatterns += static(settings.STATIC_URL, document_root=settings.STATIC_ROOT)
