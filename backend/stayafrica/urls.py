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
    path('api/v1/', include('apps.users.api_urls')),
    path('api/v1/', include('apps.properties.urls')),
    path('api/v1/', include('apps.bookings.urls')),
    path('api/v1/', include('apps.payments.urls')),
    path('api/v1/', include('apps.reviews.urls')),
    path('api/v1/', include('apps.experiences.urls')),
    path('api/v1/', include('apps.notifications.urls')),
    path('api/v1/messaging/', include('apps.messaging.urls')),
    path('api/v1/admin/', include('apps.admin_dashboard.urls')),
]

if settings.DEBUG:
    urlpatterns += static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
    urlpatterns += static(settings.STATIC_URL, document_root=settings.STATIC_ROOT)
