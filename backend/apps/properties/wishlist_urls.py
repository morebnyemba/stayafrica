"""
Wishlist app URLs for user favorite properties management
"""
from django.urls import path, include
from rest_framework.routers import DefaultRouter
from . import views

app_name = 'wishlist'

router = DefaultRouter()
router.register(r'wishlists', views.WishlistViewSet, basename='wishlist')

urlpatterns = [
    path('', include(router.urls)),
    path('check/<int:property_id>/', views.WishlistCheckView.as_view(), name='wishlist-check'),
    path('add/', views.AddToWishlistView.as_view(), name='add-to-wishlist'),
    path('remove/', views.RemoveFromWishlistView.as_view(), name='remove-from-wishlist'),
]
