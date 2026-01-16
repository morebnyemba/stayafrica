"""
Serializers for Shared Wishlists
"""
from rest_framework import serializers
from apps.properties.wishlist_models import Wishlist, WishlistItem, WishlistVote, WishlistComment
from apps.properties.serializers import PropertyListSerializer
from apps.users.models import User


class WishlistItemSerializer(serializers.ModelSerializer):
    """Serializer for wishlist items with property details"""
    property_details = PropertyListSerializer(source='property', read_only=True)
    added_by_email = serializers.EmailField(source='added_by.email', read_only=True)
    
    class Meta:
        model = WishlistItem
        fields = [
            'id', 'property', 'property_details', 'added_by', 'added_by_email',
            'notes', 'preferred_check_in', 'preferred_check_out',
            'votes', 'added_at', 'updated_at'
        ]
        read_only_fields = ['id', 'added_by', 'votes', 'added_at', 'updated_at']


class WishlistSerializer(serializers.ModelSerializer):
    """Serializer for wishlists"""
    owner_email = serializers.EmailField(source='owner.email', read_only=True)
    collaborator_emails = serializers.SerializerMethodField()
    items = WishlistItemSerializer(many=True, read_only=True)
    item_count = serializers.SerializerMethodField()
    share_url = serializers.SerializerMethodField()
    can_edit = serializers.SerializerMethodField()
    
    class Meta:
        model = Wishlist
        fields = [
            'id', 'owner', 'owner_email', 'name', 'description',
            'privacy', 'collaborators', 'collaborator_emails',
            'items', 'item_count', 'share_token', 'share_url',
            'can_edit', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'owner', 'share_token', 'created_at', 'updated_at']
    
    def get_collaborator_emails(self, obj):
        return [user.email for user in obj.collaborators.all()]
    
    def get_item_count(self, obj):
        return obj.items.count()
    
    def get_share_url(self, obj):
        request = self.context.get('request')
        if request:
            return f"{request.scheme}://{request.get_host()}/wishlists/shared/{obj.share_token}/"
        return f"/wishlists/shared/{obj.share_token}/"
    
    def get_can_edit(self, obj):
        request = self.context.get('request')
        if request and request.user.is_authenticated:
            return obj.can_edit(request.user)
        return False


class WishlistCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating wishlists"""
    
    class Meta:
        model = Wishlist
        fields = ['name', 'description', 'privacy']


class CollaboratorSerializer(serializers.Serializer):
    """Serializer for adding/removing collaborators"""
    email = serializers.EmailField(required=True)
    
    def validate_email(self, value):
        try:
            User.objects.get(email=value)
        except User.DoesNotExist:
            raise serializers.ValidationError("User with this email does not exist")
        return value


class WishlistVoteSerializer(serializers.ModelSerializer):
    """Serializer for voting on wishlist items"""
    
    class Meta:
        model = WishlistVote
        fields = ['id', 'vote', 'created_at']
        read_only_fields = ['id', 'created_at']


class WishlistCommentSerializer(serializers.ModelSerializer):
    """Serializer for comments on wishlist items"""
    user_email = serializers.EmailField(source='user.email', read_only=True)
    
    class Meta:
        model = WishlistComment
        fields = ['id', 'user', 'user_email', 'text', 'created_at', 'updated_at']
        read_only_fields = ['id', 'user', 'created_at', 'updated_at']
