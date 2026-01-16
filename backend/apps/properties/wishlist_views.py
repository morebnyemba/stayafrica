"""
Views for Shared Wishlists
"""
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated, AllowAny
from django.shortcuts import get_object_or_404
from apps.properties.wishlist_models import Wishlist, WishlistItem, WishlistVote, WishlistComment
from apps.properties.wishlist_serializers import (
    WishlistSerializer,
    WishlistCreateSerializer,
    WishlistItemSerializer,
    CollaboratorSerializer,
    WishlistVoteSerializer,
    WishlistCommentSerializer
)
from apps.users.models import User
from apps.properties.models import Property
import logging

logger = logging.getLogger(__name__)


class WishlistViewSet(viewsets.ModelViewSet):
    """ViewSet for managing wishlists"""
    serializer_class = WishlistSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return wishlists owned by or shared with current user"""
        user = self.request.user
        from django.db.models import Q
        return Wishlist.objects.filter(
            Q(owner=user) | Q(collaborators=user) | Q(privacy='public')
        ).distinct().prefetch_related('collaborators', 'items', 'items__property')
    
    def get_serializer_class(self):
        if self.action == 'create':
            return WishlistCreateSerializer
        return WishlistSerializer
    
    def create(self, request, *args, **kwargs):
        """Create a new wishlist"""
        serializer = WishlistCreateSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        wishlist = serializer.save(owner=request.user)
        
        logger.info(f"Wishlist '{wishlist.name}' created by {request.user.email}")
        
        output_serializer = WishlistSerializer(wishlist, context={'request': request})
        return Response(output_serializer.data, status=status.HTTP_201_CREATED)
    
    def update(self, request, *args, **kwargs):
        """Update wishlist (owner only)"""
        wishlist = self.get_object()
        
        if request.user != wishlist.owner:
            return Response(
                {'error': 'Only the owner can update wishlist settings'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        return super().update(request, *args, **kwargs)
    
    def destroy(self, request, *args, **kwargs):
        """Delete wishlist (owner only)"""
        wishlist = self.get_object()
        
        if request.user != wishlist.owner:
            return Response(
                {'error': 'Only the owner can delete this wishlist'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        return super().destroy(request, *args, **kwargs)
    
    @action(detail=True, methods=['post'])
    def add_property(self, request, pk=None):
        """Add a property to the wishlist"""
        wishlist = self.get_object()
        
        if not wishlist.can_edit(request.user):
            return Response(
                {'error': 'You do not have permission to edit this wishlist'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        property_id = request.data.get('property_id')
        if not property_id:
            return Response(
                {'error': 'property_id is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            property_obj = Property.objects.get(id=property_id)
        except Property.DoesNotExist:
            return Response(
                {'error': 'Property not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        # Create or update item
        item, created = WishlistItem.objects.get_or_create(
            wishlist=wishlist,
            property=property_obj,
            defaults={
                'added_by': request.user,
                'notes': request.data.get('notes', ''),
                'preferred_check_in': request.data.get('preferred_check_in'),
                'preferred_check_out': request.data.get('preferred_check_out'),
            }
        )
        
        if not created:
            return Response(
                {'error': 'Property already in wishlist'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        logger.info(f"Property {property_id} added to wishlist {wishlist.id} by {request.user.email}")
        
        serializer = WishlistItemSerializer(item)
        return Response(serializer.data, status=status.HTTP_201_CREATED)
    
    @action(detail=True, methods=['post'])
    def remove_property(self, request, pk=None):
        """Remove a property from the wishlist"""
        wishlist = self.get_object()
        
        if not wishlist.can_edit(request.user):
            return Response(
                {'error': 'You do not have permission to edit this wishlist'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        property_id = request.data.get('property_id')
        if not property_id:
            return Response(
                {'error': 'property_id is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            item = WishlistItem.objects.get(wishlist=wishlist, property_id=property_id)
            item.delete()
            logger.info(f"Property {property_id} removed from wishlist {wishlist.id}")
            return Response({'message': 'Property removed from wishlist'}, status=status.HTTP_204_NO_CONTENT)
        except WishlistItem.DoesNotExist:
            return Response(
                {'error': 'Property not in wishlist'},
                status=status.HTTP_404_NOT_FOUND
            )
    
    @action(detail=True, methods=['post'])
    def add_collaborator(self, request, pk=None):
        """Add a collaborator to the wishlist (owner only)"""
        wishlist = self.get_object()
        
        if request.user != wishlist.owner:
            return Response(
                {'error': 'Only the owner can add collaborators'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        serializer = CollaboratorSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        
        try:
            collaborator = User.objects.get(email=serializer.validated_data['email'])
            
            if collaborator == wishlist.owner:
                return Response(
                    {'error': 'Owner is already a collaborator'},
                    status=status.HTTP_400_BAD_REQUEST
                )
            
            wishlist.collaborators.add(collaborator)
            logger.info(f"Collaborator {collaborator.email} added to wishlist {wishlist.id}")
            
            # Send notification
            try:
                from services.notification_service import NotificationService
                NotificationService.send_notification(
                    user=collaborator,
                    title="Wishlist Invitation",
                    body=f"{request.user.get_full_name() or request.user.email} invited you to collaborate on '{wishlist.name}'",
                    notification_type='system',
                    data={'wishlist_id': str(wishlist.id)},
                    deep_link=f"stayafrica://wishlists/{wishlist.id}"
                )
            except Exception as e:
                logger.warning(f"Failed to send wishlist invitation notification: {e}")
            
            return Response({'message': 'Collaborator added'}, status=status.HTTP_200_OK)
        except User.DoesNotExist:
            return Response(
                {'error': 'User not found'},
                status=status.HTTP_404_NOT_FOUND
            )
    
    @action(detail=True, methods=['post'])
    def remove_collaborator(self, request, pk=None):
        """Remove a collaborator from the wishlist (owner only)"""
        wishlist = self.get_object()
        
        if request.user != wishlist.owner:
            return Response(
                {'error': 'Only the owner can remove collaborators'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        serializer = CollaboratorSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        
        try:
            collaborator = User.objects.get(email=serializer.validated_data['email'])
            wishlist.collaborators.remove(collaborator)
            logger.info(f"Collaborator {collaborator.email} removed from wishlist {wishlist.id}")
            return Response({'message': 'Collaborator removed'}, status=status.HTTP_200_OK)
        except User.DoesNotExist:
            return Response(
                {'error': 'User not found'},
                status=status.HTTP_404_NOT_FOUND
            )
    
    @action(detail=True, methods=['get'], permission_classes=[AllowAny])
    def shared(self, request, pk=None):
        """Access wishlist via share token (public)"""
        try:
            wishlist = Wishlist.objects.get(share_token=pk)
            
            # Check if user can view
            user = request.user if request.user.is_authenticated else None
            if wishlist.privacy == 'private' and (not user or not wishlist.can_view(user)):
                return Response(
                    {'error': 'This wishlist is private'},
                    status=status.HTTP_403_FORBIDDEN
                )
            
            serializer = WishlistSerializer(wishlist, context={'request': request})
            return Response(serializer.data)
        except Wishlist.DoesNotExist:
            return Response(
                {'error': 'Wishlist not found'},
                status=status.HTTP_404_NOT_FOUND
            )
    
    @action(detail=True, methods=['post'])
    def vote_item(self, request, pk=None):
        """Vote on a wishlist item"""
        wishlist = self.get_object()
        
        if not wishlist.can_view(request.user):
            return Response(
                {'error': 'You do not have permission to vote on this wishlist'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        item_id = request.data.get('item_id')
        vote_value = request.data.get('vote')  # 1 or -1
        
        if not item_id or vote_value not in [1, -1]:
            return Response(
                {'error': 'item_id and vote (1 or -1) are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            item = WishlistItem.objects.get(id=item_id, wishlist=wishlist)
            
            # Create or update vote
            vote, created = WishlistVote.objects.update_or_create(
                wishlist_item=item,
                user=request.user,
                defaults={'vote': vote_value}
            )
            
            action_text = 'updated' if not created else 'added'
            logger.info(f"Vote {action_text} for item {item_id} in wishlist {wishlist.id}")
            
            return Response({
                'message': f'Vote {action_text}',
                'votes': item.votes
            })
        except WishlistItem.DoesNotExist:
            return Response(
                {'error': 'Wishlist item not found'},
                status=status.HTTP_404_NOT_FOUND
            )
    
    @action(detail=True, methods=['post'])
    def comment_item(self, request, pk=None):
        """Add a comment to a wishlist item"""
        wishlist = self.get_object()
        
        if not wishlist.can_view(request.user):
            return Response(
                {'error': 'You do not have permission to comment on this wishlist'},
                status=status.HTTP_403_FORBIDDEN
            )
        
        item_id = request.data.get('item_id')
        text = request.data.get('text', '').strip()
        
        if not item_id or not text:
            return Response(
                {'error': 'item_id and text are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            item = WishlistItem.objects.get(id=item_id, wishlist=wishlist)
            
            comment = WishlistComment.objects.create(
                wishlist_item=item,
                user=request.user,
                text=text
            )
            
            logger.info(f"Comment added to item {item_id} in wishlist {wishlist.id}")
            
            serializer = WishlistCommentSerializer(comment)
            return Response(serializer.data, status=status.HTTP_201_CREATED)
        except WishlistItem.DoesNotExist:
            return Response(
                {'error': 'Wishlist item not found'},
                status=status.HTTP_404_NOT_FOUND
            )
