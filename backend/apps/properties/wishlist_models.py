"""
Shared Wishlists Models
Enable collaborative trip planning with shared property lists
"""
from django.db import models
from apps.users.models import User
from apps.properties.models import Property
import uuid


class Wishlist(models.Model):
    """
    Collection of properties that can be shared with multiple users
    """
    PRIVACY_CHOICES = [
        ('private', 'Private'),
        ('shared', 'Shared with Collaborators'),
        ('public', 'Public'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    owner = models.ForeignKey(
        User,
        on_delete=models.CASCADE,
        related_name='owned_wishlists',
        help_text='User who created the wishlist'
    )
    
    name = models.CharField(max_length=255, help_text='Wishlist name (e.g., "Summer Vacation 2026")')
    description = models.TextField(blank=True, help_text='Optional description')
    
    privacy = models.CharField(
        max_length=20,
        choices=PRIVACY_CHOICES,
        default='private',
        help_text='Who can view this wishlist'
    )
    
    # Collaboration
    collaborators = models.ManyToManyField(
        User,
        related_name='collaborative_wishlists',
        blank=True,
        help_text='Users who can add/remove properties'
    )
    
    # Properties in wishlist
    properties = models.ManyToManyField(
        Property,
        through='WishlistItem',
        related_name='in_wishlists'
    )
    
    # Share link
    share_token = models.CharField(
        max_length=64,
        unique=True,
        blank=True,
        help_text='Token for sharing wishlist via link'
    )
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-updated_at']
        indexes = [
            models.Index(fields=['owner']),
            models.Index(fields=['share_token']),
        ]
    
    def __str__(self):
        return f"{self.name} by {self.owner.email}"
    
    def save(self, *args, **kwargs):
        if not self.share_token:
            # Generate unique share token
            self.share_token = uuid.uuid4().hex
        super().save(*args, **kwargs)
    
    def can_edit(self, user):
        """Check if user can edit this wishlist"""
        return user == self.owner or user in self.collaborators.all()
    
    def can_view(self, user):
        """Check if user can view this wishlist"""
        if self.privacy == 'public':
            return True
        if user == self.owner or user in self.collaborators.all():
            return True
        return False


class WishlistItem(models.Model):
    """
    Through model for Wishlist-Property relationship with additional metadata
    """
    wishlist = models.ForeignKey(Wishlist, on_delete=models.CASCADE, related_name='items')
    property = models.ForeignKey(Property, on_delete=models.CASCADE)
    
    added_by = models.ForeignKey(
        User,
        on_delete=models.SET_NULL,
        null=True,
        related_name='wishlist_items_added',
        help_text='User who added this property'
    )
    
    notes = models.TextField(
        blank=True,
        help_text='Personal notes about this property'
    )
    
    # Trip details (optional)
    preferred_check_in = models.DateField(null=True, blank=True)
    preferred_check_out = models.DateField(null=True, blank=True)
    
    # Voting/feedback from collaborators
    votes = models.IntegerField(default=0, help_text='Net votes from collaborators')
    
    added_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-votes', '-added_at']
        unique_together = ('wishlist', 'property')
        indexes = [
            models.Index(fields=['wishlist', '-votes']),
        ]
    
    def __str__(self):
        return f"{self.property.title} in {self.wishlist.name}"


class WishlistVote(models.Model):
    """
    Track individual votes on wishlist items
    """
    VOTE_CHOICES = [
        (1, 'Upvote'),
        (-1, 'Downvote'),
    ]
    
    wishlist_item = models.ForeignKey(
        WishlistItem,
        on_delete=models.CASCADE,
        related_name='vote_records'
    )
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    vote = models.IntegerField(choices=VOTE_CHOICES)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        unique_together = ('wishlist_item', 'user')
        indexes = [
            models.Index(fields=['wishlist_item', 'user']),
        ]
    
    def __str__(self):
        vote_type = 'upvote' if self.vote == 1 else 'downvote'
        return f"{self.user.email} {vote_type} on {self.wishlist_item.property.title}"
    
    def save(self, *args, **kwargs):
        # Update the item's vote count
        if self.pk:
            # Existing vote - remove old vote count
            old_vote = WishlistVote.objects.get(pk=self.pk)
            self.wishlist_item.votes -= old_vote.vote
        
        super().save(*args, **kwargs)
        
        # Add new vote count
        self.wishlist_item.votes += self.vote
        self.wishlist_item.save(update_fields=['votes'])
    
    def delete(self, *args, **kwargs):
        # Update vote count when deleted
        self.wishlist_item.votes -= self.vote
        self.wishlist_item.save(update_fields=['votes'])
        super().delete(*args, **kwargs)


class WishlistComment(models.Model):
    """
    Comments/discussion on wishlist items
    """
    wishlist_item = models.ForeignKey(
        WishlistItem,
        on_delete=models.CASCADE,
        related_name='comments'
    )
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    text = models.TextField()
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['created_at']
        indexes = [
            models.Index(fields=['wishlist_item', 'created_at']),
        ]
    
    def __str__(self):
        return f"Comment by {self.user.email} on {self.wishlist_item.property.title}"
