from django.db import models
from apps.users.models import User
from apps.properties.models import Property


class SavedProperty(models.Model):
    """
    Model to store user's saved/favorite properties (wishlist)
    """
    user = models.ForeignKey(
        User, 
        on_delete=models.CASCADE, 
        related_name='saved_properties'
    )
    property = models.ForeignKey(
        Property, 
        on_delete=models.CASCADE, 
        related_name='saved_by_users'
    )
    created_at = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        ordering = ['-created_at']
        unique_together = ('user', 'property')
        indexes = [
            models.Index(fields=['user', 'property']),
            models.Index(fields=['user']),
        ]
    
    def __str__(self):
        return f'{self.user.email} saved {self.property.title}'
