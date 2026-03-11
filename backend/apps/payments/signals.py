from django.db.models.signals import post_save
from django.dispatch import receiver
from django.contrib.auth import get_user_model
from .models import Wallet

User = get_user_model()

@receiver(post_save, sender=User)
def create_wallet_for_new_user(sender, instance, created, **kwargs):
    """
    Automatically create a wallet for a newly created user.
    """
    if created:
        Wallet.objects.get_or_create(user=instance)
