from django.core.cache import cache
from django.db.models.signals import post_save, post_delete
from django.dispatch import receiver
from apps.properties.models import Property

CACHE_KEY_PREFIX = 'property_list'


@receiver([post_save, post_delete], sender=Property)
def clear_property_list_cache(**kwargs):
    cache.delete_pattern(f"{CACHE_KEY_PREFIX}*")
