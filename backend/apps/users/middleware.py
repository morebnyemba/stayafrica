from django.utils import timezone
from django.utils.deprecation import MiddlewareMixin
from apps.users.models import User


class UpdateLastSeenMiddleware(MiddlewareMixin):
    """
    Update user's last_seen timestamp and online status on each request
    """
    def process_request(self, request):
        if request.user.is_authenticated:
            # Update last_seen and set online status
            User.objects.filter(pk=request.user.pk).update(
                last_seen=timezone.now(),
                is_online=True
            )
        return None
