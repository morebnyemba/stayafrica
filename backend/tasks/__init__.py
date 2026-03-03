"""
Celery tasks for StayAfrica backend.

All task submodules are explicitly imported here so that Celery registers
every @shared_task regardless of autodiscover or CELERY_IMPORTS settings.
"""
from tasks import email_tasks      # noqa: F401
from tasks import payment_tasks    # noqa: F401
from tasks import notification_tasks  # noqa: F401
from tasks import image_tasks      # noqa: F401
from tasks import geocoding_tasks  # noqa: F401
from tasks import analytics_tasks  # noqa: F401
