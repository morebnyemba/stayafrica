"""
Payment tasks for async processing.
Handles expiration of stale payments that remain in 'initiated' status.
"""
from celery import shared_task
from django.utils import timezone
from datetime import timedelta
import logging

logger = logging.getLogger(__name__)


@shared_task(name='tasks.payment_tasks.expire_stale_payments')
def expire_stale_payments():
    """
    Mark payments stuck in 'initiated' status for over 10 minutes as 'failed'.

    This prevents payments from being stuck forever when webhooks fail
    or the user abandons the payment flow.
    """
    from apps.payments.models import Payment

    cutoff = timezone.now() - timedelta(minutes=10)
    stale_payments = Payment.objects.filter(
        status='initiated',
        created_at__lt=cutoff,
    )

    count = stale_payments.count()
    if count > 0:
        refs = list(stale_payments.values_list('gateway_ref', flat=True)[:20])
        logger.info(
            f"Expiring {count} stale initiated payment(s): {refs}"
        )
        stale_payments.update(status='failed')
    else:
        logger.debug("No stale initiated payments found")

    return f"Expired {count} stale payment(s)"
