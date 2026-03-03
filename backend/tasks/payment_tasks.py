"""
Payment tasks for async processing.
Handles expiration of stale payments that remain in 'initiated' status,
automatic host payouts, and admin stats refresh.
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


@shared_task(bind=True, max_retries=2, default_retry_delay=120)
def process_host_payouts(self):
    """
    Automatically credit host wallets for completed bookings that haven't
    been paid out yet. Runs hourly to catch any missed webhook payouts.
    """
    from django.db import transaction as db_transaction
    from apps.bookings.models import Booking
    from apps.payments.models import Wallet, WalletTransaction
    from django.db.models import F
    import uuid

    # Find completed bookings not yet paid out
    paid_booking_ids = WalletTransaction.objects.filter(
        metadata__type='host_payout',
        status='completed',
    ).values_list('booking_id', flat=True)

    unpaid_bookings = Booking.objects.filter(
        status='completed',
    ).exclude(
        id__in=paid_booking_ids,
    ).select_related('rental_property__host')[:100]  # batch of 100

    processed = 0
    errors = 0

    for booking in unpaid_bookings:
        try:
            with db_transaction.atomic():
                host = booking.rental_property.host
                wallet, _ = Wallet.objects.get_or_create(user=host)

                payout_amount = booking.grand_total - booking.commission_fee

                WalletTransaction.objects.create(
                    wallet=wallet,
                    booking=booking,
                    txn_type='credit',
                    status='completed',
                    amount=payout_amount,
                    currency=booking.currency,
                    reference=f'AUTO-PAYOUT-{booking.booking_ref}-{uuid.uuid4().hex[:8]}',
                    metadata={'type': 'host_payout', 'booking_ref': booking.booking_ref, 'auto': True},
                )

                wallet.balance = F('balance') + payout_amount
                wallet.save(update_fields=['balance', 'updated_at'])
                processed += 1
        except Exception as e:
            errors += 1
            logger.error(f"Auto-payout failed for booking {booking.booking_ref}: {e}")

    if processed > 0:
        logger.info(f"Auto-payout: {processed} hosts credited, {errors} errors")
    return {'processed': processed, 'errors': errors}


@shared_task
def refresh_admin_stats():
    """
    Refresh cached AdminStats with current totals.
    Runs hourly so the admin dashboard always shows near-real-time data.
    """
    from apps.admin_dashboard.models import AdminStats
    from apps.bookings.models import Booking
    from apps.users.models import User
    from apps.properties.models import Property
    from django.db.models import Sum

    stats, _ = AdminStats.objects.get_or_create(pk=1)
    stats.total_revenue = Booking.objects.filter(
        status='completed',
    ).aggregate(Sum('grand_total'))['grand_total__sum'] or 0
    stats.total_bookings = Booking.objects.count()
    stats.total_users = User.objects.count()
    stats.active_hosts = User.objects.filter(role='host', is_verified=True).count()
    stats.total_properties = Property.objects.count()
    stats.save()

    logger.info(f"Admin stats refreshed: revenue={stats.total_revenue}, bookings={stats.total_bookings}")
    return {'revenue': float(stats.total_revenue), 'bookings': stats.total_bookings}
