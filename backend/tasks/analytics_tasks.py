"""
Analytics tasks — periodic computation of host/property analytics,
summaries, revenue projections, and performance benchmarks.
"""
from celery import shared_task
from datetime import timedelta, date
import logging

logger = logging.getLogger(__name__)


def _add_months(source_date, months):
    """Add months to a date without requiring python-dateutil."""
    month = source_date.month - 1 + months
    year = source_date.year + month // 12
    month = month % 12 + 1
    return date(year, month, 1)


@shared_task(bind=True, max_retries=2, default_retry_delay=120)
def compute_daily_property_analytics(self):
    """
    Compute yesterday's analytics for every active property.
    Scheduled: daily at 02:00 UTC.
    """
    from apps.properties.models import Property
    from services.host_analytics_advanced import HostAnalyticsService

    yesterday = (date.today() - timedelta(days=1))
    properties = Property.objects.filter(status='active')
    computed = 0
    errors = 0

    for prop in properties.iterator():
        try:
            HostAnalyticsService.calculate_property_analytics(prop, yesterday)
            computed += 1
        except Exception as exc:
            errors += 1
            logger.error(
                "Failed to compute analytics for property %s: %s",
                prop.id, exc,
            )

    logger.info(
        "Daily property analytics: %d computed, %d errors (date=%s)",
        computed, errors, yesterday,
    )
    return {"computed": computed, "errors": errors, "date": str(yesterday)}


@shared_task(bind=True, max_retries=2, default_retry_delay=120)
def generate_host_summaries(self):
    """
    Generate daily, weekly, and monthly HostAnalyticsSummary records
    for every host that owns at least one property.
    Scheduled: daily at 03:00 UTC (after property analytics complete).
    """
    from django.contrib.auth import get_user_model
    from services.host_analytics_advanced import HostAnalyticsService

    User = get_user_model()
    today = date.today()
    yesterday = today - timedelta(days=1)

    # Hosts with at least one property
    hosts = User.objects.filter(
        properties__isnull=False,
    ).distinct()

    periods = [
        ("daily", yesterday, yesterday),
        ("weekly", yesterday - timedelta(days=6), yesterday),
        ("monthly", yesterday.replace(day=1), yesterday),
    ]

    total = 0
    errors = 0

    for host in hosts.iterator():
        for period_name, start, end in periods:
            try:
                HostAnalyticsService.generate_host_summary(
                    host, start, end, period=period_name,
                )
                total += 1
            except Exception as exc:
                errors += 1
                logger.error(
                    "Failed %s summary for host %s: %s",
                    period_name, host.id, exc,
                )

    logger.info(
        "Host summaries: %d generated, %d errors", total, errors,
    )
    return {"generated": total, "errors": errors}


@shared_task(bind=True, max_retries=2, default_retry_delay=120)
def generate_revenue_projections(self):
    """
    Generate revenue projections for the next 3 months for every host.
    Scheduled: weekly (Sunday 04:00 UTC).
    """
    from django.contrib.auth import get_user_model
    from services.host_analytics_advanced import HostAnalyticsService

    User = get_user_model()
    today = date.today()
    hosts = User.objects.filter(properties__isnull=False).distinct()
    total = 0
    errors = 0

    for host in hosts.iterator():
        for months_ahead in range(1, 4):
            target_month = _add_months(today, months_ahead)
            try:
                HostAnalyticsService.project_revenue(host, target_month)
                total += 1
            except Exception as exc:
                errors += 1
                logger.error(
                    "Failed projection for host %s month %s: %s",
                    host.id, target_month, exc,
                )

    logger.info(
        "Revenue projections: %d generated, %d errors", total, errors,
    )
    return {"generated": total, "errors": errors}


@shared_task(bind=True, max_retries=2, default_retry_delay=120)
def compute_performance_benchmarks(self):
    """
    Compute regional/property-type performance benchmarks from aggregate data.
    Scheduled: weekly (Sunday 05:00 UTC).
    """
    from apps.properties.analytics_models import PerformanceBenchmark
    from apps.properties.models import Property
    from apps.bookings.models import Booking
    from django.db.models import Avg, Count, F, ExpressionWrapper, fields
    from django.db.models.functions import TruncDate
    from decimal import Decimal

    today = date.today()
    bench_month = today.replace(day=1)
    thirty_days_ago = today - timedelta(days=30)
    total = 0

    # Get unique (region, property_type) combos from active properties
    combos = (
        Property.objects.filter(status='active')
        .exclude(city__isnull=True)
        .exclude(city='')
        .values_list('city', 'property_type')
        .distinct()
    )

    for region, prop_type in combos:
        try:
            properties = Property.objects.filter(
                city=region, property_type=prop_type, status='active',
            )
            prop_count = properties.count()
            if prop_count < 2:
                continue

            # Bookings for these properties in last 30 days
            bookings = Booking.objects.filter(
                rental_property__in=properties,
                status__in=['confirmed', 'completed'],
                created_at__date__gte=thirty_days_ago,
            )

            agg = bookings.aggregate(
                avg_rate=Avg('grand_total'),
                total_bookings=Count('id'),
            )

            avg_nightly_rate = agg['avg_rate'] or Decimal('0')
            total_bookings = agg['total_bookings'] or 0

            # Average length of stay
            stays = bookings.annotate(
                stay_length=ExpressionWrapper(
                    F('check_out') - F('check_in'),
                    output_field=fields.DurationField(),
                )
            ).aggregate(avg_stay=Avg('stay_length'))
            avg_los_days = Decimal('3.0')
            if stays['avg_stay']:
                avg_los_days = Decimal(str(stays['avg_stay'].days or 3))

            # Simplified occupancy
            avg_occupancy = Decimal(
                min((total_bookings * float(avg_los_days)) / (prop_count * 30) * 100, 100)
            ).quantize(Decimal('0.01'))

            # Revenue per available night
            total_rev = bookings.aggregate(
                total=Avg('grand_total'),
            )
            rev_per_night = Decimal('0')
            if total_rev['total'] and prop_count > 0:
                rev_per_night = (
                    total_rev['total'] * total_bookings / (prop_count * 30)
                ).quantize(Decimal('0.01'))

            # Avg lead time (days between booking creation and check-in)
            avg_lead = 7
            lead_bookings = bookings.annotate(
                created_date=TruncDate('created_at'),
                lead_time=ExpressionWrapper(
                    F('check_in') - TruncDate('created_at'),
                    output_field=fields.DurationField(),
                )
            ).aggregate(avg_lt=Avg('lead_time'))
            if lead_bookings.get('avg_lt') and hasattr(lead_bookings['avg_lt'], 'days'):
                avg_lead = max(lead_bookings['avg_lt'].days, 0)

            # Simplified percentiles (estimate from average)
            occ_75 = min(avg_occupancy * Decimal('1.15'), Decimal('100'))
            occ_90 = min(avg_occupancy * Decimal('1.30'), Decimal('100'))
            rate_75 = avg_nightly_rate * Decimal('1.20')
            rate_90 = avg_nightly_rate * Decimal('1.45')

            PerformanceBenchmark.objects.update_or_create(
                region=region,
                property_type=prop_type,
                month=bench_month,
                defaults={
                    'avg_occupancy_rate': avg_occupancy,
                    'avg_nightly_rate': avg_nightly_rate,
                    'avg_revenue_per_available_night': rev_per_night,
                    'avg_booking_lead_time_days': avg_lead,
                    'avg_length_of_stay': avg_los_days,
                    'occupancy_75th_percentile': occ_75,
                    'occupancy_90th_percentile': occ_90,
                    'rate_75th_percentile': rate_75,
                    'rate_90th_percentile': rate_90,
                    'sample_size': prop_count,
                },
            )
            total += 1

        except Exception as exc:
            logger.error(
                "Benchmark error for %s/%s: %s", region, prop_type, exc,
            )

    logger.info("Performance benchmarks: %d updated", total)
    return {"updated": total}


@shared_task
def compute_message_analytics():
    """
    Compute yesterday's messaging analytics for every host.
    Scheduled: daily at 02:30 UTC.
    """
    from django.contrib.auth import get_user_model
    from apps.messaging.models import Message
    from apps.messaging.automated_models import MessageAnalytics
    from django.db.models import Avg, Count, Q
    from django.utils import timezone as tz

    User = get_user_model()
    yesterday = date.today() - timedelta(days=1)
    start_dt = tz.make_aware(
        tz.datetime.combine(yesterday, tz.datetime.min.time())
    )
    end_dt = start_dt + timedelta(days=1)

    hosts = User.objects.filter(properties__isnull=False).distinct()
    total = 0

    for host in hosts.iterator():
        try:
            sent = Message.objects.filter(
                sender=host,
                created_at__gte=start_dt,
                created_at__lt=end_dt,
            ).count()

            received = Message.objects.filter(
                receiver=host,
                created_at__gte=start_dt,
                created_at__lt=end_dt,
            ).count()

            MessageAnalytics.objects.update_or_create(
                host=host,
                date=yesterday,
                defaults={
                    'messages_sent': sent,
                    'messages_received': received,
                },
            )
            total += 1
        except Exception as exc:
            logger.error("Message analytics error for host %s: %s", host.id, exc)

    logger.info("Message analytics: %d hosts processed", total)
    return {"processed": total}


@shared_task(bind=True, max_retries=1, default_retry_delay=300, rate_limit='1/m')
def refresh_property_pois(self):
    """
    Import POIs from OpenStreetMap for active properties that have no
    associated POIs yet.  Scheduled: weekly (Monday 05:30 UTC).
    """
    from apps.properties.models import Property

    properties = Property.objects.filter(
        status='active',
        location__isnull=False,
    ).exclude(
        nearby_pois__isnull=False,
    ).distinct()[:50]  # batch of 50 to stay within Overpass rate limits

    count = 0
    for prop in properties:
        try:
            from services.poi_service import POIService
            import time
            # associate_pois_with_property auto-imports from OSM if no POIs
            # exist nearby, then creates PropertyPOI junction records.
            associated = POIService.associate_pois_with_property(prop, radius_km=5)
            if associated > 0:
                count += 1
            time.sleep(2)  # respect Overpass rate limits
        except Exception as exc:
            logger.error("POI refresh error for property %s: %s", prop.id, exc)

    logger.info("POI refresh: %d properties updated", count)
    return {"updated": count}
