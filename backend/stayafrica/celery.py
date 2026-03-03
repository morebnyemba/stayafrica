import os
from celery import Celery
from celery.signals import beat_init

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'stayafrica.settings')

app = Celery('stayafrica')
app.config_from_object('django.conf:settings', namespace='CELERY')

# Ensure ALL task modules are imported and registered.
# Belt-and-suspenders: include= here + imports in tasks/__init__.py
# + CELERY_IMPORTS in settings.py. All three mechanisms ensure tasks register.
app.conf.include = [
    'tasks.email_tasks',
    'tasks.payment_tasks',
    'tasks.notification_tasks',
    'tasks.image_tasks',
    'tasks.geocoding_tasks',
    'tasks.analytics_tasks',
]
app.autodiscover_tasks()


@beat_init.connect
def force_sync_beat_schedule(sender, **kwargs):
    """
    On beat startup, sync CELERY_BEAT_SCHEDULE into django_celery_beat DB tables.
    Resets last_run_at so tasks fire immediately after deploy/restart.
    """
    import django
    django.setup()

    from django.conf import settings as django_settings
    beat_schedule = getattr(django_settings, 'CELERY_BEAT_SCHEDULE', {})
    if not beat_schedule:
        return

    try:
        from django_celery_beat.models import (
            PeriodicTask, PeriodicTasks, IntervalSchedule, CrontabSchedule,
        )
    except Exception:
        return

    from celery.schedules import crontab as celery_crontab
    import json, logging
    logger = logging.getLogger('celery.beat.sync')

    synced = 0
    for name, entry in beat_schedule.items():
        task_name = entry.get('task', '')
        sched = entry.get('schedule')
        if not task_name or not sched:
            continue
        try:
            crontab_obj = None
            interval_obj = None

            if isinstance(sched, celery_crontab):
                crontab_obj, _ = CrontabSchedule.objects.get_or_create(
                    minute=sched._orig_minute,
                    hour=sched._orig_hour,
                    day_of_week=sched._orig_day_of_week,
                    day_of_month=sched._orig_day_of_month,
                    month_of_year=sched._orig_month_of_year,
                )
            else:
                total_seconds = sched.total_seconds() if hasattr(sched, 'total_seconds') else float(sched)
                if total_seconds >= 60:
                    every = int(total_seconds // 60)
                    period = IntervalSchedule.MINUTES
                else:
                    every = int(total_seconds)
                    period = IntervalSchedule.SECONDS
                interval_obj, _ = IntervalSchedule.objects.get_or_create(
                    every=every, period=period,
                )

            queue = entry.get('options', {}).get('queue', '')
            task_kwargs = json.dumps(entry.get('kwargs', {}))
            task_args = json.dumps(entry.get('args', []))

            defaults = {
                'task': task_name,
                'crontab': crontab_obj,
                'interval': interval_obj if not crontab_obj else None,
                'kwargs': task_kwargs,
                'args': task_args,
                'queue': queue,
                'enabled': True,
                'last_run_at': None,  # reset so task fires immediately
            }

            _, created = PeriodicTask.objects.update_or_create(
                name=name, defaults=defaults,
            )
            synced += 1
        except Exception as exc:
            logger.warning(f'Failed to sync beat entry "{name}": {exc}')

    # Signal the scheduler to reload from DB
    try:
        PeriodicTasks.changed()
    except Exception:
        pass

    if synced:
        logger.info(f'Synced {synced}/{len(beat_schedule)} beat schedule entries to DB')


@app.task(bind=True)
def debug_task(self):
    print(f'Request: {self.request!r}')
