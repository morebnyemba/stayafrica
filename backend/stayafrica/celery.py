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
    On every beat startup, force-sync the ENTIRE CELERY_BEAT_SCHEDULE from
    settings into django_celery_beat DB tables — task names, schedules,
    queues, and args. Prevents stale/corrupted DB entries from overriding
    the canonical schedule in settings.py.
    """
    import django
    django.setup()

    from django.conf import settings
    from django_celery_beat.models import (
        PeriodicTask, IntervalSchedule, CrontabSchedule,
    )
    from celery.schedules import crontab as celery_crontab, schedule as celery_schedule
    import json

    beat_schedule = getattr(settings, 'CELERY_BEAT_SCHEDULE', {})
    if not beat_schedule:
        return

    for name, entry in beat_schedule.items():
        task_name = entry.get('task', '')
        sched = entry.get('schedule')
        if not task_name or not sched:
            continue
        try:
            # Build the correct schedule object
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
                # timedelta / schedule interval
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

            pt = PeriodicTask.objects.filter(name=name).first()
            if pt:
                # Force-update all fields to match settings.py
                pt.task = task_name
                pt.crontab = crontab_obj
                pt.interval = interval_obj if not crontab_obj else None
                pt.kwargs = task_kwargs
                pt.args = task_args
                pt.queue = queue
                pt.enabled = True
                pt.save()
            else:
                # Create missing entry
                PeriodicTask.objects.create(
                    name=name,
                    task=task_name,
                    crontab=crontab_obj,
                    interval=interval_obj if not crontab_obj else None,
                    kwargs=task_kwargs,
                    args=task_args,
                    queue=queue,
                    enabled=True,
                )
        except Exception:
            pass  # DB not ready yet or table missing — skip silently


@app.task(bind=True)
def debug_task(self):
    print(f'Request: {self.request!r}')
