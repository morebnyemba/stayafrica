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
    On every beat startup, force-sync CELERY_BEAT_SCHEDULE from settings
    into the django_celery_beat DB tables. This prevents manual DB edits
    from corrupting task-to-function mappings.
    """
    import django
    django.setup()

    from django.conf import settings
    from django_celery_beat.models import PeriodicTask, IntervalSchedule, CrontabSchedule
    import json

    beat_schedule = getattr(settings, 'CELERY_BEAT_SCHEDULE', {})
    if not beat_schedule:
        return

    for name, entry in beat_schedule.items():
        task_name = entry.get('task', '')
        if not task_name:
            continue
        try:
            pt = PeriodicTask.objects.filter(name=name).first()
            if pt and pt.task != task_name:
                pt.task = task_name
                pt.kwargs = json.dumps(entry.get('kwargs', {}))
                pt.args = json.dumps(entry.get('args', []))
                pt.queue = entry.get('options', {}).get('queue', '')
                pt.save()
        except Exception:
            pass  # DB not ready yet or table missing — skip silently


@app.task(bind=True)
def debug_task(self):
    print(f'Request: {self.request!r}')
