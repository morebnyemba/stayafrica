import os
from celery import Celery

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'stayafrica.settings')

app = Celery('stayafrica')
app.config_from_object('django.conf:settings', namespace='CELERY')
app.autodiscover_tasks()
# Explicitly discover each task module in the top-level tasks/ package
# autodiscover_tasks(['tasks']) only looks for tasks/tasks.py by default,
# so we must specify related_name for each actual module.
for _module in ['email_tasks', 'image_tasks', 'payment_tasks', 'geocoding_tasks', 'notification_tasks']:
    app.autodiscover_tasks(['tasks'], related_name=_module)

@app.task(bind=True)
def debug_task(self):
    print(f'Request: {self.request!r}')
