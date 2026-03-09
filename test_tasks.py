import os
import sys
import django
from datetime import timedelta
from django.utils import timezone

# Add backend to path so imports work
sys.path.append(os.path.join(os.getcwd(), 'backend'))

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'stayafrica.settings')
django.setup()

from tasks.booking_expiration_tasks import expire_pending_bookings, send_pending_booking_reminders

print("Import successful")

try:
    # Run the tasks synchronously (direct call, not .delay())
    print("Running expire_pending_bookings...")
    result_expire = expire_pending_bookings()
    print(f"Expire task result: {result_expire}")
    
    print("Running send_pending_booking_reminders...")
    result_remind = send_pending_booking_reminders()
    print(f"Reminder task result: {result_remind}")
    
except Exception as e:
    print(f"Error running tasks: {e}")
    import traceback
    traceback.print_exc()
