from django.db import models
from django.conf import settings
from apps.messaging.models import Conversation
from apps.bookings.models import Booking
from apps.properties.models import Property

User = settings.AUTH_USER_MODEL

class SupportTicket(models.Model):
    PRIORITY_CHOICES = [
        ('low', 'Low'), 
        ('medium', 'Medium'),
        ('high', 'High'), 
        ('urgent', 'Urgent'),
    ]
    STATUS_CHOICES = [
        ('open', 'Open'), 
        ('assigned', 'Assigned'),
        ('in_progress', 'In Progress'), 
        ('waiting_customer', 'Waiting on Customer'),
        ('resolved', 'Resolved'), 
        ('closed', 'Closed'),
    ]
    CATEGORY_CHOICES = [
        ('booking', 'Booking Issue'), 
        ('payment', 'Payment Issue'),
        ('property', 'Property Issue'), 
        ('account', 'Account Issue'),
        ('technical', 'Technical/Bug'), 
        ('other', 'Other'),
    ]

    conversation = models.OneToOneField(Conversation, related_name='support_ticket', on_delete=models.CASCADE)
    requester = models.ForeignKey(User, related_name='support_tickets', on_delete=models.CASCADE)
    assigned_agent = models.ForeignKey(User, null=True, blank=True, related_name='assigned_tickets', on_delete=models.SET_NULL)
    
    category = models.CharField(max_length=20, choices=CATEGORY_CHOICES)
    priority = models.CharField(max_length=20, choices=PRIORITY_CHOICES, default='medium')
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='open')
    subject = models.CharField(max_length=255)
    
    related_booking = models.ForeignKey(Booking, null=True, blank=True, on_delete=models.SET_NULL)
    related_property = models.ForeignKey(Property, null=True, blank=True, on_delete=models.SET_NULL)
    
    resolution_notes = models.TextField(blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    resolved_at = models.DateTimeField(null=True, blank=True)
    satisfaction_rating = models.IntegerField(null=True, blank=True)  # 1-5

    class Meta:
        ordering = ['-created_at']

    def __str__(self):
        return f"Ticket #{self.id}: {self.subject} ({self.get_status_display()})"


class SupportTicketEvent(models.Model):
    """Audit log for ticket changes"""
    ticket = models.ForeignKey(SupportTicket, related_name='events', on_delete=models.CASCADE)
    actor = models.ForeignKey(User, on_delete=models.CASCADE)
    event_type = models.CharField(max_length=50)  # e.g., 'status_change', 'assignment', 'note'
    old_value = models.CharField(max_length=255, blank=True)
    new_value = models.CharField(max_length=255, blank=True)
    note = models.TextField(blank=True)
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        ordering = ['created_at']

    def __str__(self):
        return f"{self.event_type} on Ticket #{self.ticket.id} by {self.actor}"


class CannedResponse(models.Model):
    """Pre-written responses for common support scenarios"""
    title = models.CharField(max_length=100)
    body = models.TextField()
    category = models.CharField(max_length=20, choices=SupportTicket.CATEGORY_CHOICES)
    is_active = models.BooleanField(default=True)

    def __str__(self):
        return self.title


class BugReport(models.Model):
    STATUS_CHOICES = [
        ('new', 'New'), 
        ('triaged', 'Triaged'),
        ('in_progress', 'In Progress'), 
        ('fixed', 'Fixed'),
        ('wont_fix', "Won't Fix"), 
        ('duplicate', 'Duplicate'),
    ]
    SEVERITY_CHOICES = [
        ('critical', 'Critical'), 
        ('major', 'Major'),
        ('minor', 'Minor'), 
        ('cosmetic', 'Cosmetic'),
    ]

    reporter = models.ForeignKey(User, related_name='bug_reports', on_delete=models.CASCADE)
    title = models.CharField(max_length=255)
    description = models.TextField()
    steps_to_reproduce = models.TextField(blank=True)
    expected_behavior = models.TextField(blank=True)
    actual_behavior = models.TextField(blank=True)
    
    severity = models.CharField(max_length=20, choices=SEVERITY_CHOICES, default='minor')
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='new')

    # Auto-captured context
    browser_info = models.JSONField(default=dict, blank=True)
    page_url = models.URLField(blank=True)
    console_logs = models.TextField(blank=True)
    network_errors = models.TextField(blank=True)
    screenshot = models.ImageField(upload_to='support/bug_screenshots/%Y/%m/', null=True, blank=True)

    # Linkage to full SupportTicket
    support_ticket = models.ForeignKey(SupportTicket, null=True, blank=True, related_name='bug_reports', on_delete=models.SET_NULL)
    assigned_to = models.ForeignKey(User, null=True, blank=True, related_name='assigned_bugs', on_delete=models.SET_NULL)

    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        ordering = ['-created_at']

    def __str__(self):
        return f"Bug Report #{self.id}: {self.title}"
