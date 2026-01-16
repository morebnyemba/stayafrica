"""
Identity Verification Models
Handles document upload, selfie capture, and verification workflow
"""
from django.db import models
from django.utils import timezone
from apps.users.models import User
import uuid


class IdentityVerification(models.Model):
    """
    Identity verification requests with document and selfie
    """
    VERIFICATION_STATUS = [
        ('pending', 'Pending Review'),
        ('under_review', 'Under Review'),
        ('approved', 'Approved'),
        ('rejected', 'Rejected'),
        ('expired', 'Expired'),
    ]
    
    DOCUMENT_TYPES = [
        ('passport', 'Passport'),
        ('national_id', 'National ID Card'),
        ('drivers_license', 'Driver\'s License'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name='identity_verifications')
    
    # Document information
    document_type = models.CharField(max_length=30, choices=DOCUMENT_TYPES)
    document_number = models.CharField(max_length=100, help_text='ID/Passport number')
    document_country = models.CharField(max_length=100, help_text='Issuing country')
    document_expiry_date = models.DateField(null=True, blank=True, help_text='Document expiry date')
    
    # Uploaded files
    document_front_image = models.ImageField(
        upload_to='identity_verification/%Y/%m/%d/',
        help_text='Front side of document'
    )
    document_back_image = models.ImageField(
        upload_to='identity_verification/%Y/%m/%d/',
        null=True,
        blank=True,
        help_text='Back side of document (if applicable)'
    )
    selfie_image = models.ImageField(
        upload_to='identity_verification/%Y/%m/%d/',
        help_text='Selfie for identity verification'
    )
    
    # Verification status
    status = models.CharField(max_length=20, choices=VERIFICATION_STATUS, default='pending')
    submitted_at = models.DateTimeField(auto_now_add=True)
    reviewed_at = models.DateTimeField(null=True, blank=True)
    reviewed_by = models.ForeignKey(
        User,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='verifications_reviewed',
        help_text='Admin who reviewed this verification'
    )
    
    # Review details
    rejection_reason = models.TextField(
        blank=True,
        help_text='Reason for rejection (if applicable)'
    )
    admin_notes = models.TextField(
        blank=True,
        help_text='Internal admin notes'
    )
    
    # Verification method
    verification_method = models.CharField(
        max_length=30,
        choices=[
            ('manual', 'Manual Review'),
            ('automated', 'Automated (AI)'),
            ('third_party', 'Third Party Service'),
        ],
        default='manual'
    )
    
    # Expiry
    expires_at = models.DateTimeField(
        null=True,
        blank=True,
        help_text='Verification expiry date (typically 2 years)'
    )
    
    # Metadata
    metadata = models.JSONField(
        default=dict,
        blank=True,
        help_text='Additional verification data'
    )
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-submitted_at']
        indexes = [
            models.Index(fields=['user', 'status']),
            models.Index(fields=['status', 'submitted_at']),
            models.Index(fields=['document_type']),
        ]
    
    def __str__(self):
        return f"{self.user.email} - {self.document_type} ({self.get_status_display()})"
    
    def approve(self, admin_user, notes=''):
        """Approve verification"""
        self.status = 'approved'
        self.reviewed_at = timezone.now()
        self.reviewed_by = admin_user
        self.admin_notes = notes
        # Set expiry to 2 years from now
        self.expires_at = timezone.now() + timezone.timedelta(days=730)
        self.save()
        
        # Update user's verified status
        self.user.is_verified = True
        self.user.save(update_fields=['is_verified'])
    
    def reject(self, admin_user, reason, notes=''):
        """Reject verification"""
        self.status = 'rejected'
        self.reviewed_at = timezone.now()
        self.reviewed_by = admin_user
        self.rejection_reason = reason
        self.admin_notes = notes
        self.save()
    
    def is_expired(self):
        """Check if verification has expired"""
        if self.status == 'approved' and self.expires_at:
            return timezone.now() > self.expires_at
        return False
    
    def check_and_expire(self):
        """Check and update expiry status"""
        if self.is_expired():
            self.status = 'expired'
            self.save(update_fields=['status'])
            # Update user's verified status
            self.user.is_verified = False
            self.user.save(update_fields=['is_verified'])
            return True
        return False


class VerificationAttempt(models.Model):
    """
    Track verification attempts for rate limiting and fraud detection
    """
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name='verification_attempts')
    verification = models.ForeignKey(
        IdentityVerification,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='attempts'
    )
    
    ip_address = models.GenericIPAddressField(null=True, blank=True)
    user_agent = models.TextField(blank=True)
    
    success = models.BooleanField(default=False)
    error_message = models.TextField(blank=True)
    
    attempted_at = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        ordering = ['-attempted_at']
        indexes = [
            models.Index(fields=['user', '-attempted_at']),
            models.Index(fields=['ip_address']),
        ]
    
    def __str__(self):
        status = 'Success' if self.success else 'Failed'
        return f"{self.user.email} - {status} ({self.attempted_at})"


class VerificationSettings(models.Model):
    """
    Global verification settings and configuration
    """
    # Rate limiting
    max_attempts_per_day = models.IntegerField(
        default=3,
        help_text='Maximum verification attempts per user per day'
    )
    max_attempts_per_month = models.IntegerField(
        default=10,
        help_text='Maximum verification attempts per user per month'
    )
    
    # Document requirements
    require_document_back = models.BooleanField(
        default=False,
        help_text='Require back side of document'
    )
    require_selfie = models.BooleanField(
        default=True,
        help_text='Require selfie for verification'
    )
    
    # Image requirements
    min_image_width = models.IntegerField(default=800, help_text='Minimum image width in pixels')
    min_image_height = models.IntegerField(default=600, help_text='Minimum image height in pixels')
    max_image_size_mb = models.IntegerField(default=10, help_text='Maximum image size in MB')
    
    # Verification expiry
    verification_valid_years = models.IntegerField(
        default=2,
        help_text='Years before verification expires'
    )
    
    # Host requirements
    require_verification_for_hosting = models.BooleanField(
        default=True,
        help_text='Hosts must be verified before listing properties'
    )
    
    # Booking requirements
    require_verification_for_booking = models.BooleanField(
        default=False,
        help_text='Guests must be verified before booking'
    )
    
    # Third-party integration
    use_third_party_service = models.BooleanField(
        default=False,
        help_text='Use third-party verification service (e.g., Smile Identity, Onfido)'
    )
    third_party_api_key = models.CharField(
        max_length=255,
        blank=True,
        help_text='API key for third-party service'
    )
    
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = 'Verification Settings'
        verbose_name_plural = 'Verification Settings'
    
    def __str__(self):
        return 'Verification Settings'
    
    @classmethod
    def get_settings(cls):
        """Get or create settings singleton"""
        settings, created = cls.objects.get_or_create(id=1)
        return settings
