from django.db import models
from django.contrib.auth import get_user_model
from apps.bookings.models import Booking
import uuid

User = get_user_model()

class Payment(models.Model):
    PROVIDER_CHOICES = [
        ('paynow', 'Paynow'),
        ('payfast', 'PayFast'),
        ('paypal', 'PayPal'),
        ('ozow', 'Ozow'),
        ('stripe', 'Stripe'),
        ('flutterwave', 'Flutterwave'),
        ('paystack', 'Paystack'),
        ('cash_on_arrival', 'Cash on Arrival'),
    ]
    
    STATUS_CHOICES = [
        ('initiated', 'Initiated'),
        ('success', 'Success'),
        ('failed', 'Failed'),
        ('pending', 'Pending'),
    ]
    
    booking = models.OneToOneField(Booking, on_delete=models.CASCADE, related_name='payment')
    provider = models.CharField(max_length=20, choices=PROVIDER_CHOICES)
    gateway_ref = models.CharField(max_length=255, unique=True, db_index=True)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='initiated')
    
    amount = models.DecimalField(max_digits=10, decimal_places=2)
    currency = models.CharField(max_length=3, default='USD')
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['booking']),
            models.Index(fields=['provider']),
            models.Index(fields=['status']),
        ]
    
    def __str__(self):
        return f'{self.booking.booking_ref} - {self.provider} ({self.status})'


class Wallet(models.Model):
    STATUS_CHOICES = [
        ('active', 'Active'),
        ('suspended', 'Suspended'),
        ('closed', 'Closed'),
    ]

    user = models.OneToOneField(User, on_delete=models.CASCADE, related_name='wallet')
    balance = models.DecimalField(max_digits=12, decimal_places=2, default=0)
    currency = models.CharField(max_length=3, default='USD')
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='active')
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['user']),
            models.Index(fields=['status']),
        ]

    def __str__(self):
        return f'{self.user.email} wallet ({self.currency})'


class BankAccount(models.Model):
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name='bank_accounts')
    bank_name = models.CharField(max_length=255)
    account_name = models.CharField(max_length=255)
    account_number = models.CharField(max_length=64)
    branch_code = models.CharField(max_length=64, blank=True)
    country = models.CharField(max_length=100, blank=True)
    is_primary = models.BooleanField(default=False)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['user']),
            models.Index(fields=['is_primary']),
        ]

    def __str__(self):
        return f'{self.bank_name} - {self.account_number}'


class WalletTransaction(models.Model):
    TYPE_CHOICES = [
        ('credit', 'Credit'),
        ('debit', 'Debit'),
        ('refund', 'Refund'),
        ('adjustment', 'Adjustment'),
    ]

    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('completed', 'Completed'),
        ('failed', 'Failed'),
        ('reversed', 'Reversed'),
    ]

    wallet = models.ForeignKey(Wallet, on_delete=models.CASCADE, related_name='transactions')
    booking = models.ForeignKey(Booking, on_delete=models.SET_NULL, null=True, blank=True, related_name='wallet_transactions')
    txn_type = models.CharField(max_length=20, choices=TYPE_CHOICES)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    amount = models.DecimalField(max_digits=12, decimal_places=2)
    currency = models.CharField(max_length=3, default='USD')
    reference = models.CharField(max_length=64, unique=True, db_index=True)
    metadata = models.JSONField(default=dict, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['wallet']),
            models.Index(fields=['status']),
            models.Index(fields=['txn_type']),
        ]

    def save(self, *args, **kwargs):
        if not self.reference:
            self.reference = f'TXN-{uuid.uuid4().hex[:12].upper()}'
        super().save(*args, **kwargs)

    def __str__(self):
        return f'{self.reference} ({self.txn_type})'


class Withdrawal(models.Model):
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('processing', 'Processing'),
        ('completed', 'Completed'),
        ('failed', 'Failed'),
    ]

    wallet = models.ForeignKey(Wallet, on_delete=models.CASCADE, related_name='withdrawals')
    bank_account = models.ForeignKey(BankAccount, on_delete=models.PROTECT, related_name='withdrawals')
    amount = models.DecimalField(max_digits=12, decimal_places=2)
    currency = models.CharField(max_length=3, default='USD')
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    reference = models.CharField(max_length=64, unique=True, db_index=True)
    processed_at = models.DateTimeField(null=True, blank=True)
    notes = models.TextField(blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['wallet']),
            models.Index(fields=['status']),
        ]

    def save(self, *args, **kwargs):
        if not self.reference:
            self.reference = f'WD-{uuid.uuid4().hex[:12].upper()}'
        super().save(*args, **kwargs)

    def __str__(self):
        return f'{self.reference} - {self.amount} {self.currency}'


class PaymentMethod(models.Model):
    """
    Stored payment methods for faster checkout
    Supports tokenization from Stripe, Paynow, Flutterwave, Paystack
    """
    PROVIDER_CHOICES = [
        ('stripe', 'Stripe'),
        ('paynow', 'Paynow'),
        ('flutterwave', 'Flutterwave'),
        ('paystack', 'Paystack'),
    ]
    
    METHOD_TYPE_CHOICES = [
        ('card', 'Credit/Debit Card'),
        ('mobile', 'Mobile Money'),
        ('bank', 'Bank Transfer'),
        ('ussd', 'USSD'),
    ]
    
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name='payment_methods')
    provider = models.CharField(max_length=20, choices=PROVIDER_CHOICES)
    method_type = models.CharField(max_length=20, choices=METHOD_TYPE_CHOICES)
    
    # Display name (e.g., "My Visa Card", "Ecocash Account")
    name = models.CharField(max_length=100)
    
    # Tokenized data (NEVER store raw card data)
    provider_token = models.CharField(max_length=500, help_text='Token from payment provider')
    
    # Display info (last 4 digits, expiry for cards)
    last_four = models.CharField(max_length=4, blank=True)
    expiry_month = models.IntegerField(null=True, blank=True)
    expiry_year = models.IntegerField(null=True, blank=True)
    
    # Phone number for mobile money
    phone_number = models.CharField(max_length=20, blank=True)
    
    # Flags
    is_default = models.BooleanField(default=False)
    is_verified = models.BooleanField(default=False)
    
    # Soft delete
    deleted_at = models.DateTimeField(null=True, blank=True)
    
    # Timestamps
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-is_default', '-created_at']
        indexes = [
            models.Index(fields=['user', 'deleted_at']),
            models.Index(fields=['provider']),
            models.Index(fields=['is_default']),
        ]
        constraints = [
            models.UniqueConstraint(
                fields=['user', 'provider', 'provider_token'],
                condition=models.Q(deleted_at__isnull=True),
                name='unique_active_payment_method'
            )
        ]
    
    def save(self, *args, **kwargs):
        # If setting as default, unset other defaults for this user
        if self.is_default:
            PaymentMethod.objects.filter(
                user=self.user,
                deleted_at__isnull=True
            ).exclude(id=self.id).update(is_default=False)
        super().save(*args, **kwargs)
    
    def __str__(self):
        return f'{self.user.email} - {self.name} ({self.get_provider_display()})'
    
    def soft_delete(self):
        """Soft delete the payment method"""
        from django.utils import timezone
        self.deleted_at = timezone.now()
        self.save()
