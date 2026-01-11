from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.payments.models import Payment, Wallet, WalletTransaction, Withdrawal, BankAccount


@admin.register(Payment)
class PaymentAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Payment management"""
    
    list_display = ['gateway_ref_display', 'booking_display', 'provider', 'status_badge', 
                    'amount_display', 'created_at']
    list_filter = ['provider', 'status', 'currency', 'created_at']
    search_fields = ['gateway_ref', 'booking__booking_ref', 'booking__guest__email']
    readonly_fields = ['gateway_ref', 'amount', 'currency', 'created_at', 'updated_at', 'payment_details']
    date_hierarchy = 'created_at'
    list_select_related = ['booking', 'booking__guest']
    list_per_page = 25
    actions = ['mark_success', 'mark_failed', 'mark_refunded']
    
    fieldsets = (
        (_('Payment Information'), {
            'fields': ('gateway_ref', 'payment_details', 'status'),
        }),
        (_('Transaction Details'), {
            'fields': ('booking', 'provider', 'amount', 'currency'),
        }),
        (_('Metadata'), {
            'fields': ('metadata',),
            'classes': ['collapse'],
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )

    @admin.action(description=_('Mark selected payments as success'))
    def mark_success(self, request, queryset):
        updated = queryset.update(status='success')
        self.message_user(request, f'{updated} payment(s) marked as successful.')

    @admin.action(description=_('Mark selected payments as failed'))
    def mark_failed(self, request, queryset):
        updated = queryset.update(status='failed')
        self.message_user(request, f'{updated} payment(s) marked as failed.')

    @admin.action(description=_('Mark selected payments as refunded'))
    def mark_refunded(self, request, queryset):
        updated = queryset.update(status='refunded')
        self.message_user(request, f'{updated} payment(s) refunded.')

    @display(description=_('Status'), ordering='status', label=True)
    def status_badge(self, obj):
        colors = {
            'initiated': 'warning',
            'pending': 'info',
            'success': 'success',
            'failed': 'danger',
            'refunded': 'secondary',
        }
        return {
            'value': obj.get_status_display(),
            'color': colors.get(obj.status, 'secondary'),
        }

    @display(description=_('Reference'))
    def gateway_ref_display(self, obj):
        return obj.gateway_ref[:20] + '...' if len(obj.gateway_ref) > 20 else obj.gateway_ref

    @display(description=_('Booking'))
    def booking_display(self, obj):
        return f"{obj.booking.booking_ref}"

    @display(description=_('Amount'), ordering='amount')
    def amount_display(self, obj):
        return f"{obj.currency} {obj.amount:.2f}"

    @display(description=_('Payment Details'))
    def payment_details(self, obj):
        if obj.id:
            details = f"""
            <div style="padding: 10px; background: #F4F1EA; border-left: 3px solid #3A5C50; border-radius: 4px;">
                <strong style="color: #122F26;">Payment Details</strong><br/>
                <span style="color: #3A5C50;">
                    Provider: {obj.provider.upper()}<br/>
                    Amount: {obj.currency} {obj.amount:.2f}<br/>
                    Booking: {obj.booking.booking_ref}<br/>
                    Status: {obj.get_status_display()}
                </span>
            </div>
            """
            return format_html(details)
        return "Save to see details"

    def has_add_permission(self, request):
        return False


@admin.register(Wallet)
class WalletAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Wallet management"""
    
    list_display = ['user_display', 'balance_display', 'status_badge', 'updated_at']
    list_filter = ['status', 'currency', 'created_at']
    search_fields = ['user__email', 'user__username']
    readonly_fields = ['balance', 'created_at', 'updated_at', 'wallet_summary']
    list_select_related = ['user']
    list_per_page = 25
    actions = ['activate_wallets', 'suspend_wallets', 'close_wallets']
    
    fieldsets = (
        (_('Wallet Information'), {
            'fields': ('user', 'wallet_summary', 'status'),
        }),
        (_('Balance'), {
            'fields': ('balance', 'currency'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )

    @display(description=_('Status'), ordering='status', label=True)
    def status_badge(self, obj):
        colors = {
            'active': 'success',
            'suspended': 'warning',
            'closed': 'danger',
        }
        return {
            'value': obj.get_status_display(),
            'color': colors.get(obj.status, 'secondary'),
        }

    @display(description=_('User'))
    def user_display(self, obj):
        return obj.user.get_full_name() or obj.user.email

    @display(description=_('Balance'), ordering='balance')
    def balance_display(self, obj):
        return f"{obj.currency} {obj.balance:.2f}"

    @display(description=_('Summary'))
    def wallet_summary(self, obj):
        if obj.id:
            summary = f"""
            <div style="padding: 10px; background: #F4F1EA; border-left: 3px solid #D9B168; border-radius: 4px;">
                <strong style="color: #122F26;">Wallet Summary</strong><br/>
                <span style="color: #3A5C50;">
                    Owner: {obj.user.get_full_name() or obj.user.email}<br/>
                    Balance: {obj.currency} {obj.balance:.2f}<br/>
                    Status: {obj.get_status_display()}
                </span>
            </div>
            """
            return format_html(summary)
        return "Save to see summary"

    @admin.action(description=_('Mark selected wallets as active'))
    def activate_wallets(self, request, queryset):
        updated = queryset.update(status='active')
        self.message_user(request, f'{updated} wallet(s) activated.')

    @admin.action(description=_('Suspend selected wallets'))
    def suspend_wallets(self, request, queryset):
        updated = queryset.update(status='suspended')
        self.message_user(request, f'{updated} wallet(s) suspended.')

    @admin.action(description=_('Close selected wallets'))
    def close_wallets(self, request, queryset):
        updated = queryset.update(status='closed')
        self.message_user(request, f'{updated} wallet(s) closed.')


@admin.register(BankAccount)
class BankAccountAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Bank Account management"""
    
    list_display = ['user_display', 'bank_name', 'masked_account_number', 'country', 
                    'primary_badge', 'created_at']
    list_filter = ['is_primary', 'country', 'created_at']
    search_fields = ['user__email', 'bank_name', 'account_name', 'account_number']
    readonly_fields = ['created_at', 'updated_at']
    list_select_related = ['user']
    actions = ['make_primary']
    
    fieldsets = (
        (_('Account Owner'), {
            'fields': ('user',),
        }),
        (_('Bank Details'), {
            'fields': ('bank_name', 'account_name', 'account_number', 'branch_code'),
        }),
        (_('Location'), {
            'fields': ('country',),
        }),
        (_('Settings'), {
            'fields': ('is_primary',),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )

    @display(description=_('User'))
    def user_display(self, obj):
        return obj.user.get_full_name() or obj.user.email

    @display(description=_('Account Number'))
    def masked_account_number(self, obj):
        """Mask account number for security"""
        if len(obj.account_number) > 4:
            return f"****{obj.account_number[-4:]}"
        return "****"

    @display(description=_('Primary'), label=True)
    def primary_badge(self, obj):
        if obj.is_primary:
            return {'value': 'Primary', 'color': 'success'}
        return {'value': 'Secondary', 'color': 'secondary'}

    @admin.action(description=_('Mark selected accounts as primary'))
    def make_primary(self, request, queryset):
        # First unset all primary flags for affected users
        for account in queryset:
            BankAccount.objects.filter(user=account.user).update(is_primary=False)
        # Then set selected as primary
        updated = queryset.update(is_primary=True)
        self.message_user(request, f'{updated} account(s) set as primary.')


@admin.register(WalletTransaction)
class WalletTransactionAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Wallet Transaction management"""
    
    list_display = ['reference', 'wallet_display', 'txn_type', 'status_badge', 
                    'amount_display', 'created_at']
    list_filter = ['txn_type', 'status', 'currency', 'created_at']
    search_fields = ['reference', 'wallet__user__email', 'booking__booking_ref']
    readonly_fields = ['reference', 'amount', 'currency', 'created_at', 'updated_at']
    list_select_related = ['wallet', 'wallet__user', 'booking']
    date_hierarchy = 'created_at'
    list_per_page = 25
    
    fieldsets = (
        (_('Transaction Information'), {
            'fields': ('reference', 'wallet', 'txn_type', 'status'),
        }),
        (_('Amount'), {
            'fields': ('amount', 'currency'),
        }),
        (_('Related'), {
            'fields': ('booking',),
        }),
        (_('Description'), {
            'fields': ('description',),
            'classes': ['collapse'],
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )

    @display(description=_('Status'), ordering='status', label=True)
    def status_badge(self, obj):
        colors = {
            'pending': 'warning',
            'completed': 'success',
            'failed': 'danger',
            'reversed': 'info',
        }
        return {
            'value': obj.get_status_display(),
            'color': colors.get(obj.status, 'secondary'),
        }

    @display(description=_('Wallet'))
    def wallet_display(self, obj):
        return obj.wallet.user.email

    @display(description=_('Amount'), ordering='amount')
    def amount_display(self, obj):
        prefix = '+' if obj.txn_type == 'credit' else '-'
        return f"{prefix} {obj.currency} {obj.amount:.2f}"


@admin.register(Withdrawal)
class WithdrawalAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Withdrawal management"""
    
    list_display = ['reference', 'wallet_display', 'amount_display', 'status_badge', 
                    'processed_at', 'created_at']
    list_filter = ['status', 'currency', 'created_at', 'processed_at']
    search_fields = ['reference', 'wallet__user__email', 'bank_account__account_number']
    readonly_fields = ['reference', 'amount', 'currency', 'created_at', 'updated_at', 'withdrawal_summary']
    list_select_related = ['wallet', 'wallet__user', 'bank_account']
    date_hierarchy = 'created_at'
    actions = ['mark_processing', 'mark_completed', 'mark_failed']
    
    fieldsets = (
        (_('Withdrawal Information'), {
            'fields': ('reference', 'withdrawal_summary', 'status'),
        }),
        (_('Details'), {
            'fields': ('wallet', 'bank_account', 'amount', 'currency'),
        }),
        (_('Processing'), {
            'fields': ('processed_at', 'admin_notes'),
            'classes': ['collapse'],
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )

    @display(description=_('Status'), ordering='status', label=True)
    def status_badge(self, obj):
        colors = {
            'pending': 'warning',
            'processing': 'info',
            'completed': 'success',
            'failed': 'danger',
        }
        return {
            'value': obj.get_status_display(),
            'color': colors.get(obj.status, 'secondary'),
        }

    @display(description=_('Wallet'))
    def wallet_display(self, obj):
        return obj.wallet.user.email

    @display(description=_('Amount'), ordering='amount')
    def amount_display(self, obj):
        return f"{obj.currency} {obj.amount:.2f}"

    @display(description=_('Summary'))
    def withdrawal_summary(self, obj):
        if obj.id:
            summary = f"""
            <div style="padding: 10px; background: #F4F1EA; border-left: 3px solid #122F26; border-radius: 4px;">
                <strong style="color: #122F26;">Withdrawal Summary</strong><br/>
                <span style="color: #3A5C50;">
                    User: {obj.wallet.user.get_full_name() or obj.wallet.user.email}<br/>
                    Amount: {obj.currency} {obj.amount:.2f}<br/>
                    Bank: {obj.bank_account.bank_name}<br/>
                    Status: {obj.get_status_display()}
                </span>
            </div>
            """
            return format_html(summary)
        return "Save to see summary"

    @admin.action(description=_('Mark selected withdrawals as processing'))
    def mark_processing(self, request, queryset):
        updated = queryset.update(status='processing')
        self.message_user(request, f'{updated} withdrawal(s) marked as processing.')

    @admin.action(description=_('Mark selected withdrawals as completed'))
    def mark_completed(self, request, queryset):
        from django.utils import timezone
        updated = queryset.update(status='completed', processed_at=timezone.now())
        self.message_user(request, f'{updated} withdrawal(s) completed.')

    @admin.action(description=_('Mark selected withdrawals as failed'))
    def mark_failed(self, request, queryset):
        updated = queryset.update(status='failed')
        self.message_user(request, f'{updated} withdrawal(s) failed.')
