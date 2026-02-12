from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.payments.models import Payment, Wallet, WalletTransaction, Withdrawal, BankAccount, PaymentMethod
from apps.payments.pricing_models import PricingRule, PropertyFee, PropertyTax, CurrencyExchangeRate
from apps.payments.tax_models import TaxJurisdiction, TaxRate, BookingTax, TaxRemittance, TaxExemption


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
        """
        Mask account number for security - consistent format
        Always shows **** followed by up to 4 digits
        """
        if len(obj.account_number) <= 4:
            # For very short account numbers, show only asterisks
            return "****"
        else:
            # Show last 4 digits only
            return f"****{obj.account_number[-4:]}"

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


@admin.register(PricingRule)
class PricingRuleAdmin(UnfoldModelAdmin):
    """Admin interface for dynamic pricing rules"""
    
    list_display = ['property_title', 'name', 'rule_type', 'active_badge', 'adjustment_display', 
                    'date_range', 'priority']
    list_filter = ['rule_type', 'is_active', 'adjustment_type', 'created_at']
    search_fields = ['name', 'property__title', 'property__host__email']
    list_select_related = ['property', 'property__host']
    list_per_page = 25
    ordering = ['property', '-priority', 'start_date']
    
    fieldsets = (
        (_('Basic Information'), {
            'fields': ('property', 'name', 'rule_type', 'is_active', 'priority'),
        }),
        (_('Adjustment'), {
            'fields': ('adjustment_type', 'adjustment_value'),
        }),
        (_('Date Range'), {
            'fields': ('start_date', 'end_date'),
            'classes': ['collapse'],
        }),
        (_('Stay Requirements'), {
            'fields': ('min_nights', 'max_nights'),
            'classes': ['collapse'],
        }),
        (_('Booking Window'), {
            'fields': ('min_days_advance', 'max_days_advance'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Property'))
    def property_title(self, obj):
        return f"{obj.property.title} ({obj.property.host.email})"
    
    @display(description=_('Active'), label=True)
    def active_badge(self, obj):
        return {
            'value': 'Active' if obj.is_active else 'Inactive',
            'color': 'success' if obj.is_active else 'secondary',
        }
    
    @display(description=_('Adjustment'))
    def adjustment_display(self, obj):
        if obj.adjustment_type == 'percentage':
            return f"{obj.adjustment_value}%"
        return f"${obj.adjustment_value}"
    
    @display(description=_('Date Range'))
    def date_range(self, obj):
        if obj.start_date and obj.end_date:
            return f"{obj.start_date} to {obj.end_date}"
        return "Always active"


@admin.register(PropertyFee)
class PropertyFeeAdmin(UnfoldModelAdmin):
    """Admin interface for property fees"""
    
    list_display = ['property_title', 'name', 'fee_type', 'amount_display', 'charge_type', 
                    'active_badge', 'mandatory_badge']
    list_filter = ['fee_type', 'charge_type', 'is_mandatory', 'is_active', 'created_at']
    search_fields = ['name', 'property__title']
    list_select_related = ['property']
    list_per_page = 25
    
    fieldsets = (
        (_('Basic Information'), {
            'fields': ('property', 'fee_type', 'name', 'is_active'),
        }),
        (_('Fee Details'), {
            'fields': ('amount', 'charge_type', 'is_mandatory'),
        }),
        (_('Conditions'), {
            'fields': ('applies_after_guests',),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Property'))
    def property_title(self, obj):
        return obj.property.title
    
    @display(description=_('Amount'))
    def amount_display(self, obj):
        return f"${obj.amount:.2f}"
    
    @display(description=_('Active'), label=True)
    def active_badge(self, obj):
        return {
            'value': 'Active' if obj.is_active else 'Inactive',
            'color': 'success' if obj.is_active else 'secondary',
        }
    
    @display(description=_('Mandatory'), label=True)
    def mandatory_badge(self, obj):
        return {
            'value': 'Required' if obj.is_mandatory else 'Optional',
            'color': 'info' if obj.is_mandatory else 'secondary',
        }


@admin.register(PropertyTax)
class PropertyTaxAdmin(UnfoldModelAdmin):
    """Admin interface for property taxes"""
    
    list_display = ['property_title', 'name', 'tax_type', 'rate_display', 'active_badge', 
                    'applies_to_display']
    list_filter = ['tax_type', 'is_active', 'applies_to_base_price', 'applies_to_fees', 'created_at']
    search_fields = ['name', 'property__title']
    list_select_related = ['property']
    list_per_page = 25
    
    fieldsets = (
        (_('Basic Information'), {
            'fields': ('property', 'tax_type', 'name', 'is_active'),
        }),
        (_('Tax Rate'), {
            'fields': ('rate',),
        }),
        (_('Applies To'), {
            'fields': ('applies_to_base_price', 'applies_to_fees'),
        }),
    )
    
    @display(description=_('Property'))
    def property_title(self, obj):
        return obj.property.title
    
    @display(description=_('Rate'))
    def rate_display(self, obj):
        return f"{obj.rate}%"
    
    @display(description=_('Active'), label=True)
    def active_badge(self, obj):
        return {
            'value': 'Active' if obj.is_active else 'Inactive',
            'color': 'success' if obj.is_active else 'secondary',
        }
    
    @display(description=_('Applies To'))
    def applies_to_display(self, obj):
        parts = []
        if obj.applies_to_base_price:
            parts.append('Base Price')
        if obj.applies_to_fees:
            parts.append('Fees')
        return ', '.join(parts) if parts else 'None'


@admin.register(CurrencyExchangeRate)
class CurrencyExchangeRateAdmin(UnfoldModelAdmin):
    """Admin interface for currency exchange rates"""
    
    list_display = ['currency_pair', 'rate', 'active_badge', 'last_updated']
    list_filter = ['from_currency', 'to_currency', 'is_active', 'last_updated']
    search_fields = ['from_currency', 'to_currency']
    list_per_page = 25
    ordering = ['from_currency', 'to_currency']
    
    fieldsets = (
        (_('Currency Pair'), {
            'fields': ('from_currency', 'to_currency'),
        }),
        (_('Exchange Rate'), {
            'fields': ('rate', 'is_active'),
        }),
        (_('Metadata'), {
            'fields': ('last_updated',),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Currency Pair'))
    def currency_pair(self, obj):
        return f"{obj.from_currency} → {obj.to_currency}"
    
    @display(description=_('Active'), label=True)
    def active_badge(self, obj):
        return {
            'value': 'Active' if obj.is_active else 'Inactive',
            'color': 'success' if obj.is_active else 'secondary',
        }


@admin.register(PaymentMethod)
class PaymentMethodAdmin(UnfoldModelAdmin):
    """Admin interface for stored payment methods"""
    
    list_display = ['user_display', 'name', 'provider', 'method_type', 'masked_info', 
                    'default_badge', 'verified_badge', 'created_at']
    list_filter = ['provider', 'method_type', 'is_default', 'is_verified', 'created_at']
    search_fields = ['user__email', 'name', 'last_four', 'phone_number']
    readonly_fields = ['id', 'provider_token', 'created_at', 'updated_at']
    list_select_related = ['user']
    list_per_page = 25
    actions = ['verify_methods', 'set_as_default']
    
    fieldsets = (
        (_('User'), {
            'fields': ('user',),
        }),
        (_('Payment Method'), {
            'fields': ('name', 'provider', 'method_type'),
        }),
        (_('Token Information'), {
            'fields': ('provider_token',),
            'classes': ['collapse'],
        }),
        (_('Display Information'), {
            'fields': ('last_four', 'expiry_month', 'expiry_year', 'phone_number'),
        }),
        (_('Flags'), {
            'fields': ('is_default', 'is_verified'),
        }),
        (_('Metadata'), {
            'fields': ('id', 'deleted_at', 'created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('User'))
    def user_display(self, obj):
        return obj.user.get_full_name() or obj.user.email
    
    @display(description=_('Payment Info'))
    def masked_info(self, obj):
        if obj.last_four:
            return f"****{obj.last_four}"
        elif obj.phone_number:
            return obj.phone_number
        return '-'
    
    @display(description=_('Default'), label=True)
    def default_badge(self, obj):
        return {
            'value': 'Default' if obj.is_default else 'Secondary',
            'color': 'success' if obj.is_default else 'secondary',
        }
    
    @display(description=_('Verified'), label=True)
    def verified_badge(self, obj):
        return {
            'value': 'Verified' if obj.is_verified else 'Unverified',
            'color': 'success' if obj.is_verified else 'warning',
        }
    
    @admin.action(description=_('Verify selected payment methods'))
    def verify_methods(self, request, queryset):
        updated = queryset.update(is_verified=True)
        self.message_user(request, f'{updated} payment method(s) verified.')
    
    @admin.action(description=_('Set as default'))
    def set_as_default(self, request, queryset):
        for method in queryset:
            PaymentMethod.objects.filter(user=method.user).update(is_default=False)
            method.is_default = True
            method.save()
        self.message_user(request, f'{queryset.count()} payment method(s) set as default.')


@admin.register(TaxJurisdiction)
class TaxJurisdictionAdmin(UnfoldModelAdmin):
    """Admin interface for tax jurisdictions"""
    
    list_display = ['name', 'jurisdiction_type', 'code', 'country_code', 'active_badge', 'created_at']
    list_filter = ['jurisdiction_type', 'is_active', 'country_code', 'created_at']
    search_fields = ['name', 'code', 'country_code', 'city_name']
    readonly_fields = ['created_at', 'updated_at']
    list_per_page = 25
    
    fieldsets = (
        (_('Basic Information'), {
            'fields': ('name', 'jurisdiction_type', 'code', 'is_active'),
        }),
        (_('Geographic Information'), {
            'fields': ('country_code', 'state_province_code', 'city_name'),
        }),
        (_('Hierarchy'), {
            'fields': ('parent_jurisdiction',),
            'classes': ['collapse'],
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Active'), label=True)
    def active_badge(self, obj):
        return {
            'value': 'Active' if obj.is_active else 'Inactive',
            'color': 'success' if obj.is_active else 'secondary',
        }


@admin.register(TaxRate)
class TaxRateAdmin(UnfoldModelAdmin):
    """Admin interface for tax rates"""
    
    list_display = ['name', 'jurisdiction', 'tax_type', 'rate_display', 'active_badge', 'created_at']
    list_filter = ['tax_type', 'is_active', 'created_at']
    search_fields = ['name', 'jurisdiction__name', 'jurisdiction__code']
    readonly_fields = ['created_at', 'updated_at']
    list_select_related = ['jurisdiction']
    list_per_page = 25
    
    fieldsets = (
        (_('Basic Information'), {
            'fields': ('jurisdiction', 'name', 'tax_type', 'is_active'),
        }),
        (_('Rate Configuration'), {
            'fields': ('rate', 'is_compound'),
        }),
        (_('Applicability'), {
            'fields': ('applies_to_accommodation', 'applies_to_cleaning_fee', 'applies_to_service_fee'),
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Rate'))
    def rate_display(self, obj):
        return f"{obj.rate}%"
    
    @display(description=_('Active'), label=True)
    def active_badge(self, obj):
        return {
            'value': 'Active' if obj.is_active else 'Inactive',
            'color': 'success' if obj.is_active else 'secondary',
        }


@admin.register(BookingTax)
class BookingTaxAdmin(UnfoldModelAdmin):
    """Admin interface for booking taxes"""
    
    list_display = ['booking_ref', 'tax_name', 'tax_amount_display', 'tax_rate_display', 'calculation_date']
    list_filter = ['tax_rate__tax_type', 'calculation_date']
    search_fields = ['booking__booking_ref', 'tax_rate__name']
    readonly_fields = ['booking', 'tax_rate', 'tax_amount', 'taxable_amount', 'calculation_date']
    list_select_related = ['booking', 'tax_rate', 'tax_rate__jurisdiction']
    list_per_page = 25
    date_hierarchy = 'calculation_date'
    
    fieldsets = (
        (_('Booking'), {
            'fields': ('booking',),
        }),
        (_('Tax Information'), {
            'fields': ('tax_rate', 'taxable_amount', 'tax_amount'),
        }),
        (_('Timestamps'), {
            'fields': ('calculation_date',),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Booking'))
    def booking_ref(self, obj):
        return obj.booking.booking_ref
    
    @display(description=_('Tax Name'))
    def tax_name(self, obj):
        return obj.tax_rate.name
    
    @display(description=_('Tax Amount'))
    def tax_amount_display(self, obj):
        return f"${obj.tax_amount:.2f}"
    
    @display(description=_('Rate'))
    def tax_rate_display(self, obj):
        return f"{obj.tax_rate.rate_percentage}%"
    
    def has_add_permission(self, request):
        return False
    
    def has_change_permission(self, request, obj=None):
        return False


@admin.register(TaxRemittance)
class TaxRemittanceAdmin(UnfoldModelAdmin):
    """Admin interface for tax remittances"""
    
    list_display = ['remittance_reference', 'jurisdiction', 'period_display', 'total_amount_display', 
                    'status_badge', 'remittance_date']
    list_filter = ['status', 'jurisdiction__jurisdiction_type', 'created_at']
    search_fields = ['remittance_reference', 'jurisdiction__name']
    readonly_fields = ['remittance_reference', 'created_at', 'updated_at']
    list_select_related = ['jurisdiction']
    list_per_page = 25
    date_hierarchy = 'period_start'
    actions = ['mark_remitted', 'mark_pending']
    
    fieldsets = (
        (_('Reference'), {
            'fields': ('remittance_reference', 'jurisdiction', 'status'),
        }),
        (_('Period'), {
            'fields': ('period_start', 'period_end'),
        }),
        (_('Amounts'), {
            'fields': ('total_tax_collected', 'total_bookings'),
        }),
        (_('Remittance'), {
            'fields': ('remittance_date', 'report_file_url', 'confirmation_file_url', 'notes'),
            'classes': ['collapse'],
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Period'))
    def period_display(self, obj):
        return f"{obj.period_start} to {obj.period_end}"
    
    @display(description=_('Total Amount'))
    def total_amount_display(self, obj):
        return f"${obj.total_tax_collected:.2f}"
    
    @display(description=_('Status'), label=True)
    def status_badge(self, obj):
        colors = {
            'pending': 'warning',
            'remitted': 'success',
            'overdue': 'danger',
        }
        return {
            'value': obj.get_status_display() if hasattr(obj, 'get_status_display') else obj.status,
            'color': colors.get(obj.status, 'secondary'),
        }
    
    @admin.action(description=_('Mark as remitted'))
    def mark_remitted(self, request, queryset):
        from django.utils import timezone
        updated = queryset.update(status='remitted', remittance_date=timezone.now())
        self.message_user(request, f'{updated} remittance(s) marked as remitted.')
    
    @admin.action(description=_('Mark as pending'))
    def mark_pending(self, request, queryset):
        updated = queryset.update(status='pending')
        self.message_user(request, f'{updated} remittance(s) marked as pending.')


@admin.register(TaxExemption)
class TaxExemptionAdmin(UnfoldModelAdmin):
    """Admin interface for tax exemptions"""
    
    list_display = ['exemption_type', 'host_display', 'jurisdiction', 'certificate_number', 
                    'active_badge', 'valid_to']
    list_filter = ['exemption_type', 'is_active', 'created_at']
    search_fields = ['exemption_certificate_number', 'host__email', 'jurisdiction__name']
    readonly_fields = ['created_at', 'updated_at']
    list_select_related = ['host', 'jurisdiction', 'property', 'tax_rate']
    list_per_page = 25
    
    fieldsets = (
        (_('Exemption Details'), {
            'fields': ('exemption_type', 'exemption_certificate_number', 'is_active'),
        }),
        (_('Applies To'), {
            'fields': ('host', 'property', 'jurisdiction', 'tax_rate'),
        }),
        (_('Validity'), {
            'fields': ('valid_from', 'valid_to'),
        }),
        (_('Reason'), {
            'fields': ('reason',),
            'classes': ['collapse'],
        }),
        (_('Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse'],
        }),
    )
    
    @display(description=_('Host'))
    def host_display(self, obj):
        if obj.host:
            return obj.host.get_full_name() or obj.host.email
        return 'Global'
    
    @display(description=_('Certificate #'))
    def certificate_number(self, obj):
        return obj.exemption_certificate_number or '-'
    
    @display(description=_('Active'), label=True)
    def active_badge(self, obj):
        return {
            'value': 'Active' if obj.is_active else 'Inactive',
            'color': 'success' if obj.is_active else 'secondary',
        }
