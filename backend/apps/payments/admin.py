from django.contrib import admin
from django.utils.html import format_html
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from apps.payments.models import Payment, Wallet, WalletTransaction, Withdrawal, BankAccount


@admin.register(Payment)
class PaymentAdmin(UnfoldModelAdmin):
    list_display = ['gateway_ref', 'booking', 'provider', 'status_badge', 'amount', 'currency', 'created_at']
    list_filter = ['provider', 'status', 'currency', 'created_at']
    search_fields = ['gateway_ref', 'booking__booking_ref']
    readonly_fields = ['gateway_ref', 'amount', 'currency', 'created_at', 'updated_at']
    raw_id_fields = ['booking']
    date_hierarchy = 'created_at'
    list_select_related = ['booking']

    actions = ['mark_success', 'mark_failed']

    def mark_success(self, request, queryset):
        queryset.update(status='success')
    mark_success.short_description = 'Mark selected payments as success'

    def mark_failed(self, request, queryset):
        queryset.update(status='failed')
    mark_failed.short_description = 'Mark selected payments as failed'

    @admin.display(description='Status')
    def status_badge(self, obj):
        colors = {
            'initiated': 'bg-amber-100 text-amber-700',
            'pending': 'bg-blue-100 text-blue-700',
            'success': 'bg-green-100 text-green-700',
            'failed': 'bg-red-100 text-red-700',
        }
        klass = colors.get(obj.status, 'bg-gray-100 text-gray-700')
        return format_html('<span class="{} px-2 py-1 rounded">{}</span>', klass, obj.get_status_display())

    def has_add_permission(self, request):
        return False


@admin.register(Wallet)
class WalletAdmin(UnfoldModelAdmin):
    list_display = ['user', 'balance', 'currency', 'status_badge', 'updated_at']
    list_filter = ['status', 'currency', 'created_at']
    search_fields = ['user__email']
    readonly_fields = ['balance', 'created_at', 'updated_at']
    list_select_related = ['user']
    actions = ['activate_wallets', 'suspend_wallets', 'close_wallets']

    @admin.display(description='Status')
    def status_badge(self, obj):
        colors = {
            'active': 'bg-green-100 text-green-700',
            'suspended': 'bg-amber-100 text-amber-700',
            'closed': 'bg-red-100 text-red-700',
        }
        klass = colors.get(obj.status, 'bg-gray-100 text-gray-700')
        return format_html('<span class="{} px-2 py-1 rounded">{}</span>', klass, obj.get_status_display())

    def activate_wallets(self, request, queryset):
        queryset.update(status='active')
    activate_wallets.short_description = 'Mark selected wallets as active'

    def suspend_wallets(self, request, queryset):
        queryset.update(status='suspended')
    suspend_wallets.short_description = 'Suspend selected wallets'

    def close_wallets(self, request, queryset):
        queryset.update(status='closed')
    close_wallets.short_description = 'Close selected wallets'


@admin.register(BankAccount)
class BankAccountAdmin(UnfoldModelAdmin):
    list_display = ['user', 'bank_name', 'account_number', 'country', 'is_primary', 'created_at']
    list_filter = ['is_primary', 'country', 'created_at']
    search_fields = ['user__email', 'bank_name', 'account_number', 'account_name']
    readonly_fields = ['created_at', 'updated_at']
    list_select_related = ['user']
    actions = ['make_primary']

    def make_primary(self, request, queryset):
        queryset.update(is_primary=True)
    make_primary.short_description = 'Mark selected accounts as primary'


@admin.register(WalletTransaction)
class WalletTransactionAdmin(UnfoldModelAdmin):
    list_display = ['reference', 'wallet', 'txn_type', 'status_badge', 'amount', 'currency', 'created_at']
    list_filter = ['txn_type', 'status', 'currency', 'created_at']
    search_fields = ['reference', 'wallet__user__email', 'booking__booking_ref']
    readonly_fields = ['reference', 'amount', 'currency', 'created_at', 'updated_at']
    list_select_related = ['wallet', 'wallet__user', 'booking']
    raw_id_fields = ['wallet', 'booking']
    date_hierarchy = 'created_at'

    @admin.display(description='Status')
    def status_badge(self, obj):
        colors = {
            'pending': 'bg-amber-100 text-amber-700',
            'completed': 'bg-green-100 text-green-700',
            'failed': 'bg-red-100 text-red-700',
            'reversed': 'bg-blue-100 text-blue-700',
        }
        klass = colors.get(obj.status, 'bg-gray-100 text-gray-700')
        return format_html('<span class="{} px-2 py-1 rounded">{}</span>', klass, obj.get_status_display())


@admin.register(Withdrawal)
class WithdrawalAdmin(UnfoldModelAdmin):
    list_display = ['reference', 'wallet', 'bank_account', 'amount', 'currency', 'status_badge', 'processed_at', 'created_at']
    list_filter = ['status', 'currency', 'created_at', 'processed_at']
    search_fields = ['reference', 'wallet__user__email', 'bank_account__account_number']
    readonly_fields = ['reference', 'amount', 'currency', 'created_at', 'updated_at']
    list_select_related = ['wallet', 'wallet__user', 'bank_account']
    raw_id_fields = ['wallet', 'bank_account']
    date_hierarchy = 'created_at'
    actions = ['mark_processing', 'mark_completed', 'mark_failed']

    @admin.display(description='Status')
    def status_badge(self, obj):
        colors = {
            'pending': 'bg-amber-100 text-amber-700',
            'processing': 'bg-blue-100 text-blue-700',
            'completed': 'bg-green-100 text-green-700',
            'failed': 'bg-red-100 text-red-700',
        }
        klass = colors.get(obj.status, 'bg-gray-100 text-gray-700')
        return format_html('<span class="{} px-2 py-1 rounded">{}</span>', klass, obj.get_status_display())

    def mark_processing(self, request, queryset):
        queryset.update(status='processing')
    mark_processing.short_description = 'Mark selected withdrawals as processing'

    def mark_completed(self, request, queryset):
        queryset.update(status='completed')
    mark_completed.short_description = 'Mark selected withdrawals as completed'

    def mark_failed(self, request, queryset):
        queryset.update(status='failed')
    mark_failed.short_description = 'Mark selected withdrawals as failed'

    @admin.display(description='Status')
    def status_badge(self, obj):
        colors = {
            'initiated': 'bg-amber-100 text-amber-700',
            'pending': 'bg-blue-100 text-blue-700',
            'success': 'bg-green-100 text-green-700',
            'failed': 'bg-red-100 text-red-700',
        }
        klass = colors.get(obj.status, 'bg-gray-100 text-gray-700')
        return format_html('<span class="{} px-2 py-1 rounded">{}</span>', klass, obj.get_status_display())
