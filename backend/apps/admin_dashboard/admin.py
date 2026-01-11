from django.contrib import admin
from django.utils.html import format_html
from django.utils.translation import gettext_lazy as _
from unfold.admin import ModelAdmin as UnfoldModelAdmin
from unfold.decorators import display
from apps.admin_dashboard.models import SystemConfiguration, AuditLog, AdminStats


@admin.register(SystemConfiguration)
class SystemConfigurationAdmin(UnfoldModelAdmin):
    """
    Enhanced admin interface for system configuration (singleton) with StayAfrica theme
    """
    
    list_display = ['id', 'commission_rate_display', 'service_fee_display', 
                    'default_currency', 'maintenance_mode', 'updated_at']
    
    fieldsets = (
        (_('💰 Pricing Configuration'), {
            'fields': ('commission_rate', 'service_fee', 'default_currency'),
            'description': 'Configure booking fees and default currency for the platform',
            'classes': ['tab'],
        }),
        (_('🇿🇼 Paynow (Zimbabwe)'), {
            'fields': ('paynow_integration_id', 'paynow_integration_key', 'paynow_webhook_secret'),
            'classes': ['collapse', 'tab'],
            'description': 'Paynow payment gateway configuration for Zimbabwe',
        }),
        (_('🇿🇦 PayFast (South Africa)'), {
            'fields': ('payfast_merchant_id', 'payfast_merchant_key', 'payfast_passphrase', 
                      'payfast_webhook_secret'),
            'classes': ['collapse', 'tab'],
            'description': 'PayFast payment gateway configuration for South Africa',
        }),
        (_('💳 Stripe (International)'), {
            'fields': ('stripe_secret_key', 'stripe_publishable_key', 'stripe_webhook_secret'),
            'classes': ['collapse', 'tab'],
            'description': 'Stripe payment gateway configuration for international payments',
        }),
        (_('📋 Business Rules'), {
            'fields': ('max_advance_booking_days', 'max_stay_duration_days', 
                      'review_window_days', 'review_edit_window_days'),
            'description': 'Configure booking and review time limits',
            'classes': ['tab'],
        }),
        (_('✉️ Email Settings'), {
            'fields': ('admin_email', 'support_email'),
            'classes': ['collapse', 'tab'],
            'description': 'Configure email addresses for system notifications',
        }),
        (_('🔧 Maintenance'), {
            'fields': ('maintenance_mode', 'maintenance_message'),
            'classes': ['collapse', 'tab'],
            'description': 'Enable maintenance mode to temporarily disable the site',
        }),
        (_('⏰ Timestamps'), {
            'fields': ('created_at', 'updated_at'),
            'classes': ['collapse', 'tab'],
        }),
    )
    
    readonly_fields = ('created_at', 'updated_at', 'configuration_summary')
    
    # Add configuration summary as first field in pricing
    def get_fieldsets(self, request, obj=None):
        fieldsets = super().get_fieldsets(request, obj)
        if obj:  # If editing existing configuration
            # Add summary to first fieldset
            first_fieldset = list(fieldsets[0])
            first_fieldset[1] = dict(first_fieldset[1])
            fields = list(first_fieldset[1]['fields'])
            if 'configuration_summary' not in fields:
                fields.insert(0, 'configuration_summary')
            first_fieldset[1]['fields'] = tuple(fields)
            fieldsets = (tuple(first_fieldset),) + fieldsets[1:]
        return fieldsets

    @display(description=_('Commission Rate'))
    def commission_rate_display(self, obj):
        return f"{obj.commission_rate * 100:.1f}%"

    @display(description=_('Service Fee'))
    def service_fee_display(self, obj):
        return f"{obj.default_currency} {obj.service_fee:.2f}"

    @display(description=_('System Configuration Summary'))
    def configuration_summary(self, obj):
        if obj and obj.id:
            # Payment gateways status
            paynow_status = '✓' if obj.paynow_integration_id else '✗'
            payfast_status = '✓' if obj.payfast_merchant_id else '✗'
            stripe_status = '✓' if obj.stripe_secret_key else '✗'
            
            summary = f"""
            <div style="padding: 15px; background: #F4F1EA; border-left: 4px solid #D9B168; 
                        border-radius: 6px; margin-bottom: 15px;">
                <h3 style="color: #122F26; margin: 0 0 12px 0; font-size: 18px;">
                    🏠 StayAfrica System Configuration
                </h3>
                
                <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 15px;">
                    <div style="background: white; padding: 10px; border-radius: 4px; 
                                border-left: 3px solid #3A5C50;">
                        <strong style="color: #122F26;">💰 Pricing</strong><br/>
                        <span style="color: #3A5C50;">
                            Commission: {obj.commission_rate * 100:.1f}%<br/>
                            Service Fee: {obj.default_currency} {obj.service_fee:.2f}<br/>
                            Currency: {obj.default_currency}
                        </span>
                    </div>
                    
                    <div style="background: white; padding: 10px; border-radius: 4px; 
                                border-left: 3px solid #D9B168;">
                        <strong style="color: #122F26;">💳 Payment Gateways</strong><br/>
                        <span style="color: #3A5C50;">
                            Paynow (ZW): {paynow_status}<br/>
                            PayFast (ZA): {payfast_status}<br/>
                            Stripe (Intl): {stripe_status}
                        </span>
                    </div>
                    
                    <div style="background: white; padding: 10px; border-radius: 4px; 
                                border-left: 3px solid #3A5C50;">
                        <strong style="color: #122F26;">📋 Business Rules</strong><br/>
                        <span style="color: #3A5C50;">
                            Max Booking: {obj.max_advance_booking_days} days<br/>
                            Max Stay: {obj.max_stay_duration_days} days<br/>
                            Review Window: {obj.review_window_days} days
                        </span>
                    </div>
                    
                    <div style="background: white; padding: 10px; border-radius: 4px; 
                                border-left: 3px solid {'#B91C1C' if obj.maintenance_mode else '#D9B168'};">
                        <strong style="color: #122F26;">🔧 System Status</strong><br/>
                        <span style="color: {'#B91C1C' if obj.maintenance_mode else '#3A5C50'};">
                            Maintenance Mode: {'🔴 ACTIVE' if obj.maintenance_mode else '✓ Normal Operation'}<br/>
                            Admin Email: {obj.admin_email or 'Not Set'}<br/>
                            Support Email: {obj.support_email or 'Not Set'}
                        </span>
                    </div>
                </div>
            </div>
            """
            return format_html(summary)
        return "Configuration will be displayed after saving"
    
    def has_add_permission(self, request):
        """Prevent adding more than one configuration"""
        return not SystemConfiguration.objects.exists()
    
    def has_delete_permission(self, request, obj=None):
        """Prevent deletion of configuration"""
        return False


@admin.register(AuditLog)
class AuditLogAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Audit Log with StayAfrica theme"""
    
    list_display = ['id', 'user_display', 'action', 'object_display', 'timestamp']
    list_filter = ['action', 'timestamp', 'content_type']
    search_fields = ['user__email', 'user__username', 'action', 'object_id']
    readonly_fields = ['user', 'action', 'timestamp', 'changes', 'content_type', 
                      'object_id', 'audit_summary']
    date_hierarchy = 'timestamp'
    list_per_page = 50
    list_select_related = ['user', 'content_type']
    
    fieldsets = (
        (_('Audit Information'), {
            'fields': ('audit_summary',),
        }),
        (_('Details'), {
            'fields': ('user', 'action', 'content_type', 'object_id', 'timestamp'),
        }),
        (_('Changes'), {
            'fields': ('changes',),
            'classes': ['collapse'],
            'description': 'JSON representation of changes made',
        }),
    )

    @display(description=_('User'))
    def user_display(self, obj):
        if obj.user:
            return obj.user.get_full_name() or obj.user.email
        return 'System'

    @display(description=_('Object'))
    def object_display(self, obj):
        if obj.content_type:
            return f"{obj.content_type.model} #{obj.object_id}"
        return '-'

    @display(description=_('Audit Summary'))
    def audit_summary(self, obj):
        if obj.id:
            # Determine action color
            action_colors = {
                'create': '#10B981',  # Green
                'update': '#3A5C50',  # Moss Green
                'delete': '#B91C1C',  # Red
            }
            action_color = action_colors.get(obj.action.lower(), '#D9B168')
            
            summary = f"""
            <div style="padding: 10px; background: #F4F1EA; border-left: 3px solid {action_color}; border-radius: 4px;">
                <strong style="color: #122F26;">Audit Log Entry</strong><br/>
                <span style="color: #3A5C50;">
                    User: {obj.user.get_full_name() or obj.user.email if obj.user else 'System'}<br/>
                    Action: <strong style="color: {action_color};">{obj.action.upper()}</strong><br/>
                    Object: {obj.content_type.model if obj.content_type else 'N/A'} #{obj.object_id}<br/>
                    Time: {obj.timestamp.strftime('%B %d, %Y at %H:%M:%S')}
                </span>
            </div>
            """
            return format_html(summary)
        return "Audit log details"

    def has_add_permission(self, request):
        """Audit logs are created automatically"""
        return False

    def has_delete_permission(self, request, obj=None):
        """Prevent deletion of audit logs for compliance"""
        return request.user.is_superuser  # Only superusers can delete


@admin.register(AdminStats)
class AdminStatsAdmin(UnfoldModelAdmin):
    """Enhanced admin interface for Admin Statistics with StayAfrica theme"""
    
    list_display = ['id', 'revenue_display', 'bookings_count', 'users_count', 
                    'hosts_count', 'last_updated']
    readonly_fields = ['total_revenue', 'total_bookings', 'total_users', 'active_hosts',
                      'last_updated', 'stats_summary']
    
    fieldsets = (
        (_('Dashboard Statistics'), {
            'fields': ('stats_summary',),
        }),
        (_('Revenue & Bookings'), {
            'fields': ('total_revenue', 'total_bookings'),
        }),
        (_('Users & Hosts'), {
            'fields': ('total_users', 'active_hosts'),
        }),
        (_('Last Updated'), {
            'fields': ('last_updated',),
        }),
    )

    @display(description=_('Total Revenue'))
    def revenue_display(self, obj):
        return f"${obj.total_revenue:,.2f}"

    @display(description=_('Bookings'))
    def bookings_count(self, obj):
        return f"{obj.total_bookings:,}"

    @display(description=_('Users'))
    def users_count(self, obj):
        return f"{obj.total_users:,}"

    @display(description=_('Active Hosts'))
    def hosts_count(self, obj):
        return f"{obj.active_hosts:,}"

    @display(description=_('Statistics Summary'))
    def stats_summary(self, obj):
        if obj and obj.id:
            summary = f"""
            <div style="padding: 15px; background: #F4F1EA; border-left: 4px solid #D9B168; 
                        border-radius: 6px;">
                <h3 style="color: #122F26; margin: 0 0 15px 0; font-size: 18px;">
                    📊 Platform Statistics
                </h3>
                
                <div style="display: grid; grid-template-columns: repeat(2, 1fr); gap: 12px;">
                    <div style="background: white; padding: 12px; border-radius: 4px; 
                                border-left: 3px solid #10B981;">
                        <div style="font-size: 24px; font-weight: bold; color: #10B981;">
                            ${obj.total_revenue:,.2f}
                        </div>
                        <div style="color: #3A5C50; margin-top: 4px;">Total Revenue</div>
                    </div>
                    
                    <div style="background: white; padding: 12px; border-radius: 4px; 
                                border-left: 3px solid #3A5C50;">
                        <div style="font-size: 24px; font-weight: bold; color: #3A5C50;">
                            {obj.total_bookings:,}
                        </div>
                        <div style="color: #3A5C50; margin-top: 4px;">Total Bookings</div>
                    </div>
                    
                    <div style="background: white; padding: 12px; border-radius: 4px; 
                                border-left: 3px solid #D9B168;">
                        <div style="font-size: 24px; font-weight: bold; color: #D9B168;">
                            {obj.total_users:,}
                        </div>
                        <div style="color: #3A5C50; margin-top: 4px;">Total Users</div>
                    </div>
                    
                    <div style="background: white; padding: 12px; border-radius: 4px; 
                                border-left: 3px solid #122F26;">
                        <div style="font-size: 24px; font-weight: bold; color: #122F26;">
                            {obj.active_hosts:,}
                        </div>
                        <div style="color: #3A5C50; margin-top: 4px;">Active Hosts</div>
                    </div>
                </div>
                
                <div style="margin-top: 12px; padding: 8px; background: white; border-radius: 4px; text-align: center;">
                    <span style="color: #3A5C50;">
                        Last Updated: {obj.last_updated.strftime('%B %d, %Y at %H:%M:%S')}
                    </span>
                </div>
            </div>
            """
            return format_html(summary)
        return "Statistics will be displayed after calculation"

    def has_add_permission(self, request):
        """Stats are calculated automatically"""
        return False

    def has_delete_permission(self, request, obj=None):
        """Prevent deletion of statistics"""
        return False
