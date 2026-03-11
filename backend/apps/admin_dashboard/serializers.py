from rest_framework import serializers
from apps.admin_dashboard.models import AuditLog, AdminStats, SystemConfiguration

class AuditLogSerializer(serializers.ModelSerializer):
    user_email = serializers.CharField(source='user.email', read_only=True, default=None)
    user_first_name = serializers.CharField(source='user.first_name', read_only=True, default='')
    user_last_name = serializers.CharField(source='user.last_name', read_only=True, default='')
    
    class Meta:
        model = AuditLog
        fields = ['id', 'user', 'user_email', 'user_first_name', 'user_last_name', 'action', 'object_id', 'changes', 'timestamp']
        read_only_fields = ['id', 'timestamp']

class AdminStatsSerializer(serializers.ModelSerializer):
    class Meta:
        model = AdminStats
        fields = [
            'total_revenue', 'total_bookings', 'total_users', 'active_hosts',
            'total_properties', 'last_updated'
        ]
        read_only_fields = ['last_updated']

class SystemConfigurationSerializer(serializers.ModelSerializer):
    class Meta:
        model = SystemConfiguration
        fields = [
            'commission_rate', 'service_fee', 'default_currency',
            'max_advance_booking_days', 'max_stay_duration_days', 'review_window_days'
        ]
        read_only_fields = ['id']
