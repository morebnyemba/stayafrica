from rest_framework import serializers
from apps.admin_dashboard.models import AuditLog, AdminStats

class AuditLogSerializer(serializers.ModelSerializer):
    user_email = serializers.CharField(source='user.email', read_only=True)
    
    class Meta:
        model = AuditLog
        fields = ['id', 'user', 'user_email', 'action', 'object_id', 'changes', 'timestamp']
        read_only_fields = ['id', 'timestamp']

class AdminStatsSerializer(serializers.ModelSerializer):
    class Meta:
        model = AdminStats
        fields = [
            'total_revenue', 'total_bookings', 'total_users', 'active_hosts',
            'total_properties', 'last_updated'
        ]
        read_only_fields = ['last_updated']
