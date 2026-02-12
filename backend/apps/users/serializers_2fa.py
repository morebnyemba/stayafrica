"""
Two-Factor Authentication Serializers
"""
from rest_framework import serializers
from django.contrib.auth import get_user_model

User = get_user_model()


class TwoFactorSetupSerializer(serializers.Serializer):
    """Serializer for 2FA setup response"""
    secret = serializers.CharField(read_only=True)
    qr_code = serializers.CharField(read_only=True)
    backup_codes = serializers.ListField(
        child=serializers.CharField(),
        read_only=True
    )
    
    class Meta:
        fields = ['secret', 'qr_code', 'backup_codes']


class TwoFactorVerifySerializer(serializers.Serializer):
    """Serializer for verifying TOTP token"""
    token = serializers.CharField(
        max_length=6,
        min_length=6,
        required=True,
        help_text="6-digit TOTP code from authenticator app"
    )
    
    def validate_token(self, value):
        """Validate token is numeric"""
        if not value.isdigit():
            raise serializers.ValidationError("Token must be 6 digits")
        return value


class TwoFactorEnableSerializer(serializers.Serializer):
    """Serializer for enabling 2FA"""
    token = serializers.CharField(
        max_length=6,
        min_length=6,
        required=True,
        help_text="6-digit TOTP code to verify setup"
    )
    
    def validate_token(self, value):
        """Validate token is numeric"""
        if not value.isdigit():
            raise serializers.ValidationError("Token must be 6 digits")
        return value


class TwoFactorDisableSerializer(serializers.Serializer):
    """Serializer for disabling 2FA"""
    password = serializers.CharField(
        write_only=True,
        required=True,
        help_text="User password for verification"
    )


class TwoFactorStatusSerializer(serializers.ModelSerializer):
    """Serializer for 2FA status"""
    class Meta:
        model = User
        fields = ['two_factor_enabled']
        read_only_fields = ['two_factor_enabled']


class BackupCodesRegenerateSerializer(serializers.Serializer):
    """Serializer for regenerating backup codes"""
    password = serializers.CharField(
        write_only=True,
        required=True,
        help_text="User password for verification"
    )


class BackupCodeVerifySerializer(serializers.Serializer):
    """Serializer for verifying backup code during login"""
    backup_code = serializers.CharField(
        required=True,
        help_text="One of your backup codes"
    )
    email = serializers.EmailField(required=True)
    password = serializers.CharField(write_only=True, required=True)


class LoginWith2FASerializer(serializers.Serializer):
    """Serializer for login with 2FA"""
    email = serializers.EmailField(required=True)
    password = serializers.CharField(write_only=True, required=True)
    token = serializers.CharField(
        max_length=6,
        min_length=6,
        required=False,
        help_text="6-digit TOTP code (if 2FA enabled)"
    )
    backup_code = serializers.CharField(
        required=False,
        help_text="Backup code (alternative to TOTP)"
    )
    
    def validate_token(self, value):
        """Validate token is numeric if provided"""
        if value and not value.isdigit():
            raise serializers.ValidationError("Token must be 6 digits")
        return value
