"""
Serializers for Identity Verification
"""
from rest_framework import serializers
from apps.users.verification_models import IdentityVerification, VerificationAttempt, VerificationSettings
from django.utils import timezone
from datetime import timedelta


class IdentityVerificationSerializer(serializers.ModelSerializer):
    """Serializer for identity verification submissions"""
    
    user_email = serializers.EmailField(source='user.email', read_only=True)
    status_display = serializers.CharField(source='get_status_display', read_only=True)
    document_type_display = serializers.CharField(source='get_document_type_display', read_only=True)
    is_expired = serializers.SerializerMethodField()
    
    class Meta:
        model = IdentityVerification
        fields = [
            'id', 'user_email', 'document_type', 'document_type_display',
            'document_number', 'document_country', 'document_expiry_date',
            'document_front_image', 'document_back_image', 'selfie_image',
            'status', 'status_display', 'submitted_at', 'reviewed_at',
            'rejection_reason', 'expires_at', 'is_expired'
        ]
        read_only_fields = [
            'id', 'status', 'submitted_at', 'reviewed_at',
            'rejection_reason', 'expires_at'
        ]
    
    def get_is_expired(self, obj):
        return obj.is_expired()
    
    def validate_document_front_image(self, value):
        """Validate front document image"""
        settings = VerificationSettings.get_settings()
        
        # Check file size
        if value.size > settings.max_image_size_mb * 1024 * 1024:
            raise serializers.ValidationError(
                f'Image size cannot exceed {settings.max_image_size_mb}MB'
            )
        
        # Check file format
        if not value.name.lower().endswith(('.jpg', '.jpeg', '.png')):
            raise serializers.ValidationError(
                'Only JPG, JPEG, and PNG formats are allowed'
            )
        
        return value
    
    def validate_selfie_image(self, value):
        """Validate selfie image"""
        settings = VerificationSettings.get_settings()
        
        if not settings.require_selfie:
            return value
        
        # Check file size
        if value.size > settings.max_image_size_mb * 1024 * 1024:
            raise serializers.ValidationError(
                f'Image size cannot exceed {settings.max_image_size_mb}MB'
            )
        
        # Check file format
        if not value.name.lower().endswith(('.jpg', '.jpeg', '.png')):
            raise serializers.ValidationError(
                'Only JPG, JPEG, and PNG formats are allowed'
            )
        
        return value
    
    def validate(self, data):
        """Validate complete verification submission"""
        settings = VerificationSettings.get_settings()
        user = self.context['request'].user
        
        # Check rate limiting
        today = timezone.now().date()
        attempts_today = VerificationAttempt.objects.filter(
            user=user,
            attempted_at__date=today
        ).count()
        
        if attempts_today >= settings.max_attempts_per_day:
            raise serializers.ValidationError(
                f'Maximum {settings.max_attempts_per_day} attempts per day exceeded. Please try again tomorrow.'
            )
        
        # Check if document back is required
        if settings.require_document_back and not data.get('document_back_image'):
            raise serializers.ValidationError({
                'document_back_image': 'Back side of document is required'
            })
        
        # Check if selfie is required
        if settings.require_selfie and not data.get('selfie_image'):
            raise serializers.ValidationError({
                'selfie_image': 'Selfie is required for verification'
            })
        
        # Check document expiry
        if data.get('document_expiry_date'):
            if data['document_expiry_date'] < timezone.now().date():
                raise serializers.ValidationError({
                    'document_expiry_date': 'Document has expired'
                })
        
        return data
    
    def create(self, validated_data):
        """Create verification and log attempt"""
        user = self.context['request'].user
        request = self.context['request']
        
        # Create verification
        verification = IdentityVerification.objects.create(
            user=user,
            **validated_data
        )
        
        # Log attempt
        VerificationAttempt.objects.create(
            user=user,
            verification=verification,
            ip_address=self.get_client_ip(request),
            user_agent=request.META.get('HTTP_USER_AGENT', ''),
            success=True
        )
        
        return verification
    
    def get_client_ip(self, request):
        """Get client IP address"""
        x_forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR')
        if x_forwarded_for:
            return x_forwarded_for.split(',')[0]
        return request.META.get('REMOTE_ADDR')


class VerificationReviewSerializer(serializers.Serializer):
    """Serializer for admin review actions"""
    
    action = serializers.ChoiceField(choices=['approve', 'reject'], required=True)
    reason = serializers.CharField(required=False, allow_blank=True)
    notes = serializers.CharField(required=False, allow_blank=True)
    
    def validate(self, data):
        if data['action'] == 'reject' and not data.get('reason'):
            raise serializers.ValidationError({
                'reason': 'Rejection reason is required'
            })
        return data


class VerificationStatusSerializer(serializers.ModelSerializer):
    """Simple serializer for verification status"""
    
    status_display = serializers.CharField(source='get_status_display', read_only=True)
    document_type_display = serializers.CharField(source='get_document_type_display', read_only=True)
    
    class Meta:
        model = IdentityVerification
        fields = [
            'id', 'status', 'status_display', 'document_type',
            'document_type_display', 'submitted_at', 'reviewed_at',
            'rejection_reason', 'expires_at'
        ]
        read_only_fields = fields


class VerificationSettingsSerializer(serializers.ModelSerializer):
    """Serializer for verification settings (admin only)"""
    
    class Meta:
        model = VerificationSettings
        fields = [
            'max_attempts_per_day', 'max_attempts_per_month',
            'require_document_back', 'require_selfie',
            'min_image_width', 'min_image_height', 'max_image_size_mb',
            'verification_valid_years',
            'require_verification_for_hosting',
            'require_verification_for_booking',
            'use_third_party_service',
            'updated_at'
        ]
        read_only_fields = ['updated_at']
