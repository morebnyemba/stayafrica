"""
Serializers for Identity Verification
"""
from rest_framework import serializers
from apps.users.verification_models import IdentityVerification, VerificationAttempt, VerificationSettings
from django.utils import timezone
from django.core.files.storage import default_storage
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


class VerificationSubmissionSerializer(serializers.Serializer):
    """
    Serializer for the 2-step verification flow:
    1) Frontend uploads files via /upload/ endpoint -> gets back file paths
    2) Frontend POSTs JSON with those paths + metadata here

    Accepts frontend field names and maps to model fields.
    """
    document_type = serializers.CharField()
    document_number = serializers.CharField()
    issued_country = serializers.CharField()  # maps to document_country
    expiry_date = serializers.DateField(required=False, allow_null=True)  # maps to document_expiry_date
    front_image = serializers.CharField()  # URL/path from upload step -> document_front_image
    back_image = serializers.CharField(required=False, allow_blank=True, allow_null=True)  # -> document_back_image
    selfie_image = serializers.CharField()  # URL/path from upload step -> selfie_image

    def _extract_path(self, url_or_path):
        """Extract the storage-relative path from a full URL or relative path."""
        if not url_or_path:
            return ''
        # If it's a full URL, extract the path after /media/
        if '/media/' in url_or_path:
            return url_or_path.split('/media/')[-1]
        # If it starts with /media/, strip it
        if url_or_path.startswith('/media/'):
            return url_or_path[7:]
        # Already a relative path like "verification/1/uuid.png"
        return url_or_path

    def validate_front_image(self, value):
        path = self._extract_path(value)
        if not path or not default_storage.exists(path):
            raise serializers.ValidationError('Front document image not found. Please re-upload.')
        return path

    def validate_selfie_image(self, value):
        path = self._extract_path(value)
        if not path or not default_storage.exists(path):
            raise serializers.ValidationError('Selfie image not found. Please re-upload.')
        return path

    def validate_back_image(self, value):
        if not value:
            return ''
        path = self._extract_path(value)
        if not default_storage.exists(path):
            raise serializers.ValidationError('Back document image not found. Please re-upload.')
        return path

    def validate_expiry_date(self, value):
        if value and value < timezone.now().date():
            raise serializers.ValidationError('Document has expired')
        return value

    def validate(self, data):
        settings = VerificationSettings.get_settings()
        user = self.context['request'].user

        # Rate limiting
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
        if settings.require_document_back and not data.get('back_image'):
            raise serializers.ValidationError({
                'back_image': 'Back side of document is required'
            })

        # Check if selfie is required
        if settings.require_selfie and not data.get('selfie_image'):
            raise serializers.ValidationError({
                'selfie_image': 'Selfie is required for verification'
            })

        return data

    def create(self, validated_data):
        user = self.context['request'].user
        request = self.context['request']

        verification = IdentityVerification.objects.create(
            user=user,
            document_type=validated_data['document_type'],
            document_number=validated_data['document_number'],
            document_country=validated_data['issued_country'],
            document_expiry_date=validated_data.get('expiry_date'),
            document_front_image=validated_data['front_image'],
            document_back_image=validated_data.get('back_image', ''),
            selfie_image=validated_data['selfie_image'],
        )

        # Log attempt
        x_forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR')
        ip = x_forwarded_for.split(',')[0] if x_forwarded_for else request.META.get('REMOTE_ADDR')

        VerificationAttempt.objects.create(
            user=user,
            verification=verification,
            ip_address=ip,
            user_agent=request.META.get('HTTP_USER_AGENT', ''),
            success=True,
        )

        return verification


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
