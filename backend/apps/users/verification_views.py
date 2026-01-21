"""
Views for Identity Verification
"""
from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated, IsAdminUser
from rest_framework.parsers import MultiPartParser, FormParser
from django.shortcuts import get_object_or_404
from django.core.files.storage import default_storage
from django.conf import settings
from apps.users.verification_models import IdentityVerification, VerificationSettings
from apps.users.verification_serializers import (
    IdentityVerificationSerializer,
    VerificationReviewSerializer,
    VerificationStatusSerializer,
    VerificationSettingsSerializer
)
import logging
import uuid
import os

logger = logging.getLogger(__name__)


class IdentityVerificationViewSet(viewsets.ModelViewSet):
    """
    ViewSet for identity verification
    """
    serializer_class = IdentityVerificationSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        """Return verifications for current user or all if admin"""
        if self.request.user.is_admin_user:
            return IdentityVerification.objects.all().select_related(
                'user', 'reviewed_by'
            ).order_by('-submitted_at')
        return IdentityVerification.objects.filter(
            user=self.request.user
        ).order_by('-submitted_at')
    
    def create(self, request, *args, **kwargs):
        """Submit new identity verification"""
        # Check if user already has pending or approved verification
        existing = IdentityVerification.objects.filter(
            user=request.user,
            status__in=['pending', 'under_review', 'approved']
        ).first()
        
        if existing:
            if existing.status == 'approved':
                # Check if expired
                if not existing.is_expired():
                    return Response(
                        {'error': 'You already have an approved verification that is still valid'},
                        status=status.HTTP_400_BAD_REQUEST
                    )
            else:
                return Response(
                    {
                        'error': f'You already have a {existing.get_status_display().lower()} verification request',
                        'verification_id': existing.id,
                        'status': existing.status
                    },
                    status=status.HTTP_400_BAD_REQUEST
                )
        
        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        verification = serializer.save()
        
        logger.info(f"Identity verification submitted for user {request.user.email}")
        
        return Response(
            serializer.data,
            status=status.HTTP_201_CREATED
        )
    
    @action(detail=False, methods=['get'])
    def current_status(self, request):
        """Get current verification status for user"""
        verification = IdentityVerification.objects.filter(
            user=request.user
        ).order_by('-submitted_at').first()
        
        if not verification:
            return Response({
                'has_verification': False,
                'is_verified': request.user.is_verified
            })
        
        # Check if expired
        verification.check_and_expire()
        
        serializer = VerificationStatusSerializer(verification)
        return Response({
            'has_verification': True,
            'is_verified': request.user.is_verified,
            'verification': serializer.data
        })
    
    @action(detail=False, methods=['post'], parser_classes=[MultiPartParser, FormParser])
    def upload(self, request):
        """
        Upload a document or selfie image for verification.
        This is a standalone upload endpoint that returns the file URL.
        The frontend can then submit the verification with the uploaded URLs.
        """
        if 'file' not in request.FILES:
            return Response(
                {'error': 'No file provided'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        file = request.FILES['file']
        
        # Validate file type (image/jpeg is the standard MIME type for JPEG images)
        allowed_types = ['image/jpeg', 'image/png']
        if file.content_type not in allowed_types:
            return Response(
                {'error': 'Invalid file type. Only JPEG and PNG images are allowed.'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Validate file size (max 10MB)
        max_size = 10 * 1024 * 1024  # 10MB
        if file.size > max_size:
            return Response(
                {'error': 'File size too large. Maximum allowed size is 10MB.'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Generate a unique filename
        ext = os.path.splitext(file.name)[1].lower()
        if ext not in ['.jpg', '.jpeg', '.png']:
            ext = '.jpg'
        unique_filename = f"verification/{request.user.id}/{uuid.uuid4()}{ext}"
        
        try:
            # Save the file
            saved_path = default_storage.save(unique_filename, file)
            
            # Generate the file URL
            file_url = request.build_absolute_uri(
                settings.MEDIA_URL + saved_path
            )
            
            logger.info(f"Verification document uploaded by user {request.user.id}: {saved_path}")
            
            return Response({
                'url': file_url,
                'path': saved_path,
                'filename': file.name,
                'size': file.size,
            }, status=status.HTTP_201_CREATED)
            
        except Exception as e:
            logger.error(f"Error uploading verification document: {e}")
            return Response(
                {'error': 'Failed to upload file. Please try again.'},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
    
    @action(detail=True, methods=['post'], permission_classes=[IsAdminUser])
    def review(self, request, pk=None):
        """
        Admin action to review and approve/reject verification
        POST body: {"action": "approve|reject", "reason": "...", "notes": "..."}
        """
        verification = self.get_object()
        
        if verification.status not in ['pending', 'under_review']:
            return Response(
                {'error': 'This verification has already been reviewed'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        serializer = VerificationReviewSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        
        action_type = serializer.validated_data['action']
        reason = serializer.validated_data.get('reason', '')
        notes = serializer.validated_data.get('notes', '')
        
        if action_type == 'approve':
            verification.approve(request.user, notes)
            logger.info(f"Verification {verification.id} approved by {request.user.email}")
            
            # Send push notification to user
            try:
                from services.notification_service import NotificationService
                NotificationService.send_notification(
                    user=verification.user,
                    title="Identity Verified! ✅",
                    body="Your identity has been verified successfully.",
                    notification_type='system',
                    deep_link='stayafrica://profile'
                )
            except Exception as e:
                logger.warning(f"Failed to send verification approval notification: {e}")
            
            # Send email notification
            try:
                from tasks.email_tasks import send_identity_verification_email
                send_identity_verification_email.delay(verification.user.id, 'approved')
            except Exception as e:
                logger.warning(f"Failed to queue verification approval email: {e}")
            
            return Response({
                'message': 'Verification approved successfully',
                'verification': IdentityVerificationSerializer(verification).data
            })
        
        elif action_type == 'reject':
            verification.reject(request.user, reason, notes)
            logger.info(f"Verification {verification.id} rejected by {request.user.email}")
            
            # Send push notification to user
            try:
                from services.notification_service import NotificationService
                NotificationService.send_notification(
                    user=verification.user,
                    title="Verification Update",
                    body=f"Your identity verification was not approved. {reason}",
                    notification_type='system',
                    deep_link='stayafrica://verification'
                )
            except Exception as e:
                logger.warning(f"Failed to send verification rejection notification: {e}")
            
            # Send email notification
            try:
                from tasks.email_tasks import send_identity_verification_email
                send_identity_verification_email.delay(verification.user.id, 'rejected', reason)
            except Exception as e:
                logger.warning(f"Failed to queue verification rejection email: {e}")
            
            return Response({
                'message': 'Verification rejected',
                'verification': IdentityVerificationSerializer(verification).data
            })
    
    @action(detail=False, methods=['get'], permission_classes=[IsAdminUser])
    def pending_reviews(self, request):
        """Get all pending verifications for admin review"""
        pending = IdentityVerification.objects.filter(
            status__in=['pending', 'under_review']
        ).select_related('user').order_by('submitted_at')
        
        serializer = IdentityVerificationSerializer(pending, many=True)
        return Response({
            'count': pending.count(),
            'results': serializer.data
        })
    
    @action(detail=False, methods=['get'], permission_classes=[IsAdminUser])
    def statistics(self, request):
        """Get verification statistics for admin"""
        from django.db.models import Count
        from datetime import timedelta
        from django.utils import timezone
        
        # Overall stats
        total = IdentityVerification.objects.count()
        by_status = dict(IdentityVerification.objects.values_list('status').annotate(Count('status')))
        
        # Recent stats (last 30 days)
        thirty_days_ago = timezone.now() - timedelta(days=30)
        recent = IdentityVerification.objects.filter(submitted_at__gte=thirty_days_ago).count()
        recent_approved = IdentityVerification.objects.filter(
            submitted_at__gte=thirty_days_ago,
            status='approved'
        ).count()
        
        # Approval rate
        total_reviewed = IdentityVerification.objects.filter(
            status__in=['approved', 'rejected']
        ).count()
        total_approved = IdentityVerification.objects.filter(status='approved').count()
        approval_rate = (total_approved / total_reviewed * 100) if total_reviewed > 0 else 0
        
        return Response({
            'total_verifications': total,
            'by_status': by_status,
            'recent_submissions': recent,
            'recent_approved': recent_approved,
            'approval_rate': round(approval_rate, 2),
            'pending_review': by_status.get('pending', 0) + by_status.get('under_review', 0),
        })


class VerificationSettingsViewSet(viewsets.ViewSet):
    """ViewSet for verification settings (admin only)"""
    permission_classes = [IsAdminUser]
    
    def list(self, request):
        """Get verification settings"""
        settings = VerificationSettings.get_settings()
        serializer = VerificationSettingsSerializer(settings)
        return Response(serializer.data)
    
    def update(self, request, pk=None):
        """Update verification settings"""
        settings = VerificationSettings.get_settings()
        serializer = VerificationSettingsSerializer(settings, data=request.data, partial=True)
        serializer.is_valid(raise_exception=True)
        serializer.save()
        
        logger.info(f"Verification settings updated by {request.user.email}")
        return Response(serializer.data)
