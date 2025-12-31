from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import AllowAny, IsAuthenticated
from rest_framework_simplejwt.views import TokenObtainPairView, TokenRefreshView
from apps.users.throttles import LoginRateThrottle, AnonLoginRateThrottle
from rest_framework_simplejwt.tokens import RefreshToken
from django.contrib.auth.password_validation import validate_password
from django.core.exceptions import ValidationError
from django.db import transaction
from apps.users.models import User
from apps.users.serializers import (
    UserSerializer,
    UserProfileSerializer,
    CustomTokenObtainPairSerializer
)
from utils.decorators import api_ratelimit, log_action
from utils.helpers import sanitize_input
from services.audit_logger import AuditLoggerService
import logging

logger = logging.getLogger(__name__)

# Try to import Celery tasks, but gracefully handle if Celery isn't available
try:
    from tasks.email_tasks import send_verification_email
    CELERY_AVAILABLE = True
except (ImportError, ModuleNotFoundError) as e:
    logger.warning(f"Celery tasks not available: {e}")
    CELERY_AVAILABLE = False
    send_verification_email = None

class CustomTokenObtainPairView(TokenObtainPairView):
    serializer_class = CustomTokenObtainPairSerializer
    throttle_classes = [LoginRateThrottle, AnonLoginRateThrottle]

class UserViewSet(viewsets.ModelViewSet):
    queryset = User.objects.all()
    serializer_class = UserSerializer
    permission_classes = [IsAuthenticated]
    
    def get_permissions(self):
        if self.action in ['create', 'register']:
            return [AllowAny()]
        return super().get_permissions()
    
    @action(detail=False, methods=['get', 'put', 'patch'], permission_classes=[IsAuthenticated])
    def profile(self, request):
        """Get or update current user profile"""
        user = request.user
        
        if request.method == 'GET':
            serializer = UserProfileSerializer(user)
            return Response(serializer.data)
        
        # Sanitize text inputs
        if 'bio' in request.data:
            request.data['bio'] = sanitize_input(request.data['bio'])
        if 'first_name' in request.data:
            request.data['first_name'] = sanitize_input(request.data['first_name'])
        if 'last_name' in request.data:
            request.data['last_name'] = sanitize_input(request.data['last_name'])
        
        serializer = UserProfileSerializer(user, data=request.data, partial=True)
        if serializer.is_valid():
            with transaction.atomic():
                updated_user = serializer.save()
                
                # Log the action
                AuditLoggerService.log_action(
                    user=user,
                    action='update',
                    model=User,
                    object_id=user.id,
                    changes={'fields_updated': list(request.data.keys())}
                )
            
            logger.info(f"User profile updated: {user.id}")
            return Response(serializer.data)
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
    
    @action(detail=False, methods=['post'], permission_classes=[IsAuthenticated])
    @log_action('change_password')
    def change_password(self, request):
        """Change password with validation"""
        user = request.user
        old_password = request.data.get('old_password')
        new_password = request.data.get('new_password')
        
        if not old_password or not new_password:
            return Response(
                {'error': 'old_password and new_password are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if not user.check_password(old_password):
            logger.warning(f"Failed password change attempt for user {user.id}")
            return Response(
                {'old_password': 'Wrong password.'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Validate new password strength
        try:
            validate_password(new_password, user)
        except ValidationError as e:
            return Response(
                {'new_password': list(e.messages)},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        with transaction.atomic():
            user.set_password(new_password)
            user.save()
            
            # Log the action
            AuditLoggerService.log_action(
                user=user,
                action='change_password',
                model=User,
                object_id=user.id,
                changes={'action': 'password_changed'}
            )
        
        logger.info(f"Password changed for user {user.id}")
        return Response({'status': 'password changed successfully'})
    
    @action(detail=False, methods=['post'], permission_classes=[AllowAny])
    @log_action('register_user')
    def register(self, request):
        """Register new user with validation"""
        # Sanitize inputs
        if 'email' in request.data:
            request.data['email'] = request.data['email'].lower().strip()
        if 'username' in request.data:
            request.data['username'] = sanitize_input(request.data['username'])
        if 'first_name' in request.data:
            request.data['first_name'] = sanitize_input(request.data['first_name'])
        if 'last_name' in request.data:
            request.data['last_name'] = sanitize_input(request.data['last_name'])
        
        # Validate password strength
        password = request.data.get('password')
        if password:
            try:
                validate_password(password)
            except ValidationError as e:
                return Response(
                    {'password': list(e.messages)},
                    status=status.HTTP_400_BAD_REQUEST
                )
        
        serializer = UserSerializer(data=request.data)
        if serializer.is_valid():
            with transaction.atomic():
                user = serializer.save()
                
                # Send verification email if Celery is available
                if CELERY_AVAILABLE and send_verification_email:
                    try:
                        send_verification_email.delay(user.id)
                    except Exception as e:
                        logger.warning(f"Could not queue verification email: {e}")
                else:
                    logger.info("Celery not available, skipping verification email")
                
                # Log the action
                from django.contrib.contenttypes.models import ContentType
                content_type = ContentType.objects.get_for_model(User)
                AuditLoggerService.log_action(
                    user=user,
                    action='register',
                    content_type=content_type,
                    object_id=user.id,
                    changes={'email': user.email, 'role': user.role}
                )
            
            logger.info(f"New user registered: {user.email}")
            return Response(serializer.data, status=status.HTTP_201_CREATED)
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
    
    @action(detail=False, methods=['post'], permission_classes=[AllowAny])
    def request_password_reset(self, request):
        """
        Request password reset email
        NOTE: Full implementation requires token storage (Redis/Database)
        This is a placeholder that demonstrates the flow
        """
        email = request.data.get('email', '').lower().strip()
        
        if not email:
            return Response(
                {'error': 'Email is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            user = User.objects.get(email=email)
            
            # Generate reset token
            from utils.helpers import generate_verification_token
            import hashlib
            reset_token = generate_verification_token()
            
            # Create secure user identifier
            user_hash = hashlib.sha256(f"{user.id}{user.email}".encode()).hexdigest()[:16]
            
            # TODO: Store reset token in Redis with 1-hour expiry
            # from django.core.cache import cache
            # cache.set(f'reset_{user_hash}', reset_token, timeout=3600)
            
            from tasks.email_tasks import send_password_reset_email
            send_password_reset_email.delay(user.id, reset_token)
            
            logger.info(f"Password reset requested for {email}")
        except User.DoesNotExist:
            # Don't reveal if email exists or not (security best practice)
            logger.warning(f"Password reset requested for non-existent email: {email}")
        
        # Always return success to prevent email enumeration
        return Response({
            'status': 'If the email exists, a password reset link has been sent'
        })
    
    @action(detail=False, methods=['post'], permission_classes=[IsAuthenticated])
    def verify_account(self, request):
        """Verify user account (placeholder for email verification)"""
        user = request.user
        token = request.data.get('token')
        
        # TODO: Verify token from Redis/database
        # For now, just mark as verified
        if not user.is_verified:
            user.is_verified = True
            user.save()
            
            logger.info(f"Account verified for user {user.id}")
            return Response({'status': 'Account verified successfully'})
        
        return Response({'status': 'Account already verified'})

    @action(detail=False, methods=['post'], permission_classes=[IsAuthenticated])
    @log_action('upgrade_to_host')
    def upgrade_to_host(self, request):
        """Allow an authenticated guest to become a host and issue fresh tokens."""
        user = request.user

        if user.role == 'host':
            return Response({'status': 'already_host', 'role': user.role})

        with transaction.atomic():
            user.role = 'host'
            user.save(update_fields=['role'])

            AuditLoggerService.log_action(
                user=user,
                action='upgrade_role',
                model=User,
                object_id=user.id,
                changes={'role': 'host'}
            )

        # Issue fresh tokens so the new role claim is reflected immediately
        refresh = RefreshToken.for_user(user)
        access = refresh.access_token

        profile = UserProfileSerializer(user).data

        return Response({
            'status': 'upgraded',
            'user': profile,
            'access': str(access),
            'refresh': str(refresh),
        }, status=status.HTTP_200_OK)
