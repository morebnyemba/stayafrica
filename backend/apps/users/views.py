from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import AllowAny, IsAuthenticated, IsAdminUser
from rest_framework_simplejwt.views import TokenObtainPairView, TokenRefreshView
from apps.users.throttles import LoginRateThrottle, AnonLoginRateThrottle
from django.contrib.auth.password_validation import validate_password
from django.core.exceptions import ValidationError
from django.core import signing
from django.core.signing import BadSignature, SignatureExpired
from django.db import transaction
from apps.users.models import User, UserPreference, UserPropertyInteraction
from apps.users.serializers import (
    UserSerializer,
    UserProfileSerializer,
    CustomTokenObtainPairSerializer,
    issue_token_pair,
    UserPreferenceSerializer,
    UserPropertyInteractionSerializer
)
from utils.decorators import api_ratelimit, log_action
from utils.helpers import sanitize_input
from services.audit_logger import AuditLoggerService
from urllib.parse import urlparse
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
    lookup_value_regex = r'\d+'  # Only match numeric IDs, so /users/verification/ won't conflict
    
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
        
        try:
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
                    from django.contrib.contenttypes.models import ContentType
                    content_type = ContentType.objects.get_for_model(User)
                    AuditLoggerService.log_action(
                        user=user,
                        action='update',
                        content_type=content_type,
                        object_id=user.id,
                        changes={'fields_updated': list(request.data.keys())}
                    )
                
                logger.info(f"User profile updated: {user.id}")
                return Response(serializer.data)
            
            logger.error(f"Profile update validation failed for user {user.id}: {serializer.errors}")
            return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            logger.error(f"Error updating profile for user {user.id}: {str(e)}", exc_info=True)
            return Response(
                {'error': 'Failed to update profile. Please check your input and try again.'},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
    
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
            try:
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
            except Exception as e:
                # Catch IntegrityError for duplicate email/username
                error_str = str(e).lower()
                if 'email' in error_str or 'unique' in error_str:
                    return Response(
                        {'email': ['An account with this email already exists.']},
                        status=status.HTTP_400_BAD_REQUEST
                    )
                logger.error(f"Registration error: {e}", exc_info=True)
                return Response(
                    {'detail': 'Registration failed. Please try again.'},
                    status=status.HTTP_500_INTERNAL_SERVER_ERROR
                )
            
            logger.info(f"New user registered: {user.email}")
            
            # Generate JWT tokens for auto-login after registration
            token_pair = issue_token_pair(user)
            
            response_data = serializer.data
            response_data['access'] = token_pair['access']
            response_data['refresh'] = token_pair['refresh']
            
            return Response(response_data, status=status.HTTP_201_CREATED)
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
    
    @action(detail=False, methods=['post'], permission_classes=[AllowAny])
    def request_password_reset(self, request):
        """
        Request password reset email.
        Always returns a generic success response to prevent email enumeration.
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
            reset_token = generate_verification_token()
            
            # Store reset token in Redis with 1-hour expiry
            from django.core.cache import cache
            cache_key = f'reset_token_{reset_token}'
            cache.set(cache_key, user.id, timeout=3600)
            cached_user_id = cache.get(cache_key)

            # Signed token fallback: survives cross-instance cache inconsistencies.
            signed_reset_token = signing.dumps(
                {
                    't': reset_token,
                    'u': user.id,
                    'ph': user.password,
                },
                salt='password-reset',
            )

            # Prefer the request origin so reset links stay in the same environment
            # (dev/staging/prod) that generated the token.
            frontend_url = None
            origin = request.headers.get('Origin')
            referer = request.headers.get('Referer')
            if origin:
                frontend_url = origin.rstrip('/')
            elif referer:
                parsed = urlparse(referer)
                if parsed.scheme and parsed.netloc:
                    frontend_url = f"{parsed.scheme}://{parsed.netloc}"
            
            from tasks.email_tasks import send_password_reset_email
            # Send password reset inline so it works even when Celery workers are offline.
            send_password_reset_email(user.id, signed_reset_token, frontend_url=frontend_url)
            
            logger.info(
                "Password reset requested for %s; token stored=%s; cache_key=%s; frontend_url=%s; signed_token_issued=%s",
                email,
                cached_user_id == user.id,
                cache_key,
                frontend_url,
                True,
            )
        except User.DoesNotExist:
            # Don't reveal if email exists or not (security best practice)
            logger.warning(f"Password reset requested for non-existent email: {email}")
        
        # Always return success to prevent email enumeration
        return Response({
            'status': 'If the email exists, a password reset link has been sent'
        })

    @action(detail=False, methods=['post'], permission_classes=[AllowAny])
    def confirm_password_reset(self, request):
        """Confirm password reset using a one-time token from email."""
        token = request.data.get('token', '').strip()
        new_password = request.data.get('new_password', '')
        confirm_password = request.data.get('confirm_password', '')

        if not token or not new_password:
            return Response(
                {'error': 'token and new_password are required'},
                status=status.HTTP_400_BAD_REQUEST
            )

        if confirm_password and new_password != confirm_password:
            return Response(
                {'confirm_password': ['Passwords do not match']},
                status=status.HTTP_400_BAD_REQUEST
            )

        from django.core.cache import cache
        cache_key = f'reset_token_{token}'
        user_id = cache.get(cache_key)
        used_signed_fallback = False
        signed_payload = None

        # Try signed token first to recover from cross-instance cache misses.
        try:
            signed_payload = signing.loads(token, salt='password-reset', max_age=3600)
            signed_raw_token = signed_payload.get('t')
            if signed_raw_token:
                signed_cache_key = f'reset_token_{signed_raw_token}'
                signed_cached_user_id = cache.get(signed_cache_key)
                if signed_cached_user_id:
                    user_id = signed_cached_user_id
                    cache_key = signed_cache_key
        except (BadSignature, SignatureExpired):
            signed_payload = None

        logger.info(
            "Password reset confirmation attempt; cache_hit=%s; cache_key=%s; token_prefix=%s; signed_payload=%s",
            bool(user_id),
            cache_key,
            token[:8],
            bool(signed_payload),
        )

        user = None
        if user_id:
            try:
                user = User.objects.get(id=user_id)
            except User.DoesNotExist:
                cache.delete(cache_key)
                return Response(
                    {'error': 'Invalid password reset request.'},
                    status=status.HTTP_400_BAD_REQUEST
                )
        elif signed_payload:
            # Fallback path: trust signed payload if it still matches current password hash.
            payload_user_id = signed_payload.get('u')
            payload_password_hash = signed_payload.get('ph')
            if payload_user_id and payload_password_hash:
                try:
                    user = User.objects.get(id=payload_user_id)
                    if user.password == payload_password_hash:
                        used_signed_fallback = True
                    else:
                        user = None
                except User.DoesNotExist:
                    user = None

        if not user:
            logger.warning(
                "Password reset token not found or expired; cache_key=%s; token_prefix=%s",
                cache_key,
                token[:8],
            )
            return Response(
                {'error': 'Password reset link has expired or is invalid.'},
                status=status.HTTP_400_BAD_REQUEST
            )

        # Validate new password strength against Django password validators.
        try:
            validate_password(new_password, user)
        except ValidationError as e:
            return Response(
                {'new_password': list(e.messages)},
                status=status.HTTP_400_BAD_REQUEST
            )

        with transaction.atomic():
            user.set_password(new_password)
            user.save(update_fields=['password'])

            AuditLoggerService.log_action(
                user=user,
                action='password_reset',
                model=User,
                object_id=user.id,
                changes={'action': 'password_reset_completed'}
            )

        # Single-use token: remove immediately after successful reset.
        cache.delete(cache_key)
        if signed_payload and signed_payload.get('t'):
            cache.delete(f"reset_token_{signed_payload.get('t')}")

        logger.info(
            "Password reset completed for user %s; signed_fallback=%s",
            user.id,
            used_signed_fallback,
        )
        return Response({'status': 'Password has been reset successfully'})
    
    @action(detail=False, methods=['post'], permission_classes=[AllowAny])
    def verify_email(self, request):
        """
        Verify user email with token
        POST body: {"user_hash": "...", "token": "..."}
        """
        from django.core.cache import cache
        
        user_hash = request.data.get('user_hash')
        token = request.data.get('token')
        
        if not user_hash or not token:
            return Response(
                {'error': 'user_hash and token are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Look up the token in cache
        cache_key = f'verify_{user_hash}'
        stored_data = cache.get(cache_key)
        
        if not stored_data:
            return Response(
                {'error': 'Verification link has expired or is invalid. Please request a new one.'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if stored_data.get('token') != token:
            return Response(
                {'error': 'Invalid verification token'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Get the user
        user_id = stored_data.get('user_id')
        try:
            user = User.objects.get(id=user_id)
        except User.DoesNotExist:
            return Response(
                {'error': 'User not found'},
                status=status.HTTP_404_NOT_FOUND
            )
        
        if user.is_verified:
            # Delete the token since it's been used
            cache.delete(cache_key)
            return Response({'status': 'Account already verified'})
        
        # Mark user as verified
        with transaction.atomic():
            user.is_verified = True
            user.save(update_fields=['is_verified'])
            
            from django.contrib.contenttypes.models import ContentType
            content_type = ContentType.objects.get_for_model(User)
            AuditLoggerService.log_action(
                user=user,
                action='email_verified',
                content_type=content_type,
                object_id=user.id,
                changes={'is_verified': True}
            )
        
        # Delete the verification token
        cache.delete(cache_key)
        
        logger.info(f"Email verified for user {user.id}")
        return Response({
            'status': 'Email verified successfully',
            'message': 'Your email has been verified. You can now log in.'
        })

    @action(detail=False, methods=['post'], permission_classes=[IsAuthenticated])
    def resend_verification(self, request):
        """Resend verification email to the current user"""
        user = request.user
        
        if user.is_verified:
            return Response({'status': 'Account already verified'})
        
        # Send verification email
        if CELERY_AVAILABLE and send_verification_email:
            try:
                send_verification_email.delay(user.id)
                logger.info(f"Verification email resent for user {user.id}")
                return Response({
                    'status': 'Verification email sent',
                    'message': 'Please check your email for the verification link.'
                })
            except Exception as e:
                logger.warning(f"Could not queue verification email: {e}")
                return Response(
                    {'error': 'Failed to send verification email. Please try again later.'},
                    status=status.HTTP_500_INTERNAL_SERVER_ERROR
                )
        else:
            return Response(
                {'error': 'Email service is not available'},
                status=status.HTTP_503_SERVICE_UNAVAILABLE
            )
    
    @action(detail=False, methods=['post'], permission_classes=[IsAuthenticated])
    def verify_account(self, request):
        """Verify user account (placeholder for email verification)"""
        user = request.user
        token = request.data.get('token')
        
        # Verify token from Redis cache
        from django.core.cache import cache
        import hashlib
        user_hash = hashlib.sha256(f"{user.id}{user.email}".encode()).hexdigest()[:16]
        stored_token = cache.get(f'reset_{user_hash}')
        
        if token and stored_token and token != stored_token:
            return Response(
                {'error': 'Invalid or expired verification token'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Token valid or no token required — clear from cache
        if stored_token:
            cache.delete(f'reset_{user_hash}')
        
        if not user.is_verified:
            user.is_verified = True
            user.save()
            
            logger.info(f"Account verified for user {user.id}")
            return Response({'status': 'Account verified successfully'})
        
        return Response({'status': 'Account already verified'})

    @action(detail=False, methods=['post'], permission_classes=[IsAuthenticated])
    def refresh_session(self, request):
        """Issue fresh tokens so middleware-visible claims match the current user record."""
        user = request.user

        token_pair = issue_token_pair(user)
        profile = UserProfileSerializer(user).data

        return Response({
            'status': 'session_refreshed',
            'user': profile,
            'access': token_pair['access'],
            'refresh': token_pair['refresh'],
        }, status=status.HTTP_200_OK)

    @action(detail=True, methods=['get'], permission_classes=[IsAuthenticated])
    def preferences(self, request, pk=None):
        """Admin or Self action to get a user's preferences"""
        user = self.get_object()
        if request.user != user and not request.user.is_staff:
            return Response(
                {'error': 'You do not have permission to view these preferences.'},
                status=status.HTTP_403_FORBIDDEN
            )
            
        prefs, _ = UserPreference.objects.get_or_create(user=user)
        serializer = UserPreferenceSerializer(prefs)
        return Response(serializer.data)

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
        token_pair = issue_token_pair(user)

        profile = UserProfileSerializer(user).data

        return Response({
            'status': 'upgraded',
            'user': profile,
            'access': token_pair['access'],
            'refresh': token_pair['refresh'],
        }, status=status.HTTP_200_OK)

    @action(detail=False, methods=['post'], permission_classes=[IsAuthenticated])
    @log_action('switch_profile')
    def switch_profile(self, request):
        """Switch active profile between 'guest' and 'host' mode.
        Only users with role='host' or role='admin' can switch to host mode.
        """
        user = request.user
        target_profile = request.data.get('profile')

        if target_profile not in ['guest', 'host']:
            return Response(
                {'error': "Invalid profile. Must be 'guest' or 'host'."},
                status=status.HTTP_400_BAD_REQUEST
            )

        # Security check: Only actual hosts/admins can activate host mode
        if target_profile == 'host' and user.role not in ['host', 'admin']:
            return Response(
                {'error': 'You must be a registered host to switch to host mode. Please upgrade your account first.'},
                status=status.HTTP_403_FORBIDDEN
            )

        if user.active_profile == target_profile:
            return Response({'status': 'already_in_mode', 'active_profile': user.active_profile})

        with transaction.atomic():
            user.active_profile = target_profile
            user.save(update_fields=['active_profile'])

            AuditLoggerService.log_action(
                user=user,
                action='switch_profile',
                model=User,
                object_id=user.id,
                changes={'active_profile': target_profile}
            )

        # Issue fresh tokens so the new active_profile claim is reflected immediately
        token_pair = issue_token_pair(user)

        profile = UserProfileSerializer(user).data

        logger.info(f"User {user.id} switched active_profile to {target_profile}")
        return Response({
            'status': 'switched',
            'active_profile': target_profile,
            'user': profile,
            'access': token_pair['access'],
            'refresh': token_pair['refresh'],
        }, status=status.HTTP_200_OK)

    @action(detail=True, methods=['post'], permission_classes=[IsAdminUser])
    def verify(self, request, pk=None):
        """Admin action to verify a user's identity/email."""
        user = self.get_object()
        user.is_verified = True
        user.save()
        
        # Log the action
        from services.audit_logger import AuditLoggerService
        AuditLoggerService.log_action(
            user=request.user,
            action='verify_user',
            model=User,
            object_id=user.id,
            changes={'is_verified': True}
        )
        
        serializer = self.get_serializer(user)
        return Response(serializer.data)

    @action(detail=True, methods=['post'], permission_classes=[IsAdminUser])
    def suspend(self, request, pk=None):
        """Admin action to suspend a user."""
        user = self.get_object()
        reason = request.data.get('reason', 'No reason provided')
        
        user.is_active = False
        user.save()
        
        # Log the action
        from services.audit_logger import AuditLoggerService
        AuditLoggerService.log_action(
            user=request.user,
            action='suspend_user',
            model=User,
            object_id=user.id,
            changes={'is_active': False, 'reason': reason}
        )
        
        serializer = self.get_serializer(user)
        return Response(serializer.data)


class UserPreferenceViewSet(viewsets.ModelViewSet):
    """Manage user preferences for personalized recommendations"""
    serializer_class = UserPreferenceSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        return UserPreference.objects.filter(user=self.request.user)
    
    def get_object(self):
        # Get or create preferences for the current user
        obj, created = UserPreference.objects.get_or_create(user=self.request.user)
        return obj
    
    @action(detail=False, methods=['get'])
    def my_preferences(self, request):
        """Get current user's preferences"""
        preferences, created = UserPreference.objects.get_or_create(user=request.user)
        serializer = self.get_serializer(preferences)
        return Response(serializer.data)
    
    @action(detail=False, methods=['post', 'put', 'patch'])
    def update_preferences(self, request):
        """Update current user's preferences"""
        preferences, created = UserPreference.objects.get_or_create(user=request.user)
        serializer = self.get_serializer(preferences, data=request.data, partial=True)
        if serializer.is_valid():
            serializer.save()
            return Response(serializer.data)
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
    
    @action(detail=False, methods=['post'])
    def update_location(self, request):
        """Update user's last known location"""
        lat = request.data.get('latitude')
        lng = request.data.get('longitude')
        
        if not lat or not lng:
            return Response(
                {'error': 'latitude and longitude are required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        preferences, created = UserPreference.objects.get_or_create(user=request.user)
        preferences.last_latitude = float(lat)
        preferences.last_longitude = float(lng)
        preferences.save()
        
        return Response({'status': 'location updated'})

class UserPropertyInteractionViewSet(viewsets.ModelViewSet):
    """Track user interactions with properties"""
    serializer_class = UserPropertyInteractionSerializer
    permission_classes = [IsAuthenticated]
    http_method_names = ['get', 'post']  # Only allow read and create
    
    def get_queryset(self):
        return UserPropertyInteraction.objects.filter(user=self.request.user)
    
    def perform_create(self, serializer):
        serializer.save(user=self.request.user)
    
    @action(detail=False, methods=['post'])
    def track_view(self, request):
        """Track when user views a property"""
        property_id = request.data.get('property_id')
        duration = request.data.get('duration_seconds', 0)
        
        if not property_id:
            return Response(
                {'error': 'property_id is required'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        interaction = UserPropertyInteraction.objects.create(
            user=request.user,
            property_id=property_id,
            interaction_type='view',
            viewed_duration_seconds=duration
        )
        
        serializer = self.get_serializer(interaction)
        return Response(serializer.data, status=status.HTTP_201_CREATED)

