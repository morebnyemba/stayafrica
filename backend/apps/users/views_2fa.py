"""
Two-Factor Authentication API Views
"""
from rest_framework import status
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response
from rest_framework_simplejwt.tokens import RefreshToken
from django.contrib.auth import authenticate, get_user_model
from drf_spectacular.utils import extend_schema, OpenApiResponse

from .two_factor import TwoFactorService
from .serializers_2fa import (
    TwoFactorSetupSerializer,
    TwoFactorVerifySerializer,
    TwoFactorEnableSerializer,
    TwoFactorDisableSerializer,
    TwoFactorStatusSerializer,
    BackupCodesRegenerateSerializer,
    BackupCodeVerifySerializer,
    LoginWith2FASerializer
)

User = get_user_model()


@extend_schema(
    tags=['2FA'],
    summary='Setup 2FA',
    description='Generate QR code and backup codes for 2FA setup',
    responses={
        200: OpenApiResponse(
            response=TwoFactorSetupSerializer,
            description='2FA setup data'
        )
    }
)
@api_view(['POST'])
@permission_classes([IsAuthenticated])
def setup_2fa(request):
    """
    Setup 2FA for the authenticated user
    Returns QR code and backup codes
    """
    user = request.user
    
    # Setup 2FA
    secret, qr_code, backup_codes = TwoFactorService.setup_2fa(user)
    
    return Response({
        'secret': secret,
        'qr_code': qr_code,
        'backup_codes': backup_codes,
        'message': 'Scan the QR code with your authenticator app, then verify with a 6-digit code to enable 2FA'
    })


@extend_schema(
    tags=['2FA'],
    summary='Enable 2FA',
    description='Enable 2FA after verifying TOTP token',
    request=TwoFactorEnableSerializer,
    responses={
        200: OpenApiResponse(description='2FA enabled successfully'),
        400: OpenApiResponse(description='Invalid token')
    }
)
@api_view(['POST'])
@permission_classes([IsAuthenticated])
def enable_2fa(request):
    """
    Enable 2FA after verifying setup token
    """
    serializer = TwoFactorEnableSerializer(data=request.data)
    serializer.is_valid(raise_exception=True)
    
    user = request.user
    token = serializer.validated_data['token']
    
    if TwoFactorService.enable_2fa(user, token):
        return Response({
            'message': '2FA enabled successfully',
            'two_factor_enabled': True
        })
    
    return Response(
        {'error': 'Invalid verification code'},
        status=status.HTTP_400_BAD_REQUEST
    )


@extend_schema(
    tags=['2FA'],
    summary='Disable 2FA',
    description='Disable 2FA after password verification',
    request=TwoFactorDisableSerializer,
    responses={
        200: OpenApiResponse(description='2FA disabled successfully'),
        400: OpenApiResponse(description='Invalid password')
    }
)
@api_view(['POST'])
@permission_classes([IsAuthenticated])
def disable_2fa(request):
    """
    Disable 2FA after password verification
    """
    serializer = TwoFactorDisableSerializer(data=request.data)
    serializer.is_valid(raise_exception=True)
    
    user = request.user
    password = serializer.validated_data['password']
    
    # Verify password
    if not user.check_password(password):
        return Response(
            {'error': 'Invalid password'},
            status=status.HTTP_400_BAD_REQUEST
        )
    
    TwoFactorService.disable_2fa(user)
    
    return Response({
        'message': '2FA disabled successfully',
        'two_factor_enabled': False
    })


@extend_schema(
    tags=['2FA'],
    summary='Get 2FA status',
    description='Check if 2FA is enabled for the authenticated user',
    responses={
        200: OpenApiResponse(response=TwoFactorStatusSerializer)
    }
)
@api_view(['GET'])
@permission_classes([IsAuthenticated])
def get_2fa_status(request):
    """
    Get 2FA status for the authenticated user
    """
    serializer = TwoFactorStatusSerializer(request.user)
    return Response(serializer.data)


@extend_schema(
    tags=['2FA'],
    summary='Verify 2FA token',
    description='Verify TOTP token (for testing setup before enabling)',
    request=TwoFactorVerifySerializer,
    responses={
        200: OpenApiResponse(description='Token is valid'),
        400: OpenApiResponse(description='Invalid token')
    }
)
@api_view(['POST'])
@permission_classes([IsAuthenticated])
def verify_token(request):
    """
    Verify a TOTP token (for testing)
    """
    serializer = TwoFactorVerifySerializer(data=request.data)
    serializer.is_valid(raise_exception=True)
    
    user = request.user
    token = serializer.validated_data['token']
    
    if not user.two_factor_secret:
        return Response(
            {'error': '2FA not setup yet'},
            status=status.HTTP_400_BAD_REQUEST
        )
    
    if TwoFactorService.verify_token(user.two_factor_secret, token):
        return Response({'message': 'Token is valid', 'valid': True})
    
    return Response(
        {'error': 'Invalid token', 'valid': False},
        status=status.HTTP_400_BAD_REQUEST
    )


@extend_schema(
    tags=['2FA'],
    summary='Regenerate backup codes',
    description='Generate new backup codes (invalidates old ones)',
    request=BackupCodesRegenerateSerializer,
    responses={
        200: OpenApiResponse(description='New backup codes generated'),
        400: OpenApiResponse(description='Invalid password or 2FA not enabled')
    }
)
@api_view(['POST'])
@permission_classes([IsAuthenticated])
def regenerate_backup_codes(request):
    """
    Regenerate backup codes
    """
    serializer = BackupCodesRegenerateSerializer(data=request.data)
    serializer.is_valid(raise_exception=True)
    
    user = request.user
    password = serializer.validated_data['password']
    
    # Verify password
    if not user.check_password(password):
        return Response(
            {'error': 'Invalid password'},
            status=status.HTTP_400_BAD_REQUEST
        )
    
    if not user.two_factor_enabled:
        return Response(
            {'error': '2FA is not enabled'},
            status=status.HTTP_400_BAD_REQUEST
        )
    
    backup_codes = TwoFactorService.regenerate_backup_codes(user)
    
    return Response({
        'message': 'Backup codes regenerated successfully',
        'backup_codes': backup_codes
    })


@extend_schema(
    tags=['Auth'],
    summary='Login with 2FA',
    description='Login with email/password and optional 2FA token',
    request=LoginWith2FASerializer,
    responses={
        200: OpenApiResponse(description='Login successful'),
        401: OpenApiResponse(description='Invalid credentials or 2FA token required')
    }
)
@api_view(['POST'])
def login_with_2fa(request):
    """
    Login with 2FA support
    If 2FA is enabled, token or backup_code is required
    """
    serializer = LoginWith2FASerializer(data=request.data)
    serializer.is_valid(raise_exception=True)
    
    email = serializer.validated_data['email']
    password = serializer.validated_data['password']
    token = serializer.validated_data.get('token')
    backup_code = serializer.validated_data.get('backup_code')
    
    # Authenticate user
    try:
        user = User.objects.get(email=email)
    except User.DoesNotExist:
        return Response(
            {'error': 'Invalid credentials'},
            status=status.HTTP_401_UNAUTHORIZED
        )
    
    # Check password
    if not user.check_password(password):
        return Response(
            {'error': 'Invalid credentials'},
            status=status.HTTP_401_UNAUTHORIZED
        )
    
    # Check if 2FA is enabled
    if user.two_factor_enabled:
        # Verify token or backup code
        if token:
            if not TwoFactorService.verify_token(user.two_factor_secret, token):
                return Response(
                    {'error': 'Invalid 2FA token'},
                    status=status.HTTP_401_UNAUTHORIZED
                )
        elif backup_code:
            if not TwoFactorService.verify_backup_code(user, backup_code):
                return Response(
                    {'error': 'Invalid backup code'},
                    status=status.HTTP_401_UNAUTHORIZED
                )
        else:
            # 2FA enabled but no token/backup_code provided
            return Response(
                {
                    'error': '2FA token required',
                    'two_factor_required': True
                },
                status=status.HTTP_401_UNAUTHORIZED
            )
    
    # Generate tokens
    refresh = RefreshToken.for_user(user)
    
    return Response({
        'refresh': str(refresh),
        'access': str(refresh.access_token),
        'user': {
            'id': user.id,
            'email': user.email,
            'username': user.username,
            'role': user.role,
            'two_factor_enabled': user.two_factor_enabled
        }
    })
