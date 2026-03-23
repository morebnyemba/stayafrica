"""
Social Authentication API Views

Provides:
- GET /auth/social/config/  → public client IDs for each provider (no secrets)
- POST /auth/social/<provider>/login/  → exchange OAuth code/token for JWT
"""
from rest_framework import status
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import AllowAny
from rest_framework.response import Response
from django.contrib.auth import get_user_model
from allauth.socialaccount.models import SocialApp
from drf_spectacular.utils import extend_schema, OpenApiResponse, inline_serializer
from rest_framework import serializers
from apps.users.serializers import issue_token_pair
from utils.decorators import api_ratelimit

import jwt
import time
import logging
import requests

User = get_user_model()
logger = logging.getLogger(__name__)


@extend_schema(
    tags=['Social Auth'],
    summary='Get OAuth provider config',
    description='Returns public client IDs for configured social providers. '
                'Never exposes secrets — safe to call from any client.',
    responses={
        200: inline_serializer(
            name='SocialConfigResponse',
            fields={
                'providers': serializers.DictField(
                    child=serializers.DictField(),
                    help_text='Map of provider→{client_id, name}',
                )
            },
        ),
    },
)
@api_view(['GET'])
@permission_classes([AllowAny])
def social_auth_config(request):
    """Return public OAuth client IDs stored in the database."""
    apps = SocialApp.objects.all()
    providers = {}
    for app in apps:
        providers[app.provider] = {
            'client_id': app.client_id,
            'name': app.name,
        }
    return Response({'providers': providers})


# ── OAuth code/token exchange helpers ───────────────────────────────

GOOGLE_TOKEN_URL = 'https://oauth2.googleapis.com/token'
GOOGLE_USERINFO_URL = 'https://www.googleapis.com/oauth2/v2/userinfo'

FACEBOOK_TOKEN_URL = 'https://graph.facebook.com/v21.0/oauth/access_token'
FACEBOOK_USERINFO_URL = 'https://graph.facebook.com/me?fields=id,email,first_name,last_name,picture'

APPLE_TOKEN_URL = 'https://appleid.apple.com/auth/token'
APPLE_KEYS_URL = 'https://appleid.apple.com/auth/keys'


def _exchange_google_code(code: str, redirect_uri: str):
    """Exchange Google auth code for user info."""
    app = SocialApp.objects.filter(provider='google').first()
    if not app:
        return None, 'Google social login is not configured'

    # Exchange code for access token
    token_resp = requests.post(GOOGLE_TOKEN_URL, data={
        'code': code,
        'client_id': app.client_id,
        'client_secret': app.secret,
        'redirect_uri': redirect_uri,
        'grant_type': 'authorization_code',
    }, timeout=15)

    if token_resp.status_code != 200:
        return None, f'Failed to exchange Google code: {token_resp.text}'

    access_token = token_resp.json().get('access_token')

    # Get user info
    user_resp = requests.get(GOOGLE_USERINFO_URL, headers={
        'Authorization': f'Bearer {access_token}',
    }, timeout=10)

    if user_resp.status_code != 200:
        return None, 'Failed to fetch Google user info'

    return user_resp.json(), None


def _exchange_facebook_code(code: str, redirect_uri: str):
    """Exchange Facebook auth code for user info."""
    app = SocialApp.objects.filter(provider='facebook').first()
    if not app:
        return None, 'Facebook social login is not configured'

    token_resp = requests.get(FACEBOOK_TOKEN_URL, params={
        'code': code,
        'client_id': app.client_id,
        'client_secret': app.secret,
        'redirect_uri': redirect_uri,
    }, timeout=15)

    if token_resp.status_code != 200:
        return None, f'Failed to exchange Facebook code: {token_resp.text}'

    access_token = token_resp.json().get('access_token')

    user_resp = requests.get(FACEBOOK_USERINFO_URL, params={
        'access_token': access_token,
    }, timeout=10)

    if user_resp.status_code != 200:
        return None, 'Failed to fetch Facebook user info'

    return user_resp.json(), None


def _generate_apple_client_secret(app):
    """
    Generate a short-lived JWT client secret for Apple Sign In.
    Apple requires: team_id, key_id, and a .p8 private key.
    These are stored in the SocialApp.settings JSON field:
      {"team_id": "...", "key_id": "...", "private_key": "-----BEGIN PRIVATE KEY-----\\n..."}
    """
    settings = app.settings or {}
    team_id = settings.get('team_id', '')
    key_id = settings.get('key_id', '')
    private_key = settings.get('private_key', '')

    if not all([team_id, key_id, private_key]):
        return None

    now = int(time.time())
    payload = {
        'iss': team_id,
        'iat': now,
        'exp': now + 86400 * 180,  # 6 months max
        'aud': 'https://appleid.apple.com',
        'sub': app.client_id,
    }
    return jwt.encode(payload, private_key, algorithm='ES256', headers={'kid': key_id})


def _exchange_apple_code(code: str, redirect_uri: str):
    """Exchange Apple auth code for user info via id_token."""
    app = SocialApp.objects.filter(provider='apple').first()
    if not app:
        return None, 'Apple social login is not configured'

    client_secret = _generate_apple_client_secret(app)
    if not client_secret:
        return None, (
            'Apple Sign In is not fully configured. '
            'Set team_id, key_id, and private_key in the Apple SocialApp settings (Django Admin).'
        )

    # Exchange auth code for tokens
    token_resp = requests.post(APPLE_TOKEN_URL, data={
        'client_id': app.client_id,
        'client_secret': client_secret,
        'code': code,
        'grant_type': 'authorization_code',
        'redirect_uri': redirect_uri,
    }, timeout=15)

    if token_resp.status_code != 200:
        logger.error(f'Apple token exchange failed: {token_resp.text}')
        return None, f'Failed to exchange Apple code: {token_resp.text}'

    token_data = token_resp.json()
    id_token = token_data.get('id_token')
    if not id_token:
        return None, 'Apple did not return an id_token'

    # Decode id_token (Apple's JWT). We verify the signature using Apple's public keys.
    try:
        # Fetch Apple's public keys
        keys_resp = requests.get(APPLE_KEYS_URL, timeout=10)
        apple_keys = keys_resp.json().get('keys', [])

        # Decode header to find matching key
        header = jwt.get_unverified_header(id_token)
        matching_key = next((k for k in apple_keys if k['kid'] == header['kid']), None)
        if not matching_key:
            return None, 'Apple id_token key not found in Apple JWKS'

        public_key = jwt.algorithms.RSAAlgorithm.from_jwk(matching_key)
        claims = jwt.decode(
            id_token,
            public_key,
            algorithms=['RS256'],
            audience=app.client_id,
            issuer='https://appleid.apple.com',
        )
    except jwt.PyJWTError as e:
        logger.error(f'Apple id_token verification failed: {e}')
        return None, f'Apple id_token verification failed: {str(e)}'

    # Apple returns: sub (unique user ID), email (optional, only on first login)
    user_info = {
        'sub': claims.get('sub', ''),
        'email': claims.get('email', ''),
        'email_verified': claims.get('email_verified', False),
    }
    return user_info, None


def _get_or_create_social_user(provider: str, social_id: str, email: str,
                                first_name: str = '', last_name: str = '',
                                email_verified: bool = True, role: str = 'guest'):
    """
    Get or create a user from social provider data.

    Account linking (step 2) only happens when email_verified=True to
    prevent a malicious provider account with an unverified email from
    hijacking an existing local account.
    """
    field_map = {
        'google': 'google_id',
        'facebook': 'facebook_id',
        'apple': 'apple_id',
    }
    id_field = field_map.get(provider)
    if not id_field:
        return None

    # 1. Try to find by social ID
    try:
        user = User.objects.get(**{id_field: social_id})
        return user
    except User.DoesNotExist:
        pass

    # 2. Try to find by email and link — only if provider email is verified
    if email and email_verified:
        try:
            user = User.objects.get(email=email)
            setattr(user, id_field, social_id)
            user.save(update_fields=[id_field])
            logger.info(f'Linked {provider} account (id={social_id}) to existing user {user.email}')
            return user
        except User.DoesNotExist:
            pass

    # 3. Create new user
    if not email:
        return None

    # Only allow 'guest' or 'host' roles on creation
    if role not in ('guest', 'host'):
        role = 'guest'

    user = User.objects.create_user(
        email=email,
        username=email,
        first_name=first_name or '',
        last_name=last_name or '',
        role=role,
        is_verified=True,  # Social accounts are pre-verified
        **{id_field: social_id},
    )
    return user


@extend_schema(
    tags=['Social Auth'],
    summary='Social login (code exchange)',
    description=(
        'Exchange an OAuth authorization code (or access token) for JWT tokens. '
        'Supports google, facebook, apple.'
    ),
    request=inline_serializer(
        name='SocialLoginRequest',
        fields={
            'code': serializers.CharField(required=False, help_text='OAuth authorization code'),
            'access_token': serializers.CharField(required=False, help_text='OAuth access token (alternative)'),
            'redirect_uri': serializers.CharField(required=True),
            'role': serializers.ChoiceField(choices=['guest', 'host'], required=False, help_text='Desired role for new accounts (default: guest)'),
        },
    ),
    responses={
        200: OpenApiResponse(description='JWT tokens + user'),
        400: OpenApiResponse(description='Missing or invalid data'),
        401: OpenApiResponse(description='Authentication failed'),
    },
)
@api_view(['POST'])
@permission_classes([AllowAny])
@api_ratelimit(rate='20/m')
def social_login(request, provider):
    """
    Exchange OAuth code / access_token for StayAfrica JWT.

    1. If `code` is provided → exchange with provider for access token → get user info
    2. If `access_token` is provided → use it directly to get user info
    3. Find or create local user
    4. Issue JWT
    """
    code = request.data.get('code')
    access_token = request.data.get('access_token')
    redirect_uri = request.data.get('redirect_uri', '')
    requested_role = request.data.get('role', 'guest')

    if not code and not access_token:
        return Response(
            {'detail': 'Either code or access_token is required'},
            status=status.HTTP_400_BAD_REQUEST,
        )

    user_info = None
    error_msg = None

    # ── Direct access_token path (mobile flows) ─────────────────
    if access_token and not code:
        if provider == 'google':
            resp = requests.get(GOOGLE_USERINFO_URL, headers={
                'Authorization': f'Bearer {access_token}',
            }, timeout=10)
            if resp.status_code == 200:
                user_info = resp.json()
            else:
                error_msg = 'Invalid Google access token'

        elif provider == 'facebook':
            resp = requests.get(FACEBOOK_USERINFO_URL, params={
                'access_token': access_token,
            }, timeout=10)
            if resp.status_code == 200:
                user_info = resp.json()
            else:
                error_msg = 'Invalid Facebook access token'

        elif provider == 'apple':
            error_msg = 'Apple login via access_token is not supported'

        else:
            error_msg = f'Unknown provider: {provider}'

    # ── Authorization code path (web flows) ──────────────────────
    elif code:
        if provider == 'google':
            user_info, error_msg = _exchange_google_code(code, redirect_uri)
        elif provider == 'facebook':
            user_info, error_msg = _exchange_facebook_code(code, redirect_uri)
        elif provider == 'apple':
            user_info, error_msg = _exchange_apple_code(code, redirect_uri)
        else:
            error_msg = f'Unknown provider: {provider}'

    if error_msg:
        return Response({'detail': error_msg}, status=status.HTTP_401_UNAUTHORIZED)

    if not user_info:
        return Response(
            {'detail': 'Could not retrieve user information from provider'},
            status=status.HTTP_401_UNAUTHORIZED,
        )

    # ── Normalise user info across providers ─────────────────────
    email_verified = True  # Default: Google always verifies; Facebook see below
    if provider == 'google':
        social_id = str(user_info.get('id', ''))
        email = user_info.get('email', '')
        first_name = user_info.get('given_name', '')
        last_name = user_info.get('family_name', '')
        email_verified = user_info.get('verified_email', True)
    elif provider == 'facebook':
        social_id = str(user_info.get('id', ''))
        email = user_info.get('email', '')
        first_name = user_info.get('first_name', '')
        last_name = user_info.get('last_name', '')
        # Facebook does NOT guarantee email is verified
        email_verified = False
    elif provider == 'apple':
        social_id = str(user_info.get('sub', user_info.get('id', '')))
        email = user_info.get('email', '')
        first_name = user_info.get('name', {}).get('firstName', '') if isinstance(user_info.get('name'), dict) else ''
        last_name = user_info.get('name', {}).get('lastName', '') if isinstance(user_info.get('name'), dict) else ''
        email_verified = user_info.get('email_verified', False)
    else:
        return Response({'detail': f'Unknown provider: {provider}'}, status=status.HTTP_400_BAD_REQUEST)

    if not social_id:
        return Response({'detail': 'No social ID returned by provider'}, status=status.HTTP_401_UNAUTHORIZED)

    # ── Get or create user ───────────────────────────────────────
    user = _get_or_create_social_user(provider, social_id, email, first_name, last_name, email_verified=email_verified, role=requested_role)
    if not user:
        return Response(
            {'detail': 'Unable to create or find user account. An email address is required.'},
            status=status.HTTP_400_BAD_REQUEST,
        )

    # ── Issue JWT ────────────────────────────────────────────────
    token_pair = issue_token_pair(user)

    return Response({
        'access': token_pair['access'],
        'refresh': token_pair['refresh'],
        'user': {
            'id': user.id,
            'email': user.email,
            'username': getattr(user, 'username', ''),
            'first_name': user.first_name,
            'last_name': user.last_name,
            'role': getattr(user, 'role', 'guest'),
            'is_verified': getattr(user, 'is_verified', False),
            'two_factor_enabled': getattr(user, 'two_factor_enabled', False),
        },
    })
