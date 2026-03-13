from urllib.parse import parse_qs
from channels.db import database_sync_to_async
from django.contrib.auth.models import AnonymousUser
from rest_framework_simplejwt.tokens import AccessToken
from rest_framework_simplejwt.exceptions import TokenError, InvalidToken
from django.contrib.auth import get_user_model

User = get_user_model()

@database_sync_to_async
def get_user_from_token(token):
    try:
        access_token = AccessToken(token)
        user_id = access_token['user_id']
        user = User.objects.get(id=user_id)
        if not user.is_active:
            return AnonymousUser()
        return user
    except (TokenError, InvalidToken, User.DoesNotExist, KeyError):
        return AnonymousUser()

class JWTAuthMiddleware:
    """
    Custom Channels middleware that authenticates users using a JWT token 
    passed in the query string (e.g., ?token=xyz...).
    """
    def __init__(self, inner):
        self.inner = inner

    async def __call__(self, scope, receive, send):
        query_string = scope.get("query_string", b"").decode("utf-8")
        query_params = parse_qs(query_string)
        
        token = query_params.get("token", [None])[0]
        
        if token:
            scope["user"] = await get_user_from_token(token)
        else:
            # Fallback to whatever user was already established (e.g. session cookie)
            # or default to AnonymousUser if none.
            if "user" not in scope:
                scope["user"] = AnonymousUser()
            
        return await self.inner(scope, receive, send)

def JWTAuthMiddlewareStack(inner):
    """
    Wraps the standard Channels AuthMiddlewareStack with our JWT middleware
    so that both session cookies and JWT tokens work.
    """
    from channels.auth import AuthMiddlewareStack
    return JWTAuthMiddleware(AuthMiddlewareStack(inner))
