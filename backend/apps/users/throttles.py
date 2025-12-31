from rest_framework.throttling import UserRateThrottle, AnonRateThrottle


class LoginRateThrottle(UserRateThrottle):
    scope = 'login'


class AnonLoginRateThrottle(AnonRateThrottle):
    scope = 'login'
