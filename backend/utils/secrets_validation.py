"""
Startup validation for required environment variables and secrets.

Run this as part of Django's AppConfig.ready() or as a management command
to catch misconfiguration before the app serves requests.
"""
import os
import sys
import logging

logger = logging.getLogger(__name__)

# Required in production (DEBUG=False)
REQUIRED_PRODUCTION_VARS = [
    'SECRET_KEY',
    'DATABASE_URL',
    'ALLOWED_HOSTS',
]

# Strongly recommended in production
RECOMMENDED_PRODUCTION_VARS = [
    'SENTRY_DSN',
    'REDIS_URL',
    'CORS_ALLOWED_ORIGINS',
]

# Dangerous default values that must be changed
DANGEROUS_DEFAULTS = {
    'SECRET_KEY': [
        'django-insecure-dev-key-change-in-production',
        'django-insecure',
        'change-me',
        'your-secret-key',
    ],
}


def validate_secrets(fail_hard=False):
    """
    Validate that required environment variables are set and secure.
    
    Args:
        fail_hard: If True, raise SystemExit on critical errors.
                   Set to True in production entrypoints.
    
    Returns:
        dict with 'errors', 'warnings', and 'valid' keys.
    """
    is_production = os.getenv('DEBUG', 'False') != 'True'
    errors = []
    warnings = []

    if not is_production:
        logger.info("Secrets validation: running in development mode, relaxed checks")
        return {'errors': [], 'warnings': [], 'valid': True}

    # Check required vars
    for var in REQUIRED_PRODUCTION_VARS:
        value = os.getenv(var)
        if not value:
            errors.append(f"Missing required env var: {var}")

    # Check for dangerous defaults
    for var, dangerous_values in DANGEROUS_DEFAULTS.items():
        value = os.getenv(var, '')
        for dangerous in dangerous_values:
            if dangerous in value.lower():
                errors.append(
                    f"CRITICAL: {var} contains insecure default value. "
                    f"Set a strong random value in production."
                )
                break

    # Check SECRET_KEY length
    secret_key = os.getenv('SECRET_KEY', '')
    if len(secret_key) < 50:
        warnings.append(
            f"SECRET_KEY is only {len(secret_key)} chars. "
            f"Recommend 50+ chars for production."
        )

    # Check recommended vars
    for var in RECOMMENDED_PRODUCTION_VARS:
        if not os.getenv(var):
            warnings.append(f"Recommended env var not set: {var}")

    # Check database isn't SQLite in production
    db_engine = os.getenv('DATABASE_URL', '')
    if 'sqlite' in db_engine.lower():
        errors.append("SQLite detected in DATABASE_URL. Use PostgreSQL in production.")

    # Log results
    valid = len(errors) == 0
    if errors:
        for error in errors:
            logger.error(f"[SECRETS] {error}")
    if warnings:
        for warning in warnings:
            logger.warning(f"[SECRETS] {warning}")
    if valid:
        logger.info("[SECRETS] All production secrets validated successfully")

    if not valid and fail_hard:
        logger.critical(
            "[SECRETS] Production startup blocked due to missing/insecure secrets. "
            "Fix the errors above and restart."
        )
        sys.exit(1)

    return {'errors': errors, 'warnings': warnings, 'valid': valid}
