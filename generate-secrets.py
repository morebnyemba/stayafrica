#!/usr/bin/env python3
"""
Generate Secure Secrets for StayAfrica Production Deployment

This script generates cryptographically secure secrets for the .env file.
Run this script and copy the output to your .env file.

Usage:
    python generate-secrets.py
"""

import secrets
import string

def generate_django_secret_key(length=50):
    """Generate a Django SECRET_KEY"""
    chars = string.ascii_letters + string.digits + '!@#$%^&*(-_=+)'
    return ''.join(secrets.choice(chars) for _ in range(length))

def generate_password(length=32):
    """Generate a secure password"""
    # Use alphanumeric + special characters
    alphabet = string.ascii_letters + string.digits + '!@#$%^&*-_=+'
    return ''.join(secrets.choice(alphabet) for _ in range(length))

def generate_hex_token(length=64):
    """Generate a hex token"""
    return secrets.token_hex(length // 2)

def main():
    print("=" * 80)
    print("StayAfrica - Secure Secrets Generator")
    print("=" * 80)
    print("\nCopy these values to your .env file:\n")
    print("-" * 80)
    
    print("\n# Django Core")
    print(f"SECRET_KEY={generate_django_secret_key()}")
    
    print("\n# Database")
    print(f"DB_PASSWORD={generate_password()}")
    
    print("\n# Redis")
    print(f"REDIS_PASSWORD={generate_password()}")
    
    print("\n# JWT")
    print(f"JWT_SECRET_KEY={generate_hex_token()}")
    
    print("\n" + "-" * 80)
    print("\n✅ Secrets generated successfully!")
    print("\n⚠️  IMPORTANT:")
    print("   1. Copy these values to your .env file")
    print("   2. Keep these secrets secure and never commit them to git")
    print("   3. Use different secrets for each environment (dev, staging, prod)")
    print("   4. Backup these secrets securely (password manager recommended)")
    print("\n" + "=" * 80)

if __name__ == "__main__":
    main()
