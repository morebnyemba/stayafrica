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

def update_env_file(file_path, secrets_dict):
    """Update placeholders in .env file with generated secrets"""
    import os
    import re
    
    if not os.path.exists(".env.example"):
        print(f"⚠️  Error: .env.example not found. Cannot create {file_path}")
        return False
        
    with open(".env.example", "r") as f:
        content = f.read()
        
    for key, value in secrets_dict.items():
        # Look for the key and replace its value
        # Pattern: KEY=value-to-replace
        pattern = rf"^({key}=).*$"
        # Use \g<1> to avoid ambiguity with digits following the backreference
        content = re.sub(pattern, rf"\g<1>{value}", content, flags=re.MULTILINE)
        
    with open(file_path, "w") as f:
        f.write(content)
    return True

def main():
    print("=" * 80)
    print("StayAfrica - Secure Secrets Generator")
    print("=" * 80)
    
    secrets_dict = {
        "SECRET_KEY": generate_django_secret_key(),
        "DB_PASSWORD": generate_password(),
        "REDIS_PASSWORD": generate_password(),
        "JWT_SECRET_KEY": generate_hex_token()
    }
    
    print("\nGenerated Secrets:")
    for key, value in secrets_dict.items():
        print(f"  {key}: [SECURE]")
    
    print("\nUpdating environment files...")
    
    updated_env = update_env_file(".env", secrets_dict)
    updated_prod = update_env_file(".env.prod", secrets_dict)
    
    if updated_env and updated_prod:
        print("\n[SUCCESS] Successfully updated .env and .env.prod!")
    elif updated_env:
        print("\n[SUCCESS] Successfully updated .env")
    
    print("\n" + "-" * 80)
    print("\nIMPORTANT:")
    print("   1. Verify the values in your .env files using 'nano .env'")
    print("   2. Keep these secrets secure and never commit them to git")
    print("   3. Backup these secrets securely (password manager recommended)")
    print("\n" + "=" * 80)

if __name__ == "__main__":
    main()
