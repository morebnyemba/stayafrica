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
    """Generate a secure password (URL-safe)"""
    # Exclude problematic characters: @, :, /, ^, spaces
    alphabet = string.ascii_letters + string.digits + "!#*-_=+"
    return ''.join(secrets.choice(alphabet) for _ in range(length))

def generate_hex_token(length=64):
    """Generate a hex token"""
    return secrets.token_hex(length // 2)

def get_existing_secrets(file_path):
    """Parse existing .env file to find already set secrets"""
    import os
    if not os.path.exists(file_path):
        return {}
        
    existing = {}
    with open(file_path, "r") as f:
        for line in f:
            if "=" in line and not line.startswith("#"):
                key, value = line.split("=", 1)
                value = value.strip()
                # Consider it "set" if it doesn't look like a placeholder
                if value and "YOUR_" not in value and "PLACEHOLDER" not in value:
                    existing[key.strip()] = value
    return existing

def update_env_file(file_path, secrets_dict):
    """Update placeholders in .env file with generated secrets, preserving existing ones"""
    import os
    import re
    
    # Start with .env.example as the template if file doesn't exist
    template_path = ".env.example"
    if not os.path.exists(template_path):
        print(f"⚠️  Error: {template_path} not found.")
        return False
        
    # Get existing values from the target file
    existing = get_existing_secrets(file_path)
    
    # Read the template
    with open(template_path, "r") as f:
        content = f.read()
        
    final_secrets = {}
    for key, gen_value in secrets_dict.items():
        # Use existing value if available and not empty, otherwise use generated one
        val = existing.get(key, gen_value)
        final_secrets[key] = val

    for key, value in final_secrets.items():
        pattern = rf"^({key}=).*$"
        content = re.sub(pattern, rf"\g<1>{value}", content, flags=re.MULTILINE)
        
    # Special handle for REDIS_URL/CELERY URLs to ensure they use the correct password
    # if we just generated a new one or if they need to be consistent.
    redis_pass = final_secrets.get("REDIS_PASSWORD")
    if redis_pass:
        redis_url = f"redis://:{redis_pass}@redis:6379/0"
        content = re.sub(r"^(REDIS_URL=).*$", rf"\g<1>{redis_url}", content, flags=re.MULTILINE)
        content = re.sub(r"^(CELERY_BROKER_URL=).*$", rf"\g<1>{redis_url}", content, flags=re.MULTILINE)
        content = re.sub(r"^(CELERY_RESULT_BACKEND=).*$", rf"\g<1>{redis_url}", content, flags=re.MULTILINE)

    with open(file_path, "w") as f:
        f.write(content)
    return True

def main():
    print("=" * 80)
    print("StayAfrica - Smart Secrets Generator")
    print("=" * 80)
    print("Note: This script will preserve any non-empty values already in your .env files.")
    
    secrets_dict = {
        "SECRET_KEY": generate_django_secret_key(),
        "DB_PASSWORD": generate_password(),
        "REDIS_PASSWORD": generate_password(),
        "JWT_SECRET_KEY": generate_hex_token()
    }
    
    print("\nChecking environment files...")
    
    updated_env = update_env_file(".env", secrets_dict)
    updated_prod = update_env_file(".env.prod", secrets_dict)
    
    if updated_env and updated_prod:
        print("\n[SUCCESS] Successfully synced .env and .env.prod!")
    
    print("\n" + "-" * 80)
    print("IMPORTANT: If you manually changed passwords, they have been preserved.")
    print("If you want to force-regenerate, delete the value in .env (e.g. DB_PASSWORD=) and run this again.")
    print("-" * 80)
    print("\n" + "=" * 80)

if __name__ == "__main__":
    main()
