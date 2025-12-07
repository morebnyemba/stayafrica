#!/usr/bin/env python
"""Quick test of Django settings module to verify fixes."""

import os
import sys
import django
from pathlib import Path

# Add the project root to the path
BASE_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(BASE_DIR))

# Set Django settings module
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'stayafrica.settings')

try:
    django.setup()
    print("✓ Django settings loaded successfully!")
    print(f"✓ DEBUG mode: {django.conf.settings.DEBUG}")
    print(f"✓ Database engine: {django.conf.settings.DATABASES['default']['ENGINE']}")
    print(f"✓ Installed apps count: {len(django.conf.settings.INSTALLED_APPS)}")
    print("\n✓ All checks passed - backend is ready for development!")
except Exception as e:
    print(f"✗ Error loading Django settings: {e}")
    import traceback
    traceback.print_exc()
    sys.exit(1)
