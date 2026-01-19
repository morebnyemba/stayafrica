#!/usr/bin/env python
"""
Fix PostgreSQL sequence issues for django_migrations table.
This script handles the case where the sequence already exists but needs to be reset.
"""
import os
import sys
from pathlib import Path

# Add the parent directory to sys.path so Python can find the stayafrica module
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import django

# Setup Django
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'stayafrica.settings')
django.setup()

from django.db import connection


def fix_migration_sequence():
    """Fix the django_migrations sequence if it exists but is out of sync."""
    with connection.cursor() as cursor:
        try:
            # Check if the django_migrations table exists
            cursor.execute("""
                SELECT EXISTS (
                    SELECT FROM information_schema.tables 
                    WHERE table_name = 'django_migrations'
                );
            """)
            table_exists = cursor.fetchone()[0]
            
            if table_exists:
                print("✓ django_migrations table exists, checking sequence...")
                
                # Get the max ID from the table
                cursor.execute("SELECT MAX(id) FROM django_migrations;")
                max_id = cursor.fetchone()[0] or 0
                
                # Reset the sequence to the correct value
                cursor.execute(
                    "SELECT setval('django_migrations_id_seq', %s, true);",
                    [max_id]
                )
                print(f"✓ Sequence reset to {max_id}")
                
            else:
                print("✓ django_migrations table does not exist yet (first run)")
                
        except Exception as e:
            # If sequence doesn't exist yet, that's fine (first run)
            if "does not exist" in str(e):
                print("✓ Sequence does not exist yet (first run)")
            else:
                print(f"Warning: Could not fix sequence: {e}")
                # Don't fail - let Django handle it


if __name__ == '__main__':
    fix_migration_sequence()
