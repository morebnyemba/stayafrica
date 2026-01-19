#!/usr/bin/env python
"""
Complete database cleanup script for StayAfrica.
Handles various database state issues that can occur during development.
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


def cleanup_database():
    """Clean up database state issues."""
    print("=" * 60)
    print("Database Cleanup Script")
    print("=" * 60)
    
    with connection.cursor() as cursor:
        try:
            # Check if django_migrations table exists
            cursor.execute("""
                SELECT EXISTS (
                    SELECT FROM information_schema.tables 
                    WHERE table_name = 'django_migrations'
                );
            """)
            table_exists = cursor.fetchone()[0]
            
            if not table_exists:
                print("✓ No cleanup needed - fresh database")
                return
            
            # Fix sequence issues
            print("\n1. Checking migration sequence...")
            try:
                cursor.execute("SELECT MAX(id) FROM django_migrations;")
                max_id = cursor.fetchone()[0] or 0
                
                # Check if sequence exists
                cursor.execute("""
                    SELECT EXISTS (
                        SELECT FROM pg_class 
                        WHERE relname = 'django_migrations_id_seq'
                    );
                """)
                seq_exists = cursor.fetchone()[0]
                
                if seq_exists:
                    cursor.execute(
                        "SELECT setval('django_migrations_id_seq', %s, true);",
                        [max_id]
                    )
                    print(f"  ✓ Sequence reset to {max_id}")
                else:
                    print("  ✓ Sequence does not exist yet")
                    
            except Exception as e:
                print(f"  ⚠ Warning: {e}")
            
            # List applied migrations
            print("\n2. Current migration status:")
            cursor.execute("""
                SELECT app, name 
                FROM django_migrations 
                ORDER BY applied DESC 
                LIMIT 10;
            """)
            migrations = cursor.fetchall()
            
            if migrations:
                print(f"  Last 10 applied migrations:")
                for app, name in migrations:
                    print(f"    - {app}: {name}")
            else:
                print("  No migrations applied yet")
            
            # Check for duplicate sequences
            print("\n3. Checking for duplicate sequences...")
            cursor.execute("""
                SELECT relname, COUNT(*) as count
                FROM pg_class
                WHERE relkind = 'S'
                GROUP BY relname
                HAVING COUNT(*) > 1;
            """)
            duplicates = cursor.fetchall()
            
            if duplicates:
                print("  ⚠ Found duplicate sequences:")
                for name, count in duplicates:
                    print(f"    - {name}: {count} copies")
            else:
                print("  ✓ No duplicate sequences found")
            
            print("\n" + "=" * 60)
            print("Cleanup complete!")
            print("=" * 60)
            
        except Exception as e:
            print(f"\n❌ Error during cleanup: {e}")
            sys.exit(1)


if __name__ == '__main__':
    cleanup_database()
