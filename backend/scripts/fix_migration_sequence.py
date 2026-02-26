#!/usr/bin/env python
"""
Fix PostgreSQL sequence issues and inconsistent migration history
for django_migrations table.

Handles:
  1. Sequence out-of-sync after manual DB changes.
  2. Stale migration records for local apps whose migration files were
     regenerated (e.g. after a clean makemigrations).
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
from django.apps import apps


# ---------------------------------------------------------------------------
# Local apps whose migration files live inside this repo.
# If the migration files are regenerated (new names) we must clean the old
# database records so Django does not see an inconsistent history.
# ---------------------------------------------------------------------------
LOCAL_APP_LABELS = [
    'users',
    'properties',
    'bookings',
    'payments',
    'reviews',
    'messaging',
    'admin_dashboard',
    'experiences',
    'notifications',
    'health',
]


def _table_exists(cursor, table_name):
    cursor.execute(
        "SELECT EXISTS (SELECT FROM information_schema.tables WHERE table_name = %s);",
        [table_name],
    )
    return cursor.fetchone()[0]


def fix_migration_sequence(cursor):
    """Fix the django_migrations sequence if it exists but is out of sync."""
    try:
        cursor.execute("SELECT MAX(id) FROM django_migrations;")
        max_id = cursor.fetchone()[0] or 0
        cursor.execute(
            "SELECT setval('django_migrations_id_seq', %s, true);",
            [max_id],
        )
        print(f"  ✓ Sequence reset to {max_id}")
    except Exception as e:
        if "does not exist" in str(e):
            print("  ✓ Sequence does not exist yet (first run)")
        else:
            print(f"  ⚠ Could not fix sequence: {e}")


def fix_inconsistent_history(cursor):
    """
    Remove stale migration records for local apps so that fresh
    makemigrations output can be applied (or fake-applied) cleanly.
    """
    # Build the set of migration names that actually exist on disk
    from django.db.migrations.loader import MigrationLoader
    loader = MigrationLoader(connection, ignore_no_migrations=True)
    disk_migrations = set(loader.disk_migrations.keys())  # (app_label, name)

    for app_label in LOCAL_APP_LABELS:
        # Fetch what the DB thinks is applied for this app
        cursor.execute(
            "SELECT id, name FROM django_migrations WHERE app = %s ORDER BY id;",
            [app_label],
        )
        rows = cursor.fetchall()
        if not rows:
            continue

        stale = [
            (row_id, name)
            for row_id, name in rows
            if (app_label, name) not in disk_migrations
        ]

        if stale:
            ids_to_delete = [r[0] for r in stale]
            names = [r[1] for r in stale]
            cursor.execute(
                "DELETE FROM django_migrations WHERE id = ANY(%s);",
                [ids_to_delete],
            )
            print(f"  ✓ Removed {len(stale)} stale record(s) for '{app_label}': {names}")
        else:
            print(f"  ✓ '{app_label}' migration records are consistent")


def main():
    with connection.cursor() as cursor:
        if not _table_exists(cursor, 'django_migrations'):
            print("✓ django_migrations table does not exist yet (first run)")
            return

        print("🔧 Fixing migration sequence…")
        fix_migration_sequence(cursor)

        print("🔧 Cleaning stale migration records for local apps…")
        fix_inconsistent_history(cursor)


if __name__ == '__main__':
    main()
