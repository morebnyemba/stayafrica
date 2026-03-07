"""
Django management command to diagnose and fix common migration issues.

Usage:
  python manage.py fix_migrations              # Diagnose only
  python manage.py fix_migrations --fix        # Diagnose and auto-fix
  python manage.py fix_migrations --app bookings  # Single app
"""
import sys
from io import StringIO

from django.apps import apps
from django.core.management import call_command
from django.core.management.base import BaseCommand, CommandError
from django.db import connection
from django.db.migrations.executor import MigrationExecutor
from django.db.migrations.loader import MigrationLoader


class Command(BaseCommand):
    help = "Diagnose and fix common migration issues (unapplied migrations, schema drift, conflicts)"

    def add_arguments(self, parser):
        parser.add_argument(
            "--fix",
            action="store_true",
            help="Attempt to auto-fix detected issues (default is diagnose only)",
        )
        parser.add_argument(
            "--app",
            type=str,
            help="Limit to a specific app label (e.g. bookings)",
        )
        parser.add_argument(
            "--fake-missing",
            action="store_true",
            help="Fake-apply migrations whose schema already exists in the DB",
        )

    def handle(self, *args, **options):
        self.fix_mode = options["fix"]
        self.target_app = options.get("app")
        self.fake_missing = options.get("fake_missing", False)
        self.issues = []
        self.fixes_applied = []

        self.stdout.write(self.style.HTTP_INFO("\n🔍 StayAfrica Migration Diagnostics\n" + "=" * 50))

        self._check_unapplied_migrations()
        self._check_migration_conflicts()
        self._check_schema_drift()
        self._check_stale_applied()

        self.stdout.write("\n" + "=" * 50)
        if not self.issues:
            self.stdout.write(self.style.SUCCESS("✅ No migration issues detected!"))
        else:
            self.stdout.write(self.style.WARNING(f"⚠️  Found {len(self.issues)} issue(s):"))
            for i, issue in enumerate(self.issues, 1):
                self.stdout.write(f"  {i}. {issue}")

        if self.fixes_applied:
            self.stdout.write(self.style.SUCCESS(f"\n🔧 Applied {len(self.fixes_applied)} fix(es):"))
            for fix in self.fixes_applied:
                self.stdout.write(f"  ✓ {fix}")

        if self.issues and not self.fix_mode:
            self.stdout.write(self.style.NOTICE("\n💡 Run with --fix to attempt auto-fixes"))

        self.stdout.write("")

    # ------------------------------------------------------------------
    # 1. Unapplied migrations
    # ------------------------------------------------------------------
    def _check_unapplied_migrations(self):
        self.stdout.write(self.style.HTTP_INFO("\n📋 Checking unapplied migrations..."))
        executor = MigrationExecutor(connection)
        plan = executor.migration_plan(executor.loader.graph.leaf_nodes())

        if self.target_app:
            plan = [(m, backwards) for m, backwards in plan if m.app_label == self.target_app]

        if not plan:
            self.stdout.write(self.style.SUCCESS("  All migrations applied."))
            return

        unapplied = [(m.app_label, m.name) for m, _backwards in plan]
        for app_label, name in unapplied:
            msg = f"Unapplied: {app_label}.{name}"
            self.issues.append(msg)
            self.stdout.write(self.style.WARNING(f"  ⚠️  {msg}"))

        if self.fix_mode:
            self.stdout.write(self.style.HTTP_INFO("  🔧 Applying unapplied migrations..."))
            for app_label, name in unapplied:
                if self.fake_missing and self._migration_schema_exists(app_label, name, executor):
                    self.stdout.write(f"    Fake-applying {app_label}.{name} (schema already exists)")
                    call_command("migrate", app_label, name, fake=True, verbosity=0)
                    self.fixes_applied.append(f"Fake-applied {app_label}.{name}")
                else:
                    try:
                        call_command("migrate", app_label, name, verbosity=1)
                        self.fixes_applied.append(f"Applied {app_label}.{name}")
                    except Exception as e:
                        self.stdout.write(self.style.ERROR(f"    ❌ Failed to apply {app_label}.{name}: {e}"))

    # ------------------------------------------------------------------
    # 2. Migration conflicts (multiple leaf nodes)
    # ------------------------------------------------------------------
    def _check_migration_conflicts(self):
        self.stdout.write(self.style.HTTP_INFO("\n📋 Checking for migration conflicts..."))
        loader = MigrationLoader(connection, ignore_no_migrations=True)
        conflicts = loader.detect_conflicts()

        if self.target_app:
            conflicts = {k: v for k, v in conflicts.items() if k == self.target_app}

        if not conflicts:
            self.stdout.write(self.style.SUCCESS("  No conflicts."))
            return

        for app_label, names in conflicts.items():
            msg = f"Conflict in {app_label}: multiple leaf migrations {names}"
            self.issues.append(msg)
            self.stdout.write(self.style.ERROR(f"  ❌ {msg}"))

        if self.fix_mode:
            self.stdout.write("  💡 Auto-fix: Run 'python manage.py makemigrations --merge' to resolve")

    # ------------------------------------------------------------------
    # 3. Schema drift – model fields missing from DB
    # ------------------------------------------------------------------
    def _check_schema_drift(self):
        self.stdout.write(self.style.HTTP_INFO("\n📋 Checking for schema drift (model vs DB)..."))

        app_labels = [self.target_app] if self.target_app else [
            a.label for a in apps.get_app_configs()
            if a.name.startswith("apps.")
        ]

        introspection = connection.introspection
        with connection.cursor() as cursor:
            db_tables = introspection.table_names(cursor)

        drift_found = False
        for app_label in app_labels:
            try:
                app_config = apps.get_app_config(app_label)
            except LookupError:
                continue

            for model in app_config.get_models():
                table = model._meta.db_table
                if table not in db_tables:
                    # Table entirely missing — only flag if migrations exist
                    loader = MigrationLoader(connection, ignore_no_migrations=True)
                    if app_label in loader.migrated_apps:
                        msg = f"Table '{table}' missing from DB (model {model.__name__})"
                        self.issues.append(msg)
                        self.stdout.write(self.style.ERROR(f"  ❌ {msg}"))
                        drift_found = True
                    continue

                with connection.cursor() as cursor:
                    db_columns = {
                        col.name for col in introspection.get_table_description(cursor, table)
                    }

                model_columns = {f.column for f in model._meta.local_fields}
                missing = model_columns - db_columns
                extra = db_columns - model_columns - {"id"}  # ignore auto PK edge cases

                if missing:
                    drift_found = True
                    for col in sorted(missing):
                        field = next(
                            (f for f in model._meta.local_fields if f.column == col), None
                        )
                        field_name = field.name if field else col
                        msg = f"Column '{col}' ({model.__name__}.{field_name}) missing from '{table}'"
                        self.issues.append(msg)
                        self.stdout.write(self.style.ERROR(f"  ❌ {msg}"))

                if extra:
                    for col in sorted(extra):
                        self.stdout.write(
                            self.style.WARNING(f"  ℹ️  Extra DB column '{col}' in '{table}' (no model field)")
                        )

        if not drift_found:
            self.stdout.write(self.style.SUCCESS("  Schema is in sync."))

        if drift_found and self.fix_mode:
            self.stdout.write(self.style.HTTP_INFO("  🔧 Generating missing migrations..."))
            buf = StringIO()
            try:
                call_command("makemigrations", verbosity=1, stdout=buf)
                output = buf.getvalue()
                if "No changes detected" in output:
                    self.stdout.write(
                        "  ⚠️  makemigrations found no changes — migration may already exist but is unapplied"
                    )
                else:
                    self.stdout.write(f"  {output.strip()}")
                    self.stdout.write(self.style.HTTP_INFO("  🔧 Applying new migrations..."))
                    call_command("migrate", verbosity=1)
                    self.fixes_applied.append("Generated and applied migrations for schema drift")
            except Exception as e:
                self.stdout.write(self.style.ERROR(f"  ❌ makemigrations failed: {e}"))

    # ------------------------------------------------------------------
    # 4. Stale applied migrations (in DB but file deleted)
    # ------------------------------------------------------------------
    def _check_stale_applied(self):
        self.stdout.write(self.style.HTTP_INFO("\n📋 Checking for stale migration records..."))
        loader = MigrationLoader(connection, ignore_no_migrations=True)

        with connection.cursor() as cursor:
            cursor.execute("SELECT app, name FROM django_migrations ORDER BY app, name")
            applied = cursor.fetchall()

        stale_found = False
        for app_label, name in applied:
            if self.target_app and app_label != self.target_app:
                continue
            key = (app_label, name)
            if key not in loader.disk_migrations and app_label in [
                a.label for a in apps.get_app_configs() if a.name.startswith("apps.")
            ]:
                stale_found = True
                msg = f"Stale record: {app_label}.{name} (applied in DB but file missing)"
                self.issues.append(msg)
                self.stdout.write(self.style.WARNING(f"  ⚠️  {msg}"))

        if not stale_found:
            self.stdout.write(self.style.SUCCESS("  No stale records."))

        if stale_found and self.fix_mode:
            self.stdout.write(
                "  💡 To remove stale records: "
                "DELETE FROM django_migrations WHERE app='X' AND name='Y';"
            )

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------
    def _migration_schema_exists(self, app_label, name, executor):
        """Check if the operations in a migration already exist in the DB."""
        try:
            migration = executor.loader.get_migration_by_prefix(app_label, name.split("_")[0])
            for op in migration.operations:
                if hasattr(op, "model_name"):
                    table = f"{app_label}_{op.model_name}"
                    with connection.cursor() as cursor:
                        tables = connection.introspection.table_names(cursor)
                        if table in tables:
                            return True
        except Exception:
            pass
        return False
