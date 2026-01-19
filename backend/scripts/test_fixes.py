#!/usr/bin/env python
"""
Test script to validate the URL namespace and migration fixes.
This script can be run locally with Django installed.
"""
import os
import sys
import re
from pathlib import Path

# Colors for output
GREEN = '\033[92m'
RED = '\033[91m'
YELLOW = '\033[93m'
BLUE = '\033[94m'
RESET = '\033[0m'

def print_success(msg):
    print(f"{GREEN}✓{RESET} {msg}")

def print_error(msg):
    print(f"{RED}✗{RESET} {msg}")

def print_warning(msg):
    print(f"{YELLOW}⚠{RESET} {msg}")

def print_info(msg):
    print(f"{BLUE}ℹ{RESET} {msg}")

def test_url_namespace():
    """Test that the admin namespace conflict is resolved."""
    print("\n" + "=" * 60)
    print("Testing URL Namespace Fix")
    print("=" * 60 + "\n")
    
    admin_dashboard_urls = Path(__file__).parent.parent / "apps" / "admin_dashboard" / "urls.py"
    
    if not admin_dashboard_urls.exists():
        print_error(f"Could not find {admin_dashboard_urls}")
        return False
    
    with open(admin_dashboard_urls, 'r') as f:
        content = f.read()
    
    # Check for the old problematic namespace
    if re.search(r"app_name\s*=\s*['\"]admin['\"]", content):
        print_error("Found 'app_name = admin' - this conflicts with Django's admin!")
        return False
    
    # Check for the correct namespace
    if re.search(r"app_name\s*=\s*['\"]admin_dashboard['\"]", content):
        print_success("URL namespace correctly set to 'admin_dashboard'")
        return True
    
    print_warning("Could not find app_name declaration")
    return False

def test_migration_fix_script():
    """Test that the migration fix script exists and is valid."""
    print("\n" + "=" * 60)
    print("Testing Migration Fix Script")
    print("=" * 60 + "\n")
    
    fix_script = Path(__file__).parent / "fix_migration_sequence.py"
    
    if not fix_script.exists():
        print_error(f"Migration fix script not found: {fix_script}")
        return False
    
    print_success("Migration fix script exists")
    
    # Check if it has the necessary components
    with open(fix_script, 'r') as f:
        content = f.read()
    
    required_elements = [
        ("django.setup", "Django setup"),
        ("django_migrations", "django_migrations table check"),
        ("setval", "Sequence reset command"),
        ("fix_migration_sequence", "Main function")
    ]
    
    all_present = True
    for element, description in required_elements:
        if element in content:
            print_success(f"  Contains {description}")
        else:
            print_error(f"  Missing {description}")
            all_present = False
    
    return all_present

def test_entrypoint_integration():
    """Test that entrypoint.sh calls the fix script."""
    print("\n" + "=" * 60)
    print("Testing Entrypoint Integration")
    print("=" * 60 + "\n")
    
    entrypoint = Path(__file__).parent.parent / "entrypoint.sh"
    
    if not entrypoint.exists():
        print_error(f"Entrypoint script not found: {entrypoint}")
        return False
    
    print_success("Entrypoint script exists")
    
    with open(entrypoint, 'r') as f:
        content = f.read()
    
    if "fix_migration_sequence" in content:
        print_success("Entrypoint calls fix_migration_sequence.py")
        
        # Check order: fix should come before migrate
        fix_pos = content.find("fix_migration_sequence")
        migrate_pos = content.find("migrate")
        
        if fix_pos < migrate_pos:
            print_success("Fix script runs before migrations (correct order)")
            return True
        else:
            print_error("Fix script runs after migrations (wrong order)")
            return False
    else:
        print_error("Entrypoint does not call fix_migration_sequence.py")
        return False

def test_cleanup_script():
    """Test that the cleanup script exists."""
    print("\n" + "=" * 60)
    print("Testing Database Cleanup Script")
    print("=" * 60 + "\n")
    
    cleanup_script = Path(__file__).parent / "cleanup_database.py"
    
    if not cleanup_script.exists():
        print_error(f"Cleanup script not found: {cleanup_script}")
        return False
    
    print_success("Database cleanup script exists")
    
    with open(cleanup_script, 'r') as f:
        content = f.read()
    
    checks = [
        ("django_migrations", "Migration table check"),
        ("setval", "Sequence management"),
        ("duplicate", "Duplicate detection"),
        ("cleanup_database", "Main function")
    ]
    
    all_present = True
    for check, description in checks:
        if check in content:
            print_success(f"  Contains {description}")
        else:
            print_warning(f"  Missing {description}")
            all_present = False
    
    return all_present

def test_urls_configuration():
    """Test the main URLs configuration."""
    print("\n" + "=" * 60)
    print("Testing Main URLs Configuration")
    print("=" * 60 + "\n")
    
    main_urls = Path(__file__).parent.parent / "stayafrica" / "urls.py"
    
    if not main_urls.exists():
        print_error(f"Main URLs file not found: {main_urls}")
        return False
    
    print_success("Main URLs file exists")
    
    with open(main_urls, 'r') as f:
        content = f.read()
    
    # Check that admin_dashboard is included
    if "admin_dashboard.urls" in content:
        print_success("Admin dashboard URLs included")
    else:
        print_warning("Admin dashboard URLs not found")
    
    # Count admin patterns
    admin_count = len(re.findall(r"path\(['\"]admin", content))
    if admin_count == 1:
        print_success(f"Single admin path found (Django's default)")
    elif admin_count > 1:
        print_warning(f"Multiple admin paths found ({admin_count})")
    
    return True

def main():
    """Run all tests."""
    print("\n" + "=" * 60)
    print("StayAfrica Fix Validation Tests")
    print("=" * 60)
    
    tests = [
        ("URL Namespace", test_url_namespace),
        ("Migration Fix Script", test_migration_fix_script),
        ("Entrypoint Integration", test_entrypoint_integration),
        ("Cleanup Script", test_cleanup_script),
        ("URLs Configuration", test_urls_configuration),
    ]
    
    results = []
    for name, test_func in tests:
        try:
            result = test_func()
            results.append((name, result))
        except Exception as e:
            print_error(f"Error running {name} test: {e}")
            results.append((name, False))
    
    # Summary
    print("\n" + "=" * 60)
    print("Test Summary")
    print("=" * 60 + "\n")
    
    passed = sum(1 for _, result in results if result)
    total = len(results)
    
    for name, result in results:
        status = f"{GREEN}PASS{RESET}" if result else f"{RED}FAIL{RESET}"
        print(f"  {name}: {status}")
    
    print(f"\n{passed}/{total} tests passed")
    
    if passed == total:
        print(f"\n{GREEN}All tests passed! ✓{RESET}\n")
        return 0
    else:
        print(f"\n{RED}Some tests failed! ✗{RESET}\n")
        return 1

if __name__ == '__main__':
    sys.exit(main())
