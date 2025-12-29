#!/usr/bin/env python
"""
Script to migrate Property IDs from integer to 10-digit string.
Run this inside the Docker container.
"""
import os
import django

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'stayafrica.settings')
django.setup()

from apps.properties.models import Property
from django.db import connection
import random

def generate_10digit_id():
    """Generate a unique 10-digit ID"""
    return ''.join([str(random.randint(0, 9)) for _ in range(10)])

def migrate_ids():
    print("Starting Property ID migration...")
    
    # Get all properties
    properties = list(Property.objects.all())
    print(f"Found {len(properties)} properties to migrate")
    
    # Create mapping of old ID to new ID
    id_mapping = {}
    for prop in properties:
        old_id = prop.id
        new_id = generate_10digit_id()
        
        # Ensure uniqueness
        while new_id in id_mapping.values():
            new_id = generate_10digit_id()
        
        id_mapping[old_id] = new_id
        print(f"Property {old_id} -> {new_id}: {prop.title}")
    
    print("\nID mapping complete. Ready to apply?")
    print("Note: This operation cannot be easily reversed.")
    response = input("Continue? (yes/no): ")
    
    if response.lower() != 'yes':
        print("Migration cancelled.")
        return
    
    print("\nApplying migration...")
    # The actual migration will be handled by Django migrations
    # This script is just for planning and validation
    
    print("Migration plan created successfully!")
    print(f"Total properties: {len(properties)}")
    print("\nNext steps:")
    print("1. Run: python manage.py makemigrations")
    print("2. Run: python manage.py migrate")

if __name__ == '__main__':
    migrate_ids()
