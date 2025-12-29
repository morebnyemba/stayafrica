# Generated data migration to regenerate property IDs to 10-digit format

from django.db import migrations
import random


def regenerate_property_ids(apps, schema_editor):
    """Regenerate all property IDs to 10-digit format"""
    Property = apps.get_model('properties', 'Property')
    
    # Get all properties
    properties = Property.objects.all()
    
    id_mapping = {}  # Map old_id -> new_id for foreign keys
    
    for prop in properties:
        old_id = str(prop.id)
        
        # Generate new 10-digit ID
        while True:
            new_id = ''.join([str(random.randint(0, 9)) for _ in range(10)])
            if not Property.objects.filter(id=new_id).exists():
                break
        
        # Store mapping for foreign key updates
        id_mapping[old_id] = new_id
        
        # Update the property ID
        prop.id = new_id
        prop.save(update_fields=['id'])
    
    # Update SavedProperty foreign keys
    SavedProperty = apps.get_model('properties', 'SavedProperty')
    for saved in SavedProperty.objects.all():
        old_property_id = str(saved.property_id)
        if old_property_id in id_mapping:
            saved.property_id = id_mapping[old_property_id]
            saved.save(update_fields=['property_id'])
    
    # Update PropertyImage foreign keys
    PropertyImage = apps.get_model('properties', 'PropertyImage')
    for img in PropertyImage.objects.all():
        old_property_id = str(img.property_id)
        if old_property_id in id_mapping:
            img.property_id = id_mapping[old_property_id]
            img.save(update_fields=['property_id'])
    
    # Update Booking foreign keys if they exist
    try:
        Booking = apps.get_model('bookings', 'Booking')
        for booking in Booking.objects.all():
            old_property_id = str(booking.property_id)
            if old_property_id in id_mapping:
                booking.property_id = id_mapping[old_property_id]
                booking.save(update_fields=['property_id'])
    except LookupError:
        pass  # Bookings app may not exist yet


def reverse_regenerate_property_ids(apps, schema_editor):
    """This migration cannot be safely reversed"""
    pass


class Migration(migrations.Migration):

    dependencies = [
        ('properties', '0005_alter_property_address_alter_property_main_image'),
    ]

    operations = [
        migrations.RunPython(regenerate_property_ids, reverse_regenerate_property_ids),
    ]
