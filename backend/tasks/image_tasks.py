"""
Image processing tasks for async optimization
"""
from celery import shared_task
from PIL import Image
from io import BytesIO
from django.core.files.uploadedfile import InMemoryUploadedFile
import logging
import os

logger = logging.getLogger(__name__)


@shared_task(bind=True, max_retries=3)
def optimize_image(self, image_path, quality=85, max_width=1920, max_height=1080):
    """
    Optimize image for storage and delivery
    - Compress image
    - Resize if too large
    - Convert to web-friendly format
    """
    try:
        if not os.path.exists(image_path):
            logger.error(f"Image not found: {image_path}")
            return False
        
        img = Image.open(image_path)
        
        # Convert RGBA to RGB if necessary
        if img.mode == 'RGBA':
            background = Image.new('RGB', img.size, (255, 255, 255))
            background.paste(img, mask=img.split()[3])
            img = background
        
        # Resize if too large
        if img.width > max_width or img.height > max_height:
            img.thumbnail((max_width, max_height), Image.Resampling.LANCZOS)
        
        # Save optimized image
        img.save(image_path, 'JPEG', quality=quality, optimize=True)
        
        logger.info(f"Image optimized successfully: {image_path}")
        return True
    except Exception as exc:
        logger.error(f"Error optimizing image {image_path}: {str(exc)}")
        raise self.retry(exc=exc, countdown=60 * (2 ** self.request.retries))


@shared_task(bind=True, max_retries=3)
def generate_thumbnails(self, image_path, sizes=None):
    """
    Generate multiple thumbnail sizes for an image
    Default sizes: thumb (150x150), medium (400x400), large (800x800)
    """
    if sizes is None:
        sizes = {
            'thumb': (150, 150),
            'medium': (400, 400),
            'large': (800, 800),
        }
    
    try:
        if not os.path.exists(image_path):
            logger.error(f"Image not found: {image_path}")
            return False
        
        img = Image.open(image_path)
        
        # Convert RGBA to RGB if necessary
        if img.mode == 'RGBA':
            background = Image.new('RGB', img.size, (255, 255, 255))
            background.paste(img, mask=img.split()[3])
            img = background
        
        base_path = os.path.splitext(image_path)[0]
        
        for size_name, dimensions in sizes.items():
            thumb = img.copy()
            thumb.thumbnail(dimensions, Image.Resampling.LANCZOS)
            
            thumb_path = f"{base_path}_{size_name}.jpg"
            thumb.save(thumb_path, 'JPEG', quality=85, optimize=True)
            
            logger.info(f"Thumbnail generated: {thumb_path}")
        
        return True
    except Exception as exc:
        logger.error(f"Error generating thumbnails for {image_path}: {str(exc)}")
        raise self.retry(exc=exc, countdown=60 * (2 ** self.request.retries))


@shared_task
def process_property_images(property_id):
    """
    Process all images for a property
    - Optimize main image
    - Generate thumbnails
    - Optimize additional images
    """
    from apps.properties.models import Property, PropertyImage
    
    try:
        property_obj = Property.objects.get(id=property_id)
        
        # Process main image
        if property_obj.main_image and hasattr(property_obj.main_image, 'path'):
            optimize_image.delay(property_obj.main_image.path)
            generate_thumbnails.delay(property_obj.main_image.path)
        
        # Process additional images
        for prop_image in property_obj.images.all():
            if prop_image.image and hasattr(prop_image.image, 'path'):
                optimize_image.delay(prop_image.image.path)
                generate_thumbnails.delay(prop_image.image.path)
        
        logger.info(f"Image processing queued for property {property_id}")
        return True
    except Property.DoesNotExist:
        logger.error(f"Property with id {property_id} not found")
        return False
    except Exception as e:
        logger.error(f"Error processing property images: {str(e)}")
        return False


@shared_task
def cleanup_old_images(days=30):
    """
    Clean up old unused images
    Placeholder for future implementation
    """
    logger.info(f"Checking for images older than {days} days...")
    # Implementation for cleaning up old images
    pass
