"""
Celery tasks for async image processing operations
Use these for non-blocking image optimization and thumbnail generation
"""
from celery import shared_task, group
from services.image_processor import ImageProcessorService
from PIL import Image
from io import BytesIO
from django.core.files.uploadedfile import InMemoryUploadedFile
from typing import List, Dict, Optional
import logging
import os

logger = logging.getLogger(__name__)


@shared_task(bind=True, max_retries=3)
def optimize_image(self, image_path, quality=85, max_width=1920, max_height=1080):
    """
    Optimize image using enhanced ImageProcessorService
    """
    try:
        if not os.path.exists(image_path):
            logger.error(f"Image not found: {image_path}")
            return False
        
        result = ImageProcessorService.optimize_image(
            image_path,
            max_width=max_width,
            max_height=max_height,
            quality=quality,
            convert_to_webp=True,
            strip_exif=True
        )
        
        logger.info(f"Image optimized successfully: {image_path}")
        return result
    except Exception as exc:
        logger.error(f"Error optimizing image {image_path}: {str(exc)}")
        raise self.retry(exc=exc, countdown=60 * (2 ** self.request.retries))


@shared_task(bind=True, max_retries=3)
def generate_thumbnails(self, image_path, sizes=None):
    """
    Generate multiple thumbnail sizes using enhanced ImageProcessorService
    """
    try:
        if not os.path.exists(image_path):
            logger.error(f"Image not found: {image_path}")
            return False
        
        result = ImageProcessorService.generate_thumbnails(
            image_path,
            sizes=sizes
        )
        
        logger.info(f"Thumbnails generated for: {image_path}")
        return result
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
    Clean up old unused images and thumbnails
    """
    import time
    from pathlib import Path
    from django.conf import settings
    
    try:
        media_root = settings.MEDIA_ROOT
        cutoff_time = time.time() - (days * 24 * 60 * 60)
        deleted_count = 0
        
        for root, dirs, files in os.walk(media_root):
            for filename in files:
                # Clean up thumbnails and temp files
                if any(marker in filename for marker in ['_thumbnail', '_small', '_medium', '_large', '_temp', '_watermarked']):
                    filepath = os.path.join(root, filename)
                    if os.path.getmtime(filepath) < cutoff_time:
                        try:
                            os.remove(filepath)
                            deleted_count += 1
                            logger.info(f"Deleted old file: {filepath}")
                        except Exception as e:
                            logger.error(f"Failed to delete {filepath}: {str(e)}")
        
        logger.info(f"Cleanup complete: deleted {deleted_count} old files")
        return {'deleted': deleted_count}
    except Exception as e:
        logger.error(f"Image cleanup task failed: {str(e)}")
        return {'error': str(e)}


@shared_task(bind=True)
def bulk_process_images(self, image_paths: List[str]) -> Dict[str, any]:
    """
    Process multiple images in parallel
    """
    try:
        logger.info(f"Starting bulk image processing for {len(image_paths)} images")
        
        # Create parallel optimization tasks
        job = group(
            optimize_image.s(image_path) for image_path in image_paths
        )
        
        result = job.apply_async()
        processed_images = result.get()
        
        successful = sum(1 for r in processed_images if r and 'error' not in str(r))
        
        logger.info(f"Bulk processing complete: {successful}/{len(image_paths)} successful")
        
        return {
            'total': len(image_paths),
            'successful': successful,
            'failed': len(image_paths) - successful
        }
    except Exception as e:
        logger.error(f"Bulk processing task failed: {str(e)}")
        raise


@shared_task
def add_watermark_to_property_images(property_id: str, watermark_text: str = "StayAfrica"):
    """
    Add watermarks to all property images
    """
    from apps.properties.models import Property, PropertyImage
    
    try:
        property_obj = Property.objects.get(id=property_id)
        count = 0
        
        # Watermark main image
        if property_obj.main_image and hasattr(property_obj.main_image, 'path'):
            result = ImageProcessorService.add_watermark(
                property_obj.main_image.path,
                watermark_text=watermark_text
            )
            if result:
                count += 1
        
        # Watermark additional images
        for prop_image in property_obj.images.all():
            if prop_image.image and hasattr(prop_image.image, 'path'):
                result = ImageProcessorService.add_watermark(
                    prop_image.image.path,
                    watermark_text=watermark_text
                )
                if result:
                    count += 1
        
        logger.info(f"Added watermarks to {count} images for property {property_id}")
        return {'property_id': property_id, 'watermarked': count}
    except Exception as e:
        logger.error(f"Watermarking failed for property {property_id}: {str(e)}")
        return {'error': str(e)}
    # Implementation for cleaning up old images
    pass
