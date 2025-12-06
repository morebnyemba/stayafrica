"""
Image Processing Service
Handles image optimization and thumbnail generation
"""
from celery import shared_task
from PIL import Image
import io

class ImageProcessorService:
    @staticmethod
    @shared_task
    def optimize_image(image_path, max_width=1920, max_height=1080, quality=85):
        """Optimize image size and quality"""
        try:
            img = Image.open(image_path)
            img.thumbnail((max_width, max_height), Image.Resampling.LANCZOS)
            img.save(image_path, quality=quality, optimize=True)
        except Exception as e:
            print(f'Error optimizing image: {e}')
    
    @staticmethod
    @shared_task
    def generate_thumbnails(image_path):
        """Generate thumbnail versions of image"""
        sizes = [
            ('thumb', (300, 300)),
            ('medium', (600, 600)),
            ('large', (1200, 1200)),
        ]
        
        try:
            img = Image.open(image_path)
            for name, size in sizes:
                thumb = img.copy()
                thumb.thumbnail(size, Image.Resampling.LANCZOS)
                # Save thumbnail
        except Exception as e:
            print(f'Error generating thumbnails: {e}')
