"""
Image Processing Service for StayAfrica
Handles image optimization, thumbnail generation, format conversion, and watermarking

Features:
- Automatic format conversion (WebP for web, JPEG fallback)
- Multiple thumbnail sizes for responsive images
- Image compression and optimization
- EXIF data handling and privacy protection
- Watermarking support
- Async processing via Celery
- Error handling and logging
"""
from PIL import Image, ImageDraw, ImageFont, ImageFilter
from PIL.ExifTags import TAGS
from pathlib import Path
from typing import Tuple, Optional, Dict, List
import io
import os
import logging
from django.core.files.base import ContentFile
from django.core.files.storage import default_storage

logger = logging.getLogger(__name__)


class ImageProcessorService:
    """
    Comprehensive image processing service with optimization and transformations
    """
    
    # Image size presets for different use cases
    SIZES = {
        'thumbnail': (300, 300),      # Property cards, lists
        'small': (600, 400),           # Mobile views
        'medium': (1024, 768),         # Tablet/desktop views
        'large': (1920, 1080),         # Full HD displays
        'original': None,              # Keep original size
    }
    
    # Supported formats
    SUPPORTED_FORMATS = ['JPEG', 'PNG', 'WebP', 'GIF']
    
    # Quality settings
    DEFAULT_QUALITY = 85
    THUMBNAIL_QUALITY = 80
    WEBP_QUALITY = 85
    
    @classmethod
    def optimize_image(
        cls,
        image_path: str,
        max_width: int = 1920,
        max_height: int = 1080,
        quality: int = 85,
        convert_to_webp: bool = True,
        strip_exif: bool = True
    ) -> Dict[str, str]:
        """
        Optimize image size, quality, and format
        
        Args:
            image_path: Path to the image file
            max_width: Maximum width in pixels
            max_height: Maximum height in pixels
            quality: JPEG/WebP quality (1-100)
            convert_to_webp: Convert to WebP format for better compression
            strip_exif: Remove EXIF data for privacy
        
        Returns:
            Dict with paths to optimized images
        """
        try:
            with Image.open(image_path) as img:
                # Store original format
                original_format = img.format
                
                # Convert RGBA to RGB if needed
                if img.mode in ('RGBA', 'LA', 'P'):
                    background = Image.new('RGB', img.size, (255, 255, 255))
                    if img.mode == 'P':
                        img = img.convert('RGBA')
                    background.paste(img, mask=img.split()[-1] if img.mode == 'RGBA' else None)
                    img = background
                elif img.mode != 'RGB':
                    img = img.convert('RGB')
                
                # Resize if needed
                if img.width > max_width or img.height > max_height:
                    img.thumbnail((max_width, max_height), Image.Resampling.LANCZOS)
                    logger.info(f"Resized image from original to {img.width}x{img.height}")
                
                # Strip EXIF data for privacy
                if strip_exif:
                    img_without_exif = Image.new(img.mode, img.size)
                    img_without_exif.putdata(list(img.getdata()))
                    img = img_without_exif
                
                paths = {}
                base_path = Path(image_path)
                
                # Save optimized JPEG (fallback for older browsers)
                jpeg_path = str(base_path.with_suffix('.jpg'))
                img.save(
                    jpeg_path,
                    'JPEG',
                    quality=quality,
                    optimize=True,
                    progressive=True
                )
                paths['jpeg'] = jpeg_path
                logger.info(f"Saved optimized JPEG: {jpeg_path}")
                
                # Save WebP (better compression)
                if convert_to_webp:
                    webp_path = str(base_path.with_suffix('.webp'))
                    img.save(
                        webp_path,
                        'WebP',
                        quality=cls.WEBP_QUALITY,
                        method=6  # Maximum compression
                    )
                    paths['webp'] = webp_path
                    logger.info(f"Saved WebP: {webp_path}")
                
                return paths
                
        except Exception as e:
            logger.error(f"Error optimizing image {image_path}: {str(e)}", exc_info=True)
            return {'error': str(e)}
        except Exception as e:
            logger.error(f"Error optimizing image {image_path}: {str(e)}", exc_info=True)
            return {'error': str(e)}
    
    @classmethod
    def generate_thumbnails(
        cls,
        image_path: str,
        sizes: Optional[Dict[str, Tuple[int, int]]] = None,
        output_dir: Optional[str] = None
    ) -> Dict[str, str]:
        """
        Generate multiple thumbnail sizes from an image
        
        Args:
            image_path: Path to the original image
            sizes: Dict of size_name: (width, height) tuples (uses defaults if None)
            output_dir: Directory to save thumbnails (uses same dir as original if None)
        
        Returns:
            Dict mapping size names to thumbnail paths
        """
        if sizes is None:
            sizes = cls.SIZES
        
        try:
            with Image.open(image_path) as img:
                # Convert to RGB if needed
                if img.mode in ('RGBA', 'LA', 'P'):
                    background = Image.new('RGB', img.size, (255, 255, 255))
                    if img.mode == 'P':
                        img = img.convert('RGBA')
                    background.paste(img, mask=img.split()[-1] if img.mode == 'RGBA' else None)
                    img = background
                elif img.mode != 'RGB':
                    img = img.convert('RGB')
                
                base_path = Path(image_path)
                output_dir = output_dir or str(base_path.parent)
                thumbnails = {}
                
                for size_name, dimensions in sizes.items():
                    if dimensions is None:
                        continue  # Skip 'original' size
                    
                    # Create thumbnail
                    thumb = img.copy()
                    thumb.thumbnail(dimensions, Image.Resampling.LANCZOS)
                    
                    # Generate filename
                    thumb_filename = f"{base_path.stem}_{size_name}{base_path.suffix}"
                    thumb_path = os.path.join(output_dir, thumb_filename)
                    
                    # Save thumbnail
                    thumb.save(
                        thumb_path,
                        'JPEG' if base_path.suffix.lower() in ['.jpg', '.jpeg'] else 'WebP',
                        quality=cls.THUMBNAIL_QUALITY,
                        optimize=True
                    )
                    
                    thumbnails[size_name] = thumb_path
                    logger.info(f"Generated {size_name} thumbnail: {thumb_path}")
                
                return thumbnails
                
        except Exception as e:
            logger.error(f"Error generating thumbnails for {image_path}: {str(e)}", exc_info=True)
            return {'error': str(e)}
    
    @classmethod
    def add_watermark(
        cls,
        image_path: str,
        watermark_text: str = "StayAfrica",
        position: str = 'bottom-right',
        opacity: int = 128,
        font_size: int = 36
    ) -> Optional[str]:
        """
        Add watermark to an image
        
        Args:
            image_path: Path to the image
            watermark_text: Text to use as watermark
            position: 'bottom-right', 'bottom-left', 'top-right', 'top-left', 'center'
            opacity: Watermark opacity (0-255)
            font_size: Size of watermark text
        
        Returns:
            Path to watermarked image or None on error
        """
        try:
            with Image.open(image_path) as img:
                # Create transparent layer for watermark
                watermark = Image.new('RGBA', img.size, (0, 0, 0, 0))
                draw = ImageDraw.Draw(watermark)
                
                # Try to use a nice font, fallback to default
                try:
                    font = ImageFont.truetype("arial.ttf", font_size)
                except:
                    font = ImageFont.load_default()
                
                # Calculate text position
                bbox = draw.textbbox((0, 0), watermark_text, font=font)
                text_width = bbox[2] - bbox[0]
                text_height = bbox[3] - bbox[1]
                
                margin = 20
                if position == 'bottom-right':
                    x = img.width - text_width - margin
                    y = img.height - text_height - margin
                elif position == 'bottom-left':
                    x = margin
                    y = img.height - text_height - margin
                elif position == 'top-right':
                    x = img.width - text_width - margin
                    y = margin
                elif position == 'top-left':
                    x = margin
                    y = margin
                else:  # center
                    x = (img.width - text_width) // 2
                    y = (img.height - text_height) // 2
                
                # Draw text with semi-transparency
                draw.text((x, y), watermark_text, fill=(255, 255, 255, opacity), font=font)
                
                # Composite watermark onto image
                if img.mode != 'RGBA':
                    img = img.convert('RGBA')
                watermarked = Image.alpha_composite(img, watermark)
                
                # Save watermarked image
                base_path = Path(image_path)
                output_path = str(base_path.with_name(f"{base_path.stem}_watermarked{base_path.suffix}"))
                watermarked.convert('RGB').save(output_path, quality=cls.DEFAULT_QUALITY)
                
                logger.info(f"Added watermark to {image_path}")
                return output_path
                
        except Exception as e:
            logger.error(f"Error adding watermark to {image_path}: {str(e)}", exc_info=True)
            return None
    
    @classmethod
    def extract_exif_data(cls, image_path: str) -> Dict[str, any]:
        """
        Extract EXIF metadata from image
        
        Args:
            image_path: Path to the image
        
        Returns:
            Dict of EXIF data
        """
        try:
            with Image.open(image_path) as img:
                exif_data = {}
                
                if hasattr(img, '_getexif') and img._getexif():
                    exif = img._getexif()
                    for tag_id, value in exif.items():
                        tag = TAGS.get(tag_id, tag_id)
                        exif_data[tag] = value
                
                return exif_data
                
        except Exception as e:
            logger.error(f"Error extracting EXIF from {image_path}: {str(e)}")
            return {}
    
    @classmethod
    def get_image_info(cls, image_path: str) -> Dict[str, any]:
        """
        Get comprehensive image information
        
        Args:
            image_path: Path to the image
        
        Returns:
            Dict with image dimensions, format, size, etc.
        """
        try:
            with Image.open(image_path) as img:
                file_size = os.path.getsize(image_path)
                
                return {
                    'width': img.width,
                    'height': img.height,
                    'format': img.format,
                    'mode': img.mode,
                    'size_bytes': file_size,
                    'size_mb': round(file_size / (1024 * 1024), 2),
                    'aspect_ratio': round(img.width / img.height, 2) if img.height > 0 else 0,
                }
                
        except Exception as e:
            logger.error(f"Error getting image info for {image_path}: {str(e)}")
            return {}
    
    @classmethod
    def convert_format(
        cls,
        image_path: str,
        target_format: str = 'WebP',
        quality: int = 85
    ) -> Optional[str]:
        """
        Convert image to different format
        
        Args:
            image_path: Path to the image
            target_format: Target format ('JPEG', 'PNG', 'WebP')
            quality: Quality setting for lossy formats
        
        Returns:
            Path to converted image or None on error
        """
        if target_format not in cls.SUPPORTED_FORMATS:
            logger.error(f"Unsupported format: {target_format}")
            return None
        
        try:
            with Image.open(image_path) as img:
                # Convert mode if needed
                if target_format == 'JPEG' and img.mode in ('RGBA', 'LA', 'P'):
                    background = Image.new('RGB', img.size, (255, 255, 255))
                    if img.mode == 'P':
                        img = img.convert('RGBA')
                    background.paste(img, mask=img.split()[-1] if img.mode == 'RGBA' else None)
                    img = background
                
                # Generate output path
                base_path = Path(image_path)
                ext_map = {'JPEG': '.jpg', 'PNG': '.png', 'WebP': '.webp', 'GIF': '.gif'}
                output_path = str(base_path.with_suffix(ext_map.get(target_format, '.jpg')))
                
                # Save in new format
                save_kwargs = {'quality': quality, 'optimize': True}
                if target_format == 'PNG':
                    save_kwargs = {'optimize': True}
                
                img.save(output_path, target_format, **save_kwargs)
                logger.info(f"Converted {image_path} to {target_format}")
                
                return output_path
                
        except Exception as e:
            logger.error(f"Error converting {image_path} to {target_format}: {str(e)}", exc_info=True)
            return None
    
    @classmethod
    def apply_blur(
        cls,
        image_path: str,
        radius: int = 10
    ) -> Optional[str]:
        """
        Apply blur effect to image (useful for backgrounds or privacy)
        
        Args:
            image_path: Path to the image
            radius: Blur radius (higher = more blur)
        
        Returns:
            Path to blurred image or None on error
        """
        try:
            with Image.open(image_path) as img:
                blurred = img.filter(ImageFilter.GaussianBlur(radius=radius))
                
                base_path = Path(image_path)
                output_path = str(base_path.with_name(f"{base_path.stem}_blurred{base_path.suffix}"))
                blurred.save(output_path, quality=cls.DEFAULT_QUALITY)
                
                logger.info(f"Applied blur to {image_path}")
                return output_path
                
        except Exception as e:
            logger.error(f"Error applying blur to {image_path}: {str(e)}", exc_info=True)
            return None
    
    @classmethod
    def validate_image(
        cls,
        image_path: str,
        max_size_mb: int = 10,
        min_width: int = 800,
        min_height: int = 600,
        allowed_formats: Optional[List[str]] = None
    ) -> Tuple[bool, str]:
        """
        Validate image meets requirements
        
        Args:
            image_path: Path to the image
            max_size_mb: Maximum file size in MB
            min_width: Minimum width in pixels
            min_height: Minimum height in pixels
            allowed_formats: List of allowed formats
        
        Returns:
            Tuple of (is_valid, error_message)
        """
        if allowed_formats is None:
            allowed_formats = cls.SUPPORTED_FORMATS
        
        try:
            # Check file size
            file_size_mb = os.path.getsize(image_path) / (1024 * 1024)
            if file_size_mb > max_size_mb:
                return False, f"File size {file_size_mb:.2f}MB exceeds maximum {max_size_mb}MB"
            
            with Image.open(image_path) as img:
                # Check format
                if img.format not in allowed_formats:
                    return False, f"Format {img.format} not allowed. Allowed: {', '.join(allowed_formats)}"
                
                # Check dimensions
                if img.width < min_width:
                    return False, f"Width {img.width}px below minimum {min_width}px"
                if img.height < min_height:
                    return False, f"Height {img.height}px below minimum {min_height}px"
            
            return True, "Valid"
            
        except Exception as e:
            return False, f"Invalid image: {str(e)}"
