"""Image validation helpers for property media uploads."""
from typing import Iterable, Optional
from pathlib import Path
from PIL import Image
from django.core.exceptions import ValidationError


DEFAULT_ALLOWED_FORMATS: Iterable[str] = ("JPEG", "JPG", "PNG", "WEBP")


def validate_image_file(
    file_obj,
    *,
    max_size_mb: int = 8,
    max_width: int = 8000,
    max_height: int = 8000,
    allowed_formats: Optional[Iterable[str]] = None,
) -> None:
    """Validate an uploaded image for size, dimensions, and format.

    - Ensures file is under ``max_size_mb`` megabytes
    - Ensures the image can be opened by Pillow
    - Enforces a pixel dimension ceiling to avoid abusive uploads
    - Enforces a set of allowed formats
    """

    allowed = tuple(f.upper() for f in (allowed_formats or DEFAULT_ALLOWED_FORMATS))

    if hasattr(file_obj, "size") and file_obj.size is not None:
        if file_obj.size > max_size_mb * 1024 * 1024:
            raise ValidationError(f"Image exceeds {max_size_mb}MB limit")

    # Some storages provide content_type; keep it as an additional hint
    content_type = getattr(file_obj, "content_type", "") or ""
    if content_type and not content_type.startswith("image/"):
        raise ValidationError("File must be an image")

    try:
        file_obj.seek(0)
        with Image.open(file_obj) as img:
            img.verify()  # Quick verification without reading full image
    except Exception:
        raise ValidationError("Upload must be a valid image file")
    finally:
        try:
            file_obj.seek(0)
        except Exception:
            pass

    try:
        file_obj.seek(0)
        with Image.open(file_obj) as img:
            width, height = img.size
            image_format = (img.format or "").upper()

            if width > max_width or height > max_height:
                raise ValidationError(
                    f"Image dimensions {width}x{height} exceed allowed {max_width}x{max_height}"
                )

            if image_format and image_format not in allowed:
                raise ValidationError(
                    f"Unsupported image format {image_format}. Allowed: {', '.join(allowed)}"
                )
    finally:
        try:
            file_obj.seek(0)
        except Exception:
            pass


def safe_image_filename(original_name: str, suffix: str) -> str:
    """Generate a safe filename preserving the extension.

    The suffix should be unique (e.g., uuid). Extension derives from the
    provided original name; falls back to .jpg when missing.
    """

    ext = Path(original_name).suffix or ".jpg"
    return f"{suffix}{ext.lower()}"