"""
Two-Factor Authentication Service
Handles TOTP generation, verification, and backup codes
"""
import pyotp
import qrcode
import io
import base64
import secrets
from typing import Tuple, List
from django.contrib.auth import get_user_model

User = get_user_model()


class TwoFactorService:
    """Service for managing two-factor authentication"""
    
    @staticmethod
    def generate_secret() -> str:
        """Generate a new TOTP secret"""
        return pyotp.random_base32()
    
    @staticmethod
    def generate_provisioning_uri(user: User, secret: str) -> str:
        """
        Generate a provisioning URI for TOTP
        Used to create QR codes for authenticator apps
        """
        totp = pyotp.TOTP(secret)
        return totp.provisioning_uri(
            name=user.email,
            issuer_name='StayAfrica'
        )
    
    @staticmethod
    def generate_qr_code(provisioning_uri: str) -> str:
        """
        Generate a QR code image from provisioning URI
        Returns base64 encoded image
        """
        qr = qrcode.QRCode(version=1, box_size=10, border=5)
        qr.add_data(provisioning_uri)
        qr.make(fit=True)
        
        img = qr.make_image(fill_color="black", back_color="white")
        
        # Convert to base64
        buffer = io.BytesIO()
        img.save(buffer, format='PNG')
        buffer.seek(0)
        img_str = base64.b64encode(buffer.getvalue()).decode()
        
        return f"data:image/png;base64,{img_str}"
    
    @staticmethod
    def verify_token(secret: str, token: str) -> bool:
        """
        Verify a TOTP token
        Returns True if valid, False otherwise
        """
        if not secret or not token:
            return False
        
        totp = pyotp.TOTP(secret)
        # Allow for time drift (±1 interval)
        return totp.verify(token, valid_window=1)
    
    @staticmethod
    def generate_backup_codes(count: int = 8) -> List[str]:
        """
        Generate backup codes for 2FA recovery
        Returns a list of random backup codes
        """
        return [
            f"{secrets.token_hex(4)}-{secrets.token_hex(4)}"
            for _ in range(count)
        ]
    
    @staticmethod
    def verify_backup_code(user: User, code: str) -> bool:
        """
        Verify and consume a backup code
        Returns True if valid, False otherwise
        """
        if not user.backup_codes or code not in user.backup_codes:
            return False
        
        # Remove the used backup code
        user.backup_codes.remove(code)
        user.save(update_fields=['backup_codes'])
        
        return True
    
    @staticmethod
    def setup_2fa(user: User) -> Tuple[str, str, List[str]]:
        """
        Setup 2FA for a user
        Returns: (secret, qr_code_data_uri, backup_codes)
        """
        # Generate secret and backup codes
        secret = TwoFactorService.generate_secret()
        backup_codes = TwoFactorService.generate_backup_codes()
        
        # Generate QR code
        provisioning_uri = TwoFactorService.generate_provisioning_uri(user, secret)
        qr_code = TwoFactorService.generate_qr_code(provisioning_uri)
        
        # Save to user (not enabled yet)
        user.two_factor_secret = secret
        user.backup_codes = backup_codes
        user.save(update_fields=['two_factor_secret', 'backup_codes'])
        
        return secret, qr_code, backup_codes
    
    @staticmethod
    def enable_2fa(user: User, token: str) -> bool:
        """
        Enable 2FA after verifying setup token
        Returns True if successful, False otherwise
        """
        if not user.two_factor_secret:
            return False
        
        if TwoFactorService.verify_token(user.two_factor_secret, token):
            user.two_factor_enabled = True
            user.save(update_fields=['two_factor_enabled'])
            return True
        
        return False
    
    @staticmethod
    def disable_2fa(user: User):
        """Disable 2FA for a user"""
        user.two_factor_enabled = False
        user.two_factor_secret = None
        user.backup_codes = []
        user.save(update_fields=['two_factor_enabled', 'two_factor_secret', 'backup_codes'])
    
    @staticmethod
    def regenerate_backup_codes(user: User) -> List[str]:
        """
        Regenerate backup codes for a user
        Returns new list of backup codes
        """
        backup_codes = TwoFactorService.generate_backup_codes()
        user.backup_codes = backup_codes
        user.save(update_fields=['backup_codes'])
        
        return backup_codes
