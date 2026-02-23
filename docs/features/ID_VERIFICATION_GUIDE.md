# Enhanced ID Verification System

## Overview
Comprehensive identity verification system with document upload, selfie capture, and admin review workflow.

## Features Implemented
- ✅ Document upload (Passport, National ID, Driver's License)
- ✅ Selfie capture for identity matching
- ✅ Admin review interface with approve/reject workflow
- ✅ Rate limiting (attempts per day/month)
- ✅ Verification expiry (2 years default)
- ✅ Automatic status updates
- ✅ Push notification integration
- ✅ Verification attempt tracking
- ✅ Configurable settings
- ✅ Image validation (size, format, dimensions)

## Models

### IdentityVerification
Main model for verification requests with:
- Document type (passport, national_id, drivers_license)
- Document number and country
- Document expiry date
- Uploaded images (front, back, selfie)
- Verification status (pending, under_review, approved, rejected, expired)
- Review details (reviewer, timestamp, notes, rejection reason)
- Expiry management

### VerificationAttempt
Tracks all verification attempts for:
- Rate limiting enforcement
- Fraud detection
- Audit trail
- IP address tracking

### VerificationSettings
Global configuration for:
- Rate limits (attempts per day/month)
- Document requirements
- Image requirements (size, dimensions)
- Verification expiry period
- Host/guest requirements
- Third-party service integration

## API Endpoints

### Submit Verification
```http
POST /api/v1/users/verification/
Content-Type: multipart/form-data

Fields:
- document_type: 'passport' | 'national_id' | 'drivers_license'
- document_number: string
- document_country: string
- document_expiry_date: 'YYYY-MM-DD' (optional)
- document_front_image: file (required)
- document_back_image: file (optional)
- selfie_image: file (required)
```

**Response**:
```json
{
  "id": "uuid",
  "document_type": "passport",
  "document_number": "P12345678",
  "status": "pending",
  "submitted_at": "2026-01-16T10:00:00Z"
}
```

**Validation**:
- Max 3 attempts per day (configurable)
- Image size ≤ 10MB (configurable)
- Image format: JPG, JPEG, PNG
- Document must not be expired
- No duplicate pending/approved verifications

---

### Get Current Status
```http
GET /api/v1/users/verification/current_status/
```

**Response**:
```json
{
  "has_verification": true,
  "is_verified": true,
  "verification": {
    "id": "uuid",
    "status": "approved",
    "document_type": "passport",
    "submitted_at": "2026-01-16T10:00:00Z",
    "reviewed_at": "2026-01-16T12:00:00Z",
    "expires_at": "2028-01-16T12:00:00Z"
  }
}
```

---

### List User's Verifications
```http
GET /api/v1/users/verification/
```

**Response**:
```json
{
  "count": 2,
  "results": [
    {
      "id": "uuid",
      "status": "approved",
      "document_type": "passport",
      "submitted_at": "2026-01-16T10:00:00Z"
    }
  ]
}
```

---

### Admin: Review Verification
```http
POST /api/v1/users/verification/{id}/review/
Content-Type: application/json

{
  "action": "approve",  // or "reject"
  "reason": "Document is blurry",  // required for reject
  "notes": "Admin notes"  // optional
}
```

**Response**:
```json
{
  "message": "Verification approved successfully",
  "verification": { ... }
}
```

---

### Admin: Pending Reviews
```http
GET /api/v1/users/verification/pending_reviews/
```

**Response**:
```json
{
  "count": 5,
  "results": [ ... ]
}
```

---

### Admin: Statistics
```http
GET /api/v1/users/verification/statistics/
```

**Response**:
```json
{
  "total_verifications": 150,
  "by_status": {
    "pending": 10,
    "approved": 120,
    "rejected": 15,
    "expired": 5
  },
  "recent_submissions": 25,
  "recent_approved": 20,
  "approval_rate": 88.89,
  "pending_review": 10
}
```

---

### Admin: Get/Update Settings
```http
GET /api/v1/users/verification-settings/
PUT /api/v1/users/verification-settings/1/
```

**Response/Body**:
```json
{
  "max_attempts_per_day": 3,
  "max_attempts_per_month": 10,
  "require_document_back": false,
  "require_selfie": true,
  "min_image_width": 800,
  "min_image_height": 600,
  "max_image_size_mb": 10,
  "verification_valid_years": 2,
  "require_verification_for_hosting": true,
  "require_verification_for_booking": false,
  "use_third_party_service": false
}
```

## Client Integration

### Web (React/Next.js)

```javascript
// Submit verification
const submitVerification = async (data) => {
  const formData = new FormData();
  formData.append('document_type', data.documentType);
  formData.append('document_number', data.documentNumber);
  formData.append('document_country', data.documentCountry);
  formData.append('document_front_image', data.frontImage);
  formData.append('selfie_image', data.selfieImage);
  
  if (data.backImage) {
    formData.append('document_back_image', data.backImage);
  }
  
  const response = await fetch('/api/v1/users/verification/', {
    method: 'POST',
    headers: {
      'Authorization': `Bearer ${token}`,
    },
    body: formData,
  });
  
  return response.json();
};

// Check verification status
const checkStatus = async () => {
  const response = await fetch('/api/v1/users/verification/current_status/', {
    headers: {
      'Authorization': `Bearer ${token}`,
    },
  });
  
  return response.json();
};
```

### Mobile (React Native)

```javascript
import * as ImagePicker from 'expo-image-picker';
import * as DocumentPicker from 'expo-document-picker';

const captureDocument = async () => {
  const { status } = await ImagePicker.requestCameraPermissionsAsync();
  
  if (status !== 'granted') {
    alert('Camera permission required');
    return null;
  }
  
  const result = await ImagePicker.launchCameraAsync({
    mediaTypes: ImagePicker.MediaTypeOptions.Images,
    quality: 0.8,
    base64: false,
  });
  
  if (!result.canceled) {
    return result.assets[0];
  }
  
  return null;
};

const captureSelfie = async () => {
  const result = await ImagePicker.launchCameraAsync({
    mediaTypes: ImagePicker.MediaTypeOptions.Images,
    quality: 0.8,
    cameraType: ImagePicker.CameraType.front, // Front camera
    base64: false,
  });
  
  if (!result.canceled) {
    return result.assets[0];
  }
  
  return null;
};

const submitVerification = async (data) => {
  const formData = new FormData();
  
  formData.append('document_type', data.documentType);
  formData.append('document_number', data.documentNumber);
  formData.append('document_country', data.documentCountry);
  
  // Add document front image
  formData.append('document_front_image', {
    uri: data.frontImage.uri,
    type: 'image/jpeg',
    name: 'document_front.jpg',
  });
  
  // Add selfie
  formData.append('selfie_image', {
    uri: data.selfieImage.uri,
    type: 'image/jpeg',
    name: 'selfie.jpg',
  });
  
  const response = await fetch(`${API_URL}/users/verification/`, {
    method: 'POST',
    headers: {
      'Authorization': `Bearer ${token}`,
    },
    body: formData,
  });
  
  return response.json();
};
```

## Admin Review Workflow

### Dashboard
1. Navigate to **Admin > Users > Identity Verifications**
2. Filter by status: "Pending" or "Under Review"
3. View submitted documents and selfie
4. Review document details

### Approval Process
1. Verify document is:
   - Clear and readable
   - Not expired
   - Matches user's profile information
2. Verify selfie:
   - Face is clearly visible
   - Matches document photo (manual check)
3. Click "Approve" or "Reject"
4. Add notes for internal record
5. User receives push notification

### Bulk Actions
- Select multiple verifications
- Choose "Approve selected verifications"
- All selected verifications approved at once

## Security & Privacy

### Data Protection
- Images stored securely with restricted access
- HTTPS required for all uploads
- Images encrypted at rest
- Access logged for audit trail

### Privacy Compliance
- Images deleted after verification period expires
- User can request data deletion
- Minimal PII stored in metadata
- GDPR/POPIA compliant

### Fraud Prevention
- Rate limiting prevents spam
- IP address tracking for suspicious activity
- Multiple failed attempts flagged for review
- Document number uniqueness check (optional)

## Configuration

### Django Settings
```python
# Media files configuration
MEDIA_URL = '/media/'
MEDIA_ROOT = BASE_DIR / 'media'

# File upload settings
FILE_UPLOAD_MAX_MEMORY_SIZE = 10485760  # 10MB
DATA_UPLOAD_MAX_MEMORY_SIZE = 10485760  # 10MB
```

### Verification Settings (via Admin or API)
- **Max attempts per day**: 3
- **Max attempts per month**: 10
- **Require document back**: False (depends on document type)
- **Require selfie**: True
- **Min image width**: 800px
- **Min image height**: 600px
- **Max image size**: 10MB
- **Verification validity**: 2 years
- **Require for hosting**: True
- **Require for booking**: False

## Monitoring & Maintenance

### Admin Tasks
- Review pending verifications daily
- Monitor approval/rejection rates
- Check for suspicious patterns
- Update settings as needed
- Purge expired verifications

### Automated Tasks (Celery)
```python
from celery import shared_task
from apps.users.verification_models import IdentityVerification

@shared_task
def check_expired_verifications():
    """Check and expire old verifications"""
    verifications = IdentityVerification.objects.filter(
        status='approved',
        expires_at__lte=timezone.now()
    )
    
    for verification in verifications:
        verification.check_and_expire()
```

### Metrics to Track
- Submission rate (daily/weekly)
- Approval rate (%)
- Average review time
- Rejection reasons (categorized)
- Expired verifications
- Re-submission rate

## Troubleshooting

### Image Upload Fails
- Check file size < 10MB
- Verify format is JPG/JPEG/PNG
- Ensure proper permissions
- Check MEDIA_ROOT is writable

### Rate Limit Exceeded
- User exceeded max attempts per day
- Wait 24 hours or admin can reset
- Check VerificationAttempt records

### Verification Not Approved
- Document may be unclear/expired
- Selfie doesn't match
- Information mismatch
- Check rejection_reason in admin

## Future Enhancements
- [ ] AI-powered document verification (OCR)
- [ ] Liveness detection for selfies
- [ ] Integration with Smile Identity
- [ ] Integration with Onfido
- [ ] Video verification option
- [ ] Batch document processing
- [ ] Advanced fraud detection
- [ ] Biometric matching
- [ ] Blockchain verification records

## Testing

### Manual Testing
1. Submit verification with valid documents
2. Check submission appears in admin
3. Admin reviews and approves
4. User receives notification
5. Check `user.is_verified` is True

### API Testing
```python
# Test verification submission
response = client.post('/api/v1/users/verification/', {
    'document_type': 'passport',
    'document_number': 'P12345678',
    'document_country': 'ZW',
    'document_front_image': front_image_file,
    'selfie_image': selfie_file,
})
assert response.status_code == 201

# Test rate limiting
for i in range(4):
    response = client.post('/api/v1/users/verification/', data)
    if i < 3:
        assert response.status_code == 201
    else:
        assert response.status_code == 400
        assert 'exceeded' in response.json()['error'].lower()
```

---

**Implementation Complete!** ✅
- All API endpoints functional
- Admin review interface ready
- Mobile/web integration guides provided
- Security and privacy compliant
