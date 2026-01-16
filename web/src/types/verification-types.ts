export interface VerificationDocument {
  id: string;
  document_type: 'PASSPORT' | 'NATIONAL_ID' | 'DRIVERS_LICENSE';
  document_number: string;
  front_image: string;
  back_image?: string;
  issued_country: string;
  expiry_date?: string;
  uploaded_at: string;
}

export interface VerificationSelfie {
  id: string;
  image: string;
  uploaded_at: string;
}

export interface VerificationStatus {
  id: string;
  user: string;
  status: 'PENDING' | 'UNDER_REVIEW' | 'APPROVED' | 'REJECTED';
  document?: VerificationDocument;
  selfie?: VerificationSelfie;
  rejection_reason?: string;
  admin_notes?: string;
  verified_at?: string;
  created_at: string;
  updated_at: string;
}

export interface DocumentUploadProgress {
  file: File;
  progress: number;
  url?: string;
  error?: string;
}
