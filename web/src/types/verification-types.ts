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
  document_type?: string;
  document_type_display?: string;
  document_number?: string;
  document_country?: string;
  document_expiry_date?: string;
  document_front_image?: string;
  document_back_image?: string;
  selfie_image?: string;
  document?: VerificationDocument;
  selfie?: VerificationSelfie;
  rejection_reason?: string;
  admin_notes?: string;
  verified_at?: string;
  created_at: string;
  updated_at: string;
  expires_at?: string;
  is_expired?: boolean;
}

export interface DocumentUploadProgress {
  file: File;
  progress: number;
  url?: string;
  error?: string;
}
