'use client';

import { useState, useRef } from 'react';
import { Upload, X, FileText, Loader2 } from 'lucide-react';
import { useDocumentUpload } from './useDocumentUpload';
import Image from 'next/image';

interface DocumentUploadProps {
  documentType: 'PASSPORT' | 'NATIONAL_ID' | 'DRIVERS_LICENSE';
  onUploadComplete: (frontUrl: string, backUrl?: string) => void;
  requiresBackImage?: boolean;
}

export const DocumentUpload = ({ 
  documentType, 
  onUploadComplete,
  requiresBackImage = false 
}: DocumentUploadProps) => {
  const [frontPreview, setFrontPreview] = useState<string | null>(null);
  const [backPreview, setBackPreview] = useState<string | null>(null);
  const [frontUrl, setFrontUrl] = useState<string | null>(null);
  const [backUrl, setBackUrl] = useState<string | null>(null);
  const [isUploading, setIsUploading] = useState(false);
  
  const frontInputRef = useRef<HTMLInputElement>(null);
  const backInputRef = useRef<HTMLInputElement>(null);
  
  const { uploadFile } = useDocumentUpload();

  const handleFileSelect = async (file: File, side: 'front' | 'back') => {
    // Preview
    const reader = new FileReader();
    reader.onloadend = () => {
      if (side === 'front') {
        setFrontPreview(reader.result as string);
      } else {
        setBackPreview(reader.result as string);
      }
    };
    reader.readAsDataURL(file);

    // Upload
    setIsUploading(true);
    try {
      const url = await uploadFile(file, `${documentType}_${side}`);
      
      if (side === 'front') {
        setFrontUrl(url);
      } else {
        setBackUrl(url);
      }

      // Notify parent if both required uploads are complete
      if (side === 'front' && !requiresBackImage) {
        onUploadComplete(url);
      } else if (side === 'back' && frontUrl) {
        onUploadComplete(frontUrl, url);
      } else if (side === 'front' && backUrl) {
        onUploadComplete(url, backUrl);
      }
    } catch (error) {
      console.error('Upload failed:', error);
    } finally {
      setIsUploading(false);
    }
  };

  const handleFrontChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (file) handleFileSelect(file, 'front');
  };

  const handleBackChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (file) handleFileSelect(file, 'back');
  };

  const clearFront = () => {
    setFrontPreview(null);
    setFrontUrl(null);
    if (frontInputRef.current) frontInputRef.current.value = '';
  };

  const clearBack = () => {
    setBackPreview(null);
    setBackUrl(null);
    if (backInputRef.current) backInputRef.current.value = '';
  };

  const getDocumentLabel = () => {
    switch (documentType) {
      case 'PASSPORT': return 'Passport';
      case 'NATIONAL_ID': return 'National ID';
      case 'DRIVERS_LICENSE': return 'Driver\'s License';
    }
  };

  return (
    <div className="space-y-4">
      <div>
        <label className="block text-sm font-medium text-gray-700 mb-2">
          {getDocumentLabel()} - Front Side *
        </label>
        
        {!frontPreview ? (
          <div
            onClick={() => frontInputRef.current?.click()}
            className="border-2 border-dashed border-gray-300 rounded-lg p-8 text-center hover:border-blue-500 transition-colors cursor-pointer"
          >
            <Upload className="h-12 w-12 text-gray-400 mx-auto mb-2" />
            <p className="text-sm text-gray-600">Click to upload front side</p>
            <p className="text-xs text-gray-400 mt-1">PNG, JPG up to 10MB</p>
          </div>
        ) : (
          <div className="relative border rounded-lg overflow-hidden">
            <Image
              src={frontPreview}
              alt="Document front preview"
              width={400}
              height={300}
              className="w-full h-64 object-contain bg-gray-50"
            />
            <button
              onClick={clearFront}
              className="absolute top-2 right-2 p-1 bg-red-600 text-white rounded-full hover:bg-red-700"
              aria-label="Remove front image"
            >
              <X className="h-4 w-4" />
            </button>
            {isUploading && !frontUrl && (
              <div className="absolute inset-0 bg-black bg-opacity-50 flex items-center justify-center">
                <Loader2 className="h-8 w-8 text-white animate-spin" />
              </div>
            )}
          </div>
        )}
        
        <input
          ref={frontInputRef}
          type="file"
          accept="image/*"
          onChange={handleFrontChange}
          className="hidden"
        />
      </div>

      {requiresBackImage && (
        <div>
          <label className="block text-sm font-medium text-gray-700 mb-2">
            {getDocumentLabel()} - Back Side *
          </label>
          
          {!backPreview ? (
            <div
              onClick={() => backInputRef.current?.click()}
              className="border-2 border-dashed border-gray-300 rounded-lg p-8 text-center hover:border-blue-500 transition-colors cursor-pointer"
            >
              <FileText className="h-12 w-12 text-gray-400 mx-auto mb-2" />
              <p className="text-sm text-gray-600">Click to upload back side</p>
              <p className="text-xs text-gray-400 mt-1">PNG, JPG up to 10MB</p>
            </div>
          ) : (
            <div className="relative border rounded-lg overflow-hidden">
              <Image
                src={backPreview}
                alt="Document back preview"
                width={400}
                height={300}
                className="w-full h-64 object-contain bg-gray-50"
              />
              <button
                onClick={clearBack}
                className="absolute top-2 right-2 p-1 bg-red-600 text-white rounded-full hover:bg-red-700"
                aria-label="Remove back image"
              >
                <X className="h-4 w-4" />
              </button>
              {isUploading && !backUrl && (
                <div className="absolute inset-0 bg-black bg-opacity-50 flex items-center justify-center">
                  <Loader2 className="h-8 w-8 text-white animate-spin" />
                </div>
              )}
            </div>
          )}
          
          <input
            ref={backInputRef}
            type="file"
            accept="image/*"
            onChange={handleBackChange}
            className="hidden"
          />
        </div>
      )}
    </div>
  );
};
