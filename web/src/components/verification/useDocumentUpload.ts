import { useState, useCallback } from 'react';
import axios from 'axios';
import { DocumentUploadProgress } from '@/types/verification-types';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';

export const useDocumentUpload = () => {
  const [uploads, setUploads] = useState<Map<string, DocumentUploadProgress>>(new Map());

  const uploadFile = useCallback(async (file: File, fieldName: string): Promise<string> => {
    const uploadId = `${fieldName}_${Date.now()}`;
    
    setUploads(prev => new Map(prev).set(uploadId, {
      file,
      progress: 0,
    }));

    try {
      const token = localStorage.getItem('access_token');
      const formData = new FormData();
      formData.append('file', file);

      const response = await axios.post(
        `${API_BASE_URL}/api/v1/users/verification/upload/`,
        formData,
        {
          headers: {
            Authorization: `Bearer ${token}`,
            'Content-Type': 'multipart/form-data',
          },
          onUploadProgress: (progressEvent) => {
            const progress = progressEvent.total
              ? Math.round((progressEvent.loaded * 100) / progressEvent.total)
              : 0;
            
            setUploads(prev => {
              const next = new Map(prev);
              const upload = next.get(uploadId);
              if (upload) {
                next.set(uploadId, { ...upload, progress });
              }
              return next;
            });
          },
        }
      );

      const url = response.data.url || response.data.file_url;
      
      setUploads(prev => {
        const next = new Map(prev);
        const upload = next.get(uploadId);
        if (upload) {
          next.set(uploadId, { ...upload, progress: 100, url });
        }
        return next;
      });

      return url;
    } catch (error) {
      const errorMessage = axios.isAxiosError(error) 
        ? error.response?.data?.message || error.message
        : 'Upload failed';
      
      setUploads(prev => {
        const next = new Map(prev);
        const upload = next.get(uploadId);
        if (upload) {
          next.set(uploadId, { ...upload, error: errorMessage });
        }
        return next;
      });

      throw new Error(errorMessage);
    }
  }, []);

  const clearUpload = useCallback((uploadId: string) => {
    setUploads(prev => {
      const next = new Map(prev);
      next.delete(uploadId);
      return next;
    });
  }, []);

  return {
    uploads: Array.from(uploads.entries()).map(([id, data]) => ({ id, ...data })),
    uploadFile,
    clearUpload,
  };
};
