'use client';

import { useState, useRef, useEffect } from 'react';
import { Camera, X, RefreshCw, Loader2 } from 'lucide-react';
import { useDocumentUpload } from './useDocumentUpload';
import Image from 'next/image';

interface SelfieCaptureProps {
  onCaptureComplete: (imageUrl: string) => void;
}

export const SelfieCapture = ({ onCaptureComplete }: SelfieCaptureProps) => {
  const [preview, setPreview] = useState<string | null>(null);
  const [imageUrl, setImageUrl] = useState<string | null>(null);
  const [isUploading, setIsUploading] = useState(false);
  const [isCameraActive, setIsCameraActive] = useState(false);
  const [stream, setStream] = useState<MediaStream | null>(null);
  
  const videoRef = useRef<HTMLVideoElement>(null);
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const fileInputRef = useRef<HTMLInputElement>(null);
  
  const { uploadFile } = useDocumentUpload();

  useEffect(() => {
    return () => {
      // Cleanup: stop camera stream on unmount
      if (stream) {
        stream.getTracks().forEach(track => track.stop());
      }
    };
  }, [stream]);

  const startCamera = async () => {
    try {
      const mediaStream = await navigator.mediaDevices.getUserMedia({ 
        video: { facingMode: 'user' } 
      });
      
      if (videoRef.current) {
        videoRef.current.srcObject = mediaStream;
      }
      
      setStream(mediaStream);
      setIsCameraActive(true);
    } catch (error) {
      console.error('Error accessing camera:', error);
      alert('Unable to access camera. Please check permissions or upload a photo instead.');
    }
  };

  const stopCamera = () => {
    if (stream) {
      stream.getTracks().forEach(track => track.stop());
      setStream(null);
    }
    setIsCameraActive(false);
  };

  const capturePhoto = () => {
    if (videoRef.current && canvasRef.current) {
      const video = videoRef.current;
      const canvas = canvasRef.current;
      
      canvas.width = video.videoWidth;
      canvas.height = video.videoHeight;
      
      const ctx = canvas.getContext('2d');
      if (ctx) {
        ctx.drawImage(video, 0, 0);
        const imageData = canvas.toDataURL('image/jpeg');
        setPreview(imageData);
        stopCamera();
        
        // Convert to blob and upload
        canvas.toBlob(async (blob) => {
          if (blob) {
            const file = new File([blob], 'selfie.jpg', { type: 'image/jpeg' });
            await handleUpload(file);
          }
        }, 'image/jpeg');
      }
    }
  };

  const handleFileSelect = async (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (file) {
      // Preview
      const reader = new FileReader();
      reader.onloadend = () => {
        setPreview(reader.result as string);
      };
      reader.readAsDataURL(file);
      
      await handleUpload(file);
    }
  };

  const handleUpload = async (file: File) => {
    setIsUploading(true);
    try {
      const url = await uploadFile(file, 'selfie');
      setImageUrl(url);
      onCaptureComplete(url);
    } catch (error) {
      console.error('Upload failed:', error);
    } finally {
      setIsUploading(false);
    }
  };

  const clear = () => {
    setPreview(null);
    setImageUrl(null);
    stopCamera();
    if (fileInputRef.current) fileInputRef.current.value = '';
  };

  return (
    <div className="space-y-4">
      <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
        <h4 className="text-sm font-medium text-blue-900 mb-2">Selfie Guidelines</h4>
        <ul className="text-xs text-blue-800 space-y-1">
          <li>• Face the camera directly</li>
          <li>• Remove glasses and hats</li>
          <li>• Ensure good lighting</li>
          <li>• Keep a neutral expression</li>
          <li>• Make sure your full face is visible</li>
        </ul>
      </div>

      {!preview ? (
        <div className="space-y-4">
          {isCameraActive ? (
            <div className="relative">
              <video
                ref={videoRef}
                autoPlay
                playsInline
                className="w-full rounded-lg border"
              />
              <div className="flex gap-2 mt-4">
                <button
                  onClick={capturePhoto}
                  className="flex-1 flex items-center justify-center gap-2 px-4 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
                >
                  <Camera className="h-5 w-5" />
                  <span>Capture Photo</span>
                </button>
                <button
                  onClick={stopCamera}
                  className="px-4 py-3 border border-gray-300 rounded-lg hover:bg-gray-50"
                >
                  Cancel
                </button>
              </div>
            </div>
          ) : (
            <div className="space-y-3">
              <button
                onClick={startCamera}
                className="w-full flex items-center justify-center gap-2 px-4 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
              >
                <Camera className="h-5 w-5" />
                <span>Use Camera</span>
              </button>
              
              <div className="relative">
                <div className="absolute inset-0 flex items-center">
                  <div className="w-full border-t border-gray-300" />
                </div>
                <div className="relative flex justify-center text-sm">
                  <span className="px-2 bg-white text-gray-500">or</span>
                </div>
              </div>
              
              <button
                onClick={() => fileInputRef.current?.click()}
                className="w-full flex items-center justify-center gap-2 px-4 py-3 border border-gray-300 rounded-lg hover:bg-gray-50"
              >
                <Camera className="h-5 w-5" />
                <span>Upload Photo</span>
              </button>
            </div>
          )}
        </div>
      ) : (
        <div className="relative border rounded-lg overflow-hidden">
          <Image
            src={preview}
            alt="Selfie preview"
            width={400}
            height={400}
            className="w-full h-96 object-contain bg-gray-50"
          />
          <div className="absolute top-2 right-2 flex gap-2">
            <button
              onClick={clear}
              className="p-2 bg-white text-gray-700 rounded-full shadow-lg hover:bg-gray-100"
              aria-label="Retake photo"
            >
              <RefreshCw className="h-4 w-4" />
            </button>
            <button
              onClick={clear}
              className="p-2 bg-red-600 text-white rounded-full shadow-lg hover:bg-red-700"
              aria-label="Remove photo"
            >
              <X className="h-4 w-4" />
            </button>
          </div>
          {isUploading && !imageUrl && (
            <div className="absolute inset-0 bg-black bg-opacity-50 flex items-center justify-center">
              <Loader2 className="h-8 w-8 text-white animate-spin" />
            </div>
          )}
        </div>
      )}

      <canvas ref={canvasRef} className="hidden" />
      <input
        ref={fileInputRef}
        type="file"
        accept="image/*"
        onChange={handleFileSelect}
        className="hidden"
      />
    </div>
  );
};
