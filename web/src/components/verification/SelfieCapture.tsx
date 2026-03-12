'use client';

import { useState, useRef, useEffect, useCallback } from 'react';
import { Camera, X, RefreshCw, Loader2, CheckCircle, AlertCircle } from 'lucide-react';
import { useDocumentUpload } from './useDocumentUpload';
import toast from 'react-hot-toast';

interface SelfieCaptureProps {
  onCaptureComplete: (imageUrl: string) => void;
}

type LivenessStatus = 'no-face' | 'face-detected' | 'face-stable' | 'ready';

// Extend global types for the experimental FaceDetector API
declare global {
  interface Window {
    FaceDetector?: new (options?: { fastMode?: boolean; maxDetectedFaces?: number }) => {
      detect(image: HTMLVideoElement): Promise<Array<{ boundingBox: DOMRectReadOnly }>>;
    };
  }
}

export const SelfieCapture = ({ onCaptureComplete }: SelfieCaptureProps) => {
  const [preview, setPreview] = useState<string | null>(null);
  const [imageUrl, setImageUrl] = useState<string | null>(null);
  const [isUploading, setIsUploading] = useState(false);
  const [isCameraActive, setIsCameraActive] = useState(false);
  const [stream, setStream] = useState<MediaStream | null>(null);

  // Liveness state
  const [livenessSupported, setLivenessSupported] = useState<boolean | null>(null);
  const [livenessStatus, setLivenessStatus] = useState<LivenessStatus>('no-face');

  const videoRef = useRef<HTMLVideoElement>(null);
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const fileInputRef = useRef<HTMLInputElement>(null);
  const detectorRef = useRef<InstanceType<NonNullable<Window['FaceDetector']>> | null>(null);
  const rafRef = useRef<number | null>(null);
  const faceStableSinceRef = useRef<number | null>(null);

  const { uploadFile } = useDocumentUpload();

  // Check FaceDetector support once on mount
  useEffect(() => {
    setLivenessSupported(typeof window !== 'undefined' && 'FaceDetector' in window);
  }, []);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      if (stream) stream.getTracks().forEach(t => t.stop());
      if (rafRef.current) cancelAnimationFrame(rafRef.current);
    };
  }, [stream]);

  const runDetectionLoop = useCallback(async () => {
    if (!detectorRef.current || !videoRef.current || videoRef.current.readyState < 2) {
      rafRef.current = requestAnimationFrame(runDetectionLoop);
      return;
    }

    try {
      const faces = await detectorRef.current.detect(videoRef.current);
      if (faces.length > 0) {
        if (!faceStableSinceRef.current) {
          faceStableSinceRef.current = Date.now();
          setLivenessStatus('face-detected');
        } else if (Date.now() - faceStableSinceRef.current >= 1000) {
          setLivenessStatus('ready');
        }
      } else {
        faceStableSinceRef.current = null;
        setLivenessStatus('no-face');
      }
    } catch {
      // Silently continue on detection errors
    }

    rafRef.current = requestAnimationFrame(runDetectionLoop);
  }, []);

  const startCamera = async () => {
    // Check if we're in a secure context (HTTPS or localhost) — required for getUserMedia
    if (typeof window !== 'undefined' && !window.isSecureContext) {
      alert('Camera access requires a secure connection (HTTPS). Please access this site over HTTPS or upload a photo instead.');
      return;
    }

    // Check if getUserMedia is available
    if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
      alert('Camera is not supported in this browser. Please upload a photo instead.');
      return;
    }

    try {
      const mediaStream = await navigator.mediaDevices.getUserMedia({
        video: { facingMode: 'user', width: { ideal: 640 }, height: { ideal: 480 } },
      });

      if (videoRef.current) {
        videoRef.current.srcObject = mediaStream;
      }

      setStream(mediaStream);
      setIsCameraActive(true);
      setLivenessStatus('no-face');
      faceStableSinceRef.current = null;

      // Start face detection loop if supported
      if (livenessSupported && window.FaceDetector) {
        detectorRef.current = new window.FaceDetector({ maxDetectedFaces: 1, fastMode: true });
        rafRef.current = requestAnimationFrame(runDetectionLoop);
      }
    } catch (error) {
      console.error('Camera error:', error);
      if (error instanceof DOMException) {
        switch (error.name) {
          case 'NotAllowedError':
            alert('Camera permission was denied. Please allow camera access in your browser settings (click the lock/camera icon in the address bar) and try again, or upload a photo instead.');
            break;
          case 'NotFoundError':
            alert('No camera found on this device. Please upload a photo instead.');
            break;
          case 'NotReadableError':
            alert('Camera is in use by another application. Please close other apps using the camera and try again.');
            break;
          case 'OverconstrainedError':
            // Retry without constraints
            try {
              const fallbackStream = await navigator.mediaDevices.getUserMedia({ video: true });
              if (videoRef.current) {
                videoRef.current.srcObject = fallbackStream;
              }
              setStream(fallbackStream);
              setIsCameraActive(true);
              setLivenessStatus('no-face');
              faceStableSinceRef.current = null;
              if (livenessSupported && window.FaceDetector) {
                detectorRef.current = new window.FaceDetector({ maxDetectedFaces: 1, fastMode: true });
                rafRef.current = requestAnimationFrame(runDetectionLoop);
              }
              return;
            } catch {
              alert('Unable to access camera. Please upload a photo instead.');
            }
            break;
          default:
            alert('Unable to access camera. Please check permissions or upload a photo instead.');
        }
      } else {
        alert('Unable to access camera. Please check permissions or upload a photo instead.');
      }
    }
  };

  const stopCamera = useCallback(() => {
    if (stream) {
      stream.getTracks().forEach(t => t.stop());
      setStream(null);
    }
    if (rafRef.current) {
      cancelAnimationFrame(rafRef.current);
      rafRef.current = null;
    }
    detectorRef.current = null;
    setIsCameraActive(false);
    setLivenessStatus('no-face');
    faceStableSinceRef.current = null;
  }, [stream]);

  const capturePhoto = () => {
    if (!videoRef.current || !canvasRef.current) return;
    const video = videoRef.current;
    const canvas = canvasRef.current;

    // Guard: if the video hasn't started playing, dimensions will be 0
    if (video.videoWidth === 0 || video.videoHeight === 0) {
      toast.error('Camera feed is not ready yet. Please wait a moment and try again.');
      return;
    }

    canvas.width = video.videoWidth;
    canvas.height = video.videoHeight;

    const ctx = canvas.getContext('2d');
    if (ctx) {
      ctx.drawImage(video, 0, 0);
      const imageData = canvas.toDataURL('image/jpeg');
      setPreview(imageData);
      stopCamera();

      canvas.toBlob(async blob => {
        if (blob) {
          const file = new File([blob], 'selfie.jpg', { type: 'image/jpeg' });
          await handleUpload(file);
        }
      }, 'image/jpeg');
    }
  };

  const handleFileSelect = async (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (file) {
      const reader = new FileReader();
      reader.onloadend = () => setPreview(reader.result as string);
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
      toast.error('Failed to upload selfie. Please try again.');
      setPreview(null);
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

  // Determine if capture is allowed
  const canCapture = !livenessSupported || livenessStatus === 'ready';

  const livenessIndicator = () => {
    if (!livenessSupported) {
      return (
        <div className="flex items-center gap-2 text-xs text-amber-700 bg-amber-50 border border-amber-200 rounded-lg px-3 py-2">
          <AlertCircle className="h-4 w-4 flex-shrink-0" />
          <span>Live face detection not supported in this browser. Position your face and capture manually.</span>
        </div>
      );
    }
    switch (livenessStatus) {
      case 'no-face':
        return (
          <div className="flex items-center gap-2 text-xs text-red-700 bg-red-50 border border-red-200 rounded-lg px-3 py-2">
            <div className="h-2 w-2 rounded-full bg-red-500 animate-pulse flex-shrink-0" />
            No face detected — position your face in the frame
          </div>
        );
      case 'face-detected':
        return (
          <div className="flex items-center gap-2 text-xs text-amber-700 bg-amber-50 border border-amber-200 rounded-lg px-3 py-2">
            <div className="h-2 w-2 rounded-full bg-amber-400 animate-pulse flex-shrink-0" />
            Face detected — hold still…
          </div>
        );
      case 'ready':
        return (
          <div className="flex items-center gap-2 text-xs text-green-700 bg-green-50 border border-green-200 rounded-lg px-3 py-2">
            <CheckCircle className="h-4 w-4 flex-shrink-0" />
            Face confirmed — ready to capture!
          </div>
        );
      default:
        return null;
    }
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
            <div className="relative space-y-3">
              {/* Face guide oval overlay */}
              <div className="relative">
                <video
                  ref={videoRef}
                  autoPlay
                  playsInline
                  muted
                  className="w-full rounded-lg border"
                />
                <div className="absolute inset-0 flex items-center justify-center pointer-events-none">
                  <div
                    className="border-4 rounded-[50%] opacity-60"
                    style={{
                      width: '45%',
                      height: '65%',
                      borderColor: livenessStatus === 'ready' ? '#16a34a' : livenessStatus === 'face-detected' ? '#d97706' : '#6b7280',
                      transition: 'border-color 0.3s',
                    }}
                  />
                </div>
              </div>

              {/* Liveness status */}
              {livenessIndicator()}

              <div className="flex gap-2">
                <button
                  onClick={capturePhoto}
                  disabled={!canCapture}
                  className="flex-1 flex items-center justify-center gap-2 px-4 py-3 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700 disabled:opacity-40 disabled:cursor-not-allowed transition-opacity"
                >
                  <Camera className="h-5 w-5" />
                  <span>{canCapture ? 'Capture Photo' : 'Waiting for face…'}</span>
                </button>
                <button
                  onClick={stopCamera}
                  className="px-4 py-3 border border-primary-300 rounded-lg hover:bg-sand-50"
                >
                  Cancel
                </button>
              </div>
            </div>
          ) : (
            <div className="space-y-3">
              <button
                onClick={startCamera}
                className="w-full flex items-center justify-center gap-2 px-4 py-3 bg-secondary-600 text-white rounded-lg hover:bg-secondary-700"
              >
                <Camera className="h-5 w-5" />
                <span>Use Camera</span>
              </button>

              <div className="relative">
                <div className="absolute inset-0 flex items-center">
                  <div className="w-full border-t border-primary-300" />
                </div>
                <div className="relative flex justify-center text-sm">
                  <span className="px-2 bg-white text-primary-400">or</span>
                </div>
              </div>

              <button
                onClick={() => fileInputRef.current?.click()}
                className="w-full flex items-center justify-center gap-2 px-4 py-3 border border-primary-300 rounded-lg hover:bg-sand-50"
              >
                <Camera className="h-5 w-5" />
                <span>Upload Photo</span>
              </button>
            </div>
          )}
        </div>
      ) : (
        <div className="relative border rounded-lg overflow-hidden">
          {/* eslint-disable-next-line @next/next/no-img-element */}
          <img
            src={preview}
            alt="Selfie preview"
            className="w-full h-96 object-contain bg-sand-50"
          />
          <div className="absolute top-2 right-2 flex gap-2">
            <button
              onClick={clear}
              className="p-2 bg-white text-primary-700 rounded-full shadow-lg hover:bg-primary-100"
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
