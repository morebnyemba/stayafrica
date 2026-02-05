'use client';

import { useState, useEffect, useCallback } from 'react';
import { ChevronLeft, ChevronRight } from 'lucide-react';

interface PropertyImageCarouselProps {
  images: Array<{ id: string; image_url?: string; url?: string; caption?: string; alt?: string; image?: string }>;
  title: string;
  mainImage?: string;
}

export function PropertyImageCarousel({ images, title, mainImage }: PropertyImageCarouselProps) {
  const [currentIndex, setCurrentIndex] = useState(0);
  const [imageError, setImageError] = useState<Set<number>>(new Set());

  // Prepare images array with fallback
  const processedImages = images && images.length > 0 
    ? images.map(img => ({
        ...img,
        url: img.image_url || img.url || img.image
      }))
    : mainImage 
    ? [{ id: 'main', url: mainImage, caption: title }]
    : [];

  if (!processedImages || processedImages.length === 0) {
    return (
      <div className="w-full h-96 bg-gradient-to-br from-primary-100 to-sand-100 dark:from-primary-800 dark:to-primary-700 rounded-2xl flex items-center justify-center">
        <div className="text-center">
          <p className="text-primary-600 dark:text-sand-400 text-lg mb-2">No images available</p>
          <p className="text-primary-500 dark:text-sand-500 text-sm">Property images will appear here</p>
        </div>
      </div>
    );
  }

  const handlePrev = useCallback(() => {
    setCurrentIndex((prev) => (prev === 0 ? processedImages.length - 1 : prev - 1));
  }, [processedImages.length]);

  const handleNext = useCallback(() => {
    setCurrentIndex((prev) => (prev === processedImages.length - 1 ? 0 : prev + 1));
  }, [processedImages.length]);

  // Keyboard navigation
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === 'ArrowLeft') {
        handlePrev();
      } else if (e.key === 'ArrowRight') {
        handleNext();
      }
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [handlePrev, handleNext]);

  const currentImage = processedImages[currentIndex];
  const imageUrl = currentImage?.url;

  const handleImageError = () => {
    setImageError(prev => new Set(prev).add(currentIndex));
  };

  return (
    <div className="relative w-full h-96 rounded-2xl overflow-hidden group bg-primary-100 dark:bg-primary-800">
      {/* Main image */}
      {imageUrl && !imageError.has(currentIndex) ? (
        <img
          src={imageUrl}
          alt={currentImage.caption || currentImage.alt || title}
          className="w-full h-full object-cover"
          onError={handleImageError}
        />
      ) : (
        <div className="w-full h-full flex items-center justify-center bg-gradient-to-br from-primary-100 to-sand-100 dark:from-primary-800 dark:to-primary-700">
          <p className="text-primary-600 dark:text-sand-400">Image not available</p>
        </div>
      )}

      {/* Navigation buttons */}
      {processedImages.length > 1 && (
        <>
          <button
            onClick={handlePrev}
            aria-label="Previous image"
            className="absolute left-4 top-1/2 -translate-y-1/2 bg-white/90 dark:bg-primary-900/90 hover:bg-white dark:hover:bg-primary-800 text-primary-900 dark:text-sand-100 p-3 rounded-full transition-all duration-200 opacity-0 group-hover:opacity-100 shadow-lg hover:scale-110 z-10"
          >
            <ChevronLeft className="w-6 h-6" />
          </button>
          <button
            onClick={handleNext}
            aria-label="Next image"
            className="absolute right-4 top-1/2 -translate-y-1/2 bg-white/90 dark:bg-primary-900/90 hover:bg-white dark:hover:bg-primary-800 text-primary-900 dark:text-sand-100 p-3 rounded-full transition-all duration-200 opacity-0 group-hover:opacity-100 shadow-lg hover:scale-110 z-10"
          >
            <ChevronRight className="w-6 h-6" />
          </button>

          {/* Image indicators */}
          <div className="absolute bottom-4 left-1/2 -translate-x-1/2 flex items-center space-x-2 bg-black/70 backdrop-blur-sm px-4 py-2 rounded-full">
            {processedImages.map((_, index) => (
              <button
                key={index}
                onClick={() => setCurrentIndex(index)}
                aria-label={`Go to image ${index + 1}`}
                className={`h-2 rounded-full transition-all duration-300 ${
                  index === currentIndex ? 'bg-white w-8' : 'bg-white/60 w-2 hover:bg-white/80'
                }`}
              />
            ))}
          </div>

          {/* Image counter */}
          <div className="absolute top-4 right-4 bg-black/70 backdrop-blur-sm text-white px-4 py-2 rounded-full text-sm font-medium shadow-lg">
            {currentIndex + 1} / {processedImages.length}
          </div>
        </>
      )}

      {/* Keyboard navigation hint */}
      {processedImages.length > 1 && (
        <div className="absolute bottom-20 left-1/2 -translate-x-1/2 opacity-0 group-hover:opacity-100 transition-opacity duration-300">
          <p className="text-white text-xs bg-black/60 backdrop-blur-sm px-3 py-1 rounded-full">
            Use arrow keys to navigate
          </p>
        </div>
      )}
    </div>
  );
}
