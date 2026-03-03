'use client';

import { useState, useEffect, useCallback } from 'react';
import { ChevronLeft, ChevronRight, X, Grid2X2 } from 'lucide-react';

interface PropertyImageCarouselProps {
  images: Array<{ id: string; image_url?: string; url?: string; caption?: string; alt?: string; image?: string }>;
  title: string;
  mainImage?: string;
}

export function PropertyImageCarousel({ images, title, mainImage }: PropertyImageCarouselProps) {
  const [currentIndex, setCurrentIndex] = useState(0);
  const [showGallery, setShowGallery] = useState(false);
  const [imageError, setImageError] = useState<Set<number>>(new Set());

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
      <div className="w-full aspect-[16/9] bg-gradient-to-br from-primary-100 to-sand-100 dark:from-primary-800 dark:to-primary-700 rounded-xl flex items-center justify-center">
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
      if (showGallery) {
        if (e.key === 'ArrowLeft') handlePrev();
        else if (e.key === 'ArrowRight') handleNext();
        else if (e.key === 'Escape') setShowGallery(false);
      }
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [handlePrev, handleNext, showGallery]);

  const handleImageError = (idx: number) => {
    setImageError(prev => new Set(prev).add(idx));
  };

  const getImageUrl = (idx: number) => processedImages[idx]?.url;

  const ImageOrFallback = ({ idx, className }: { idx: number; className?: string }) => {
    const url = getImageUrl(idx);
    if (!url || imageError.has(idx)) {
      return (
        <div className={`bg-gradient-to-br from-primary-100 to-sand-100 dark:from-primary-800 dark:to-primary-700 flex items-center justify-center ${className || ''}`}>
          <p className="text-primary-500 dark:text-sand-500 text-sm">No image</p>
        </div>
      );
    }
    return (
      <img
        src={url}
        alt={processedImages[idx]?.caption || processedImages[idx]?.alt || `${title} photo ${idx + 1}`}
        className={`object-cover w-full h-full ${className || ''}`}
        onError={() => handleImageError(idx)}
      />
    );
  };

  // Bento grid for 2+ images on desktop
  const gridImages = processedImages.slice(0, 5);
  const hasMultiple = processedImages.length > 1;

  return (
    <>
      {/* Desktop: Bento Photo Grid */}
      <div className="hidden md:block">
        <div
          className={`relative rounded-xl overflow-hidden cursor-pointer ${
            gridImages.length >= 5
              ? 'grid grid-cols-4 grid-rows-2 gap-1.5 h-[400px]'
              : gridImages.length >= 3
              ? 'grid grid-cols-4 grid-rows-2 gap-1.5 h-[400px]'
              : gridImages.length === 2
              ? 'grid grid-cols-2 gap-1.5 h-[400px]'
              : 'h-[400px]'
          }`}
          onClick={() => setShowGallery(true)}
        >
          {gridImages.length === 1 && (
            <div className="w-full h-full">
              <ImageOrFallback idx={0} className="rounded-xl" />
            </div>
          )}

          {gridImages.length === 2 && (
            <>
              <div className="overflow-hidden rounded-l-xl">
                <ImageOrFallback idx={0} />
              </div>
              <div className="overflow-hidden rounded-r-xl">
                <ImageOrFallback idx={1} />
              </div>
            </>
          )}

          {gridImages.length >= 3 && gridImages.length <= 4 && (
            <>
              <div className="col-span-2 row-span-2 overflow-hidden rounded-l-xl">
                <ImageOrFallback idx={0} />
              </div>
              <div className="overflow-hidden">
                <ImageOrFallback idx={1} />
              </div>
              <div className={`overflow-hidden ${gridImages.length === 3 ? 'rounded-tr-xl' : ''}`}>
                <ImageOrFallback idx={2} />
              </div>
              {gridImages.length === 4 && (
                <>
                  <div className="overflow-hidden">
                    <ImageOrFallback idx={3} />
                  </div>
                  <div className="overflow-hidden rounded-br-xl bg-primary-100 dark:bg-primary-800" />
                </>
              )}
              {gridImages.length === 3 && (
                <div className="col-span-2 overflow-hidden rounded-br-xl bg-primary-100 dark:bg-primary-800 flex items-center justify-center" />
              )}
            </>
          )}

          {gridImages.length >= 5 && (
            <>
              <div className="col-span-2 row-span-2 overflow-hidden rounded-l-xl">
                <ImageOrFallback idx={0} />
              </div>
              <div className="overflow-hidden">
                <ImageOrFallback idx={1} />
              </div>
              <div className="overflow-hidden rounded-tr-xl">
                <ImageOrFallback idx={2} />
              </div>
              <div className="overflow-hidden">
                <ImageOrFallback idx={3} />
              </div>
              <div className="overflow-hidden rounded-br-xl">
                <ImageOrFallback idx={4} />
              </div>
            </>
          )}

          {/* Show all photos button */}
          {processedImages.length > 1 && (
            <button
              onClick={(e) => {
                e.stopPropagation();
                setShowGallery(true);
              }}
              className="absolute bottom-4 right-4 flex items-center gap-2 bg-white dark:bg-primary-800 text-primary-900 dark:text-sand-50 px-4 py-2 rounded-lg text-sm font-medium shadow-md hover:shadow-lg transition border border-primary-200 dark:border-primary-600"
            >
              <Grid2X2 className="w-4 h-4" />
              Show all photos
            </button>
          )}
        </div>
      </div>

      {/* Mobile: Swipeable Carousel */}
      <div className="md:hidden">
        <div className="relative w-full aspect-[4/3] rounded-xl overflow-hidden group bg-primary-100 dark:bg-primary-800">
          <ImageOrFallback idx={currentIndex} />

          {hasMultiple && (
            <>
              <button
                onClick={handlePrev}
                aria-label="Previous image"
                className="absolute left-3 top-1/2 -translate-y-1/2 bg-white/90 dark:bg-primary-800/90 p-2 rounded-full shadow-md opacity-0 group-hover:opacity-100 active:opacity-100 transition-opacity z-10"
              >
                <ChevronLeft className="w-5 h-5 text-primary-900 dark:text-sand-50" />
              </button>
              <button
                onClick={handleNext}
                aria-label="Next image"
                className="absolute right-3 top-1/2 -translate-y-1/2 bg-white/90 dark:bg-primary-800/90 p-2 rounded-full shadow-md opacity-0 group-hover:opacity-100 active:opacity-100 transition-opacity z-10"
              >
                <ChevronRight className="w-5 h-5 text-primary-900 dark:text-sand-50" />
              </button>

              {/* Dot indicators */}
              <div className="absolute bottom-3 left-1/2 -translate-x-1/2 flex items-center gap-1.5 z-10">
                {processedImages.slice(0, 5).map((_, idx) => (
                  <div
                    key={idx}
                    className={`rounded-full transition-all duration-200 ${
                      idx === currentIndex
                        ? 'h-1.5 w-1.5 bg-white'
                        : 'h-1.5 w-1.5 bg-white/50'
                    }`}
                  />
                ))}
                {processedImages.length > 5 && (
                  <span className="text-white text-[10px] ml-0.5">+{processedImages.length - 5}</span>
                )}
              </div>
            </>
          )}

          {/* Tap to open gallery */}
          <button
            onClick={() => setShowGallery(true)}
            className="absolute bottom-3 right-3 flex items-center gap-1.5 bg-white/90 dark:bg-primary-800/90 text-primary-900 dark:text-sand-50 px-3 py-1.5 rounded-full text-xs font-medium shadow-md z-10"
          >
            <Grid2X2 className="w-3.5 h-3.5" />
            {processedImages.length} photos
          </button>
        </div>
      </div>

      {/* Fullscreen Gallery Modal */}
      {showGallery && (
        <div className="fixed inset-0 z-50 bg-black flex flex-col">
          {/* Header */}
          <div className="flex items-center justify-between px-4 py-3 bg-black/80">
            <span className="text-white text-sm font-medium">
              {currentIndex + 1} / {processedImages.length}
            </span>
            <button
              onClick={() => setShowGallery(false)}
              className="text-white hover:bg-white/10 p-2 rounded-full transition"
              aria-label="Close gallery"
            >
              <X className="w-6 h-6" />
            </button>
          </div>

          {/* Main Image */}
          <div className="flex-1 flex items-center justify-center relative px-16">
            <img
              src={getImageUrl(currentIndex) || ''}
              alt={processedImages[currentIndex]?.caption || `${title} photo ${currentIndex + 1}`}
              className="max-w-full max-h-full object-contain"
            />

            {hasMultiple && (
              <>
                <button
                  onClick={handlePrev}
                  className="absolute left-4 top-1/2 -translate-y-1/2 bg-white/10 hover:bg-white/20 p-3 rounded-full transition"
                  aria-label="Previous"
                >
                  <ChevronLeft className="w-6 h-6 text-white" />
                </button>
                <button
                  onClick={handleNext}
                  className="absolute right-4 top-1/2 -translate-y-1/2 bg-white/10 hover:bg-white/20 p-3 rounded-full transition"
                  aria-label="Next"
                >
                  <ChevronRight className="w-6 h-6 text-white" />
                </button>
              </>
            )}
          </div>

          {/* Thumbnail Strip */}
          {processedImages.length > 1 && (
            <div className="flex items-center justify-center gap-2 px-4 py-3 bg-black/80 overflow-x-auto">
              {processedImages.map((img, idx) => (
                <button
                  key={img.id || idx}
                  onClick={() => setCurrentIndex(idx)}
                  className={`flex-shrink-0 w-16 h-12 rounded-md overflow-hidden transition-all ${
                    idx === currentIndex
                      ? 'ring-2 ring-white opacity-100'
                      : 'opacity-50 hover:opacity-75'
                  }`}
                >
                  <img
                    src={img.url || ''}
                    alt={`Thumbnail ${idx + 1}`}
                    className="w-full h-full object-cover"
                  />
                </button>
              ))}
            </div>
          )}
        </div>
      )}
    </>
  );
}
