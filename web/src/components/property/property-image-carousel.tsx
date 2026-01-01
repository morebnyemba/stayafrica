'use client';

import { useState } from 'react';
import { ChevronLeft, ChevronRight } from 'lucide-react';

interface PropertyImageCarouselProps {
  images: Array<{ id: string; image_url?: string; url?: string; caption?: string; alt?: string }>;
  title: string;
}

export function PropertyImageCarousel({ images, title }: PropertyImageCarouselProps) {
  const [currentIndex, setCurrentIndex] = useState(0);

  if (!images || images.length === 0) {
    return (
      <div className="w-full h-96 bg-gradient-to-br from-primary-100 to-sand-100 dark:from-primary-800 dark:to-primary-700 rounded-2xl flex items-center justify-center">
        <p className="text-primary-600 dark:text-sand-400">No images available</p>
      </div>
    );
  }

  const handlePrev = () => {
    setCurrentIndex((prev) => (prev === 0 ? images.length - 1 : prev - 1));
  };

  const handleNext = () => {
    setCurrentIndex((prev) => (prev === images.length - 1 ? 0 : prev + 1));
  };

  const currentImage = images[currentIndex];
  const imageUrl = currentImage.image_url || currentImage.url;

  return (
    <div className="relative w-full h-96 rounded-2xl overflow-hidden group">
      {/* Main image */}
      <img
        src={imageUrl}
        alt={currentImage.caption || currentImage.alt || title}
        className="w-full h-full object-cover"
      />

      {/* Navigation buttons */}
      {images.length > 1 && (
        <>
          <button
            onClick={handlePrev}
            className="absolute left-4 top-1/2 -translate-y-1/2 bg-white/80 dark:bg-primary-900/80 hover:bg-white dark:hover:bg-primary-800 text-primary-900 dark:text-sand-100 p-2 rounded-full transition opacity-0 group-hover:opacity-100 z-10"
          >
            <ChevronLeft className="w-6 h-6" />
          </button>
          <button
            onClick={handleNext}
            className="absolute right-4 top-1/2 -translate-y-1/2 bg-white/80 dark:bg-primary-900/80 hover:bg-white dark:hover:bg-primary-800 text-primary-900 dark:text-sand-100 p-2 rounded-full transition opacity-0 group-hover:opacity-100 z-10"
          >
            <ChevronRight className="w-6 h-6" />
          </button>

          {/* Image indicators */}
          <div className="absolute bottom-4 left-1/2 -translate-x-1/2 flex items-center space-x-2 bg-black/50 px-4 py-2 rounded-full">
            {images.map((_, index) => (
              <button
                key={index}
                onClick={() => setCurrentIndex(index)}
                className={`w-2 h-2 rounded-full transition ${
                  index === currentIndex ? 'bg-white w-6' : 'bg-white/50'
                }`}
              />
            ))}
          </div>

          {/* Image counter */}
          <div className="absolute top-4 right-4 bg-black/50 text-white px-3 py-1 rounded-full text-sm">
            {currentIndex + 1} / {images.length}
          </div>
        </>
      )}
    </div>
  );
}
