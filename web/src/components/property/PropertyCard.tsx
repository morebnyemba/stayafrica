/**
 * Enhanced Property Card Component
 * Features: Image carousel, amenity icons, ratings, price display, quick-view modal
 */
'use client';

import React, { useState } from 'react';
import Image from 'next/image';
import Link from 'next/link';
import { Card, CardBody } from '@/components/ui/Card';
import { Badge } from '@/components/ui/Badge';
import { Modal } from '@/components/ui/Modal';
import { Button } from '@/components/ui/Button';
import { 
  Heart, ChevronLeft, ChevronRight, Star, Wifi, UtensilsCrossed, Wind, MapPin,
  Tv, ParkingCircle, Dumbbell, Waves, Dog, Flame, Snowflake, Baby, Accessibility
} from 'lucide-react';
import { cn } from '@/lib/utils';

export interface Property {
  id: string;
  title: string;
  location: string;
  price: number;
  rating: number;
  reviewCount: number;
  images: string[];
  amenities: string[];
  beds: number;
  baths: number;
  guests: number;
  isFavorite?: boolean;
}

interface PropertyCardProps {
  property: Property;
  onFavorite?: (id: string) => void;
  onBook?: (id: string) => void;
}

export const PropertyCard: React.FC<PropertyCardProps> = ({
  property,
  onFavorite,
  onBook,
}) => {
  const [currentImageIndex, setCurrentImageIndex] = useState(0);
  const [isFavorite, setIsFavorite] = useState(property.isFavorite || false);
  const [showQuickView, setShowQuickView] = useState(false);

  const handlePrevImage = (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    setCurrentImageIndex((prev) =>
      prev === 0 ? property.images.length - 1 : prev - 1
    );
  };

  const handleNextImage = (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    setCurrentImageIndex((prev) =>
      prev === property.images.length - 1 ? 0 : prev + 1
    );
  };

  const handleFavorite = (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    setIsFavorite(!isFavorite);
    onFavorite?.(property.id);
  };

  // Amenity icon map
  const amenityIcons: Record<string, React.ReactNode> = {
    wifi: <Wifi className="h-4 w-4" />,
    kitchen: <UtensilsCrossed className="h-4 w-4" />,
    ac: <Wind className="h-4 w-4" />,
    'air conditioning': <Wind className="h-4 w-4" />,
    tv: <Tv className="h-4 w-4" />,
    television: <Tv className="h-4 w-4" />,
    parking: <ParkingCircle className="h-4 w-4" />,
    gym: <Dumbbell className="h-4 w-4" />,
    fitness: <Dumbbell className="h-4 w-4" />,
    pool: <Waves className="h-4 w-4" />,
    swimming: <Waves className="h-4 w-4" />,
    'pet friendly': <Dog className="h-4 w-4" />,
    pets: <Dog className="h-4 w-4" />,
    heating: <Flame className="h-4 w-4" />,
    cooling: <Snowflake className="h-4 w-4" />,
    'baby friendly': <Baby className="h-4 w-4" />,
    accessible: <Accessibility className="h-4 w-4" />,
    wheelchair: <Accessibility className="h-4 w-4" />,
  };

  return (
    <>
      <Link href={`/property/${property.id}`}>
        <Card
          variant="default"
          hoverable
          className="overflow-hidden"
        >
          {/* Image Carousel */}
          <div className="relative h-48 sm:h-56 md:h-48 w-full bg-neutral-200 overflow-hidden">
            <Image
              src={property.images[currentImageIndex]}
              alt={property.title}
              fill
              className="object-cover"
              sizes="(max-width: 768px) 100vw, (max-width: 1200px) 50vw, 33vw"
            />

            {/* Favorite Button */}
            <button
              onClick={handleFavorite}
              className="absolute top-3 right-3 z-10 rounded-full bg-white/90 p-2 hover:bg-white transition-colors"
              aria-label={isFavorite ? 'Remove from favorites' : 'Add to favorites'}
            >
              <Heart
                className={cn('h-5 w-5 transition-colors', isFavorite && 'fill-error-500 text-error-500')}
              />
            </button>

            {/* Image Navigation */}
            {property.images.length > 1 && (
              <>
                <button
                  onClick={handlePrevImage}
                  className="absolute left-2 top-1/2 -translate-y-1/2 z-10 rounded-full bg-white/80 p-1 hover:bg-white transition-colors"
                  aria-label="Previous image"
                >
                  <ChevronLeft className="h-5 w-5" />
                </button>
                <button
                  onClick={handleNextImage}
                  className="absolute right-2 top-1/2 -translate-y-1/2 z-10 rounded-full bg-white/80 p-1 hover:bg-white transition-colors"
                  aria-label="Next image"
                >
                  <ChevronRight className="h-5 w-5" />
                </button>

                {/* Image Counter */}
                <div className="absolute bottom-3 right-3 bg-black/60 text-white text-xs px-2 py-1 rounded-full">
                  {currentImageIndex + 1} / {property.images.length}
                </div>
              </>
            )}
          </div>

          {/* Card Content */}
          <CardBody className="space-y-3">
            {/* Title & Location */}
            <div>
              <h3 className="font-semibold text-neutral-900 line-clamp-1 text-base sm:text-lg">
                {property.title}
              </h3>
              <p className="text-sm text-neutral-600 flex items-center gap-1">
                <MapPin className="h-3 w-3 sm:h-4 sm:w-4" />
                <span className="line-clamp-1">{property.location}</span>
              </p>
            </div>

            {/* Amenities */}
            <div className="flex gap-2 flex-wrap">
              {property.amenities.slice(0, 3).map((amenity) => (
                <Badge key={amenity} size="sm" variant="neutral" icon={amenityIcons[amenity.toLowerCase()]}>
                  {amenity}
                </Badge>
              ))}
            </div>

            {/* Beds, Baths, Guests */}
            <div className="flex gap-3 sm:gap-4 text-xs sm:text-sm text-neutral-600">
              <span>{property.beds} beds</span>
              <span>{property.baths} baths</span>
              <span>{property.guests} guests</span>
            </div>

            {/* Rating */}
            <div className="flex items-center gap-2">
              <div className="flex items-center gap-1">
                <Star className="h-4 w-4 fill-secondary-500 text-secondary-500" />
                <span className="font-medium text-neutral-900">{property.rating}</span>
              </div>
              <span className="text-sm text-neutral-600">
                ({property.reviewCount} reviews)
              </span>
            </div>

            {/* Price & CTA */}
            <div className="flex items-center justify-between pt-2 border-t border-neutral-200">
              <div>
                <p className="text-xl sm:text-2xl font-bold text-neutral-900">
                  ${property.price}
                </p>
                <p className="text-xs sm:text-sm text-neutral-600">per night</p>
              </div>
              <Button
                size="sm"
                onClick={(e) => {
                  e.preventDefault();
                  setShowQuickView(true);
                }}
                className="text-xs sm:text-sm"
              >
                Reserve
              </Button>
            </div>
          </CardBody>
        </Card>
      </Link>

      {/* Quick View Modal */}
      <Modal
        isOpen={showQuickView}
        onClose={() => setShowQuickView(false)}
        title={property.title}
        size="lg"
      >
        <div className="space-y-4">
          <div className="h-64 w-full bg-neutral-200 rounded-lg overflow-hidden">
            <Image
              src={property.images[currentImageIndex]}
              alt={property.title}
              width={600}
              height={400}
              className="w-full h-full object-cover"
            />
          </div>

          <div className="grid grid-cols-3 gap-4 text-center">
            <div>
              <p className="text-2xl font-bold text-neutral-900">{property.beds}</p>
              <p className="text-sm text-neutral-600">Bedrooms</p>
            </div>
            <div>
              <p className="text-2xl font-bold text-neutral-900">{property.baths}</p>
              <p className="text-sm text-neutral-600">Bathrooms</p>
            </div>
            <div>
              <p className="text-2xl font-bold text-neutral-900">{property.guests}</p>
              <p className="text-sm text-neutral-600">Guests</p>
            </div>
          </div>

          <div className="space-y-2">
            <h4 className="font-semibold text-neutral-900">Amenities</h4>
            <div className="grid grid-cols-2 gap-2">
              {property.amenities.map((amenity) => (
                <Badge key={amenity} icon={amenityIcons[amenity.toLowerCase()]}>
                  {amenity}
                </Badge>
              ))}
            </div>
          </div>

          <div className="bg-neutral-50 p-4 rounded-lg">
            <p className="text-sm text-neutral-600 mb-2">Price per night</p>
            <p className="text-3xl font-bold text-neutral-900">${property.price}</p>
          </div>
        </div>

        <div className="flex gap-3 mt-6">
          <Button
            variant="outline"
            fullWidth
            onClick={() => setShowQuickView(false)}
          >
            Close
          </Button>
          <Button
            fullWidth
            onClick={() => {
              onBook?.(property.id);
              setShowQuickView(false);
            }}
          >
            Book Now
          </Button>
        </div>
      </Modal>
    </>
  );
};
