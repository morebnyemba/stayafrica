/**
 * Property Type Categories
 * Strongly typed property categories for the StayAfrica platform
 */

export enum PropertyType {
  HOUSE = 'HOUSE',
  APARTMENT = 'APARTMENT',
  HOTEL = 'HOTEL',
  LODGE = 'LODGE',
  VILLA = 'VILLA',
  BNB = 'BNB',
  HOSTEL = 'HOSTEL',
  GUESTHOUSE = 'GUESTHOUSE',
  RESORT = 'RESORT',
  COTTAGE = 'COTTAGE',
  CAMPGROUND = 'CAMPGROUND',
  BOAT = 'BOAT',
}

export interface PropertyTypeConfig {
  id: PropertyType;
  label: string;
  description: string;
  icon: string; // lucide-react icon name
  color: string; // tailwind color class
  bgColor: string; // tailwind background color class
}

export const PROPERTY_TYPES: Record<PropertyType, PropertyTypeConfig> = {
  [PropertyType.HOUSE]: {
    id: PropertyType.HOUSE,
    label: 'Houses',
    description: 'Entire houses and homes',
    icon: 'Home',
    color: 'text-blue-600',
    bgColor: 'bg-blue-50 dark:bg-blue-900/30',
  },
  [PropertyType.APARTMENT]: {
    id: PropertyType.APARTMENT,
    label: 'Apartments',
    description: 'Modern apartments and flats',
    icon: 'Building2',
    color: 'text-purple-600',
    bgColor: 'bg-purple-50 dark:bg-purple-900/30',
  },
  [PropertyType.HOTEL]: {
    id: PropertyType.HOTEL,
    label: 'Hotels',
    description: 'Hotels with full services',
    icon: 'Building',
    color: 'text-amber-600',
    bgColor: 'bg-amber-50 dark:bg-amber-900/30',
  },
  [PropertyType.LODGE]: {
    id: PropertyType.LODGE,
    label: 'Lodges',
    description: 'Lodges and eco-resorts',
    icon: 'TreePine',
    color: 'text-green-600',
    bgColor: 'bg-green-50 dark:bg-green-900/30',
  },
  [PropertyType.VILLA]: {
    id: PropertyType.VILLA,
    label: 'Villas',
    description: 'Luxury villas and estates',
    icon: 'Castle',
    color: 'text-rose-600',
    bgColor: 'bg-rose-50 dark:bg-rose-900/30',
  },
  [PropertyType.BNB]: {
    id: PropertyType.BNB,
    label: 'B&Bs',
    description: 'Bed & Breakfast properties',
    icon: 'Wind',
    color: 'text-orange-600',
    bgColor: 'bg-orange-50 dark:bg-orange-900/30',
  },
  [PropertyType.HOSTEL]: {
    id: PropertyType.HOSTEL,
    label: 'Hostels',
    description: 'Budget hostels and dorms',
    icon: 'Users',
    color: 'text-cyan-600',
    bgColor: 'bg-cyan-50 dark:bg-cyan-900/30',
  },
  [PropertyType.GUESTHOUSE]: {
    id: PropertyType.GUESTHOUSE,
    label: 'Guesthouses',
    description: 'Cozy guesthouses',
    icon: 'Home',
    color: 'text-indigo-600',
    bgColor: 'bg-indigo-50 dark:bg-indigo-900/30',
  },
  [PropertyType.RESORT]: {
    id: PropertyType.RESORT,
    label: 'Resorts',
    description: 'All-inclusive resorts',
    icon: 'Palmtree',
    color: 'text-teal-600',
    bgColor: 'bg-teal-50 dark:bg-teal-900/30',
  },
  [PropertyType.COTTAGE]: {
    id: PropertyType.COTTAGE,
    label: 'Cottages',
    description: 'Charming cottages',
    icon: 'Home',
    color: 'text-pink-600',
    bgColor: 'bg-pink-50 dark:bg-pink-900/30',
  },
  [PropertyType.CAMPGROUND]: {
    id: PropertyType.CAMPGROUND,
    label: 'Campgrounds',
    description: 'Camping and glamping sites',
    icon: 'Tent',
    color: 'text-lime-600',
    bgColor: 'bg-lime-50 dark:bg-lime-900/30',
  },
  [PropertyType.BOAT]: {
    id: PropertyType.BOAT,
    label: 'Boats',
    description: 'Houseboats and sailboats',
    icon: 'Anchor',
    color: 'text-blue-700',
    bgColor: 'bg-blue-50 dark:bg-blue-900/30',
  },
};

// Featured types for homepage
export const FEATURED_PROPERTY_TYPES: PropertyType[] = [
  PropertyType.BNB,
  PropertyType.APARTMENT,
  PropertyType.HOTEL,
  PropertyType.LODGE,
  PropertyType.VILLA,
];

export function getPropertyTypeConfig(type: PropertyType): PropertyTypeConfig {
  return PROPERTY_TYPES[type];
}

export function getPropertyTypeLabel(type: PropertyType): string {
  return PROPERTY_TYPES[type]?.label || type;
}
