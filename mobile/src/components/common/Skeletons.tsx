import { View } from 'react-native';

interface SkeletonProps {
  width?: number | string;
  height?: number;
  borderRadius?: number;
  className?: string;
}

export function Skeleton({ width = '100%', height = 20, borderRadius = 4, className = '' }: SkeletonProps) {
  return (
    <View
      className={`bg-gray-200 animate-pulse ${className}`}
      style={{
        width,
        height,
        borderRadius,
      }}
    />
  );
}

export function PropertyCardSkeleton() {
  return (
    <View className="mb-6 rounded-2xl overflow-hidden bg-white shadow-lg border border-gray-100 p-4">
      <Skeleton height={180} className="mb-4" />
      <Skeleton height={24} width="80%" className="mb-2" />
      <Skeleton height={16} width="60%" className="mb-3" />
      <View className="flex-row justify-between items-center mb-3">
        <Skeleton height={20} width={100} />
        <Skeleton height={20} width={60} />
      </View>
      <View className="flex-row justify-between">
        <Skeleton height={16} width={60} />
        <Skeleton height={16} width={60} />
        <Skeleton height={16} width={60} />
      </View>
    </View>
  );
}

export function BookingCardSkeleton() {
  return (
    <View className="bg-white rounded-xl p-4 mb-3 shadow-sm border border-gray-100">
      <Skeleton height={20} width="70%" className="mb-2" />
      <Skeleton height={16} width="50%" className="mb-3" />
      <View className="flex-row items-center mb-2">
        <Skeleton height={16} width={150} />
      </View>
      <View className="flex-row items-center mb-3">
        <Skeleton height={16} width={100} />
      </View>
      <View className="flex-row justify-between">
        <Skeleton height={24} width={80} />
        <Skeleton height={24} width={60} />
      </View>
    </View>
  );
}

export function ProfileSkeleton() {
  return (
    <View className="p-4">
      <View className="items-center mb-6">
        <Skeleton height={96} width={96} borderRadius={48} className="mb-4" />
        <Skeleton height={24} width={150} className="mb-2" />
        <Skeleton height={16} width={200} />
      </View>
      
      <View className="space-y-4">
        {[1, 2, 3, 4].map((i) => (
          <View key={i} className="bg-white rounded-xl p-4">
            <Skeleton height={20} width="40%" className="mb-2" />
            <Skeleton height={16} width="70%" />
          </View>
        ))}
      </View>
    </View>
  );
}

export function ListSkeleton({ count = 5 }: { count?: number }) {
  return (
    <View className="p-4">
      {Array.from({ length: count }).map((_, index) => (
        <View key={index} className="mb-4">
          <PropertyCardSkeleton />
        </View>
      ))}
    </View>
  );
}
