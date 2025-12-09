import { Skeleton } from '@/components/ui/skeleton';

export default function Loading() {
  return (
    <div className="bg-sand-100 dark:bg-primary-900">
      {/* Hero Skeleton */}
      <div className="bg-gradient-to-br from-primary-900 via-primary-800 to-primary-700 py-16 md:py-24">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="text-center mb-8">
            <Skeleton className="h-12 w-96 mx-auto mb-4 bg-primary-700" />
            <Skeleton className="h-6 w-64 mx-auto bg-primary-700" />
          </div>
          <Skeleton className="h-20 max-w-4xl mx-auto rounded-full bg-primary-700" />
        </div>
      </div>

      {/* Content Skeleton */}
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        <Skeleton className="h-8 w-64 mb-8" />
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mb-12">
          {[1, 2, 3, 4].map((i) => (
            <Skeleton key={i} className="h-64 md:h-72 rounded-2xl" />
          ))}
        </div>
        
        <Skeleton className="h-8 w-48 mb-8" />
        <div className="grid md:grid-cols-3 lg:grid-cols-4 gap-6">
          {[1, 2, 3, 4, 5, 6, 7, 8].map((i) => (
            <div key={i}>
              <Skeleton className="h-64 rounded-2xl mb-3" />
              <Skeleton className="h-4 w-3/4 mb-2" />
              <Skeleton className="h-4 w-1/2 mb-2" />
              <Skeleton className="h-4 w-20" />
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
