'use client';

export function PropertyDetailContent({ propertyId }: { propertyId: string }) {
  return (
    <div className="max-w-7xl mx-auto px-4 py-12">
      <h1 className="text-3xl font-bold mb-8">Property Details</h1>
      <div className="bg-white rounded-lg p-8 border border-gray-200">
        <p className="text-gray-600">Property detail content for {propertyId} coming soon...</p>
      </div>
    </div>
  );
}
