'use client';

export function FeaturedProperties() {
  const properties = [
    {
      id: 1,
      title: 'Luxury Villa in Cape Town',
      location: 'Cape Town, South Africa',
      price: 250,
      rating: 4.9,
      image: 'https://images.unsplash.com/photo-1512917774080-9991f1c52e1d',
    },
    {
      id: 2,
      title: 'Cozy Apartment in Harare',
      location: 'Harare, Zimbabwe',
      price: 80,
      rating: 4.7,
      image: 'https://images.unsplash.com/photo-1502672260266-1c1ef2d93688',
    },
    {
      id: 3,
      title: 'Modern Home in Johannesburg',
      location: 'Johannesburg, South Africa',
      price: 150,
      rating: 4.8,
      image: 'https://images.unsplash.com/photo-1522708323590-d24dbb6b0267',
    },
  ];

  return (
    <section className="py-16 bg-gray-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="text-center mb-12">
          <h2 className="text-3xl md:text-4xl font-bold mb-4">Featured Properties</h2>
          <p className="text-gray-600 text-lg">Discover our top-rated accommodations</p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
          {properties.map((property) => (
            <div key={property.id} className="card hover:shadow-elevated transition">
              <div className="relative h-48 overflow-hidden bg-gray-200">
                <img
                  src={property.image}
                  alt={property.title}
                  className="w-full h-full object-cover"
                />
              </div>
              <div className="p-6">
                <h3 className="text-lg font-semibold mb-2">{property.title}</h3>
                <p className="text-gray-600 text-sm mb-3">{property.location}</p>
                <div className="flex justify-between items-center">
                  <div>
                    <span className="text-2xl font-bold text-primary-600">${property.price}</span>
                    <span className="text-gray-600">/night</span>
                  </div>
                  <div className="flex items-center space-x-1">
                    <span className="text-yellow-400">★</span>
                    <span className="font-semibold">{property.rating}</span>
                  </div>
                </div>
              </div>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}
