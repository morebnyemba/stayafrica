'use client';

export function Testimonials() {
  const testimonials = [
    {
      name: 'Sarah Johnson',
      role: 'Traveler from UK',
      text: 'Amazing experience! The property was beautiful and the host was very welcoming.',
      rating: 5,
    },
    {
      name: 'Thabo Mkhize',
      role: 'Host in Cape Town',
      text: 'Great platform! Easy to list properties and manage bookings.',
      rating: 5,
    },
    {
      name: 'Emma Wilson',
      role: 'Traveler from Australia',
      text: 'StayAfrica made my trip to Zimbabwe unforgettable!',
      rating: 4.5,
    },
  ];

  return (
    <section className="py-16 bg-gray-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="text-center mb-12">
          <h2 className="text-3xl md:text-4xl font-bold mb-4">What Our Users Say</h2>
          <p className="text-gray-600 text-lg">Join thousands of happy travelers and hosts</p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
          {testimonials.map((testimonial, index) => (
            <div key={index} className="card">
              <div className="flex items-center space-x-1 mb-4">
                {Array.from({ length: 5 }).map((_, i) => (
                  <span key={i} className={i < Math.floor(testimonial.rating) ? '⭐' : '☆'}>
                    {' '}
                  </span>
                ))}
              </div>
              <p className="text-gray-700 mb-4">"{testimonial.text}"</p>
              <div>
                <p className="font-semibold text-gray-900">{testimonial.name}</p>
                <p className="text-sm text-gray-600">{testimonial.role}</p>
              </div>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}
