'use client';

import { Star } from 'lucide-react';

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

  const renderStars = (rating: number) => {
    return Array.from({ length: 5 }).map((_, index) => {
      const isFilled = rating >= index + 1;
      const isHalf = rating > index && rating < index + 1;

      return (
        <Star
          key={index}
          className={`w-4 h-4 ${isFilled ? 'text-secondary-300' : 'text-secondary-200/40'}`}
          fill={isFilled || isHalf ? 'currentColor' : 'none'}
          strokeWidth={isHalf ? 1 : 2}
        />
      );
    });
  };

  return (
    <section className="py-20 bg-gradient-to-b from-primary-900 via-primary-800 to-primary-900 text-sand-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="text-center max-w-2xl mx-auto mb-14">
          <span className="inline-flex items-center justify-center rounded-full bg-secondary-500/10 text-secondary-100 px-4 py-1 text-sm font-semibold tracking-wide uppercase mb-4">
            Testimonials
          </span>
          <h2 className="text-3xl md:text-4xl font-bold mb-4">Voices From Our Community</h2>
          <p className="text-lg text-secondary-100">
            Guests and hosts trust StayAfrica for authentic stays, seamless bookings, and warm hospitality.
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
          {testimonials.map((testimonial, index) => (
            <article
              key={index}
              className="card bg-primary-900/40 border border-primary-700/60 backdrop-blur h-full p-8 flex flex-col justify-between"
            >
              <div>
                <div className="flex items-center gap-2 mb-6">
                  <div className="flex items-center gap-1">{renderStars(testimonial.rating)}</div>
                  <span className="text-secondary-200 text-sm font-semibold">{testimonial.rating.toFixed(1)}/5</span>
                </div>
                <p className="text-lg leading-relaxed text-secondary-50/90">“{testimonial.text}”</p>
              </div>
              <div className="mt-6 pt-6 border-t border-primary-700/60">
                <p className="font-semibold text-secondary-50">{testimonial.name}</p>
                <p className="text-sm text-secondary-200/80">{testimonial.role}</p>
              </div>
            </article>
          ))}
        </div>
      </div>
    </section>
  );
}
