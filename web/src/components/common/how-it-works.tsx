'use client';

import { CheckCircle } from 'lucide-react';

export function HowItWorks() {
  const steps = [
    {
      number: 1,
      title: 'Search & Discover',
      description: 'Browse properties across Africa and find your perfect match',
      icon: '🔍',
    },
    {
      number: 2,
      title: 'Book Your Stay',
      description: 'Select dates and book your accommodation instantly',
      icon: '📅',
    },
    {
      number: 3,
      title: 'Make Payment',
      description: 'Secure payment via multiple regional payment gateways',
      icon: '💳',
    },
    {
      number: 4,
      title: 'Enjoy & Review',
      description: 'Have an amazing experience and share your feedback',
      icon: '⭐',
    },
  ];

  return (
    <section className="py-16 bg-white">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="text-center mb-12">
          <h2 className="text-3xl md:text-4xl font-bold mb-4">How It Works</h2>
          <p className="text-gray-600 text-lg">Simple steps to book your next adventure</p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
          {steps.map((step) => (
            <div key={step.number} className="text-center">
              <div className="text-5xl mb-4">{step.icon}</div>
              <div className="text-3xl font-bold text-primary-600 mb-2">{step.number}</div>
              <h3 className="text-xl font-semibold mb-2">{step.title}</h3>
              <p className="text-gray-600">{step.description}</p>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}
