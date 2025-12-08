'use client';

import { CheckCircle, Compass, CalendarCheck, Wallet, Sparkles } from 'lucide-react';

export function HowItWorks() {
  const steps = [
    {
      number: 1,
      title: 'Search & Discover',
      description: 'Browse properties across Africa and find your perfect match',
      icon: <Compass className="w-6 h-6" />,
    },
    {
      number: 2,
      title: 'Book Your Stay',
      description: 'Select dates and book your accommodation instantly',
      icon: <CalendarCheck className="w-6 h-6" />,
    },
    {
      number: 3,
      title: 'Make Payment',
      description: 'Secure payment via multiple regional payment gateways',
      icon: <Wallet className="w-6 h-6" />,
    },
    {
      number: 4,
      title: 'Enjoy & Review',
      description: 'Have an amazing experience and share your feedback',
      icon: <Sparkles className="w-6 h-6" />,
    },
  ];

  return (
    <section className="py-20 bg-ivory">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="text-center max-w-3xl mx-auto mb-14">
          <span className="inline-flex items-center justify-center rounded-full bg-secondary-100 text-secondary-700 px-4 py-1 text-sm font-semibold tracking-wide uppercase mb-4">
            Seamless Journey
          </span>
          <h2 className="text-3xl md:text-4xl font-bold text-primary-900 mb-4">How StayAfrica Works</h2>
          <p className="text-lg text-primary-600">
            From your first search to your final review, every step is crafted to keep your African getaway effortless and inspiring.
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-4 gap-6">
          {steps.map((step) => (
            <div
              key={step.number}
              className="card group h-full bg-white/95 border border-primary-100/80 hover:border-secondary-400 transition flex flex-col items-center text-center px-6 py-10"
            >
              <div className="mb-6 flex items-center justify-center w-16 h-16 rounded-2xl bg-secondary-100 text-secondary-700 font-semibold text-xl shadow-inner">
                {step.icon}
              </div>
              <div className="text-sm uppercase tracking-[0.3em] text-secondary-500 mb-2">Step {step.number}</div>
              <h3 className="text-xl font-semibold text-primary-900 mb-3">{step.title}</h3>
              <p className="text-primary-600 mb-6">{step.description}</p>
              <div className="mt-auto flex items-center gap-2 text-sm text-primary-500">
                <CheckCircle className="w-4 h-4 text-secondary-500" />
                Trusted by thousands of travelers
              </div>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}
