'use client';

import Link from 'next/link';
import { Mail, Phone, MapPin, Facebook, Instagram, Twitter } from 'lucide-react';

export function Footer() {
  const currentYear = new Date().getFullYear();

  return (
    <footer className="bg-primary-900 text-sand-50 pt-16 pb-10 mt-20 relative overflow-hidden">
      <div className="absolute inset-0 bg-gradient-to-br from-primary-900 via-primary-800/90 to-primary-900 opacity-95" aria-hidden />
      <div className="absolute inset-0 pointer-events-none mix-blend-screen opacity-10 bg-[radial-gradient(circle_at_top_left,_rgba(217,177,104,0.4),_transparent_45%)]" aria-hidden />
      <div className="relative max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="grid grid-cols-1 md:grid-cols-4 gap-10 mb-12">
          {/* About */}
          <div>
            <h3 className="text-xl font-bold mb-4 text-secondary-100">StayAfrica</h3>
            <p className="text-sm text-secondary-100/80 leading-relaxed">
              Discover authentic stays, curated experiences, and trusted hosts in every corner of Africa.
            </p>
          </div>

          {/* Quick Links */}
          <div>
            <h4 className="text-lg font-semibold text-secondary-100 mb-4">Quick Links</h4>
            <ul className="space-y-3 text-sm text-secondary-200/70">
              <li>
                <Link href="/explore" className="hover:text-secondary-50 transition">
                  Explore Properties
                </Link>
              </li>
              <li>
                <Link href="/bookings" className="hover:text-secondary-50 transition">
                  My Bookings
                </Link>
              </li>
              <li>
                <Link href="/messages" className="hover:text-secondary-50 transition">
                  Messages
                </Link>
              </li>
              <li>
                <Link href="/register" className="hover:text-secondary-50 transition">
                  Become a Host
                </Link>
              </li>
            </ul>
          </div>

          {/* Support */}
          <div>
            <h4 className="text-lg font-semibold text-secondary-100 mb-4">Support</h4>
            <ul className="space-y-3 text-sm text-secondary-200/70">
              <li>
                <a href="#" className="hover:text-secondary-50 transition">
                  Help Center
                </a>
              </li>
              <li>
                <a href="#" className="hover:text-secondary-50 transition">
                  Contact Us
                </a>
              </li>
              <li>
                <a href="#" className="hover:text-secondary-50 transition">
                  Privacy Policy
                </a>
              </li>
              <li>
                <a href="#" className="hover:text-secondary-50 transition">
                  Terms & Conditions
                </a>
              </li>
            </ul>
          </div>

          {/* Contact */}
          <div>
            <h4 className="text-lg font-semibold text-secondary-100 mb-4">Contact</h4>
            <ul className="space-y-4 text-sm text-secondary-200/80">
              <li className="flex items-center space-x-3">
                <Mail className="w-4 h-4 text-secondary-200" />
                <a href="mailto:support@stayafrica.com" className="hover:text-secondary-50">
                  support@stayafrica.com
                </a>
              </li>
              <li className="flex items-center space-x-3">
                <Phone className="w-4 h-4 text-secondary-200" />
                <a href="tel:+27123456789" className="hover:text-secondary-50">
                  +27 123 456 789
                </a>
              </li>
              <li className="flex items-start space-x-3">
                <MapPin className="w-4 h-4 mt-1 text-secondary-200" />
                <span>Johannesburg, South Africa</span>
              </li>
            </ul>
          </div>
        </div>

        <div className="border-t border-primary-700/60 pt-8 flex flex-col md:flex-row justify-between items-start md:items-center gap-6">
          <p className="text-sm text-secondary-200/70">
            &copy; {currentYear} StayAfrica. All rights reserved.
          </p>
          <div className="flex items-center gap-6 text-secondary-200/70 text-sm">
            <a href="#" className="hover:text-secondary-50 transition">
              Privacy
            </a>
            <a href="#" className="hover:text-secondary-50 transition">
              Cookies
            </a>
            <a href="#" className="hover:text-secondary-50 transition">
              Accessibility
            </a>
          </div>
          <div className="flex items-center gap-4">
            <a href="#" aria-label="StayAfrica on Facebook" className="text-secondary-200/70 hover:text-secondary-50 transition">
              <Facebook className="w-5 h-5" />
            </a>
            <a href="#" aria-label="StayAfrica on Instagram" className="text-secondary-200/70 hover:text-secondary-50 transition">
              <Instagram className="w-5 h-5" />
            </a>
            <a href="#" aria-label="StayAfrica on Twitter" className="text-secondary-200/70 hover:text-secondary-50 transition">
              <Twitter className="w-5 h-5" />
            </a>
          </div>
        </div>
      </div>
    </footer>
  );
}
