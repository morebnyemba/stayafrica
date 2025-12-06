'use client';

import Link from 'next/link';
import { useAuth } from '@/context/auth-context';
import { useRouter } from 'next/navigation';
import { Menu, Search, User, LogOut, Home } from 'lucide-react';
import { useState } from 'react';

export function Navigation() {
  const { user, isAuthenticated, logout } = useAuth();
  const router = useRouter();
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false);

  const handleLogout = () => {
    logout();
    router.push('/');
  };

  return (
    <nav className="sticky top-0 z-50 bg-white border-b border-gray-200 shadow-sm">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between h-16">
          {/* Logo */}
          <Link href="/" className="flex items-center space-x-2">
            <Home className="w-6 h-6 text-primary-600" />
            <span className="text-xl font-bold text-gray-900">StayAfrica</span>
          </Link>

          {/* Center Navigation - Desktop */}
          <div className="hidden md:flex items-center space-x-8">
            <Link href="/explore" className="text-gray-700 hover:text-primary-600 transition">
              Explore
            </Link>
            <Link href="/bookings" className="text-gray-700 hover:text-primary-600 transition">
              Bookings
            </Link>
            <Link href="/messages" className="text-gray-700 hover:text-primary-600 transition">
              Messages
            </Link>
          </div>

          {/* Right Side - Desktop */}
          <div className="hidden md:flex items-center space-x-4">
            {isAuthenticated ? (
              <>
                <Link href="/dashboard" className="btn-secondary">
                  Dashboard
                </Link>
                <Link href="/profile" className="flex items-center space-x-2">
                  {user?.profile_picture ? (
                    <img
                      src={user.profile_picture}
                      alt={user.first_name}
                      className="w-8 h-8 rounded-full"
                    />
                  ) : (
                    <User className="w-5 h-5 text-gray-700" />
                  )}
                  <span className="text-gray-700">{user?.first_name}</span>
                </Link>
                <button
                  onClick={handleLogout}
                  className="text-gray-700 hover:text-red-600 transition flex items-center space-x-1"
                >
                  <LogOut className="w-4 h-4" />
                  <span>Logout</span>
                </button>
              </>
            ) : (
              <>
                <Link href="/login" className="btn-secondary">
                  Login
                </Link>
                <Link href="/register" className="btn-primary">
                  Sign Up
                </Link>
              </>
            )}
          </div>

          {/* Mobile Menu Button */}
          <button
            onClick={() => setMobileMenuOpen(!mobileMenuOpen)}
            className="md:hidden p-2 rounded-lg hover:bg-gray-100"
          >
            <Menu className="w-6 h-6" />
          </button>
        </div>

        {/* Mobile Menu */}
        {mobileMenuOpen && (
          <div className="md:hidden border-t border-gray-200 py-4 space-y-4">
            <Link href="/explore" className="block text-gray-700 hover:text-primary-600">
              Explore
            </Link>
            <Link href="/bookings" className="block text-gray-700 hover:text-primary-600">
              Bookings
            </Link>
            <Link href="/messages" className="block text-gray-700 hover:text-primary-600">
              Messages
            </Link>
            {isAuthenticated ? (
              <>
                <Link href="/dashboard" className="block btn-secondary w-full text-center">
                  Dashboard
                </Link>
                <button
                  onClick={handleLogout}
                  className="block w-full text-left text-gray-700 hover:text-red-600"
                >
                  Logout
                </button>
              </>
            ) : (
              <>
                <Link href="/login" className="block btn-secondary w-full text-center">
                  Login
                </Link>
                <Link href="/register" className="block btn-primary w-full text-center">
                  Sign Up
                </Link>
              </>
            )}
          </div>
        )}
      </div>
    </nav>
  );
}
