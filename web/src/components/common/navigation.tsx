'use client';

import React from 'react';
import Link from 'next/link';
import { useAuth } from '@/context/auth-context';
import { useRouter } from 'next/navigation';
import { Menu, User, LogOut, Home } from 'lucide-react';
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
    <nav className="sticky top-0 z-50 bg-primary-800/95 backdrop-blur border-b border-primary-700 text-sand-100 shadow-lg">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between h-16">
          {/* Logo */}
          <Link href="/" className="flex items-center space-x-2 text-sand-100">
            <Home className="w-6 h-6 text-secondary-400" />
            <span className="text-xl font-bold tracking-wide">StayAfrica</span>
          </Link>

          {/* Center Navigation - Desktop */}
          <div className="hidden md:flex items-center space-x-8">
            <Link href="/explore" className="text-sand-200 hover:text-secondary-300 transition">
              Explore
            </Link>
            <Link href="/bookings" className="text-sand-200 hover:text-secondary-300 transition">
              Bookings
            </Link>
            <Link href="/messages" className="text-sand-200 hover:text-secondary-300 transition">
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
                    <User className="w-5 h-5 text-sand-200" />
                  )}
                  <span className="text-sand-100">{user?.first_name}</span>
                </Link>
                <button
                  onClick={handleLogout}
                  className="text-sand-200 hover:text-secondary-400 transition flex items-center space-x-1"
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
            className="md:hidden p-2 rounded-lg text-sand-100 hover:bg-primary-700/60"
          >
            <Menu className="w-6 h-6" />
          </button>
        </div>

        {/* Mobile Menu */}
        {mobileMenuOpen && (
          <div className="md:hidden border-t border-primary-700 py-4 space-y-4 bg-primary-800/95 text-sand-100">
            <Link href="/explore" className="block text-sand-200 hover:text-secondary-300">
              Explore
            </Link>
            <Link href="/bookings" className="block text-sand-200 hover:text-secondary-300">
              Bookings
            </Link>
            <Link href="/messages" className="block text-sand-200 hover:text-secondary-300">
              Messages
            </Link>
            {isAuthenticated ? (
              <>
                <Link href="/dashboard" className="block btn-secondary w-full text-center">
                  Dashboard
                </Link>
                <button
                  onClick={handleLogout}
                  className="block w-full text-left text-sand-200 hover:text-secondary-400"
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
