'use client';

import Link from 'next/link';
import { useAuth } from '@/context/auth-context';
import { useRouter } from 'next/navigation';
import { Menu, User, LogOut } from 'lucide-react';
import { useState } from 'react';
import { ThemeToggle } from './theme-toggle';
import { MobileSearchBar } from './mobile-search-bar';

export function Navigation() {
  const { user, isAuthenticated, logout } = useAuth();
  const router = useRouter();
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false);

  const handleLogout = () => {
    logout();
    router.push('/');
  };

  return (
    <>
      <nav className="sticky top-0 z-50 bg-primary-800/95 backdrop-blur border-b border-primary-700 text-sand-100 shadow-lg">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex items-center justify-between h-16">
          {/* Logo */}
          <Link href="/" className="flex items-center space-x-2 text-sand-100">
            <img src="/logo.png" alt="StayAfrica" className="h-16 w-auto" />
          </Link>

          {/* Center Navigation - Desktop */}
          <div className="hidden md:flex items-center space-x-8">
            <Link href="/explore" className="text-sand-200 hover:text-secondary-300 transition font-medium">
              Stays
            </Link>
            <Link href="/experiences" className="text-sand-200 hover:text-secondary-300 transition font-medium">
              Experiences
            </Link>
            {isAuthenticated && (
              <Link href="/wishlist" className="text-sand-200 hover:text-secondary-300 transition font-medium">
                Wishlist
              </Link>
            )}
          </div>

          {/* Right Side - Desktop */}
          <div className="hidden md:flex items-center space-x-4">
            <ThemeToggle />
            {isAuthenticated ? (
              <>
                <Link href="/host" className="text-sand-200 hover:text-sand-50 transition font-medium rounded-full px-4 py-2 hover:bg-primary-700/60">
                  Become a Host
                </Link>
                <Link href="/dashboard" className="text-sand-200 hover:text-sand-50 transition font-medium">
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
                <Link href="/host" className="text-sand-200 hover:text-sand-50 transition font-medium rounded-full px-4 py-2 hover:bg-primary-700/60">
                  Become a Host
                </Link>
                <Link href="/login" className="text-sand-200 hover:text-sand-50 transition font-medium">
                  Login
                </Link>
                <Link href="/register" className="btn-primary rounded-full">
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
            <Link href="/explore" className="block text-sand-200 hover:text-secondary-300 px-4 py-2">
              Stays
            </Link>
            <Link href="/experiences" className="block text-sand-200 hover:text-secondary-300 px-4 py-2">
              Experiences
            </Link>
            <Link href="/host" className="block text-sand-200 hover:text-secondary-300 px-4 py-2">
              Become a Host
            </Link>
            <div className="flex items-center justify-between px-4 py-2">
              <span className="text-sand-200">Theme</span>
              <ThemeToggle />
            </div>
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
    <MobileSearchBar />
    </>
  );
}
