'use client';

import { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/store/auth-store';
import Link from 'next/link';
import Image from 'next/image';
import {
  LayoutDashboard,
  Users,
  Home,
  Calendar,
  CreditCard,
  Settings,
  FileText,
  LogOut,
  Menu,
  X,
  Shield,
  Star,
  Wallet,
  Receipt,
} from 'lucide-react';

const adminNavItems = [
  { label: 'Dashboard', href: '/admin', icon: LayoutDashboard },
  { label: 'Users', href: '/admin/users', icon: Users },
  { label: 'Properties', href: '/admin/properties', icon: Home },
  { label: 'Bookings', href: '/admin/bookings', icon: Calendar },
  { label: 'Reviews', href: '/admin/reviews', icon: Star },
  { label: 'Payments', href: '/admin/payments', icon: CreditCard },
  { label: 'Wallets', href: '/admin/wallets', icon: Wallet },
  { label: 'Tax Config', href: '/admin/tax-config', icon: Receipt },
  { label: 'Identity Verification', href: '/admin/identity-verification', icon: Shield },
  { label: 'Audit Logs', href: '/admin/audit-logs', icon: FileText },
  { label: 'Settings', href: '/admin/settings', icon: Settings },
];

export default function AdminLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  const router = useRouter();
  const { user, isLoading, logout } = useAuth();
  // Initialize sidebar closed on mobile, open on desktop
  const [sidebarOpen, setSidebarOpen] = useState(false);

  const isAdmin = user?.is_staff === true;

  // Set initial sidebar state based on screen size
  useEffect(() => {
    const handleResize = () => {
      // lg breakpoint is 1024px in Tailwind
      setSidebarOpen(window.innerWidth >= 1024);
    };
    
    // Set initial state
    handleResize();
    
    // Listen for window resize
    window.addEventListener('resize', handleResize);
    return () => window.removeEventListener('resize', handleResize);
  }, []);

  useEffect(() => {
    if (!isLoading && !isAdmin) {
      router.push('/login?error=admin_access_required');
    }
  }, [user, isLoading, isAdmin, router]);

  if (isLoading) {
    return (
      <div className="flex items-center justify-center min-h-screen bg-[#F4F1EA]">
        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
      </div>
    );
  }

  if (!isAdmin) {
    return null;
  }

  return (
    <div className="flex h-screen bg-[#F4F1EA]">
      {/* Mobile Sidebar Toggle Button */}
      <button
        onClick={() => setSidebarOpen(!sidebarOpen)}
        className="fixed top-4 left-4 z-50 lg:hidden bg-[#122F26] text-[#F4F1EA] p-2 rounded-lg shadow-lg hover:bg-[#3A5C50] transition-colors"
        aria-label="Toggle Sidebar"
      >
        {sidebarOpen ? <X className="w-6 h-6" /> : <Menu className="w-6 h-6" />}
      </button>

      {/* Sidebar Overlay for Mobile */}
      {sidebarOpen && (
        <div
          className="fixed inset-0 bg-black/50 z-40 lg:hidden"
          onClick={() => setSidebarOpen(false)}
        />
      )}

      {/* Sidebar */}
      <aside className={`
        ${sidebarOpen ? 'translate-x-0' : '-translate-x-full'}
        fixed lg:sticky top-0 left-0 h-screen
        w-64 bg-[#122F26] shadow-lg flex flex-col
        transition-transform duration-300 ease-in-out
        z-40 lg:translate-x-0
      `}>
        <div className="p-6 border-b border-[#3A5C50]">
          <Image
            src="/logo.png"
            alt="StayAfrica"
            width={180}
            height={50}
            className="mb-2"
          />
          <p className="text-sm text-[#D9B168] mt-2 font-medium">Admin Portal</p>
        </div>

        <nav className="flex-1 p-4 space-y-1">
          {adminNavItems.map((item) => (
            <Link
              key={item.href}
              href={item.href}
              className="flex items-center space-x-3 px-4 py-3 text-[#F4F1EA] rounded-lg hover:bg-[#3A5C50] hover:text-[#D9B168] transition-colors"
            >
              <item.icon className="w-5 h-5" />
              <span className="font-medium">{item.label}</span>
            </Link>
          ))}
        </nav>

        <div className="p-4 border-t border-[#3A5C50]">
          <div className="mb-4 p-3 bg-[#3A5C50] rounded-lg">
            <p className="text-sm font-medium text-[#F4F1EA]">{user.first_name} {user.last_name}</p>
            <p className="text-xs text-[#D9B168]">{user.email}</p>
            <p className="text-xs text-[#D9B168] mt-1 font-semibold">Admin</p>
          </div>
          <button
            onClick={logout}
            className="flex items-center space-x-2 w-full px-4 py-2 text-[#F4F1EA] bg-red-600 hover:bg-red-700 rounded-lg transition-colors"
          >
            <LogOut className="w-5 h-5" />
            <span>Logout</span>
          </button>
        </div>
      </aside>

      {/* Main Content */}
      <main className="flex-1 overflow-auto">
        {/* Desktop Sidebar Toggle */}
        <div className="hidden lg:block sticky top-0 z-30 bg-[#F4F1EA] border-b border-[#122F26]/10">
          <div className="px-4 py-3">
            <button
              onClick={() => setSidebarOpen(!sidebarOpen)}
              className="bg-[#122F26] text-[#F4F1EA] p-2 rounded-lg shadow hover:bg-[#3A5C50] transition-colors"
              aria-label="Toggle Sidebar"
            >
              <Menu className="w-5 h-5" />
            </button>
          </div>
        </div>
        {/* Content with padding to accommodate mobile toggle button */}
        <div className="p-4 pt-16 lg:pt-4">
          {children}
        </div>
      </main>
    </div>
  );
}
