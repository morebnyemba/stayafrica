'use client';

import { useEffect } from 'react';
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
} from 'lucide-react';

const adminNavItems = [
  { label: 'Dashboard', href: '/admin', icon: LayoutDashboard },
  { label: 'Users', href: '/admin/users', icon: Users },
  { label: 'Properties', href: '/admin/properties', icon: Home },
  { label: 'Bookings', href: '/admin/bookings', icon: Calendar },
  { label: 'Payments', href: '/admin/payments', icon: CreditCard },
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

  const isAdmin = user?.is_staff === true;

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
      {/* Sidebar */}
      <aside className="w-64 bg-[#122F26] shadow-lg flex flex-col">
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
        {children}
      </main>
    </div>
  );
}
