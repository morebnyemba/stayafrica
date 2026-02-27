'use client';

import Link from 'next/link';
import { useAuth } from '@/store/auth-store';
import { useRouter, usePathname } from 'next/navigation';
import {
  Menu,
  X,
  User,
  LogOut,
  Loader2,
  LayoutDashboard,
  Building2,
  Plane,
  ChevronDown,
  Heart,
  MessageSquare,
  Compass,
  Sparkles,
  Settings,
  ArrowRightLeft,
} from 'lucide-react';
import { useState, useEffect, useRef, useCallback } from 'react';
import { ThemeToggle } from './theme-toggle';
import { MobileSearchBar } from './mobile-search-bar';
import { Button } from '@/components/ui/Button';
import { NotificationCenter } from '@/components/notifications/NotificationCenter';
import { cn } from '@/lib/utils';

// ─── Nav link with active state ─────────────────────────────────────────────
function NavLink({
  href,
  children,
  icon: Icon,
  pathname,
  onClick,
  mobile = false,
}: {
  href: string;
  children: React.ReactNode;
  icon?: React.ElementType;
  pathname: string;
  onClick?: () => void;
  mobile?: boolean;
}) {
  const isActive = pathname === href || pathname.startsWith(href + '/');

  if (mobile) {
    return (
      <Link
        href={href}
        onClick={onClick}
        className={cn(
          'flex items-center gap-3 px-4 py-3 rounded-xl mx-2 transition-all duration-200',
          isActive
            ? 'bg-secondary-500/20 text-secondary-300 font-semibold'
            : 'text-sand-200 hover:bg-primary-600/50 hover:text-sand-50'
        )}
      >
        {Icon && <Icon className="w-5 h-5 flex-shrink-0" />}
        <span>{children}</span>
        {isActive && (
          <span className="ml-auto w-1.5 h-1.5 rounded-full bg-secondary-400" />
        )}
      </Link>
    );
  }

  return (
    <Link
      href={href}
      className={cn(
        'relative px-3 py-2 text-sm font-medium transition-all duration-200 rounded-lg',
        isActive
          ? 'text-secondary-300'
          : 'text-sand-200 hover:text-sand-50 hover:bg-primary-700/40'
      )}
    >
      {children}
      {isActive && (
        <span className="absolute bottom-0 left-1/2 -translate-x-1/2 w-5 h-0.5 rounded-full bg-secondary-400" />
      )}
    </Link>
  );
}

// ─── Profile mode pill ──────────────────────────────────────────────────────
function ModeBadge({ mode }: { mode: 'guest' | 'host' }) {
  return (
    <span
      className={cn(
        'inline-flex items-center gap-1 text-xs font-semibold px-2 py-0.5 rounded-full',
        mode === 'host'
          ? 'bg-secondary-500/20 text-secondary-300'
          : 'bg-sky-500/20 text-sky-300'
      )}
    >
      {mode === 'host' ? (
        <Building2 className="w-3 h-3" />
      ) : (
        <Plane className="w-3 h-3" />
      )}
      {mode === 'host' ? 'Hosting' : 'Traveling'}
    </span>
  );
}

// ─── Main Navigation ────────────────────────────────────────────────────────
export function Navigation() {
  const { user, isAuthenticated, logout, upgradeToHost, switchProfile } = useAuth();
  const router = useRouter();
  const pathname = usePathname();
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false);
  const [hostLoading, setHostLoading] = useState(false);
  const [switchLoading, setSwitchLoading] = useState(false);
  const [userMenuOpen, setUserMenuOpen] = useState(false);
  const [scrolled, setScrolled] = useState(false);
  const userMenuRef = useRef<HTMLDivElement>(null);

  const isHost = user?.role === 'host' || user?.role === 'admin';
  const activeProfile = user?.active_profile ?? 'guest';
  const dashboardHref = activeProfile === 'host' ? '/host/dashboard' : '/dashboard';

  // ── Scroll detection ────────────────────────────────────────────────────
  useEffect(() => {
    const onScroll = () => setScrolled(window.scrollY > 8);
    window.addEventListener('scroll', onScroll, { passive: true });
    return () => window.removeEventListener('scroll', onScroll);
  }, []);

  // ── Close user dropdown on outside click ────────────────────────────────
  useEffect(() => {
    function handleClickOutside(event: MouseEvent) {
      if (userMenuRef.current && !userMenuRef.current.contains(event.target as Node)) {
        setUserMenuOpen(false);
      }
    }
    document.addEventListener('mousedown', handleClickOutside);
    return () => document.removeEventListener('mousedown', handleClickOutside);
  }, []);

  // ── Close mobile menu on route change ───────────────────────────────────
  useEffect(() => {
    setMobileMenuOpen(false);
    setUserMenuOpen(false);
  }, [pathname]);

  // ── Lock body scroll when mobile menu is open ───────────────────────────
  useEffect(() => {
    document.body.style.overflow = mobileMenuOpen ? 'hidden' : '';
    return () => { document.body.style.overflow = ''; };
  }, [mobileMenuOpen]);

  const handleLogout = useCallback(() => {
    setUserMenuOpen(false);
    logout();
    // Full page navigation ensures middleware sees the cleared cookie
    window.location.replace('/');
  }, [logout]);

  const handleBecomeHost = useCallback(async () => {
    if (!isAuthenticated) { router.push('/host'); return; }
    if (isHost) { router.push('/host/dashboard'); return; }
    try {
      setHostLoading(true);
      await upgradeToHost();
      router.push('/host/dashboard');
    } catch {
      router.push('/host');
    } finally {
      setHostLoading(false);
    }
  }, [isAuthenticated, isHost, router, upgradeToHost]);

  const handleSwitchProfile = useCallback(async () => {
    const target = activeProfile === 'host' ? 'guest' : 'host';
    try {
      setSwitchLoading(true);
      await switchProfile(target);
      router.push(target === 'host' ? '/host/dashboard' : '/explore');
    } catch (error: any) {
      console.error('Failed to switch profile:', error.message);
    } finally {
      setSwitchLoading(false);
    }
  }, [activeProfile, switchProfile, router]);

  const closeMobile = () => setMobileMenuOpen(false);

  // ── User initials for avatar fallback ───────────────────────────────────
  const initials = user
    ? `${(user.first_name?.[0] || '').toUpperCase()}${(user.last_name?.[0] || '').toUpperCase()}` || user.email?.[0]?.toUpperCase() || '?'
    : '?';

  return (
    <>
      <MobileSearchBar />
      <div className="h-[52px] md:hidden" aria-hidden="true" />

      <nav
        className={cn(
          'sticky top-[52px] md:top-0 z-40 transition-all duration-300',
          scrolled
            ? 'bg-primary-800/95 backdrop-blur-md shadow-lg'
            : 'bg-primary-800 shadow-md'
        )}
      >
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex items-center justify-between h-16">
            {/* ── Logo ──────────────────────────────────────────────── */}
            <Link href="/" className="flex items-center gap-2 flex-shrink-0">
              <img src="/logo.png" alt="StayAfrica" className="h-10 w-auto" />
            </Link>

            {/* ── Center links (desktop) ────────────────────────────── */}
            <div className="hidden md:flex items-center gap-1">
              <NavLink href="/explore" pathname={pathname} icon={Compass}>
                Stays
              </NavLink>
              <NavLink href="/experiences" pathname={pathname} icon={Sparkles}>
                Experiences
              </NavLink>
              {isAuthenticated && (
                <>
                  <NavLink href="/wishlist" pathname={pathname} icon={Heart}>
                    Wishlist
                  </NavLink>
                  <NavLink href="/messages" pathname={pathname} icon={MessageSquare}>
                    Messages
                  </NavLink>
                </>
              )}
            </div>

            {/* ── Right side (desktop) ──────────────────────────────── */}
            <div className="hidden md:flex items-center gap-2">
              <ThemeToggle />

              {isAuthenticated ? (
                <>
                  <NotificationCenter />

                  {/* Switch / Become host button */}
                  {isHost ? (
                    <button
                      onClick={handleSwitchProfile}
                      disabled={switchLoading}
                      className={cn(
                        'flex items-center gap-2 px-3 py-1.5 rounded-full text-sm font-medium transition-all duration-200 border',
                        activeProfile === 'host'
                          ? 'border-sky-400/40 text-sky-300 hover:bg-sky-500/10'
                          : 'border-secondary-400/40 text-secondary-300 hover:bg-secondary-500/10'
                      )}
                    >
                      {switchLoading ? (
                        <Loader2 className="w-4 h-4 animate-spin" />
                      ) : (
                        <ArrowRightLeft className="w-4 h-4" />
                      )}
                      {activeProfile === 'host' ? 'Switch to Traveling' : 'Switch to Hosting'}
                    </button>
                  ) : (
                    <button
                      onClick={handleBecomeHost}
                      disabled={hostLoading}
                      className="flex items-center gap-2 px-3 py-1.5 rounded-full text-sm font-medium border border-secondary-400/40 text-secondary-300 hover:bg-secondary-500/10 transition-all duration-200"
                    >
                      {hostLoading ? (
                        <Loader2 className="w-4 h-4 animate-spin" />
                      ) : (
                        <Building2 className="w-4 h-4" />
                      )}
                      Become a Host
                    </button>
                  )}

                  {/* ── User avatar dropdown ─────────────────────────── */}
                  <div className="relative" ref={userMenuRef}>
                    <button
                      onClick={() => setUserMenuOpen((o) => !o)}
                      className={cn(
                        'flex items-center gap-2 pl-2 pr-3 py-1.5 rounded-full transition-all duration-200',
                        userMenuOpen
                          ? 'bg-primary-600 ring-2 ring-secondary-400/50'
                          : 'hover:bg-primary-700/60'
                      )}
                    >
                      {user?.profile_picture ? (
                        <img
                          src={user.profile_picture}
                          alt=""
                          className="w-8 h-8 rounded-full object-cover ring-2 ring-sand-300/30"
                        />
                      ) : (
                        <span className="w-8 h-8 rounded-full bg-secondary-500 text-primary-900 flex items-center justify-center text-sm font-bold">
                          {initials}
                        </span>
                      )}
                      <span className="text-sand-100 text-sm font-medium max-w-[100px] truncate">
                        {user?.first_name || 'Account'}
                      </span>
                      <ChevronDown
                        className={cn(
                          'w-4 h-4 text-sand-300 transition-transform duration-200',
                          userMenuOpen && 'rotate-180'
                        )}
                      />
                    </button>

                    {/* Dropdown */}
                    {userMenuOpen && (
                      <div className="absolute right-0 mt-2 w-64 bg-white dark:bg-primary-800 rounded-xl shadow-xl ring-1 ring-black/10 dark:ring-white/10 py-2 z-50 animate-in fade-in slide-in-from-top-2 duration-200">
                        {/* User info */}
                        <div className="px-4 py-3 border-b border-neutral-100 dark:border-primary-700">
                          <p className="text-sm font-semibold text-neutral-900 dark:text-sand-50 truncate">
                            {user?.first_name} {user?.last_name}
                          </p>
                          <p className="text-xs text-neutral-500 dark:text-sand-400 truncate">
                            {user?.email}
                          </p>
                          {isHost && (
                            <div className="mt-2">
                              <ModeBadge mode={activeProfile as 'guest' | 'host'} />
                            </div>
                          )}
                        </div>

                        <div className="py-1">
                          <Link
                            href={dashboardHref}
                            className="flex items-center gap-3 px-4 py-2.5 text-sm text-neutral-700 dark:text-sand-200 hover:bg-neutral-50 dark:hover:bg-primary-700 transition"
                          >
                            <LayoutDashboard className="w-4 h-4" />
                            Dashboard
                          </Link>
                          <Link
                            href="/profile"
                            className="flex items-center gap-3 px-4 py-2.5 text-sm text-neutral-700 dark:text-sand-200 hover:bg-neutral-50 dark:hover:bg-primary-700 transition"
                          >
                            <User className="w-4 h-4" />
                            Profile
                          </Link>
                          {isHost && (
                            <Link
                              href="/host/dashboard"
                              className="flex items-center gap-3 px-4 py-2.5 text-sm text-neutral-700 dark:text-sand-200 hover:bg-neutral-50 dark:hover:bg-primary-700 transition"
                            >
                              <Building2 className="w-4 h-4" />
                              Host Dashboard
                            </Link>
                          )}
                          <Link
                            href="/profile"
                            className="flex items-center gap-3 px-4 py-2.5 text-sm text-neutral-700 dark:text-sand-200 hover:bg-neutral-50 dark:hover:bg-primary-700 transition"
                          >
                            <Settings className="w-4 h-4" />
                            Settings
                          </Link>
                        </div>

                        <div className="border-t border-neutral-100 dark:border-primary-700 pt-1">
                          <button
                            onClick={handleLogout}
                            className="flex items-center gap-3 w-full px-4 py-2.5 text-sm text-red-600 dark:text-red-400 hover:bg-red-50 dark:hover:bg-red-500/10 transition"
                          >
                            <LogOut className="w-4 h-4" />
                            Sign out
                          </button>
                        </div>
                      </div>
                    )}
                  </div>
                </>
              ) : (
                <>
                  <button
                    onClick={handleBecomeHost}
                    disabled={hostLoading}
                    className="flex items-center gap-2 text-sand-200 hover:text-sand-50 text-sm font-medium px-3 py-2 rounded-lg hover:bg-primary-700/40 transition"
                  >
                    {hostLoading && <Loader2 className="w-4 h-4 animate-spin" />}
                    Become a Host
                  </button>
                  <Link
                    href="/login"
                    className="text-sand-200 hover:text-sand-50 text-sm font-medium px-3 py-2 rounded-lg hover:bg-primary-700/40 transition"
                  >
                    Login
                  </Link>
                  <Link href="/register">
                    <Button size="sm">Sign Up</Button>
                  </Link>
                </>
              )}
            </div>

            {/* ── Mobile right side ─────────────────────────────────── */}
            <div className="flex md:hidden items-center gap-2">
              {isAuthenticated && <NotificationCenter />}
              <ThemeToggle />
              <button
                onClick={() => setMobileMenuOpen((o) => !o)}
                className="p-2 rounded-lg text-sand-100 hover:bg-primary-700/60 transition"
                aria-label="Toggle menu"
              >
                {mobileMenuOpen ? (
                  <X className="w-6 h-6" />
                ) : (
                  <Menu className="w-6 h-6" />
                )}
              </button>
            </div>
          </div>
        </div>

        {/* ── Mobile menu overlay + drawer ────────────────────────────── */}
        {/* Backdrop */}
        <div
          className={cn(
            'fixed inset-0 bg-black/40 z-40 md:hidden transition-opacity duration-300',
            mobileMenuOpen ? 'opacity-100' : 'opacity-0 pointer-events-none'
          )}
          onClick={closeMobile}
          aria-hidden
        />

        {/* Slide-in panel */}
        <div
          className={cn(
            'fixed top-0 right-0 bottom-0 w-[85%] max-w-sm bg-primary-800 z-50 md:hidden transition-transform duration-300 ease-out overflow-y-auto',
            mobileMenuOpen ? 'translate-x-0' : 'translate-x-full'
          )}
        >
          {/* Mobile header */}
          <div className="flex items-center justify-between px-4 py-4 border-b border-primary-700">
            {isAuthenticated && user ? (
              <div className="flex items-center gap-3 min-w-0">
                {user.profile_picture ? (
                  <img
                    src={user.profile_picture}
                    alt=""
                    className="w-10 h-10 rounded-full object-cover ring-2 ring-sand-300/30 flex-shrink-0"
                  />
                ) : (
                  <span className="w-10 h-10 rounded-full bg-secondary-500 text-primary-900 flex items-center justify-center text-sm font-bold flex-shrink-0">
                    {initials}
                  </span>
                )}
                <div className="min-w-0">
                  <p className="text-sand-50 font-semibold text-sm truncate">
                    {user.first_name} {user.last_name}
                  </p>
                  {isHost && <ModeBadge mode={activeProfile as 'guest' | 'host'} />}
                </div>
              </div>
            ) : (
              <span className="text-sand-100 font-semibold">Menu</span>
            )}
            <button
              onClick={closeMobile}
              className="p-2 rounded-lg text-sand-300 hover:text-sand-50 hover:bg-primary-700/60 transition"
              aria-label="Close menu"
            >
              <X className="w-5 h-5" />
            </button>
          </div>

          {/* Mobile nav links */}
          <div className="py-3 space-y-0.5">
            <NavLink href="/explore" pathname={pathname} icon={Compass} onClick={closeMobile} mobile>
              Stays
            </NavLink>
            <NavLink href="/experiences" pathname={pathname} icon={Sparkles} onClick={closeMobile} mobile>
              Experiences
            </NavLink>
            {isAuthenticated && (
              <>
                <NavLink href="/wishlist" pathname={pathname} icon={Heart} onClick={closeMobile} mobile>
                  Wishlist
                </NavLink>
                <NavLink href="/messages" pathname={pathname} icon={MessageSquare} onClick={closeMobile} mobile>
                  Messages
                </NavLink>
              </>
            )}
          </div>

          {/* Mobile profile-switching / host actions */}
          {isAuthenticated && (
            <div className="border-t border-primary-700 py-3 space-y-0.5">
              <NavLink href={dashboardHref} pathname={pathname} icon={LayoutDashboard} onClick={closeMobile} mobile>
                Dashboard
              </NavLink>
              <NavLink href="/profile" pathname={pathname} icon={User} onClick={closeMobile} mobile>
                Profile
              </NavLink>

              {isHost ? (
                <>
                  <NavLink href="/host/dashboard" pathname={pathname} icon={Building2} onClick={closeMobile} mobile>
                    Host Dashboard
                  </NavLink>
                  <button
                    onClick={() => { closeMobile(); handleSwitchProfile(); }}
                    disabled={switchLoading}
                    className="flex items-center gap-3 px-4 py-3 rounded-xl mx-2 w-[calc(100%-16px)] text-left transition-all duration-200 text-sand-200 hover:bg-primary-600/50 hover:text-sand-50"
                  >
                    {switchLoading ? (
                      <Loader2 className="w-5 h-5 animate-spin flex-shrink-0" />
                    ) : (
                      <ArrowRightLeft className="w-5 h-5 flex-shrink-0" />
                    )}
                    {activeProfile === 'host' ? 'Switch to Traveling' : 'Switch to Hosting'}
                  </button>
                </>
              ) : (
                <button
                  onClick={() => { closeMobile(); handleBecomeHost(); }}
                  disabled={hostLoading}
                  className="flex items-center gap-3 px-4 py-3 rounded-xl mx-2 w-[calc(100%-16px)] text-left transition-all duration-200 text-secondary-300 hover:bg-secondary-500/10"
                >
                  {hostLoading ? (
                    <Loader2 className="w-5 h-5 animate-spin flex-shrink-0" />
                  ) : (
                    <Building2 className="w-5 h-5 flex-shrink-0" />
                  )}
                  Become a Host
                </button>
              )}
            </div>
          )}

          {/* Mobile bottom – auth actions */}
          <div className="border-t border-primary-700 py-4 px-4 space-y-2 mt-auto">
            {isAuthenticated ? (
              <button
                onClick={handleLogout}
                className="flex items-center gap-3 w-full text-left text-red-400 hover:bg-red-500/10 px-4 py-3 rounded-xl transition"
              >
                <LogOut className="w-5 h-5" />
                Sign out
              </button>
            ) : (
              <>
                <Link href="/login" onClick={closeMobile} className="block">
                  <Button variant="secondary" fullWidth>
                    Login
                  </Button>
                </Link>
                <Link href="/register" onClick={closeMobile} className="block">
                  <Button fullWidth>Sign Up</Button>
                </Link>
              </>
            )}
          </div>
        </div>
      </nav>
    </>
  );
}

// Placeholders to prevent build errors for legacy imports
export function BottomNav() { return null; }
export function Header() { return null; }
export function Breadcrumbs() { return null; }
