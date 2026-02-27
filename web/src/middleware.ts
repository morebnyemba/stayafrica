import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';

// Define which routes require authentication
const protectedRoutes = [
  '/dashboard',
  '/booking',
  '/bookings',
  '/profile',
  '/messages',
  '/host',
  '/wishlist',
];

// Define admin routes (require admin role)
const adminRoutes = ['/admin'];

// Define auth routes (redirect to dashboard if already logged in)
const authRoutes = ['/login', '/register'];

// Helper function to decode JWT and check if it's still valid (not expired)
function isTokenValid(token: string): boolean {
  try {
    const payload = JSON.parse(Buffer.from(token.split('.')[1], 'base64').toString());
    // Check expiration — JWT exp is in seconds, Date.now() in milliseconds
    if (payload.exp && Date.now() >= payload.exp * 1000) {
      return false;
    }
    return true;
  } catch {
    return false;
  }
}

// Helper function to decode JWT and check if user is admin
function isUserAdmin(token: string): boolean {
  try {
    // Decode JWT (without verification - just to read the payload)
    const payload = JSON.parse(Buffer.from(token.split('.')[1], 'base64').toString());
    // User is admin if role is 'admin' OR is_staff is true (matches backend logic)
    return payload.role === 'admin' || payload.is_staff === true;
  } catch (error) {
    return false;
  }
}

// Helper: delete expired cookie on any response (including redirects)
function withExpiredCookieCleanup(
  res: NextResponse,
  token: string | undefined,
  isAuthenticated: boolean
): NextResponse {
  if (token && !isAuthenticated) {
    res.cookies.delete('access_token');
  }
  return res;
}

export function middleware(request: NextRequest) {
  const { pathname } = request.nextUrl;
  
  // Get the token from cookies or headers
  const token = request.cookies.get('access_token')?.value;
  const isAuthenticated = token ? isTokenValid(token) : false;
  const hasAdminAccess = token ? isUserAdmin(token) : false;

  // Check if the current route is protected or admin
  const isProtectedRoute = protectedRoutes.some(route => pathname.startsWith(route));
  const isAdminRoute = adminRoutes.some(route => pathname.startsWith(route));
  const isAuthRoute = authRoutes.some(route => pathname.startsWith(route));

  // Redirect to login if accessing admin route without admin role
  if (isAdminRoute) {
    if (!isAuthenticated) {
      const url = new URL('/login', request.url);
      // Only pass the pathname as redirect target (no query params that include
      // "redirect" — that pattern causes infinite nested redirect loops).
      url.searchParams.set('redirect', pathname);
      url.searchParams.set('error', 'admin_access_required');
      return withExpiredCookieCleanup(NextResponse.redirect(url), token, isAuthenticated);
    }
    if (!hasAdminAccess) {
      return NextResponse.redirect(new URL('/dashboard?error=unauthorized', request.url));
    }
  }

  // Redirect to login if accessing protected route without authentication
  if (isProtectedRoute && !isAuthenticated) {
    const url = new URL('/login', request.url);
    // Only preserve pathname — strip any query string that could carry nested
    // "redirect" params and cause infinite loops.
    url.searchParams.set('redirect', pathname);
    return withExpiredCookieCleanup(NextResponse.redirect(url), token, isAuthenticated);
  }

  // Redirect to dashboard if accessing auth routes while authenticated
  if (isAuthRoute && isAuthenticated) {
    // If user was being redirected, honour that destination
    const redirect = request.nextUrl.searchParams.get('redirect');
    if (redirect && redirect.startsWith('/')) {
      // Sanitize: parse the redirect and strip any nested "redirect" params
      // to break potential loops
      const target = new URL(redirect, request.url);
      target.searchParams.delete('redirect');
      return NextResponse.redirect(target);
    }
    return NextResponse.redirect(new URL('/dashboard', request.url));
  }

  // Normal response — also clear expired cookies
  const response = NextResponse.next();
  return withExpiredCookieCleanup(response, token, isAuthenticated);
}

export const config = {
  matcher: [
    /*
     * Match all request paths except for the ones starting with:
     * - api (API routes)
     * - _next/static (static files)
     * - _next/image (image optimization files)
     * - favicon.ico (favicon file)
     * - public (public files)
     */
    '/((?!api|_next/static|_next/image|favicon.ico|public).*)',
  ],
};
