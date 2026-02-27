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

export function middleware(request: NextRequest) {
  const { pathname } = request.nextUrl;
  
  // Get the token from cookies or headers
  const token = request.cookies.get('access_token')?.value;
  const isAuthenticated = token ? isTokenValid(token) : false;
  const hasAdminAccess = token ? isUserAdmin(token) : false;

  // Clear expired cookie so client doesn't keep sending it
  const response = NextResponse.next();
  if (token && !isAuthenticated) {
    response.cookies.delete('access_token');
  }

  // Check if the current route is protected or admin
  const isProtectedRoute = protectedRoutes.some(route => pathname.startsWith(route));
  const isAdminRoute = adminRoutes.some(route => pathname.startsWith(route));
  const isAuthRoute = authRoutes.some(route => pathname.startsWith(route));

  // Redirect to login if accessing admin route without admin role
  if (isAdminRoute) {
    if (!isAuthenticated) {
      const url = new URL('/login', request.url);
      const fullPath = request.nextUrl.search ? `${pathname}${request.nextUrl.search}` : pathname;
      url.searchParams.set('redirect', fullPath);
      url.searchParams.set('error', 'admin_access_required');
      return NextResponse.redirect(url);
    }
    if (!hasAdminAccess) {
      return NextResponse.redirect(new URL('/dashboard?error=unauthorized', request.url));
    }
  }

  // Redirect to login if accessing protected route without authentication
  if (isProtectedRoute && !isAuthenticated) {
    const url = new URL('/login', request.url);
    // Preserve the full path + query string so user returns here after login
    const fullPath = request.nextUrl.search ? `${pathname}${request.nextUrl.search}` : pathname;
    url.searchParams.set('redirect', fullPath);
    return NextResponse.redirect(url);
  }

  // Redirect to dashboard if accessing auth routes while authenticated
  if (isAuthRoute && isAuthenticated) {
    // If user was being redirected, honour that destination
    const redirect = request.nextUrl.searchParams.get('redirect');
    if (redirect && redirect.startsWith('/')) {
      return NextResponse.redirect(new URL(redirect, request.url));
    }
    return NextResponse.redirect(new URL('/dashboard', request.url));
  }

  return response;
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
