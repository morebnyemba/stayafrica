import { create } from 'zustand';
import { persist } from 'zustand/middleware';
import { User } from '@/types';

// ── JWT helpers ───────────────────────────────────────────────────────────────

/** Decode a JWT payload without verification (browser-side only). */
function decodeJwtPayload(token: string): Record<string, unknown> | null {
  try {
    const base64 = token.split('.')[1];
    return JSON.parse(atob(base64));
  } catch {
    return null;
  }
}

/** Returns the number of milliseconds until the token expires, or 0 if already expired. */
function msUntilExpiry(token: string): number {
  const payload = decodeJwtPayload(token);
  if (!payload || typeof payload.exp !== 'number') return 0;
  return Math.max(0, payload.exp * 1000 - Date.now());
}

/** True when the token exists AND is not yet expired. */
function isTokenAlive(token: string | null): boolean {
  if (!token) return false;
  return msUntilExpiry(token) > 0;
}

// ── Auto-logout timer ─────────────────────────────────────────────────────────
let _expiryTimer: ReturnType<typeof setTimeout> | null = null;

function clearExpiryTimer() {
  if (_expiryTimer) {
    clearTimeout(_expiryTimer);
    _expiryTimer = null;
  }
}

/** Schedule an auto-logout just after the access token expires. */
function scheduleAutoLogout(token: string) {
  clearExpiryTimer();
  const ms = msUntilExpiry(token);
  if (ms <= 0) return; // already expired – caller should log out immediately
  // Add a 2-second buffer so the token is definitely expired when we act
  _expiryTimer = setTimeout(() => {
    // Trigger the store's logout action
    useAuthStore.getState().logout();
    // Redirect to login
    if (typeof window !== 'undefined') {
      window.location.replace('/login?expired=1');
    }
  }, ms + 2000);
}

interface TwoFactorRequired {
  two_factor_required: true;
  email: string;
  password: string;
}

interface AuthState {
  user: User | null;
  isLoading: boolean;
  isAuthenticated: boolean;
  twoFactorPending: TwoFactorRequired | null;
  setUser: (user: User | null) => void;
  setLoading: (loading: boolean) => void;
  login: (email: string, password: string) => Promise<void>;
  loginWith2FA: (email: string, password: string, token: string) => Promise<void>;
  loginWithBackupCode: (email: string, password: string, backupCode: string) => Promise<void>;
  clearTwoFactorPending: () => void;
  register: (userData: any) => Promise<void>;
  logout: () => void;
  updateProfile: (userData: Partial<User>) => Promise<void>;
  upgradeToHost: () => Promise<User>;
  switchProfile: (mode: 'guest' | 'host') => Promise<void>;
  fetchUserProfile: (token: string) => Promise<void>;
  initializeAuth: () => Promise<void>;
}

const API_BASE = process.env.NEXT_PUBLIC_API_BASE_URL
  ? `${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1`
  : (typeof window !== 'undefined' && window.location.origin.includes('localhost')
    ? 'http://localhost:8000/api/v1'
    : 'https://api.zimlegend.online/api/v1');

const setSession = async (access: string, refresh: string) => {
  if (typeof window === 'undefined') return;

  localStorage.setItem('access_token', access);
  localStorage.setItem('refresh_token', refresh);

  // Schedule auto-logout when this token expires
  scheduleAutoLogout(access);

  // Set cookie server-side so middleware/SSR see it immediately
  try {
    await fetch('/api/auth/set-cookie', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ token: access, maxAge: 60 * 60 * 24 }),
    });
  } catch (e) {
    // Fallback: set client cookie
    const isSecure = window.location.protocol === 'https:';
    document.cookie = `access_token=${access}; path=/; max-age=86400; SameSite=Lax${isSecure ? '; Secure' : ''}`;
  }
};

export const useAuthStore = create<AuthState>()(
  persist(
    (set, get) => ({
      user: null,
      isLoading: true,
      isAuthenticated: false,
      twoFactorPending: null,

      setUser: (user) => set({ user, isAuthenticated: !!user }),

      setLoading: (isLoading) => set({ isLoading }),

      clearTwoFactorPending: () => set({ twoFactorPending: null }),

      fetchUserProfile: async (token: string) => {
        try {
          const response = await fetch(`${API_BASE}/users/profile/`, {
            headers: { Authorization: `Bearer ${token}` },
          });

          if (response.ok) {
            const userData = await response.json();
            set({ user: userData, isAuthenticated: true, isLoading: false });
          } else {
            localStorage.removeItem('access_token');
            localStorage.removeItem('refresh_token');
            set({ user: null, isAuthenticated: false, isLoading: false });
          }
        } catch (error) {
          console.error('Failed to fetch user profile:', error);
          localStorage.removeItem('access_token');
          set({ user: null, isAuthenticated: false, isLoading: false });
        }
      },

      initializeAuth: async () => {
        if (typeof window === 'undefined') return;

        const token = localStorage.getItem('access_token');
        if (token && isTokenAlive(token)) {
          // Set cookie for middleware
          const isSecure = window.location.protocol === 'https:';
          document.cookie = `access_token=${token}; path=/; max-age=86400; SameSite=Lax${isSecure ? '; Secure' : ''}`;
          // Schedule auto-logout when the token expires
          scheduleAutoLogout(token);
          await get().fetchUserProfile(token);
        } else {
          // Token missing or expired – clean up
          if (token) {
            // Token existed but was expired
            localStorage.removeItem('access_token');
            localStorage.removeItem('refresh_token');
            fetch('/api/auth/clear-cookie', { method: 'POST' }).catch(() => {});
            document.cookie = 'access_token=; path=/; max-age=0';
            try { localStorage.removeItem('auth-storage'); } catch {}
          }
          set({ user: null, isAuthenticated: false, isLoading: false });
        }
      },

      login: async (email: string, password: string) => {
        try {
          const response = await fetch(`${API_BASE}/auth/login/`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ email, password }),
          });

          const data = await response.json();

          if (response.ok) {
            // Check if 2FA is required
            if (data.two_factor_required) {
              set({ twoFactorPending: { two_factor_required: true, email, password } });
              const err = new Error('2FA_REQUIRED');
              (err as any).twoFactorRequired = true;
              throw err;
            }

            const { access, refresh, user: userData } = data;
            await setSession(access, refresh);
            set({ user: userData, isAuthenticated: true, twoFactorPending: null });
          } else {
            throw new Error(data.detail || 'Login failed');
          }
        } catch (error) {
          throw error;
        }
      },

      loginWith2FA: async (email: string, password: string, token: string) => {
        try {
          const response = await fetch(`${API_BASE}/auth/login/2fa/`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ email, password, token }),
          });

          if (response.ok) {
            const { access, refresh, user: userData } = await response.json();
            await setSession(access, refresh);
            set({ user: userData, isAuthenticated: true, twoFactorPending: null });
          } else {
            const error = await response.json();
            throw new Error(error.detail || '2FA verification failed');
          }
        } catch (error) {
          throw error;
        }
      },

      loginWithBackupCode: async (email: string, password: string, backupCode: string) => {
        try {
          const response = await fetch(`${API_BASE}/auth/login/2fa/`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ email, password, backup_code: backupCode }),
          });

          if (response.ok) {
            const { access, refresh, user: userData } = await response.json();
            await setSession(access, refresh);
            set({ user: userData, isAuthenticated: true, twoFactorPending: null });
          } else {
            const error = await response.json();
            throw new Error(error.detail || 'Backup code verification failed');
          }
        } catch (error) {
          throw error;
        }
      },

      register: async (userData: any) => {
        try {
          const response = await fetch(`${API_BASE}/users/register/`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(userData),
          });

          if (response.ok) {
            const { access, refresh, user: newUser } = await response.json();
            await setSession(access, refresh);
            set({ user: newUser, isAuthenticated: true });
          } else {
            const error = await response.json();
            throw new Error(error.detail || 'Registration failed');
          }
        } catch (error) {
          throw error;
        }
      },

      logout: () => {
        if (typeof window === 'undefined') return;

        clearExpiryTimer();
        localStorage.removeItem('access_token');
        localStorage.removeItem('refresh_token');

        // Clear via server-side route (removes the cookie the same way it was set)
        fetch('/api/auth/clear-cookie', { method: 'POST' }).catch(() => {});
        // Also clear client-side as a fallback
        document.cookie = 'access_token=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT';
        document.cookie = 'access_token=; path=/; max-age=0';

        // Clear persisted Zustand state so rehydration doesn't restore the user
        try { localStorage.removeItem('auth-storage'); } catch {}

        set({ user: null, isAuthenticated: false, isLoading: false });
      },

      updateProfile: async (userData: Partial<User>) => {
        try {
          const token = localStorage.getItem('access_token');
          const response = await fetch(`${API_BASE}/users/profile/`, {
            method: 'PUT',
            headers: {
              Authorization: `Bearer ${token}`,
              'Content-Type': 'application/json',
            },
            body: JSON.stringify(userData),
          });

          if (response.ok) {
            const updated = await response.json();
            set({ user: updated });
          } else {
            const error = await response.json();
            throw new Error(error.detail || 'Profile update failed');
          }
        } catch (error) {
          throw error;
        }
      },

      upgradeToHost: async () => {
        const token = localStorage.getItem('access_token');
        if (!token) {
          throw new Error('Not authenticated');
        }

        const response = await fetch(`${API_BASE}/users/upgrade_to_host/`, {
          method: 'POST',
          headers: {
            Authorization: `Bearer ${token}`,
            'Content-Type': 'application/json',
          },
        });

        if (!response.ok) {
          const error = await response.json();
          throw new Error(error.detail || 'Upgrade failed');
        }

        const data = await response.json();
        if (data.access && data.refresh) {
          await setSession(data.access, data.refresh);
        }
        const updatedUser = data.user || data;
        set({ user: updatedUser, isAuthenticated: true });
        return updatedUser;
      },

      switchProfile: async (mode: 'guest' | 'host') => {
        const token = localStorage.getItem('access_token');
        if (!token) throw new Error('Not authenticated');

        const response = await fetch(`${API_BASE}/users/switch_profile/`, {
          method: 'POST',
          headers: {
            Authorization: `Bearer ${token}`,
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({ profile: mode }),
        });

        if (!response.ok) {
          const error = await response.json();
          throw new Error(error.error || error.detail || 'Failed to switch profile');
        }

        const data = await response.json();
        if (data.access && data.refresh) {
          await setSession(data.access, data.refresh);
        }
        const updatedUser = data.user || data;
        set({ user: updatedUser, isAuthenticated: true });
      },
    }),
    {
      name: 'auth-storage',
      partialize: (state) => ({ user: state.user }), // Only persist user data
    }
  )
);

// Hook for easy access to auth methods
export const useAuth = () => {
  const store = useAuthStore();

  if (typeof window !== 'undefined' && store.isLoading) {
    const token = localStorage.getItem('access_token');

    if (store.user && isTokenAlive(token)) {
      // Zustand rehydrated a user AND the token is still valid.
      // Mark authenticated immediately so ProtectedRoute doesn't flash-redirect.
      scheduleAutoLogout(token!);
      useAuthStore.setState({ isAuthenticated: true, isLoading: false });
    } else if (store.user && !isTokenAlive(token)) {
      // User was persisted but the token is gone or expired — force logout.
      localStorage.removeItem('access_token');
      localStorage.removeItem('refresh_token');
      fetch('/api/auth/clear-cookie', { method: 'POST' }).catch(() => {});
      document.cookie = 'access_token=; path=/; max-age=0';
      try { localStorage.removeItem('auth-storage'); } catch {}
      useAuthStore.setState({ user: null, isAuthenticated: false, isLoading: false });
    } else {
      // No persisted user — run full initialization (fetch profile etc.)
      store.initializeAuth();
    }
  }

  return store;
};
