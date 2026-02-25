import { create } from 'zustand';
import { persist } from 'zustand/middleware';
import { User } from '@/types';

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
        if (token) {
          // Set cookie for middleware
          const isSecure = window.location.protocol === 'https:';
          document.cookie = `access_token=${token}; path=/; max-age=86400; SameSite=Lax${isSecure ? '; Secure' : ''}`;
          await get().fetchUserProfile(token);
        } else {
          set({ isLoading: false });
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

        localStorage.removeItem('access_token');
        localStorage.removeItem('refresh_token');
        document.cookie = 'access_token=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT';
        set({ user: null, isAuthenticated: false });
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

  // Initialize auth on first mount
  if (typeof window !== 'undefined' && store.isLoading && !store.user) {
    store.initializeAuth();
  }

  // If user is already persisted, stop loading immediately
  if (typeof window !== 'undefined' && store.isLoading && store.user) {
    store.setLoading(false);
  }

  return store;
};
