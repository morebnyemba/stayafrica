'use client';

import React, { createContext, useContext, useEffect, useState } from 'react';
import { User } from '@/types';

interface AuthContextType {
  user: User | null;
  isLoading: boolean;
  isAuthenticated: boolean;
  login: (email: string, password: string) => Promise<void>;
  register: (userData: any) => Promise<void>;
  logout: () => void;
  updateProfile: (userData: Partial<User>) => Promise<void>;
}

const AuthContext = createContext<AuthContextType | undefined>(undefined);

export function AuthProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<User | null>(null);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    // Check for existing session/token on mount
    const token = localStorage.getItem('access_token');
    if (token) {
      // Set cookie for middleware
      const isSecure = window.location.protocol === 'https:';
      document.cookie = `access_token=${token}; path=/; max-age=86400; SameSite=Lax${isSecure ? '; Secure' : ''}`;
      // Verify token and fetch user profile
      fetchUserProfile(token);
    } else {
      setIsLoading(false);
    }
  }, []);

  const fetchUserProfile = async (token: string) => {
    try {
      const response = await fetch(`${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1/users/profile/`, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });

      if (response.ok) {
        const userData = await response.json();
        setUser(userData);
      } else {
        localStorage.removeItem('access_token');
        localStorage.removeItem('refresh_token');
      }
    } catch (error) {
      console.error('Failed to fetch user profile:', error);
      localStorage.removeItem('access_token');
    } finally {
      setIsLoading(false);
    }
  };

  const login = async (email: string, password: string) => {
    try {
      const response = await fetch(`${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1/auth/login/`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ email, password }),
      });

      if (response.ok) {
        const { access, refresh, user: userData } = await response.json();
        localStorage.setItem('access_token', access);
        localStorage.setItem('refresh_token', refresh);
        // Set cookie for middleware
        const isSecure = window.location.protocol === 'https:';
        document.cookie = `access_token=${access}; path=/; max-age=86400; SameSite=Lax${isSecure ? '; Secure' : ''}`;
        setUser(userData);
      } else {
        throw new Error('Login failed');
      }
    } catch (error) {
      throw error;
    }
  };

  const register = async (userData: any) => {
    try {
      const response = await fetch(`${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1/users/register/`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(userData),
      });

      if (response.ok) {
        const { access, refresh, user: newUser } = await response.json();
        localStorage.setItem('access_token', access);
        localStorage.setItem('refresh_token', refresh);
        // Set cookie for middleware
        const isSecure = window.location.protocol === 'https:';
        document.cookie = `access_token=${access}; path=/; max-age=86400; SameSite=Lax${isSecure ? '; Secure' : ''}`;
        setUser(newUser);
      } else {
        throw new Error('Registration failed');
      }
    } catch (error) {
      throw error;
    }
  };

  const logout = () => {
    localStorage.removeItem('access_token');
    localStorage.removeItem('refresh_token');
    // Clear cookie
    document.cookie = 'access_token=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT';
    setUser(null);
  };

  const updateProfile = async (userData: Partial<User>) => {
    try {
      const token = localStorage.getItem('access_token');
      const response = await fetch(`${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1/users/profile/`, {
        method: 'PUT',
        headers: {
          Authorization: `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(userData),
      });

      if (response.ok) {
        const updated = await response.json();
        setUser(updated);
      } else {
        throw new Error('Profile update failed');
      }
    } catch (error) {
      throw error;
    }
  };

  return (
    <AuthContext.Provider
      value={{
        user,
        isLoading,
        isAuthenticated: !!user,
        login,
        register,
        logout,
        updateProfile,
      }}
    >
      {children}
    </AuthContext.Provider>
  );
}

export function useAuth() {
  const context = useContext(AuthContext);
  if (context === undefined) {
    throw new Error('useAuth must be used within an AuthProvider');
  }
  return context;
}
