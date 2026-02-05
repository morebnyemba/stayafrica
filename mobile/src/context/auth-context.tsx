import React, { createContext, useContext, useState, useEffect } from 'react';
import { apiClient } from '@/services/api-client';
import AsyncStorage from '@react-native-async-storage/async-storage';

interface User {
  id: string;
  email: string;
  first_name: string;
  last_name: string;
  phone_number: string;
  country_of_residence: string;
  role: 'guest' | 'host' | 'admin';
  is_verified: boolean;
}

interface UpdateProfileData {
  first_name?: string;
  last_name?: string;
  phone_number?: string;
  country_of_residence?: string;
}

interface AuthContextType {
  user: User | null;
  isAuthenticated: boolean;
  isLoading: boolean;
  login: (email: string, password: string) => Promise<void>;
  register: (userData: any) => Promise<void>;
  logout: () => Promise<void>;
  refreshUser: () => Promise<void>;
  updateProfile: (data: UpdateProfileData) => Promise<void>;
}

const AuthContext = createContext<AuthContextType | undefined>(undefined);

export function AuthProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<User | null>(null);
  const [isLoading, setIsLoading] = useState(true);
  const [isAuthenticated, setIsAuthenticated] = useState(false);

  // Check if user is authenticated on mount
  useEffect(() => {
    bootstrapAsync();
  }, []);

  const bootstrapAsync = async () => {
    try {
      const hasToken = await apiClient.hasValidToken();
      if (hasToken) {
        try {
          const profile = await apiClient.getUserProfile();
          setUser(profile);
          setIsAuthenticated(true);
        } catch (profileError: any) {
          // If token is invalid (401), clear tokens and continue as guest
          if (profileError?.response?.status === 401) {
            await apiClient.clearTokens();
            setUser(null);
            setIsAuthenticated(false);
          } else {
            throw profileError;
          }
        }
      }
    } catch (error) {
      console.error('Bootstrap error:', error);
      await apiClient.clearTokens();
      setUser(null);
      setIsAuthenticated(false);
    } finally {
      setIsLoading(false);
    }
  };

  const login = async (email: string, password: string) => {
    try {
      await apiClient.login(email, password);
      const profile = await apiClient.getUserProfile();
      setUser(profile);
      setIsAuthenticated(true);
    } catch (error) {
      throw error;
    }
  };

  const register = async (userData: any) => {
    try {
      await apiClient.register(userData);
      const profile = await apiClient.getUserProfile();
      setUser(profile);
      setIsAuthenticated(true);
    } catch (error) {
      throw error;
    }
  };

  const logout = async () => {
    try {
      await apiClient.clearTokens();
      setUser(null);
      setIsAuthenticated(false);
    } catch (error) {
      console.error('Logout error:', error);
    }
  };

  const refreshUser = async () => {
    try {
      const profile = await apiClient.getUserProfile();
      setUser(profile);
    } catch (error) {
      console.error('Refresh user error:', error);
    }
  };

  const updateProfile = async (data: UpdateProfileData) => {
    try {
      const cleanedData = Object.fromEntries(
        Object.entries(data).filter(([, value]) => value !== '' && value !== undefined)
      ) as UpdateProfileData;
      const updatedUser = await apiClient.updateUserProfile(cleanedData);
      setUser(updatedUser);
    } catch (error) {
      console.error('Update profile error:', error);
      throw error;
    }
  };

  return (
    <AuthContext.Provider
      value={{
        user,
        isAuthenticated,
        isLoading,
        login,
        register,
        logout,
        refreshUser,
        updateProfile,
      }}
    >
      {children}
    </AuthContext.Provider>
  );
}

export function useAuth(): AuthContextType {
  const context = useContext(AuthContext);
  if (context === undefined) {
    throw new Error('useAuth must be used within an AuthProvider');
  }
  return context;
}
