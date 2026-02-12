import React, { createContext, useContext, useState, useEffect } from 'react';
import { apiClient } from '@/services/api-client';
import { socialAuthService } from '@/services/social-auth';
import AsyncStorage from '@react-native-async-storage/async-storage';
import { logError, logInfo } from '@/utils/logger';

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

interface TwoFactorPending {
  email: string;
  password: string;
}

interface AuthContextType {
  user: User | null;
  isAuthenticated: boolean;
  isLoading: boolean;
  twoFactorPending: TwoFactorPending | null;
  login: (email: string, password: string) => Promise<void>;
  loginWith2FA: (token: string) => Promise<void>;
  loginWithBackupCode: (backupCode: string) => Promise<void>;
  loginWithGoogle: () => Promise<void>;
  loginWithFacebook: () => Promise<void>;
  loginWithApple: () => Promise<void>;
  clearTwoFactorPending: () => void;
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
  const [twoFactorPending, setTwoFactorPending] = useState<TwoFactorPending | null>(null);

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
      logError('Bootstrap/session restore error', error);
      await apiClient.clearTokens();
      setUser(null);
      setIsAuthenticated(false);
    } finally {
      setIsLoading(false);
    }
  };

  const login = async (email: string, password: string) => {
    try {
      const result = await apiClient.login(email, password);

      // Check if 2FA is required
      if (result.two_factor_required) {
        setTwoFactorPending({ email, password });
        const err = new Error('2FA_REQUIRED');
        (err as any).twoFactorRequired = true;
        throw err;
      }

      const profile = await apiClient.getUserProfile();
      setUser(profile);
      setIsAuthenticated(true);
      setTwoFactorPending(null);
      logInfo('User logged in successfully', { userId: profile.id });
    } catch (error) {
      logError('Login failed', error, { email });
      throw error;
    }
  };

  const loginWith2FA = async (token: string) => {
    if (!twoFactorPending) throw new Error('No pending 2FA session');
    try {
      await apiClient.loginWith2FA(twoFactorPending.email, twoFactorPending.password, token);
      const profile = await apiClient.getUserProfile();
      setUser(profile);
      setIsAuthenticated(true);
      setTwoFactorPending(null);
      logInfo('User logged in with 2FA', { userId: profile.id });
    } catch (error) {
      logError('2FA verification failed', error);
      throw error;
    }
  };

  const loginWithBackupCode = async (backupCode: string) => {
    if (!twoFactorPending) throw new Error('No pending 2FA session');
    try {
      await apiClient.loginWithBackupCode(twoFactorPending.email, twoFactorPending.password, backupCode);
      const profile = await apiClient.getUserProfile();
      setUser(profile);
      setIsAuthenticated(true);
      setTwoFactorPending(null);
      logInfo('User logged in with backup code', { userId: profile.id });
    } catch (error) {
      logError('Backup code verification failed', error);
      throw error;
    }
  };

  const clearTwoFactorPending = () => {
    setTwoFactorPending(null);
  };

  const loginWithGoogle = async () => {
    try {
      const result = await socialAuthService.loginWithGoogle();
      setUser(result.user);
      setIsAuthenticated(true);
      logInfo('User logged in with Google', { userId: result.user?.id });
    } catch (error) {
      logError('Google login failed', error);
      throw error;
    }
  };

  const loginWithFacebook = async () => {
    try {
      const result = await socialAuthService.loginWithFacebook();
      setUser(result.user);
      setIsAuthenticated(true);
      logInfo('User logged in with Facebook', { userId: result.user?.id });
    } catch (error) {
      logError('Facebook login failed', error);
      throw error;
    }
  };

  const loginWithApple = async () => {
    try {
      const result = await socialAuthService.loginWithApple();
      setUser(result.user);
      setIsAuthenticated(true);
      logInfo('User logged in with Apple', { userId: result.user?.id });
    } catch (error) {
      logError('Apple login failed', error);
      throw error;
    }
  };

  const register = async (userData: any) => {
    try {
      await apiClient.register(userData);
      const profile = await apiClient.getUserProfile();
      setUser(profile);
      setIsAuthenticated(true);
      logInfo('User registered successfully', { userId: profile.id });
    } catch (error) {
      logError('Registration failed', error, { email: userData.email });
      throw error;
    }
  };

  const logout = async () => {
    try {
      await apiClient.clearTokens();
      setUser(null);
      setIsAuthenticated(false);
      logInfo('User logged out successfully');
    } catch (error) {
      logError('Logout error', error);
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
        twoFactorPending,
        login,
        loginWith2FA,
        loginWithBackupCode,
        loginWithGoogle,
        loginWithFacebook,
        loginWithApple,
        clearTwoFactorPending,
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
