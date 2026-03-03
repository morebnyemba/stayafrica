/**
 * Social Authentication Service for React Native / Expo
 *
 * Uses expo-auth-session for OAuth flows.
 * Fetches client IDs from the backend (DB-stored via Django Admin).
 */
import * as AuthSession from 'expo-auth-session';
import * as WebBrowser from 'expo-web-browser';
import * as AppleAuthentication from 'expo-apple-authentication';
import { apiClient } from './api-client';
import { Platform } from 'react-native';

// Ensure web browser auth session completes properly
WebBrowser.maybeCompleteAuthSession();

// ── Types ───────────────────────────────────────────────────────────

export interface ProviderConfig {
  client_id: string;
  name: string;
}

export interface SocialAuthResult {
  access: string;
  refresh: string;
  user: any;
}

// ── Discovery documents ─────────────────────────────────────────────

const GOOGLE_DISCOVERY: AuthSession.DiscoveryDocument = {
  authorizationEndpoint: 'https://accounts.google.com/o/oauth2/v2/auth',
  tokenEndpoint: 'https://oauth2.googleapis.com/token',
  revocationEndpoint: 'https://oauth2.googleapis.com/revoke',
};

const FACEBOOK_DISCOVERY: AuthSession.DiscoveryDocument = {
  authorizationEndpoint: 'https://www.facebook.com/v21.0/dialog/oauth',
  tokenEndpoint: 'https://graph.facebook.com/v21.0/oauth/access_token',
};

// ── Redirect URI ────────────────────────────────────────────────────

/**
 * Build the redirect URI for the current platform.
 * Uses the `stayafrica` scheme defined in app.json.
 */
function getRedirectUri(): string {
  return AuthSession.makeRedirectUri({
    scheme: 'stayafrica',
    path: 'auth/callback',
  });
}

// ── Service ─────────────────────────────────────────────────────────

class SocialAuthService {
  private providers: Record<string, ProviderConfig> = {};
  private configLoaded = false;

  /**
   * Fetch provider client IDs from the backend.
   * Called once at app launch or before the first social login.
   */
  async loadConfig(): Promise<Record<string, ProviderConfig>> {
    if (this.configLoaded) return this.providers;

    const maxRetries = 3;
    for (let attempt = 0; attempt < maxRetries; attempt++) {
      try {
        const response = await apiClient.client.get('/auth/social/config/');
        this.providers = response.data?.providers || {};
        this.configLoaded = true;
        return this.providers;
      } catch (e) {
        if (attempt < maxRetries - 1) {
          await new Promise(r => setTimeout(r, 1000 * Math.pow(2, attempt)));
        } else {
          console.warn('Failed to load social auth config after retries:', e);
        }
      }
    }
    return this.providers;
  }

  getProviders(): Record<string, ProviderConfig> {
    return this.providers;
  }

  isProviderConfigured(provider: string): boolean {
    return !!this.providers[provider]?.client_id;
  }

  // ── Google ──────────────────────────────────────────────────────

  async loginWithGoogle(role?: 'guest' | 'host'): Promise<SocialAuthResult> {
    await this.loadConfig();
    const clientId = this.providers.google?.client_id;
    if (!clientId) throw new Error('Google login is not configured');

    const redirectUri = getRedirectUri();

    const request = new AuthSession.AuthRequest({
      clientId,
      scopes: ['openid', 'profile', 'email'],
      redirectUri,
      responseType: AuthSession.ResponseType.Code,
      usePKCE: true,
    });

    const result = await request.promptAsync(GOOGLE_DISCOVERY);

    if (result.type === 'cancel' || result.type === 'dismiss') {
      throw new Error('Google sign-in was cancelled');
    }

    if (result.type !== 'success' || !result.params?.code) {
      throw new Error((result as any).params?.error_description || 'Google sign-in failed');
    }

    return this.exchangeCode('google', result.params.code, redirectUri, role);
  }

  // ── Facebook ────────────────────────────────────────────────────

  async loginWithFacebook(role?: 'guest' | 'host'): Promise<SocialAuthResult> {
    await this.loadConfig();
    const clientId = this.providers.facebook?.client_id;
    if (!clientId) throw new Error('Facebook login is not configured');

    const redirectUri = getRedirectUri();

    const request = new AuthSession.AuthRequest({
      clientId,
      scopes: ['email', 'public_profile'],
      redirectUri,
      responseType: AuthSession.ResponseType.Code,
    });

    const result = await request.promptAsync(FACEBOOK_DISCOVERY);

    if (result.type === 'cancel' || result.type === 'dismiss') {
      throw new Error('Facebook sign-in was cancelled');
    }

    if (result.type !== 'success' || !result.params?.code) {
      throw new Error((result as any).params?.error_description || 'Facebook sign-in failed');
    }

    return this.exchangeCode('facebook', result.params.code, redirectUri, role);
  }

  // ── Apple ───────────────────────────────────────────────────────

  async loginWithApple(role?: 'guest' | 'host'): Promise<SocialAuthResult> {
    await this.loadConfig();
    const clientId = this.providers.apple?.client_id;
    if (!clientId) throw new Error('Apple login is not configured');

    if (Platform.OS === 'ios') {
      // Native Apple Sign-In (required by App Store guidelines on iOS)
      const isAvailable = await AppleAuthentication.isAvailableAsync();
      if (!isAvailable) {
        throw new Error('Apple Sign In is not available on this device');
      }

      const credential = await AppleAuthentication.signInAsync({
        requestedScopes: [
          AppleAuthentication.AppleAuthenticationScope.EMAIL,
          AppleAuthentication.AppleAuthenticationScope.FULL_NAME,
        ],
      });

      if (!credential.authorizationCode) {
        throw new Error('Apple Sign In did not return an authorization code');
      }

      // Exchange the native authorization code via our backend
      return this.exchangeCode('apple', credential.authorizationCode, 'native-ios', role);
    }

    // Non-iOS: web-based Apple OAuth flow
    const redirectUri = getRedirectUri();

    const discovery: AuthSession.DiscoveryDocument = {
      authorizationEndpoint: 'https://appleid.apple.com/auth/authorize',
      tokenEndpoint: 'https://appleid.apple.com/auth/token',
    };

    const request = new AuthSession.AuthRequest({
      clientId,
      scopes: ['email', 'name'],
      redirectUri,
      responseType: AuthSession.ResponseType.Code,
    });

    const result = await request.promptAsync(discovery);

    if (result.type === 'cancel' || result.type === 'dismiss') {
      throw new Error('Apple sign-in was cancelled');
    }

    if (result.type !== 'success' || !result.params?.code) {
      throw new Error((result as any).params?.error_description || 'Apple sign-in failed');
    }

    return this.exchangeCode('apple', result.params.code, redirectUri, role);
  }

  // ── Code exchange via backend ───────────────────────────────────

  private async exchangeCode(
    provider: string,
    code: string,
    redirectUri: string,
    role?: 'guest' | 'host',
  ): Promise<SocialAuthResult> {
    const response = await apiClient.client.post(
      `/auth/social/${provider}/login/`,
      { code, redirect_uri: redirectUri, ...(role && { role }) },
    );

    const { access, refresh, user } = response.data;

    if (access && refresh) {
      await apiClient.saveTokens(access, refresh);
    }

    return { access, refresh, user };
  }
}

export const socialAuthService = new SocialAuthService();
