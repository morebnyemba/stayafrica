/**
 * Social Authentication Service for React Native / Expo
 *
 * Uses expo-auth-session for OAuth flows.
 * Fetches client IDs from the backend (DB-stored via Django Admin).
 */
import * as AuthSession from 'expo-auth-session';
import * as WebBrowser from 'expo-web-browser';
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
  authorizationEndpoint: 'https://www.facebook.com/v13.0/dialog/oauth',
  tokenEndpoint: 'https://graph.facebook.com/v13.0/oauth/access_token',
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

    try {
      const response = await apiClient.client.get('/auth/social/config/');
      this.providers = response.data?.providers || {};
      this.configLoaded = true;
    } catch (e) {
      console.warn('Failed to load social auth config:', e);
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

  async loginWithGoogle(): Promise<SocialAuthResult> {
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

    // Exchange the code via our backend
    return this.exchangeCode('google', result.params.code, redirectUri);
  }

  // ── Facebook ────────────────────────────────────────────────────

  async loginWithFacebook(): Promise<SocialAuthResult> {
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

    return this.exchangeCode('facebook', result.params.code, redirectUri);
  }

  // ── Apple ───────────────────────────────────────────────────────

  async loginWithApple(): Promise<SocialAuthResult> {
    // Apple Sign In on iOS uses the native module (expo-apple-authentication)
    // For now, fall back to web-based flow on non-iOS
    await this.loadConfig();
    const clientId = this.providers.apple?.client_id;
    if (!clientId) throw new Error('Apple login is not configured');

    if (Platform.OS === 'ios') {
      // On iOS, prefer native Apple Authentication
      // This would require expo-apple-authentication — we do a web fallback for now
      throw new Error('Apple Sign In via native iOS is not yet implemented. Please use email login.');
    }

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

    return this.exchangeCode('apple', result.params.code, redirectUri);
  }

  // ── Code exchange via backend ───────────────────────────────────

  private async exchangeCode(
    provider: string,
    code: string,
    redirectUri: string,
  ): Promise<SocialAuthResult> {
    const response = await apiClient.client.post(
      `/auth/social/${provider}/login/`,
      { code, redirect_uri: redirectUri },
    );

    const { access, refresh, user } = response.data;

    if (access && refresh) {
      await apiClient.saveTokens(access, refresh);
    }

    return { access, refresh, user };
  }
}

export const socialAuthService = new SocialAuthService();
