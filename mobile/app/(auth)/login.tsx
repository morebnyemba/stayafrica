import { useState, useEffect } from "react";
import {
  Text,
  View,
  ScrollView,
  TouchableOpacity,
  Image,
  KeyboardAvoidingView,
  Platform,
} from "react-native";
import { LinearGradient } from "expo-linear-gradient";
import { useSafeAreaInsets } from "react-native-safe-area-context";
import { Ionicons } from "@expo/vector-icons";
import { useAuth } from "@/context/auth-context";
import { useRouter } from "expo-router";
import { Input } from "@/components/common/Input";
import { Button } from "@/components/common/Button";
import { Divider } from "@/components/common/Divider";
import AsyncStorage from '@react-native-async-storage/async-storage';

export default function LoginScreen() {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [loginSuccess, setLoginSuccess] = useState(false);
  const [emailError, setEmailError] = useState<string>("");
  const [passwordError, setPasswordError] = useState<string>("");
  const [show2FA, setShow2FA] = useState(false);
  const [twoFactorCode, setTwoFactorCode] = useState("");
  const [useBackupCode, setUseBackupCode] = useState(false);
  const [backupCode, setBackupCode] = useState("");
  const { login, loginWith2FA, loginWithBackupCode, loginWithGoogle, loginWithFacebook, loginWithApple, clearTwoFactorPending, user } = useAuth();
  const router = useRouter();
  const insets = useSafeAreaInsets();

  // Watch for user changes after successful login and redirect
  useEffect(() => {
    if (loginSuccess && user) {
      setLoading(false);
      // Check for a saved redirect destination (e.g. booking page)
      AsyncStorage.getItem('auth_redirect').then((saved) => {
        if (saved) {
          AsyncStorage.removeItem('auth_redirect');
          try {
            const dest = JSON.parse(saved);
            if (dest?.pathname) {
              router.replace({ pathname: dest.pathname, params: dest.params });
              return;
            }
          } catch {}
        }
        // Default: redirect based on user role
        if (user.role === 'host') {
          router.replace('/(tabs)/host');
        } else {
          router.replace('/(tabs)/dashboard');
        }
      });
    }
  }, [loginSuccess, user, router]);

  const handleLogin = async () => {
    // Reset errors
    setEmailError("");
    setPasswordError("");
    setError(null);

    // Validate email
    if (!email.trim()) {
      setEmailError("Email is required");
      return;
    }
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email.trim())) {
      setEmailError("Please enter a valid email address");
      return;
    }

    // Validate password
    if (!password) {
      setPasswordError("Password is required");
      return;
    }
    if (password.length < 6) {
      setPasswordError("Password must be at least 6 characters");
      return;
    }

    try {
      setLoading(true);
      setLoginSuccess(false);
      await login(email.trim().toLowerCase(), password);
      setLoginSuccess(true);
    } catch (err: any) {
      if (err?.twoFactorRequired || err?.message === '2FA_REQUIRED') {
        setShow2FA(true);
        setError(null);
      } else {
        console.error("Login failed:", err);
        setError(err?.response?.data?.detail || "Invalid email or password. Please try again.");
        setLoginSuccess(false);
      }
      setLoading(false);
    }
  };

  const handle2FAVerify = async () => {
    try {
      setLoading(true);
      setError(null);
      if (useBackupCode) {
        await loginWithBackupCode(backupCode.trim());
      } else {
        await loginWith2FA(twoFactorCode.trim());
      }
      setLoginSuccess(true);
    } catch (err: any) {
      console.error("2FA verification failed:", err);
      setError(err?.response?.data?.detail || "Invalid verification code. Please try again.");
      setLoading(false);
    }
  };

  const handleCancel2FA = () => {
    setShow2FA(false);
    setTwoFactorCode("");
    setBackupCode("");
    setUseBackupCode(false);
    setError(null);
    clearTwoFactorPending();
  };

  const handleSocialLogin = async (provider: 'Google' | 'Apple' | 'Facebook') => {
    try {
      setLoading(true);
      setError(null);
      if (provider === 'Google') {
        await loginWithGoogle();
      } else if (provider === 'Facebook') {
        await loginWithFacebook();
      } else if (provider === 'Apple') {
        await loginWithApple();
      }
      setLoginSuccess(true);
    } catch (err: any) {
      if (err?.message?.includes('cancelled') || err?.message?.includes('dismiss')) {
        // User cancelled — don't show error
      } else {
        setError(err?.message || `${provider} sign-in failed. Please try again.`);
      }
    } finally {
      setLoading(false);
    }
  };

  // 2FA Verification Screen
  if (show2FA) {
    return (
      <View className="flex-1 bg-sand-100">
        <LinearGradient
          colors={["#122F26", "#1d4a3d", "#F4F1EA"]}
          locations={[0, 0.3, 0.6]}
          start={{ x: 0, y: 0 }}
          end={{ x: 0, y: 1 }}
          style={{ position: "absolute", top: 0, left: 0, right: 0, bottom: 0 }}
        />
        <KeyboardAvoidingView
          behavior={Platform.OS === "ios" ? "padding" : "height"}
          className="flex-1"
        >
          <ScrollView
            className="flex-1"
            contentContainerStyle={{
              flexGrow: 1,
              paddingTop: insets.top + 20,
              paddingBottom: insets.bottom + 20,
            }}
            showsVerticalScrollIndicator={false}
            keyboardShouldPersistTaps="handled"
          >
            <View className="flex-1 justify-center px-6">
              <View className="items-center mb-10">
                <View className="mb-6">
                  <View className="bg-white/95 rounded-3xl p-5 shadow-2xl">
                    <Ionicons name="shield-checkmark" size={60} color="#122F26" />
                  </View>
                </View>
                <Text className="text-4xl font-black text-white mb-2 text-center tracking-tight">
                  Two-Factor Auth
                </Text>
                <Text className="text-lg text-white/80 text-center">
                  Enter the code from your authenticator app
                </Text>
              </View>

              <View
                className="rounded-3xl overflow-hidden bg-white mb-6"
                style={{
                  shadowColor: '#000',
                  shadowOffset: { width: 0, height: 10 },
                  shadowOpacity: 0.15,
                  shadowRadius: 20,
                  elevation: 10,
                }}
              >
                <View className="p-6">
                  {error && (
                    <View className="mb-4 p-4 bg-red-50 rounded-xl flex-row items-start border-2 border-red-200">
                      <Ionicons name="alert-circle" size={20} color="#ef4444" style={{ marginTop: 2 }} />
                      <Text className="text-red-600 ml-2 flex-1 text-sm leading-5">{error}</Text>
                      <TouchableOpacity onPress={() => setError(null)}>
                        <Ionicons name="close" size={20} color="#ef4444" />
                      </TouchableOpacity>
                    </View>
                  )}

                  {!useBackupCode ? (
                    <Input
                      label="Verification Code"
                      icon="keypad-outline"
                      placeholder="Enter 6-digit code"
                      value={twoFactorCode}
                      onChangeText={setTwoFactorCode}
                      keyboardType="number-pad"
                      maxLength={6}
                      editable={!loading}
                    />
                  ) : (
                    <Input
                      label="Backup Code"
                      icon="key-outline"
                      placeholder="Enter backup code"
                      value={backupCode}
                      onChangeText={setBackupCode}
                      autoCapitalize="none"
                      editable={!loading}
                    />
                  )}

                  <Button
                    title="Verify"
                    onPress={handle2FAVerify}
                    loading={loading}
                    disabled={loading || (!useBackupCode && twoFactorCode.length !== 6) || (useBackupCode && !backupCode.trim())}
                    icon="checkmark-circle"
                    iconPosition="right"
                    size="lg"
                    fullWidth
                  />

                  <TouchableOpacity
                    onPress={() => {
                      setUseBackupCode(!useBackupCode);
                      setError(null);
                    }}
                    className="mt-4 items-center"
                  >
                    <Text className="text-sm font-bold text-gold">
                      {useBackupCode ? 'Use authenticator code instead' : 'Use a backup code instead'}
                    </Text>
                  </TouchableOpacity>

                  <TouchableOpacity
                    onPress={handleCancel2FA}
                    className="mt-3 items-center"
                  >
                    <Text className="text-sm text-forest/70">
                      Cancel and go back
                    </Text>
                  </TouchableOpacity>
                </View>
              </View>
            </View>
          </ScrollView>
        </KeyboardAvoidingView>
      </View>
    );
  }

  return (
    <View className="flex-1 bg-sand-100">
      {/* Background gradient */}
      <LinearGradient
        colors={["#122F26", "#1d4a3d", "#F4F1EA"]}
        locations={[0, 0.3, 0.6]}
        start={{ x: 0, y: 0 }}
        end={{ x: 0, y: 1 }}
        style={{ position: "absolute", top: 0, left: 0, right: 0, bottom: 0 }}
      />

      <KeyboardAvoidingView
        behavior={Platform.OS === "ios" ? "padding" : "height"}
        className="flex-1"
      >
        <ScrollView
          className="flex-1"
          contentContainerStyle={{ 
            flexGrow: 1,
            paddingTop: insets.top + 20,
            paddingBottom: insets.bottom + 20,
          }}
          showsVerticalScrollIndicator={false}
          keyboardShouldPersistTaps="handled"
        >
          <View className="flex-1 justify-center px-6">
            {/* Header Section */}
            <View className="items-center mb-10">
              {/* Logo */}
              <View className="mb-6">
                <View className="bg-white/95 rounded-3xl p-5 shadow-2xl">
                  <Image
                    source={require("@/../assets/logo.png")}
                    style={{ width: 90, height: 90 }}
                    resizeMode="contain"
                  />
                </View>
              </View>

              <Text className="text-4xl font-black text-white mb-2 text-center tracking-tight">
                Welcome Back
              </Text>
              <Text className="text-lg text-white/80 text-center">
                Sign in to continue your journey
              </Text>
            </View>

            {/* Form Card */}
            <View 
              className="rounded-3xl overflow-hidden bg-white mb-6"
              style={{
                shadowColor: '#000',
                shadowOffset: { width: 0, height: 10 },
                shadowOpacity: 0.15,
                shadowRadius: 20,
                elevation: 10,
              }}
            >
              <View className="p-6">
                {/* Error Message */}
                {error && (
                  <View className="mb-4 p-4 bg-red-50 rounded-xl flex-row items-start border-2 border-red-200">
                    <Ionicons name="alert-circle" size={20} color="#ef4444" style={{ marginTop: 2 }} />
                    <Text className="text-red-600 ml-2 flex-1 text-sm leading-5">{error}</Text>
                    <TouchableOpacity onPress={() => setError(null)}>
                      <Ionicons name="close" size={20} color="#ef4444" />
                    </TouchableOpacity>
                  </View>
                )}

                {/* Email Input */}
                <Input
                  label="Email Address"
                  icon="mail-outline"
                  placeholder="you@example.com"
                  value={email}
                  onChangeText={(text) => {
                    setEmail(text);
                    setError(null);
                    setEmailError("");
                  }}
                  error={emailError}
                  required
                  keyboardType="email-address"
                  autoCapitalize="none"
                  autoComplete="email"
                  editable={!loading}
                />

                {/* Password Input */}
                <View>
                  <View className="flex-row justify-between items-center mb-2 ml-1">
                    <Text className="text-sm font-semibold text-forest">
                      Password <Text className="text-red-500">*</Text>
                    </Text>
                    <TouchableOpacity>
                      <Text className="text-sm font-bold text-gold">Forgot?</Text>
                    </TouchableOpacity>
                  </View>
                  <Input
                    icon="lock-closed-outline"
                    placeholder="Enter your password"
                    value={password}
                    onChangeText={(text) => {
                      setPassword(text);
                      setError(null);
                      setPasswordError("");
                    }}
                    error={passwordError}
                    secureTextEntry
                    showPasswordToggle
                    autoComplete="password"
                    editable={!loading}
                  />
                </View>

                {/* Login Button */}
                <Button
                  title="Sign In"
                  onPress={handleLogin}
                  loading={loading}
                  disabled={loading}
                  icon="arrow-forward"
                  iconPosition="right"
                  size="lg"
                  fullWidth
                />
              </View>
            </View>

            {/* Divider */}
            <Divider label="or continue with" spacing="lg" />

            {/* Social Login Options */}
            <View className="flex-row justify-center gap-4 mb-8">
              {[
                { name: "Google" as const, icon: "logo-google", color: "#DB4437" },
                { name: "Apple" as const, icon: "logo-apple", color: "#000000" },
                { name: "Facebook" as const, icon: "logo-facebook", color: "#4267B2" },
              ].map((provider) => (
                <TouchableOpacity
                  key={provider.name}
                  className="w-16 h-16 rounded-2xl bg-white items-center justify-center"
                  activeOpacity={0.7}
                  onPress={() => handleSocialLogin(provider.name)}
                  disabled={loading}
                  style={{
                    shadowColor: '#000',
                    shadowOffset: { width: 0, height: 2 },
                    shadowOpacity: 0.1,
                    shadowRadius: 8,
                    elevation: 3,
                    opacity: loading ? 0.5 : 1,
                  }}
                >
                  <Ionicons name={provider.icon as any} size={28} color={provider.color} />
                </TouchableOpacity>
              ))}
            </View>

            {/* Register Link */}
            <View className="items-center">
              <Text className="text-forest/70 text-center mb-2">New to StayAfrica?</Text>
              <TouchableOpacity
                onPress={() => router.push("/(auth)/register")}
                activeOpacity={0.7}
                className="px-8 py-3 rounded-full bg-forest/10"
              >
                <Text className="text-forest font-bold text-base">Create an account</Text>
              </TouchableOpacity>
            </View>
          </View>
        </ScrollView>
      </KeyboardAvoidingView>
    </View>
  );
}
