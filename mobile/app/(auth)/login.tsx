import { useState, useEffect } from "react";
import {
  Text,
  View,
  ScrollView,
  TextInput,
  TouchableOpacity,
  Image,
  ActivityIndicator,
  KeyboardAvoidingView,
  Platform,
} from "react-native";
import { LinearGradient } from "expo-linear-gradient";
import { useSafeAreaInsets } from "react-native-safe-area-context";
import { Ionicons } from "@expo/vector-icons";
import { useAuth } from "@/context/auth-context";
import { useRouter } from "expo-router";

export default function LoginScreen() {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [loading, setLoading] = useState(false);
  const [focusedInput, setFocusedInput] = useState<string | null>(null);
  const [showPassword, setShowPassword] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [loginSuccess, setLoginSuccess] = useState(false);
  const { login, user } = useAuth();
  const router = useRouter();
  const insets = useSafeAreaInsets();

  // Watch for user changes after successful login and redirect based on role
  useEffect(() => {
    if (loginSuccess && user) {
      setLoading(false);
      // Redirect based on user role
      if (user.role === 'host') {
        router.replace("/(tabs)/host");
      } else {
        router.replace("/(tabs)/dashboard");
      }
    }
  }, [loginSuccess, user, router]);

  const handleLogin = async () => {
    if (!email.trim()) {
      setError("Please enter your email");
      return;
    }
    if (!password) {
      setError("Please enter your password");
      return;
    }

    try {
      setLoading(true);
      setError(null);
      setLoginSuccess(false);
      await login(email.trim().toLowerCase(), password);
      setLoginSuccess(true);
    } catch (err: any) {
      console.error("Login failed:", err);
      setError(err?.response?.data?.detail || "Invalid email or password. Please try again.");
      setLoginSuccess(false);
      setLoading(false);
    }
  };

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
                  <View className="mb-4 p-4 bg-red-50 rounded-xl flex-row items-center">
                    <Ionicons name="alert-circle" size={20} color="#ef4444" />
                    <Text className="text-red-600 ml-2 flex-1 text-sm">{error}</Text>
                    <TouchableOpacity onPress={() => setError(null)}>
                      <Ionicons name="close" size={20} color="#ef4444" />
                    </TouchableOpacity>
                  </View>
                )}

                {/* Email Input */}
                <View className="mb-5">
                  <Text className="text-sm font-semibold text-forest mb-2 ml-1">
                    Email Address
                  </Text>
                  <View
                    className={`flex-row items-center rounded-xl overflow-hidden bg-sand-50 px-4 border-2 ${
                      focusedInput === "email" ? "border-gold" : "border-sand-200"
                    }`}
                  >
                    <Ionicons 
                      name="mail-outline" 
                      size={20} 
                      color={focusedInput === "email" ? "#D9B168" : "#94a3b8"} 
                    />
                    <TextInput
                      className="flex-1 py-4 ml-3 text-base text-forest"
                      placeholder="you@example.com"
                      placeholderTextColor="#94a3b8"
                      value={email}
                      onChangeText={(text) => {
                        setEmail(text);
                        setError(null);
                      }}
                      onFocus={() => setFocusedInput("email")}
                      onBlur={() => setFocusedInput(null)}
                      editable={!loading}
                      keyboardType="email-address"
                      autoCapitalize="none"
                      autoComplete="email"
                      blurOnSubmit={false}
                      returnKeyType="next"
                    />
                  </View>
                </View>

                {/* Password Input */}
                <View className="mb-6">
                  <View className="flex-row justify-between items-center mb-2">
                    <Text className="text-sm font-semibold text-forest ml-1">Password</Text>
                    <TouchableOpacity>
                      <Text className="text-sm font-bold text-gold">Forgot?</Text>
                    </TouchableOpacity>
                  </View>
                  <View
                    className={`flex-row items-center rounded-xl overflow-hidden bg-sand-50 px-4 border-2 ${
                      focusedInput === "password" ? "border-gold" : "border-sand-200"
                    }`}
                  >
                    <Ionicons 
                      name="lock-closed-outline" 
                      size={20} 
                      color={focusedInput === "password" ? "#D9B168" : "#94a3b8"} 
                    />
                    <TextInput
                      className="flex-1 py-4 ml-3 text-base text-forest"
                      placeholder="Enter your password"
                      placeholderTextColor="#94a3b8"
                      value={password}
                      onChangeText={(text) => {
                        setPassword(text);
                        setError(null);
                      }}
                      onFocus={() => setFocusedInput("password")}
                      onBlur={() => setFocusedInput(null)}
                      secureTextEntry={!showPassword}
                      editable={!loading}
                      autoComplete="password"
                      blurOnSubmit={false}
                      returnKeyType="done"
                      onSubmitEditing={handleLogin}
                    />
                    <TouchableOpacity onPress={() => setShowPassword(!showPassword)}>
                      <Ionicons 
                        name={showPassword ? "eye-outline" : "eye-off-outline"} 
                        size={22} 
                        color="#94a3b8" 
                      />
                    </TouchableOpacity>
                  </View>
                </View>

                {/* Login Button */}
                <TouchableOpacity 
                  onPress={handleLogin} 
                  disabled={loading} 
                  activeOpacity={0.9}
                >
                  <LinearGradient
                    colors={loading ? ["#e5d9c3", "#d4c4a8"] : ["#D9B168", "#c9a158"]}
                    start={{ x: 0, y: 0 }}
                    end={{ x: 1, y: 0 }}
                    style={{ 
                      borderRadius: 14, 
                      paddingVertical: 18,
                    }}
                  >
                    <View className="flex-row justify-center items-center">
                      {loading ? (
                        <ActivityIndicator size="small" color="#122F26" />
                      ) : (
                        <>
                          <Text className="text-forest font-bold text-lg">
                            Sign In
                          </Text>
                          <Ionicons name="arrow-forward" size={20} color="#122F26" style={{ marginLeft: 8 }} />
                        </>
                      )}
                    </View>
                  </LinearGradient>
                </TouchableOpacity>
              </View>
            </View>

            {/* Divider */}
            <View className="flex-row items-center my-6">
              <View className="flex-1 h-px bg-forest/20" />
              <Text className="mx-4 text-forest/60 text-sm font-medium">or continue with</Text>
              <View className="flex-1 h-px bg-forest/20" />
            </View>

            {/* Social Login Options */}
            <View className="flex-row justify-center gap-4 mb-8">
              {[
                { name: "Google", icon: "logo-google", color: "#DB4437" },
                { name: "Apple", icon: "logo-apple", color: "#000000" },
                { name: "Facebook", icon: "logo-facebook", color: "#4267B2" },
              ].map((provider) => (
                <TouchableOpacity
                  key={provider.name}
                  className="w-16 h-16 rounded-2xl bg-white items-center justify-center"
                  activeOpacity={0.7}
                  style={{
                    shadowColor: '#000',
                    shadowOffset: { width: 0, height: 2 },
                    shadowOpacity: 0.1,
                    shadowRadius: 8,
                    elevation: 3,
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
