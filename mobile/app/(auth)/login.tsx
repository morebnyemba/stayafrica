import { useState } from "react";
import {
  Text,
  View,
  ScrollView,
  TextInput,
  TouchableOpacity,
  Image,
  Animated,
} from "react-native";
import { LinearGradient } from "expo-linear-gradient";
import { useAuth } from "@/context/auth-context";
import { useRouter } from "expo-router";

export default function LoginScreen() {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [loading, setLoading] = useState(false);
  const [focusedInput, setFocusedInput] = useState<string | null>(null);
  const { login } = useAuth();
  const router = useRouter();

  const handleLogin = async () => {
    try {
      setLoading(true);
      await login(email, password);
      router.replace("/(tabs)/explore");
    } catch (error) {
      console.error("Login failed:", error);
    } finally {
      setLoading(false);
    }
  };

  return (
    <View className="flex-1 bg-white">
      {/* Background gradient */}
      <LinearGradient
        colors={["#F4F1EA", "#FFFFFF"]}
        start={{ x: 0, y: 0 }}
        end={{ x: 0, y: 1 }}
        style={{ position: "absolute", top: 0, left: 0, right: 0, bottom: 0 }}
      />

      <ScrollView
        className="flex-1"
        contentContainerStyle={{ flexGrow: 1 }}
        showsVerticalScrollIndicator={false}
      >
        <View className="flex-1 justify-center px-6 py-8 min-h-screen">
          {/* Header Section */}
          <View className="items-center mb-10 mt-4">
            <View className="relative mb-6">
              <View className="absolute -inset-4 bg-primary-500/10 rounded-full blur-xl" />
              <View className="relative bg-white/80 rounded-3xl p-4 shadow-2xl shadow-primary-500/20">
                <Image
                  source={require("@/../assets/logo.png")}
                  style={{ width: 100, height: 100 }}
                  resizeMode="contain"
                />
              </View>
            </View>

            <View className="items-center">
              <Text className="text-4xl font-black text-primary-900 mb-2 text-center tracking-tight">
                Welcome Back
              </Text>
              <Text className="text-lg text-primary-600/80 text-center max-w-xs">
                Continue your journey with StayAfrica
              </Text>
            </View>
          </View>

          {/* Form Card */}
          <View className="rounded-3xl overflow-hidden border border-white/50 shadow-2xl shadow-primary-500/10 mb-6 bg-white p-8">
            {/* Email Input */}
            <View className="mb-6">
              <Text className="text-sm font-semibold text-primary-700 mb-2 ml-1">Email</Text>
              <View
                className={`${focusedInput === "email" ? "border-2 border-primary-600" : "border border-primary-200"} rounded-xl overflow-hidden bg-sand-50 px-4`}
              >
                <TextInput
                  className="w-full py-4 text-lg"
                  placeholder="you@example.com"
                  placeholderTextColor="#94a3b8"
                  value={email}
                  onChangeText={setEmail}
                  onFocus={() => setFocusedInput("email")}
                  onBlur={() => setFocusedInput(null)}
                  editable={!loading}
                  keyboardType="email-address"
                  autoCapitalize="none"
                />
              </View>
            </View>

            {/* Password Input */}
            <View className="mb-8">
              <View className="flex-row justify-between items-center mb-2">
                <Text className="text-sm font-semibold text-primary-700 ml-1">Password</Text>
                <TouchableOpacity>
                  <Text className="text-sm font-bold text-primary-700">Forgot?</Text>
                </TouchableOpacity>
              </View>
              <View
                className={`${focusedInput === "password" ? "border-2 border-primary-600" : "border border-primary-200"} rounded-xl overflow-hidden bg-sand-50 px-4`}
              >
                <TextInput
                  className="w-full py-4 text-lg"
                  placeholder="********"
                  placeholderTextColor="#94a3b8"
                  value={password}
                  onChangeText={setPassword}
                  onFocus={() => setFocusedInput("password")}
                  onBlur={() => setFocusedInput(null)}
                  secureTextEntry
                  editable={!loading}
                />
              </View>
            </View>

            {/* Login Button */}
            <TouchableOpacity onPress={handleLogin} disabled={loading} activeOpacity={0.9}>
              <LinearGradient
                colors={["#d9b168", "#bea04f"]}
                start={{ x: 0, y: 0 }}
                end={{ x: 1, y: 0 }}
                style={{ borderRadius: 16, paddingVertical: 20 }}
              >
                <View className="flex-row justify-center items-center">
                  {loading && (
                    <View className="mr-3">
                      <Animated.View
                        className="w-5 h-5 border-2 border-forest border-t-transparent rounded-full"
                        style={{ transform: [{ rotate: loading ? "0deg" : "360deg" }] }}
                      />
                    </View>
                  )}
                  <Text className="text-forest font-bold text-lg text-center">
                    {loading ? "Signing In..." : "Sign In"}
                  </Text>
                </View>
              </LinearGradient>
            </TouchableOpacity>
          </View>

          {/* Divider */}
          <View className="flex-row items-center my-8">
            <View className="flex-1 h-px bg-gray-200" />
            <Text className="mx-4 text-gray-400 text-sm">or continue with</Text>
            <View className="flex-1 h-px bg-gray-200" />
          </View>

          {/* Social Login Options */}
          <View className="flex-row justify-center gap-4 mb-10">
            {["google", "apple", "facebook"].map((provider) => (
              <TouchableOpacity
                key={provider}
                className="w-14 h-14 rounded-2xl bg-white border border-gray-200 items-center justify-center shadow-sm"
                activeOpacity={0.7}
              >
                <Text className="text-gray-600 font-semibold">
                  {provider === "google" ? "G" : provider === "apple" ? "A" : "F"}
                </Text>
              </TouchableOpacity>
            ))}
          </View>

          {/* Register Link */}
          <View className="items-center">
            <Text className="text-gray-600 text-center mb-1">New to StayAfrica?</Text>
            <TouchableOpacity
              onPress={() => router.push("/(auth)/register")}
              activeOpacity={0.7}
              className="px-6 py-3 rounded-full"
            >
              <Text className="text-primary-700 font-bold text-lg">Create an account</Text>
            </TouchableOpacity>
          </View>

          {/* Bottom decorative element */}
          <View className="mt-12 items-center">
            <View className="h-1 w-14 bg-gradient-to-r from-primary-600/0 via-primary-600/80 to-primary-600/0 rounded-full" />
          </View>
        </View>
      </ScrollView>
    </View>
  );
}
