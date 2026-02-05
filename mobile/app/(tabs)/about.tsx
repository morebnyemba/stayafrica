import React from 'react';
import {
  View,
  Text,
  ScrollView,
  TouchableOpacity,
  Linking,
} from 'react-native';
import { SafeAreaView } from 'react-native-safe-area-context';
import { router } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';

export default function AboutScreen() {
  const appVersion = '1.0.0';
  const buildNumber = '100';

  const handleLink = async (url: string) => {
    try {
      const supported = await Linking.canOpenURL(url);
      if (supported) {
        await Linking.openURL(url);
      }
    } catch (error) {
      console.error('Error opening link:', error);
    }
  };

  const handleEmail = () => {
    handleLink('mailto:support@stayafrica.com');
  };

  const handleWebsite = () => {
    handleLink('https://stayafrica.com');
  };

  const handlePrivacy = () => {
    handleLink('https://stayafrica.com/privacy');
  };

  const handleTerms = () => {
    handleLink('https://stayafrica.com/terms');
  };

  return (
    <SafeAreaView className="flex-1 bg-gray-50">
      {/* Header */}
      <View className="bg-white px-6 py-4 border-b border-gray-200 flex-row items-center">
        <TouchableOpacity
          onPress={() => router.back()}
          className="w-10 h-10 items-center justify-center mr-3"
        >
          <Ionicons name="arrow-back" size={24} color="#000" />
        </TouchableOpacity>
        <Text className="text-2xl font-bold text-gray-900">About</Text>
      </View>

      <ScrollView className="flex-1">
        {/* App Logo & Name */}
        <View className="bg-white p-8 items-center border-b border-gray-200">
          <View className="w-24 h-24 bg-emerald-600 rounded-3xl items-center justify-center mb-4">
            <Ionicons name="home" size={48} color="#fff" />
          </View>
          <Text className="text-2xl font-bold text-gray-900 mb-2">
            StayAfrica
          </Text>
          <Text className="text-base text-gray-600 mb-2">
            Stay Anywhere. Africa Awaits.
          </Text>
          <Text className="text-sm text-gray-500">
            Version {appVersion} (Build {buildNumber})
          </Text>
        </View>

        {/* About Section */}
        <View className="bg-white mt-4 px-6 py-4 border-b border-gray-200">
          <Text className="text-lg font-bold text-gray-900 mb-3">
            About StayAfrica
          </Text>
          <Text className="text-base text-gray-600 leading-6">
            StayAfrica is your premier accommodation booking platform across
            Zimbabwe, South Africa, Botswana, Namibia, and Zambia. We connect
            travelers with unique stays and unforgettable experiences throughout
            the African continent.
          </Text>
        </View>

        {/* Features */}
        <View className="bg-white mt-4 px-6 py-4 border-b border-gray-200">
          <Text className="text-lg font-bold text-gray-900 mb-4">
            What We Offer
          </Text>
          
          <View className="mb-4">
            <View className="flex-row items-start mb-3">
              <View className="w-10 h-10 bg-emerald-100 rounded-full items-center justify-center mr-3">
                <Ionicons name="home-outline" size={20} color="#059669" />
              </View>
              <View className="flex-1">
                <Text className="font-semibold text-gray-900 mb-1">
                  Diverse Accommodations
                </Text>
                <Text className="text-gray-600">
                  From lodges and cottages to rooms and unique stays
                </Text>
              </View>
            </View>

            <View className="flex-row items-start mb-3">
              <View className="w-10 h-10 bg-emerald-100 rounded-full items-center justify-center mr-3">
                <Ionicons name="shield-checkmark-outline" size={20} color="#059669" />
              </View>
              <View className="flex-1">
                <Text className="font-semibold text-gray-900 mb-1">
                  Secure Payments
                </Text>
                <Text className="text-gray-600">
                  Regional payment options including Paynow, PayFast, and Stripe
                </Text>
              </View>
            </View>

            <View className="flex-row items-start mb-3">
              <View className="w-10 h-10 bg-emerald-100 rounded-full items-center justify-center mr-3">
                <Ionicons name="star-outline" size={20} color="#059669" />
              </View>
              <View className="flex-1">
                <Text className="font-semibold text-gray-900 mb-1">
                  Verified Hosts
                </Text>
                <Text className="text-gray-600">
                  All hosts undergo verification for your safety
                </Text>
              </View>
            </View>

            <View className="flex-row items-start">
              <View className="w-10 h-10 bg-emerald-100 rounded-full items-center justify-center mr-3">
                <Ionicons name="chatbubbles-outline" size={20} color="#059669" />
              </View>
              <View className="flex-1">
                <Text className="font-semibold text-gray-900 mb-1">
                  24/7 Support
                </Text>
                <Text className="text-gray-600">
                  Our team is here to help whenever you need us
                </Text>
              </View>
            </View>
          </View>
        </View>

        {/* Contact & Support */}
        <View className="bg-white mt-4 px-6 py-4 border-b border-gray-200">
          <Text className="text-lg font-bold text-gray-900 mb-4">
            Get in Touch
          </Text>

          <TouchableOpacity
            onPress={handleEmail}
            className="flex-row items-center justify-between py-3 border-b border-gray-100"
          >
            <View className="flex-row items-center flex-1">
              <Ionicons name="mail-outline" size={24} color="#6b7280" />
              <Text className="text-gray-900 ml-3">support@stayafrica.com</Text>
            </View>
            <Ionicons name="chevron-forward" size={20} color="#9ca3af" />
          </TouchableOpacity>

          <TouchableOpacity
            onPress={handleWebsite}
            className="flex-row items-center justify-between py-3 border-b border-gray-100"
          >
            <View className="flex-row items-center flex-1">
              <Ionicons name="globe-outline" size={24} color="#6b7280" />
              <Text className="text-gray-900 ml-3">www.stayafrica.com</Text>
            </View>
            <Ionicons name="chevron-forward" size={20} color="#9ca3af" />
          </TouchableOpacity>
        </View>

        {/* Legal */}
        <View className="bg-white mt-4 px-6 py-4 border-b border-gray-200">
          <Text className="text-lg font-bold text-gray-900 mb-4">Legal</Text>

          <TouchableOpacity
            onPress={handleTerms}
            className="flex-row items-center justify-between py-3 border-b border-gray-100"
          >
            <View className="flex-row items-center flex-1">
              <Ionicons name="document-text-outline" size={24} color="#6b7280" />
              <Text className="text-gray-900 ml-3">Terms of Service</Text>
            </View>
            <Ionicons name="chevron-forward" size={20} color="#9ca3af" />
          </TouchableOpacity>

          <TouchableOpacity
            onPress={handlePrivacy}
            className="flex-row items-center justify-between py-3"
          >
            <View className="flex-row items-center flex-1">
              <Ionicons name="shield-outline" size={24} color="#6b7280" />
              <Text className="text-gray-900 ml-3">Privacy Policy</Text>
            </View>
            <Ionicons name="chevron-forward" size={20} color="#9ca3af" />
          </TouchableOpacity>
        </View>

        {/* Markets */}
        <View className="bg-white mt-4 px-6 py-4 mb-4">
          <Text className="text-lg font-bold text-gray-900 mb-3">
            Active Markets
          </Text>
          <View className="flex-row flex-wrap">
            {['Zimbabwe', 'South Africa', 'Botswana', 'Namibia', 'Zambia'].map(
              (country) => (
                <View
                  key={country}
                  className="bg-emerald-100 px-3 py-2 rounded-lg mr-2 mb-2"
                >
                  <Text className="text-emerald-700 font-medium">
                    {country}
                  </Text>
                </View>
              )
            )}
          </View>
        </View>

        {/* Footer */}
        <View className="px-6 py-8 items-center">
          <Text className="text-sm text-gray-500 text-center">
            © 2026 StayAfrica. All rights reserved.
          </Text>
          <Text className="text-xs text-gray-400 text-center mt-2">
            Made with ❤️ for Africa
          </Text>
        </View>
      </ScrollView>
    </SafeAreaView>
  );
}
