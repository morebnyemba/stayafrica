import React, { useState } from 'react';
import {
    View,
    Text,
    ScrollView,
    TouchableOpacity,
    TextInput,
    Linking,
} from 'react-native';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { router } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';

interface FaqItem {
    question: string;
    answer: string;
}

interface FaqSection {
    title: string;
    icon: keyof typeof Ionicons.glyphMap;
    items: FaqItem[];
}

const FAQ_SECTIONS: FaqSection[] = [
    {
        title: 'Getting Started',
        icon: 'help-circle-outline',
        items: [
            {
                question: 'How do I create an account?',
                answer: 'Tap "Sign Up" on the login screen. You can register using your email address or sign in with Google, Facebook, or Apple. After registering, you\'ll receive a verification email to activate your account.',
            },
            {
                question: 'What countries does StayAfrica operate in?',
                answer: 'StayAfrica currently operates in Zimbabwe, South Africa, Botswana, Namibia, and Zambia. We\'re continuously expanding our reach across Africa.',
            },
        ],
    },
    {
        title: 'Booking & Reservations',
        icon: 'calendar-outline',
        items: [
            {
                question: 'How do I book a property?',
                answer: 'Browse properties on the Explore tab, select your dates and number of guests. Tap "Book Now" on your chosen property, review the pricing, and complete your booking with a payment method.',
            },
            {
                question: 'Can I cancel my booking?',
                answer: 'Yes, go to your Bookings tab and select the booking you want to cancel. Cancellation policies vary by property — check the listing for specific refund terms.',
            },
            {
                question: 'How do I contact the host?',
                answer: 'You can message any host from their property listing by tapping "Contact Host." Access all conversations from the Messages tab.',
            },
        ],
    },
    {
        title: 'Payments',
        icon: 'card-outline',
        items: [
            {
                question: 'What payment methods are accepted?',
                answer: 'Payment methods depend on your country: Zimbabwe — Paynow (EcoCash, OneMoney) or Cash on Arrival; South Africa — PayFast or Ozow; International — Stripe (Visa, MasterCard).',
            },
            {
                question: 'How is pricing calculated?',
                answer: 'Your total includes: Nightly rate × Number of nights + Service fee ($3) + Cleaning fee (if applicable). The 7% commission is deducted from the host\'s payout, not added to your bill.',
            },
        ],
    },
    {
        title: 'Hosting',
        icon: 'home-outline',
        items: [
            {
                question: 'How do I become a host?',
                answer: 'Go to the Host tab, complete your profile, and submit your first property listing. Once reviewed and approved by our team, your listing will go live.',
            },
            {
                question: 'How much does it cost to list?',
                answer: 'Listing is free. StayAfrica charges a 7% commission on confirmed bookings, deducted from your payout. No upfront fees or subscriptions.',
            },
        ],
    },
    {
        title: 'Safety & Security',
        icon: 'shield-checkmark-outline',
        items: [
            {
                question: 'Is my payment information secure?',
                answer: 'Yes. We use industry-standard encryption and partner with trusted payment providers (Stripe, PayFast, Paynow). We never store full card details.',
            },
            {
                question: 'How does the review system work?',
                answer: 'After checkout, both guests and hosts can leave reviews. Reviews are visible after both parties submit theirs (or after 14 days).',
            },
        ],
    },
];

function FaqAccordion({ item }: { item: FaqItem }) {
    const [isOpen, setIsOpen] = useState(false);

    return (
        <TouchableOpacity
            onPress={() => setIsOpen(!isOpen)}
            className="py-3 border-b border-gray-100"
            activeOpacity={0.7}
        >
            <View className="flex-row items-center justify-between">
                <Text className="flex-1 text-sm font-medium text-gray-800 pr-3">
                    {item.question}
                </Text>
                <Ionicons
                    name={isOpen ? 'chevron-down' : 'chevron-forward'}
                    size={16}
                    color="#9ca3af"
                />
            </View>
            {isOpen && (
                <Text className="text-sm text-gray-600 mt-2 leading-5">
                    {item.answer}
                </Text>
            )}
        </TouchableOpacity>
    );
}

export default function HelpScreen() {
    const insets = useSafeAreaInsets();
    const [searchQuery, setSearchQuery] = useState('');

    const filteredSections = searchQuery.trim()
        ? FAQ_SECTIONS.map(section => ({
            ...section,
            items: section.items.filter(
                item =>
                    item.question.toLowerCase().includes(searchQuery.toLowerCase()) ||
                    item.answer.toLowerCase().includes(searchQuery.toLowerCase())
            ),
        })).filter(section => section.items.length > 0)
        : FAQ_SECTIONS;

    return (
        <SafeAreaView className="flex-1 bg-sand-100">
            {/* Header */}
            <LinearGradient
                colors={['#122F26', '#1d392f', '#2d4a40']}
                start={{ x: 0, y: 0 }}
                end={{ x: 1, y: 1 }}
                className="px-4 pb-6"
                style={{ paddingTop: insets.top + 12 }}
            >
                <View className="flex-row items-center mb-2">
                    <TouchableOpacity
                        onPress={() => router.back()}
                        className="w-10 h-10 rounded-xl items-center justify-center mr-3"
                        style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
                    >
                        <Ionicons name="arrow-back" size={24} color="#fff" />
                    </TouchableOpacity>
                    <Text className="text-3xl font-black text-white tracking-tight">Help</Text>
                </View>
                <Text className="text-sand-200 text-sm ml-12">Find answers to common questions</Text>
            </LinearGradient>

            <ScrollView className="flex-1" showsVerticalScrollIndicator={false}>
                {/* Search */}
                <View className="px-4 pt-4 pb-2">
                    <View className="flex-row items-center bg-white rounded-xl px-4 py-3 border border-gray-200">
                        <Ionicons name="search" size={20} color="#9ca3af" />
                        <TextInput
                            placeholder="Search for help..."
                            value={searchQuery}
                            onChangeText={setSearchQuery}
                            className="flex-1 ml-3 text-sm text-gray-900"
                            placeholderTextColor="#9ca3af"
                        />
                    </View>
                </View>

                {/* FAQ Sections */}
                {filteredSections.length === 0 ? (
                    <View className="items-center py-16 px-6">
                        <Ionicons name="help-circle-outline" size={48} color="#d1d5db" />
                        <Text className="text-lg font-medium text-gray-600 mt-4">No results found</Text>
                        <Text className="text-sm text-gray-400 mt-1 text-center">
                            Try different keywords
                        </Text>
                    </View>
                ) : (
                    <View className="px-4 pt-2">
                        {filteredSections.map(section => (
                            <View
                                key={section.title}
                                className="bg-white rounded-2xl p-4 mb-3"
                                style={{
                                    shadowColor: '#122F26',
                                    shadowOffset: { width: 0, height: 2 },
                                    shadowOpacity: 0.05,
                                    shadowRadius: 4,
                                    elevation: 2,
                                }}
                            >
                                <View className="flex-row items-center mb-2">
                                    <View className="w-8 h-8 rounded-lg bg-emerald-100 items-center justify-center mr-3">
                                        <Ionicons name={section.icon} size={16} color="#059669" />
                                    </View>
                                    <Text className="text-base font-bold text-gray-900">{section.title}</Text>
                                </View>
                                {section.items.map((item, i) => (
                                    <FaqAccordion key={i} item={item} />
                                ))}
                            </View>
                        ))}
                    </View>
                )}

                {/* Contact Support */}
                <View className="px-4 pb-8 pt-4">
                    <View className="bg-emerald-50 rounded-2xl p-6 items-center border border-emerald-200">
                        <Text className="text-lg font-bold text-gray-900 mb-1">Still need help?</Text>
                        <Text className="text-sm text-gray-600 mb-4 text-center">
                            Our support team is here for you.
                        </Text>
                        <View className="flex-row gap-3">
                            <TouchableOpacity
                                onPress={() => Linking.openURL('mailto:support@stayafrica.app')}
                                className="flex-row items-center bg-emerald-600 px-5 py-2.5 rounded-xl"
                            >
                                <Ionicons name="mail" size={16} color="#fff" />
                                <Text className="text-white font-medium ml-2 text-sm">Email</Text>
                            </TouchableOpacity>
                            <TouchableOpacity
                                onPress={() => router.push('/(tabs)/messages')}
                                className="flex-row items-center bg-white px-5 py-2.5 rounded-xl border border-gray-200"
                            >
                                <Ionicons name="chatbubbles" size={16} color="#374151" />
                                <Text className="text-gray-900 font-medium ml-2 text-sm">Chat</Text>
                            </TouchableOpacity>
                        </View>
                    </View>
                </View>
            </ScrollView>
        </SafeAreaView>
    );
}
