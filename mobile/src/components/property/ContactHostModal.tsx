import React, { useState, useMemo } from 'react';
import { View, Text, Modal, TouchableOpacity, TextInput, ActivityIndicator, Alert, KeyboardAvoidingView, ScrollView, Platform } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { useCreateConversation, useUnavailableDates } from '@/hooks/api-hooks';
import { useRouter } from 'expo-router';
import { Calendar, DateData } from 'react-native-calendars';
import { format, addDays, isBefore, parseISO } from 'date-fns';

interface ContactHostModalProps {
    visible: boolean;
    onClose: () => void;
    host: {
        id: string | number;
        first_name: string;
        last_name?: string;
    };
    propertyId?: string | number;
    userId?: string | number;
}

export function ContactHostModal({ visible, onClose, host, propertyId, userId }: ContactHostModalProps) {
    const router = useRouter();
    const today = format(new Date(), 'yyyy-MM-dd');
    const tomorrow = format(addDays(new Date(), 1), 'yyyy-MM-dd');
    const [checkIn, setCheckIn] = useState('');
    const [checkOut, setCheckOut] = useState('');
    const [showCalendar, setShowCalendar] = useState(false);
    const [selecting, setSelecting] = useState<'checkIn' | 'checkOut'>('checkIn');
    
    const [guests, setGuests] = useState('1');
    const [message, setMessage] = useState('');

    const { mutateAsync: createConversation, isPending: isSubmitting } = useCreateConversation();
    
    // Fetch unavailable dates
    const { data: unavailableDatesData } = useUnavailableDates(propertyId ? String(propertyId) : '');
    
    const unavailableDates = useMemo(() => {
        const dates: { [key: string]: any } = {};
        if (unavailableDatesData?.unavailable_dates) {
            unavailableDatesData.unavailable_dates.forEach((date: string) => {
                dates[date] = {
                    disabled: true,
                    disableTouchEvent: true,
                    selected: true,
                    startingDay: true,
                    endingDay: true,
                    color: '#fee2e2',
                    textColor: '#b91c1c',
                };
            });
        }
        // Keep current day unavailable to match web behavior (booking starts from tomorrow).
        dates[today] = {
            disabled: true,
            disableTouchEvent: true,
            selected: true,
            startingDay: true,
            endingDay: true,
            color: '#f1f5f9',
            textColor: '#94a3b8',
        };
        return dates;
    }, [unavailableDatesData, today]);

    const hasUnavailableInRange = (startDate: string, endDate: string) => {
        const start = parseISO(startDate);
        const end = parseISO(endDate);
        let cursor = addDays(start, 1);

        while (isBefore(cursor, end)) {
            const dateString = format(cursor, 'yyyy-MM-dd');
            if (unavailableDates[dateString]?.disabled) {
                return true;
            }
            cursor = addDays(cursor, 1);
        }

        return false;
    };

    const markedDates = useMemo(() => {
        const marks = { ...unavailableDates };
        
        if (checkIn) {
            marks[checkIn] = { ...marks[checkIn], selected: true, startingDay: true, color: '#d9b168', textColor: '#ffffff' };
        }
        if (checkOut) {
            marks[checkOut] = { ...marks[checkOut], selected: true, endingDay: true, color: '#d9b168', textColor: '#ffffff' };
        }
        
        if (checkIn && checkOut) {
            let start = new Date(checkIn);
            let end = new Date(checkOut);
            
            // Swap if check-out is before check-in
            if (start > end) {
                const temp = start;
                start = end;
                end = temp;
            }

            let current = addDays(start, 1);
            while (isBefore(current, end)) {
                const dateString = format(current, 'yyyy-MM-dd');
                if (!marks[dateString]?.disabled) {
                    marks[dateString] = { color: 'rgba(217, 177, 104, 0.2)', textColor: '#122f26' };
                }
                current = addDays(current, 1);
            }
        }
        
        return marks;
    }, [checkIn, checkOut, unavailableDates]);

    const onDayPress = (day: DateData) => {
        if (unavailableDates[day.dateString]?.disabled || day.dateString <= today) return;

        if (selecting === 'checkIn') {
            setCheckIn(day.dateString);
            setSelecting('checkOut');
            if (checkOut && day.dateString >= checkOut) {
                setCheckOut('');
            }
        } else {
            if (!checkIn) {
                setCheckIn(day.dateString);
                setSelecting('checkOut');
                return;
            }

            if (day.dateString <= checkIn) {
                setCheckIn(day.dateString);
                setCheckOut('');
            } else {
                if (hasUnavailableInRange(checkIn, day.dateString)) {
                    Alert.alert('Unavailable Dates', 'Your selected date range includes already booked dates. Please choose different dates.');
                    return;
                }
                setCheckOut(day.dateString);
                setShowCalendar(false);
            }
        }
    };

    const handleSubmit = async () => {
        if (!userId) {
            Alert.alert('Login Required', 'You must be logged in to contact the host.');
            return;
        }
        if (!checkIn || !checkOut) {
            Alert.alert('Missing Details', 'Please provide travel dates to give the host context.');
            return;
        }
        if (!message.trim()) {
            Alert.alert('Missing Message', 'Please include an initial message.');
            return;
        }

        // Safety regex check
        const emailRegex = /[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}/;
        const phoneRegex = /(?:\+?\d{1,3}[\s-]?)?\(?\d{3}\)?[\s-]?\d{3}[\s-]?\d{4}/;
        if (emailRegex.test(message) || phoneRegex.test(message)) {
            Alert.alert('Safety Notice', 'For your safety and privacy, please do not share direct contact information before a booking is confirmed.');
            return;
        }

        try {
            const data = await createConversation({
                property: propertyId ? String(propertyId) : undefined,
                participants: [Number(host.id), Number(userId)],
                subject: 'Inquiry from interested guest',
                initial_message: message.trim(),
                metadata: {
                    check_in: checkIn,
                    check_out: checkOut,
                    guests: parseInt(guests) || 1,
                }
            });

            Alert.alert('Success', 'Message sent to host!');
            onClose();
            if (data?.id) {
                router.push(`/(tabs)/messages/${data.id}`);
            } else {
                router.push('/(tabs)/messages');
            }
        } catch (error: any) {
            Alert.alert('Error', error?.response?.data?.error || 'Failed to start conversation');
        }
    };

    return (
        <Modal
            visible={visible}
            transparent
            animationType="slide"
            onRequestClose={onClose}
        >
            <KeyboardAvoidingView
                behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
                className="flex-1 justify-end bg-black/50"
            >
                <TouchableOpacity
                    className="flex-1"
                    activeOpacity={1}
                    onPress={onClose}
                />
                <View className="bg-white rounded-t-3xl p-6" style={{ maxHeight: '90%' }}>
                    {/* Header */}
                    <View className="flex-row justify-between items-center mb-6">
                        <Text className="text-xl font-bold text-forest">
                            Contact {host.first_name}
                        </Text>
                        <TouchableOpacity onPress={onClose} className="p-2 bg-sand-100 rounded-full">
                            <Ionicons name="close" size={20} color="#122F26" />
                        </TouchableOpacity>
                    </View>

                    <ScrollView showsVerticalScrollIndicator={false}>
                        <Text className="text-sm text-moss mb-5">
                            Tell {host.first_name} a bit about your trip so they can better help you.
                        </Text>

                        {/* Dates */}
                        <View className="flex-row gap-4 mb-4">
                            <TouchableOpacity 
                                className="flex-1"
                                onPress={() => { setSelecting('checkIn'); setShowCalendar(true); }}
                            >
                                <Text className="text-xs font-bold text-forest uppercase mb-1.5">Check-in</Text>
                                <View className="flex-row items-center border border-sand-200 rounded-xl px-3 h-12 bg-sand-50">
                                    <Ionicons name="calendar-outline" size={18} color="#789c8d" />
                                    <Text className={`flex-1 ml-2 text-sm ${checkIn ? 'text-forest' : 'text-[#a0b3aa]'}`}>
                                        {checkIn || 'Select Date'}
                                    </Text>
                                </View>
                            </TouchableOpacity>
                            <TouchableOpacity 
                                className="flex-1"
                                onPress={() => { setSelecting('checkOut'); setShowCalendar(true); }}
                            >
                                <Text className="text-xs font-bold text-forest uppercase mb-1.5">Check-out</Text>
                                <View className="flex-row items-center border border-sand-200 rounded-xl px-3 h-12 bg-sand-50">
                                    <Ionicons name="calendar-outline" size={18} color="#789c8d" />
                                    <Text className={`flex-1 ml-2 text-sm ${checkOut ? 'text-forest' : 'text-[#a0b3aa]'}`}>
                                        {checkOut || 'Select Date'}
                                    </Text>
                                </View>
                            </TouchableOpacity>
                        </View>

                        {showCalendar && (
                            <View className="mb-4 border border-sand-200 rounded-xl overflow-hidden">
                                <Calendar
                                    current={checkIn || new Date().toISOString().split('T')[0]}
                                    minDate={tomorrow}
                                    onDayPress={onDayPress}
                                    markedDates={markedDates}
                                    markingType={'period'}
                                    theme={{
                                        todayTextColor: '#d9b168',
                                        arrowColor: '#d9b168',
                                        textDayFontWeight: '500',
                                        textMonthFontWeight: 'bold',
                                        textDayHeaderFontWeight: 'bold',
                                    }}
                                />
                                <TouchableOpacity 
                                    onPress={() => setShowCalendar(false)}
                                    className="bg-sand-100 py-3 items-center border-t border-sand-200"
                                >
                                    <Text className="text-forest font-semibold">Close Calendar</Text>
                                </TouchableOpacity>
                            </View>
                        )}

                        {/* Guests */}
                        <View className="mb-4">
                            <Text className="text-xs font-bold text-forest uppercase mb-1.5">Guests</Text>
                            <View className="flex-row items-center border border-sand-200 rounded-xl px-3 h-12 bg-sand-50">
                                <Ionicons name="people-outline" size={18} color="#789c8d" />
                                <TextInput
                                    placeholder="Number of guests"
                                    value={guests}
                                    onChangeText={setGuests}
                                    keyboardType="number-pad"
                                    className="flex-1 ml-2 text-forest text-sm pt-0 pb-0"
                                    placeholderTextColor="#a0b3aa"
                                />
                            </View>
                        </View>

                        {/* Message */}
                        <View className="mb-6">
                            <Text className="text-xs font-bold text-forest uppercase mb-1.5">Message</Text>
                            <View className="border border-sand-200 rounded-xl px-3 py-3 bg-sand-50">
                                <View className="flex-row items-start mb-1">
                                    <Ionicons name="chatbubble-outline" size={18} color="#789c8d" style={{ marginTop: 2 }} />
                                    <TextInput
                                        placeholder={`Hi ${host.first_name}, I'd like to ask about...`}
                                        value={message}
                                        onChangeText={setMessage}
                                        multiline
                                        numberOfLines={4}
                                        textAlignVertical="top"
                                        style={{ minHeight: 80 }}
                                        className="flex-1 ml-2 text-forest text-sm pt-0 pb-0"
                                        placeholderTextColor="#a0b3aa"
                                    />
                                </View>
                            </View>
                            <Text className="text-xs text-moss mt-2">
                                For your safety, do not share your phone number or email before a booking is confirmed.
                            </Text>
                        </View>

                        <TouchableOpacity
                            className={`py-3.5 rounded-xl flex-row justify-center items-center ${isSubmitting ? 'bg-gold/70' : 'bg-gold'}`}
                            onPress={handleSubmit}
                            disabled={isSubmitting}
                        >
                            {isSubmitting ? (
                                <ActivityIndicator color="#ffffff" size="small" />
                            ) : (
                                <Text className="text-white font-bold text-base">Send Message</Text>
                            )}
                        </TouchableOpacity>

                        <View style={{ height: 40 }} />
                    </ScrollView>
                </View>
            </KeyboardAvoidingView>
        </Modal>
    );
}
