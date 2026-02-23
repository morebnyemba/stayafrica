import React, { useState, useEffect, useCallback } from 'react';
import {
    View,
    Text,
    ScrollView,
    TouchableOpacity,
    ActivityIndicator,
} from 'react-native';
import { SafeAreaView, useSafeAreaInsets } from 'react-native-safe-area-context';
import { router } from 'expo-router';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { apiClient } from '@/services/api-client';
import { Picker } from '@react-native-picker/picker';

interface CalendarEvent {
    id: string;
    property_id: string;
    property_title: string;
    guest_name: string;
    check_in: string;
    check_out: string;
    status: string;
    num_guests: number;
}

interface Property {
    id: string;
    title: string;
}

const MONTHS = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
const DAYS = ['S', 'M', 'T', 'W', 'T', 'F', 'S'];

const STATUS_COLORS: Record<string, { bg: string; text: string }> = {
    confirmed: { bg: '#dcfce7', text: '#15803d' },
    pending: { bg: '#fef9c3', text: '#a16207' },
    completed: { bg: '#dbeafe', text: '#1d4ed8' },
    cancelled: { bg: '#fee2e2', text: '#dc2626' },
};

const STATUS_DOT_COLORS: Record<string, string> = {
    confirmed: '#22c55e',
    pending: '#eab308',
    completed: '#3b82f6',
    cancelled: '#ef4444',
};

function getDaysInMonth(year: number, month: number): number {
    return new Date(year, month + 1, 0).getDate();
}

function getFirstDayOfMonth(year: number, month: number): number {
    return new Date(year, month, 1).getDay();
}

export default function HostCalendarScreen() {
    const insets = useSafeAreaInsets();
    const [currentDate, setCurrentDate] = useState(new Date());
    const [events, setEvents] = useState<CalendarEvent[]>([]);
    const [properties, setProperties] = useState<Property[]>([]);
    const [selectedProperty, setSelectedProperty] = useState<string>('all');
    const [loading, setLoading] = useState(true);
    const [selectedDay, setSelectedDay] = useState<number | null>(null);

    const year = currentDate.getFullYear();
    const month = currentDate.getMonth();

    const fetchCalendarData = useCallback(async () => {
        try {
            setLoading(true);
            const startDate = new Date(year, month, 1).toISOString().split('T')[0];
            const endDate = new Date(year, month + 1, 0).toISOString().split('T')[0];

            const propsRes = await apiClient.getHostProperties();
            const propsList: Property[] = (propsRes as any)?.results || propsRes || [];
            setProperties(propsList);

            const allEvents: CalendarEvent[] = [];
            const propsToFetch = selectedProperty === 'all'
                ? propsList.slice(0, 10)
                : propsList.filter(p => p.id === selectedProperty);

            for (const prop of propsToFetch) {
                try {
                    const calRes = await apiClient.getBookingCalendar(prop.id, startDate, endDate);
                    const calData: any[] = (calRes as any)?.results || calRes || [];
                    allEvents.push(...calData.map((e: any) => ({
                        ...e,
                        property_id: prop.id,
                        property_title: prop.title,
                    })));
                } catch {
                    // skip
                }
            }
            setEvents(allEvents);
        } catch (error) {
            console.error('Failed to fetch calendar data:', error);
        } finally {
            setLoading(false);
        }
    }, [year, month, selectedProperty]);

    useEffect(() => {
        fetchCalendarData();
    }, [fetchCalendarData]);

    const navigateMonth = (direction: number) => {
        setCurrentDate(prev => new Date(prev.getFullYear(), prev.getMonth() + direction, 1));
        setSelectedDay(null);
    };

    const daysInMonth = getDaysInMonth(year, month);
    const firstDay = getFirstDayOfMonth(year, month);

    const getEventsForDay = (day: number) => {
        const dateStr = `${year}-${String(month + 1).padStart(2, '0')}-${String(day).padStart(2, '0')}`;
        return events.filter(event => {
            const checkIn = event.check_in?.split('T')[0];
            const checkOut = event.check_out?.split('T')[0];
            return checkIn && checkOut && dateStr >= checkIn && dateStr <= checkOut;
        });
    };

    const selectedDayEvents = selectedDay ? getEventsForDay(selectedDay) : [];
    const today = new Date();

    return (
        <SafeAreaView className="flex-1 bg-sand-100">
            <ScrollView className="flex-1" showsVerticalScrollIndicator={false}>
                {/* Header */}
                <LinearGradient
                    colors={['#122F26', '#1d392f', '#2d4a40']}
                    start={{ x: 0, y: 0 }}
                    end={{ x: 1, y: 1 }}
                    className="px-4 pb-8"
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
                        <Text className="text-3xl font-black text-white tracking-tight">Calendar</Text>
                    </View>
                    <Text className="text-sand-200 text-sm ml-12">
                        View bookings across all properties
                    </Text>
                </LinearGradient>

                {/* Property Picker */}
                <View className="px-4 -mt-4 mb-2">
                    <View className="bg-white rounded-xl border border-gray-200 overflow-hidden">
                        <Picker
                            selectedValue={selectedProperty}
                            onValueChange={(value) => setSelectedProperty(value)}
                            style={{ height: 50 }}
                        >
                            <Picker.Item label="All Properties" value="all" />
                            {properties.map(prop => (
                                <Picker.Item key={prop.id} label={prop.title} value={prop.id} />
                            ))}
                        </Picker>
                    </View>
                </View>

                {/* Calendar */}
                <View className="px-4 mt-2">
                    <View
                        className="bg-white rounded-2xl p-4"
                        style={{
                            shadowColor: '#122F26',
                            shadowOffset: { width: 0, height: 2 },
                            shadowOpacity: 0.05,
                            shadowRadius: 4,
                            elevation: 2,
                        }}
                    >
                        {/* Month Navigation */}
                        <View className="flex-row items-center justify-between mb-4">
                            <TouchableOpacity onPress={() => navigateMonth(-1)} className="p-2">
                                <Ionicons name="chevron-back" size={20} color="#374151" />
                            </TouchableOpacity>
                            <Text className="text-base font-bold text-gray-900">
                                {MONTHS[month]} {year}
                            </Text>
                            <TouchableOpacity onPress={() => navigateMonth(1)} className="p-2">
                                <Ionicons name="chevron-forward" size={20} color="#374151" />
                            </TouchableOpacity>
                        </View>

                        {/* Day Headers */}
                        <View className="flex-row mb-1">
                            {DAYS.map((day, i) => (
                                <View key={i} className="flex-1 items-center py-1">
                                    <Text className="text-xs text-gray-400 font-medium">{day}</Text>
                                </View>
                            ))}
                        </View>

                        {/* Calendar Grid */}
                        {loading ? (
                            <View className="items-center py-8">
                                <ActivityIndicator size="small" color="#059669" />
                            </View>
                        ) : (
                            <View className="flex-row flex-wrap">
                                {/* Empty cells */}
                                {Array.from({ length: firstDay }).map((_, i) => (
                                    <View key={`e-${i}`} style={{ width: '14.28%', height: 44 }} />
                                ))}
                                {/* Days */}
                                {Array.from({ length: daysInMonth }).map((_, i) => {
                                    const day = i + 1;
                                    const dayEvents = getEventsForDay(day);
                                    const isToday = today.getDate() === day && today.getMonth() === month && today.getFullYear() === year;
                                    const isSelected = selectedDay === day;

                                    return (
                                        <TouchableOpacity
                                            key={day}
                                            onPress={() => setSelectedDay(day)}
                                            style={{ width: '14.28%', height: 44 }}
                                            className="items-center justify-center"
                                        >
                                            <View className={`w-8 h-8 rounded-full items-center justify-center ${isSelected ? 'bg-emerald-600' : isToday ? 'bg-emerald-100' : ''
                                                }`}>
                                                <Text className={`text-sm ${isSelected ? 'text-white font-bold'
                                                        : isToday ? 'text-emerald-700 font-bold'
                                                            : 'text-gray-700'
                                                    }`}>
                                                    {day}
                                                </Text>
                                            </View>
                                            {dayEvents.length > 0 && (
                                                <View className="flex-row gap-0.5 mt-0.5">
                                                    {dayEvents.slice(0, 3).map((event, idx) => (
                                                        <View
                                                            key={idx}
                                                            style={{
                                                                width: 4,
                                                                height: 4,
                                                                borderRadius: 2,
                                                                backgroundColor: STATUS_DOT_COLORS[event.status] || '#9ca3af',
                                                            }}
                                                        />
                                                    ))}
                                                </View>
                                            )}
                                        </TouchableOpacity>
                                    );
                                })}
                            </View>
                        )}

                        {/* Legend */}
                        <View className="flex-row flex-wrap mt-3 pt-3 border-t border-gray-100">
                            {Object.entries(STATUS_DOT_COLORS).map(([status, color]) => (
                                <View key={status} className="flex-row items-center mr-4 mb-1">
                                    <View style={{ width: 6, height: 6, borderRadius: 3, backgroundColor: color }} />
                                    <Text className="text-xs text-gray-500 ml-1 capitalize">{status}</Text>
                                </View>
                            ))}
                        </View>
                    </View>
                </View>

                {/* Selected Day Events */}
                <View className="px-4 mt-4 mb-8">
                    <Text className="text-base font-bold text-gray-900 mb-3">
                        {selectedDay
                            ? `${MONTHS[month]} ${selectedDay}, ${year}`
                            : 'Select a day to view bookings'}
                    </Text>

                    {!selectedDay ? (
                        <View className="items-center py-8 bg-white rounded-2xl">
                            <Ionicons name="calendar-outline" size={36} color="#d1d5db" />
                            <Text className="text-sm text-gray-400 mt-2">Tap a day above</Text>
                        </View>
                    ) : selectedDayEvents.length === 0 ? (
                        <View className="items-center py-8 bg-white rounded-2xl">
                            <Text className="text-sm text-gray-400">No bookings on this day</Text>
                        </View>
                    ) : (
                        selectedDayEvents.map(event => {
                            const colors = STATUS_COLORS[event.status] || STATUS_COLORS.pending;
                            return (
                                <View
                                    key={event.id}
                                    className="rounded-2xl p-4 mb-3"
                                    style={{ backgroundColor: colors.bg }}
                                >
                                    <View className="flex-row justify-between items-center mb-2">
                                        <Text style={{ color: colors.text }} className="font-semibold">
                                            {event.guest_name || 'Guest'}
                                        </Text>
                                        <View className="px-2 py-0.5 rounded-full" style={{ backgroundColor: `${colors.text}20` }}>
                                            <Text style={{ color: colors.text }} className="text-xs capitalize">
                                                {event.status}
                                            </Text>
                                        </View>
                                    </View>
                                    <View className="flex-row items-center mb-1">
                                        <Ionicons name="location-outline" size={12} color={colors.text} />
                                        <Text style={{ color: colors.text }} className="text-xs ml-1" numberOfLines={1}>
                                            {event.property_title}
                                        </Text>
                                    </View>
                                    <View className="flex-row items-center mb-1">
                                        <Ionicons name="time-outline" size={12} color={colors.text} />
                                        <Text style={{ color: colors.text }} className="text-xs ml-1">
                                            {event.check_in?.split('T')[0]} → {event.check_out?.split('T')[0]}
                                        </Text>
                                    </View>
                                    {event.num_guests > 0 && (
                                        <View className="flex-row items-center">
                                            <Ionicons name="people-outline" size={12} color={colors.text} />
                                            <Text style={{ color: colors.text }} className="text-xs ml-1">
                                                {event.num_guests} guest{event.num_guests > 1 ? 's' : ''}
                                            </Text>
                                        </View>
                                    )}
                                </View>
                            );
                        })
                    )}
                </View>
            </ScrollView>
        </SafeAreaView>
    );
}
