'use client';

import { useState, useEffect, useCallback } from 'react';
import { apiClient } from '@/services/api-client';
import { ChevronLeft, ChevronRight, Calendar as CalendarIcon, Users, Clock, MapPin, Building2 } from 'lucide-react';

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

const STATUS_COLORS: Record<string, string> = {
    confirmed: 'bg-green-100 text-green-700 border-green-200',
    pending: 'bg-yellow-100 text-yellow-700 border-yellow-200',
    completed: 'bg-blue-100 text-blue-700 border-blue-200',
    cancelled: 'bg-red-100 text-red-700 border-red-200',
};

const MONTHS = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
const DAYS = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];

function getDaysInMonth(year: number, month: number): number {
    return new Date(year, month + 1, 0).getDate();
}

function getFirstDayOfMonth(year: number, month: number): number {
    return new Date(year, month, 1).getDay();
}

export function HostCalendarContent() {
    const [currentDate, setCurrentDate] = useState(new Date());
    const [events, setEvents] = useState<CalendarEvent[]>([]);
    const [properties, setProperties] = useState<Property[]>([]);
    const [selectedProperty, setSelectedProperty] = useState<string>('all');
    const [selectedDay, setSelectedDay] = useState<number | null>(null);
    const [loading, setLoading] = useState(true);

    const year = currentDate.getFullYear();
    const month = currentDate.getMonth();

    const fetchCalendarData = useCallback(async () => {
        setLoading(true);
        try {
            const startDate = new Date(year, month, 1).toISOString().split('T')[0];
            const endDate = new Date(year, month + 1, 0).toISOString().split('T')[0];

            // Fetch properties list
            const propsRes = await apiClient.getHostProperties();
            const propsList = propsRes.data?.results || propsRes.data || [];
            setProperties(propsList);

            // Fetch calendar events for all or selected property
            if (selectedProperty === 'all' && propsList.length > 0) {
                // Fetch for all properties
                const allEvents: CalendarEvent[] = [];
                for (const prop of propsList.slice(0, 10)) { // Limit to first 10 properties
                    try {
                        const calRes = await apiClient.getBookingCalendar(prop.id, startDate, endDate);
                        const calData = calRes.data?.results || calRes.data || [];
                        allEvents.push(...calData.map((e: any) => ({ ...e, property_id: prop.id, property_title: prop.title })));
                    } catch {
                        // Skip properties that fail
                    }
                }
                setEvents(allEvents);
            } else if (selectedProperty !== 'all') {
                const calRes = await apiClient.getBookingCalendar(selectedProperty, startDate, endDate);
                const calData = calRes.data?.results || calRes.data || [];
                const prop = propsList.find((p: Property) => p.id === selectedProperty);
                setEvents(calData.map((e: any) => ({ ...e, property_id: selectedProperty, property_title: prop?.title || '' })));
            }
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

    return (
        <div className="max-w-6xl mx-auto px-4 py-8">
            {/* Header */}
            <div className="flex flex-col sm:flex-row items-start sm:items-center justify-between gap-4 mb-6">
                <div>
                    <h1 className="text-2xl sm:text-3xl font-bold text-primary-900">Calendar</h1>
                    <p className="text-sm text-primary-500 mt-1">
                        View bookings across {properties.length || 'your'} propert{properties.length === 1 ? 'y' : 'ies'}
                    </p>
                </div>

                {/* Property Filter */}
                <select
                    value={selectedProperty}
                    onChange={e => setSelectedProperty(e.target.value)}
                    className="px-4 py-2 rounded-lg border border-primary-200 bg-white text-primary-900 text-sm focus:outline-none focus:ring-2 focus:ring-secondary-500"
                >
                    <option value="all">All Properties</option>
                    {properties.map(prop => (
                        <option key={prop.id} value={prop.id}>{prop.title}</option>
                    ))}
                </select>
            </div>

            {loading ? (
                <div className="grid grid-cols-1 lg:grid-cols-3 gap-4 sm:gap-6">
                    <div className="lg:col-span-2 card p-4 sm:p-6 animate-pulse">
                        <div className="h-8 w-48 bg-primary-200 rounded mx-auto mb-6" />
                        <div className="grid grid-cols-7 gap-1">
                            {Array.from({ length: 35 }).map((_, i) => (
                                <div key={i} className="h-10 sm:h-14 bg-primary-100 rounded-lg" />
                            ))}
                        </div>
                    </div>
                    <div className="card p-6 animate-pulse">
                        <div className="h-6 w-32 bg-primary-200 rounded mb-4" />
                        <div className="space-y-3">
                            <div className="h-20 bg-primary-100 rounded-lg" />
                            <div className="h-20 bg-primary-100 rounded-lg" />
                        </div>
                    </div>
                </div>
            ) : (
            <div className="grid grid-cols-1 lg:grid-cols-3 gap-4 sm:gap-6">
                {/* Calendar Grid */}
                <div className="lg:col-span-2 card p-4 sm:p-6">
                    {/* Month Navigation */}
                    <div className="flex items-center justify-between mb-4 sm:mb-6">
                        <button onClick={() => navigateMonth(-1)} className="p-2.5 hover:bg-sand-100 rounded-lg transition-colors">
                            <ChevronLeft className="w-5 h-5 text-primary-600" />
                        </button>
                        <h2 className="text-base sm:text-lg font-semibold text-primary-900">
                            {MONTHS[month]} {year}
                        </h2>
                        <button onClick={() => navigateMonth(1)} className="p-2.5 hover:bg-sand-100 rounded-lg transition-colors">
                            <ChevronRight className="w-5 h-5 text-primary-600" />
                        </button>
                    </div>

                    {/* Day Headers */}
                    <div className="grid grid-cols-7 mb-2">
                        {DAYS.map(day => (
                            <div key={day} className="text-center text-xs font-medium text-primary-400 py-2">
                                {day}
                            </div>
                        ))}
                    </div>

                    {/* Calendar Days */}
                    <div className="grid grid-cols-7 gap-1">
                        {/* Empty cells for days before the 1st */}
                        {Array.from({ length: firstDay }).map((_, i) => (
                            <div key={`empty-${i}`} className="h-10 sm:h-14" />
                        ))}
                        {/* Actual days */}
                        {Array.from({ length: daysInMonth }).map((_, i) => {
                            const day = i + 1;
                            const dayEvents = getEventsForDay(day);
                            const isToday = new Date().getDate() === day && new Date().getMonth() === month && new Date().getFullYear() === year;
                            const isSelected = selectedDay === day;

                            return (
                                <button
                                    key={day}
                                    onClick={() => setSelectedDay(day)}
                                    className={`h-10 sm:h-14 rounded-lg flex flex-col items-center justify-start pt-1 transition-all relative ${isSelected
                                        ? 'bg-accent-100 ring-2 ring-accent-500'
                                        : isToday
                                            ? 'bg-accent-50'
                                            : 'hover:bg-sand-50'
                                        }`}
                                >
                                    <span className={`text-sm ${isToday
                                        ? 'font-bold text-accent-600'
                                        : 'text-primary-700'
                                        }`}>
                                        {day}
                                    </span>
                                    {dayEvents.length > 0 && (
                                        <div className="flex gap-0.5 mt-1">
                                            {dayEvents.slice(0, 3).map((event, idx) => (
                                                <div
                                                    key={idx}
                                                    className={`w-1.5 h-1.5 rounded-full ${event.status === 'confirmed' ? 'bg-green-500'
                                                        : event.status === 'pending' ? 'bg-yellow-500'
                                                            : event.status === 'completed' ? 'bg-blue-500'
                                                                : 'bg-red-500'
                                                        }`}
                                                />
                                            ))}
                                        </div>
                                    )}
                                </button>
                            );
                        })}
                    </div>

                    {/* Legend */}
                    <div className="flex flex-wrap gap-4 mt-4 pt-4 border-t border-sand-200">
                        {['confirmed', 'pending', 'completed', 'cancelled'].map(status => (
                            <div key={status} className="flex items-center gap-1.5">
                                <div className={`w-2.5 h-2.5 rounded-full ${status === 'confirmed' ? 'bg-green-500'
                                    : status === 'pending' ? 'bg-yellow-500'
                                        : status === 'completed' ? 'bg-blue-500'
                                            : 'bg-red-500'
                                    }`} />
                                <span className="text-xs text-primary-500 capitalize">{status}</span>
                            </div>
                        ))}
                    </div>
                </div>

                {/* Selected Day Details */}
                <div className="card p-4 sm:p-6">
                    <h3 className="text-lg font-semibold text-primary-900 mb-4">
                        {selectedDay
                            ? `${MONTHS[month]} ${selectedDay}, ${year}`
                            : 'Select a day'}
                    </h3>

                    {!selectedDay ? (
                        <div className="text-center py-12">
                            <div className="w-14 h-14 rounded-full bg-secondary-100 flex items-center justify-center mx-auto mb-3">
                                <CalendarIcon className="w-7 h-7 text-secondary-600" />
                            </div>
                            <p className="text-sm font-medium text-primary-700 mb-1">
                                No day selected
                            </p>
                            <p className="text-xs text-primary-500">
                                Click on a calendar day to view its bookings
                            </p>
                        </div>
                    ) : selectedDayEvents.length === 0 ? (
                        <div className="text-center py-12">
                            <div className="w-14 h-14 rounded-full bg-sand-100 flex items-center justify-center mx-auto mb-3">
                                <Building2 className="w-7 h-7 text-primary-400" />
                            </div>
                            <p className="text-sm font-medium text-primary-700 mb-1">
                                No bookings
                            </p>
                            <p className="text-xs text-primary-500">
                                This day is free across all properties
                            </p>
                        </div>
                    ) : (
                        <div className="space-y-3">
                            <p className="text-xs text-primary-500 mb-2">
                                {selectedDayEvents.length} booking{selectedDayEvents.length > 1 ? 's' : ''}
                            </p>
                            {selectedDayEvents.map(event => (
                                <div
                                    key={event.id}
                                    className={`p-4 rounded-xl border ${STATUS_COLORS[event.status] || STATUS_COLORS.pending}`}
                                >
                                    <div className="flex items-center gap-3 mb-2">
                                        <div className="w-8 h-8 rounded-full bg-white/50 flex items-center justify-center text-xs font-semibold">
                                            {(event.guest_name || 'G').charAt(0).toUpperCase()}
                                        </div>
                                        <div className="flex-1 min-w-0">
                                            <span className="text-sm font-medium block truncate">{event.guest_name || 'Guest'}</span>
                                        </div>
                                        <span className="text-xs capitalize px-2 py-0.5 rounded-full bg-white/50 font-medium">
                                            {event.status}
                                        </span>
                                    </div>
                                    <div className="space-y-1.5 text-xs opacity-80">
                                        <div className="flex items-center gap-1.5">
                                            <MapPin className="w-3 h-3 flex-shrink-0" />
                                            <span className="truncate">{event.property_title}</span>
                                        </div>
                                        <div className="flex items-center gap-1.5">
                                            <Clock className="w-3 h-3 flex-shrink-0" />
                                            <span>{event.check_in?.split('T')[0]} → {event.check_out?.split('T')[0]}</span>
                                        </div>
                                        {event.num_guests && (
                                            <div className="flex items-center gap-1.5">
                                                <Users className="w-3 h-3 flex-shrink-0" />
                                                <span>{event.num_guests} guest{event.num_guests > 1 ? 's' : ''}</span>
                                            </div>
                                        )}
                                    </div>
                                </div>
                            ))}
                        </div>
                    )}
                </div>
            </div>
            )}
        </div>
    );
}
