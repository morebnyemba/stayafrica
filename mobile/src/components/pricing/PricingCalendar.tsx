import { View, Text, TouchableOpacity, ActivityIndicator, ScrollView } from 'react-native';
import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import pricingApi from '@/services/pricing-api';
import { PricingCalendarDay } from '@/types/pricing-types';

interface PricingCalendarProps {
  propertyId: string;
}

export default function PricingCalendar({ propertyId }: PricingCalendarProps) {
  const [currentDate, setCurrentDate] = useState(new Date());
  const month = currentDate.getMonth() + 1;
  const year = currentDate.getFullYear();

  const { data: calendar, isLoading } = useQuery({
    queryKey: ['pricing-calendar', propertyId, month, year],
    queryFn: () => pricingApi.getPricingCalendar(propertyId, month, year),
  });

  const navigateMonth = (direction: 'prev' | 'next') => {
    const newDate = new Date(currentDate);
    if (direction === 'prev') {
      newDate.setMonth(newDate.getMonth() - 1);
    } else {
      newDate.setMonth(newDate.getMonth() + 1);
    }
    setCurrentDate(newDate);
  };

  const getPriceChangeIcon = (day: PricingCalendarDay) => {
    const change = ((day.dynamic_price - day.base_price) / day.base_price) * 100;
    if (change > 5) return 'trending-up';
    if (change < -5) return 'trending-down';
    return 'remove';
  };

  const getPriceChangeColor = (day: PricingCalendarDay) => {
    const change = ((day.dynamic_price - day.base_price) / day.base_price) * 100;
    if (change > 5) return { bg: '#DEF7EC', border: '#84E1BC', text: '#065F46', icon: '#10B981' };
    if (change < -5) return { bg: '#FEE2E2', border: '#FCA5A5', text: '#991B1B', icon: '#EF4444' };
    return { bg: '#F3F4F6', border: '#D1D5DB', text: '#374151', icon: '#6B7280' };
  };

  if (isLoading) {
    return (
      <View className="bg-white rounded-2xl p-6" style={{
        shadowColor: '#122F26',
        shadowOffset: { width: 0, height: 4 },
        shadowOpacity: 0.08,
        shadowRadius: 8,
        elevation: 4,
      }}>
        <ActivityIndicator size="large" color="#3A5C50" />
        <Text className="text-center text-moss mt-4">Loading calendar...</Text>
      </View>
    );
  }

  if (!calendar) return null;

  const weekDays = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];

  return (
    <View className="bg-white rounded-2xl p-4" style={{
      shadowColor: '#122F26',
      shadowOffset: { width: 0, height: 4 },
      shadowOpacity: 0.08,
      shadowRadius: 8,
      elevation: 4,
    }}>
      {/* Calendar Header */}
      <View className="flex-row items-center justify-between mb-4">
        <Text className="text-lg font-bold text-forest">
          {currentDate.toLocaleString('default', { month: 'long', year: 'numeric' })}
        </Text>
        <View className="flex-row gap-2">
          <TouchableOpacity
            onPress={() => navigateMonth('prev')}
            className="bg-sand-100 rounded-lg p-2"
          >
            <Ionicons name="chevron-back" size={20} color="#3A5C50" />
          </TouchableOpacity>
          <TouchableOpacity
            onPress={() => navigateMonth('next')}
            className="bg-sand-100 rounded-lg p-2"
          >
            <Ionicons name="chevron-forward" size={20} color="#3A5C50" />
          </TouchableOpacity>
        </View>
      </View>

      {/* Price Summary */}
      <View className="flex-row gap-2 mb-4">
        <LinearGradient
          colors={['#DBEAFE', '#BFDBFE']}
          className="flex-1 rounded-xl p-3"
        >
          <Text className="text-xs text-blue-700 font-medium">Min</Text>
          <Text className="text-xl font-bold text-blue-900">${calendar.min_price}</Text>
        </LinearGradient>
        <LinearGradient
          colors={['#E9D5FF', '#D8B4FE']}
          className="flex-1 rounded-xl p-3"
        >
          <Text className="text-xs text-purple-700 font-medium">Avg</Text>
          <Text className="text-xl font-bold text-purple-900">${calendar.avg_price}</Text>
        </LinearGradient>
        <LinearGradient
          colors={['#D1FAE5', '#A7F3D0']}
          className="flex-1 rounded-xl p-3"
        >
          <Text className="text-xs text-green-700 font-medium">Max</Text>
          <Text className="text-xl font-bold text-green-900">${calendar.max_price}</Text>
        </LinearGradient>
      </View>

      {/* Week Days */}
      <View className="flex-row mb-2">
        {weekDays.map((day) => (
          <View key={day} className="flex-1 items-center">
            <Text className="text-xs font-semibold text-moss">{day}</Text>
          </View>
        ))}
      </View>

      {/* Calendar Grid */}
      <ScrollView showsVerticalScrollIndicator={false} style={{ maxHeight: 400 }}>
        <View className="flex-row flex-wrap">
          {calendar.calendar.map((day: PricingCalendarDay, index: number) => {
            const date = new Date(day.date);
            const isToday = date.toDateString() === new Date().toDateString();
            const colors = getPriceChangeColor(day);
            const icon = getPriceChangeIcon(day);
            const dayWidth = (100 / 7).toFixed(2);

            return (
              <TouchableOpacity
                key={index}
                disabled={!day.is_available}
                className="p-1"
                style={{ 
                  width: `${dayWidth}%`,
                  opacity: day.is_available ? 1 : 0.5,
                }}
              >
                <View
                  className="rounded-lg p-2 min-h-[70px]"
                  style={{
                    backgroundColor: colors.bg,
                    borderWidth: isToday ? 2 : 1,
                    borderColor: isToday ? '#3B82F6' : colors.border,
                  }}
                >
                  <View className="flex-row items-center justify-between mb-1">
                    <Text className="text-xs font-bold" style={{ color: colors.text }}>
                      {date.getDate()}
                    </Text>
                    <Ionicons name={icon as any} size={10} color={colors.icon} />
                  </View>
                  
                  {day.is_available ? (
                    <>
                      <Text className="text-sm font-bold" style={{ color: colors.text }}>
                        ${day.dynamic_price}
                      </Text>
                      {day.base_price !== day.dynamic_price && (
                        <Text className="text-xs line-through" style={{ color: colors.text, opacity: 0.6 }}>
                          ${day.base_price}
                        </Text>
                      )}
                      {day.applied_rules.length > 0 && (
                        <View className="mt-1">
                          <View className="bg-white rounded px-1" style={{ opacity: 0.7 }}>
                            <Text className="text-[8px]" style={{ color: colors.text }} numberOfLines={1}>
                              {day.applied_rules.length} rule{day.applied_rules.length > 1 ? 's' : ''}
                            </Text>
                          </View>
                        </View>
                      )}
                    </>
                  ) : (
                    <Text className="text-xs" style={{ color: colors.text }}>N/A</Text>
                  )}
                </View>
              </TouchableOpacity>
            );
          })}
        </View>
      </ScrollView>

      {/* Legend */}
      <View className="mt-4 flex-row flex-wrap gap-3">
        <View className="flex-row items-center gap-1">
          <Ionicons name="trending-up" size={14} color="#10B981" />
          <Text className="text-xs text-moss">Increased</Text>
        </View>
        <View className="flex-row items-center gap-1">
          <Ionicons name="trending-down" size={14} color="#EF4444" />
          <Text className="text-xs text-moss">Decreased</Text>
        </View>
        <View className="flex-row items-center gap-1">
          <Ionicons name="remove" size={14} color="#6B7280" />
          <Text className="text-xs text-moss">Base price</Text>
        </View>
      </View>
    </View>
  );
}
