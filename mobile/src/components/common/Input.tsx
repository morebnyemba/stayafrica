import { View, Text, TextInput, TextInputProps, TouchableOpacity } from 'react-native';
import { useState } from 'react';
import { Ionicons } from '@expo/vector-icons';
import Animated, { useAnimatedStyle, withSpring, withTiming } from 'react-native-reanimated';

interface InputProps extends TextInputProps {
  label?: string;
  icon?: keyof typeof Ionicons.glyphMap;
  error?: string;
  helperText?: string;
  required?: boolean;
  containerClassName?: string;
  secureTextEntry?: boolean;
  showPasswordToggle?: boolean;
}

export function Input({
  label,
  icon,
  error,
  helperText,
  required = false,
  containerClassName = '',
  secureTextEntry = false,
  showPasswordToggle = false,
  ...textInputProps
}: InputProps) {
  const [isFocused, setIsFocused] = useState(false);
  const [isPasswordVisible, setIsPasswordVisible] = useState(false);

  const isSecure = secureTextEntry && !isPasswordVisible;

  const borderColor = error
    ? '#EF4444'
    : isFocused
    ? '#D9B168'
    : '#E5DFD0';

  return (
    <View className={`mb-5 ${containerClassName}`}>
      {/* Label */}
      {label && (
        <View className="flex-row items-center mb-2 ml-1">
          <Text className="text-sm font-semibold text-forest">
            {label}
          </Text>
          {required && (
            <Text className="text-red-500 ml-1 text-sm">*</Text>
          )}
        </View>
      )}

      {/* Input Container */}
      <View
        className="flex-row items-center rounded-xl overflow-hidden bg-sand-50 px-4 border-2"
        style={{ borderColor }}
      >
        {/* Leading Icon */}
        {icon && (
          <Ionicons
            name={icon}
            size={20}
            color={error ? '#EF4444' : isFocused ? '#D9B168' : '#94a3b8'}
            style={{ marginRight: 12 }}
          />
        )}

        {/* Text Input */}
        <TextInput
          className="flex-1 py-4 text-base text-forest"
          placeholderTextColor="#94a3b8"
          secureTextEntry={isSecure}
          onFocus={() => setIsFocused(true)}
          onBlur={() => setIsFocused(false)}
          {...textInputProps}
        />

        {/* Password Toggle */}
        {showPasswordToggle && secureTextEntry && (
          <TouchableOpacity
            onPress={() => setIsPasswordVisible(!isPasswordVisible)}
            hitSlop={{ top: 10, bottom: 10, left: 10, right: 10 }}
          >
            <Ionicons
              name={isPasswordVisible ? 'eye-off-outline' : 'eye-outline'}
              size={20}
              color="#94a3b8"
            />
          </TouchableOpacity>
        )}

        {/* Success Icon */}
        {!error && textInputProps.value && textInputProps.value.length > 0 && !secureTextEntry && (
          <Ionicons
            name="checkmark-circle"
            size={20}
            color="#10B981"
            style={{ marginLeft: 8 }}
          />
        )}
      </View>

      {/* Error or Helper Text */}
      {(error || helperText) && (
        <View className="flex-row items-center mt-2 ml-1">
          {error && (
            <Ionicons name="alert-circle" size={14} color="#EF4444" style={{ marginRight: 6 }} />
          )}
          <Text className={`text-xs ${error ? 'text-red-600' : 'text-moss'}`}>
            {error || helperText}
          </Text>
        </View>
      )}
    </View>
  );
}
