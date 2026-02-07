import React, { Component, ReactNode } from 'react';
import { View, Text, TouchableOpacity } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { logError } from '@/utils/logger';

interface Props {
  children: ReactNode;
  fallback?: ReactNode;
}

interface State {
  hasError: boolean;
  error: Error | null;
}

export class ErrorBoundary extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      hasError: false,
      error: null,
    };
  }

  static getDerivedStateFromError(error: Error): State {
    return {
      hasError: true,
      error,
    };
  }

  componentDidCatch(error: Error, errorInfo: React.ErrorInfo) {
    // Use centralized logger instead of console.error
    logError('ErrorBoundary caught an error', error, {
      componentStack: errorInfo.componentStack,
      errorInfo,
    });
  }

  handleReset = () => {
    this.setState({
      hasError: false,
      error: null,
    });
  };

  render() {
    if (this.state.hasError) {
      if (this.props.fallback) {
        return this.props.fallback;
      }

      return (
        <View className="flex-1 items-center justify-center bg-white px-6">
          <Ionicons name="warning-outline" size={64} color="#EF4444" />
          <Text className="text-xl font-bold text-gray-800 mt-4 text-center">
            Oops! Something went wrong
          </Text>
          <Text className="text-gray-600 text-center mt-2 mb-6">
            We're sorry for the inconvenience. Please try again.
          </Text>
          {__DEV__ && this.state.error && (
            <View className="bg-red-50 p-4 rounded-lg mb-4 max-w-full">
              <Text className="text-xs text-red-800 font-mono">
                {this.state.error.message}
              </Text>
            </View>
          )}
          <TouchableOpacity
            className="bg-primary-600 px-8 py-3 rounded-lg"
            onPress={this.handleReset}
          >
            <Text className="text-white font-semibold">Try Again</Text>
          </TouchableOpacity>
        </View>
      );
    }

    return this.props.children;
  }
}

export function ErrorMessage({ 
  message = 'Something went wrong', 
  onRetry 
}: { 
  message?: string; 
  onRetry?: () => void;
}) {
  return (
    <View className="flex-1 items-center justify-center bg-white px-6">
      <Ionicons name="alert-circle-outline" size={64} color="#EF4444" />
      <Text className="text-xl font-bold text-gray-800 mt-4 text-center">
        Error
      </Text>
      <Text className="text-gray-600 text-center mt-2 mb-6">
        {message}
      </Text>
      {onRetry && (
        <TouchableOpacity
          className="bg-primary-600 px-8 py-3 rounded-lg"
          onPress={onRetry}
        >
          <Text className="text-white font-semibold">Retry</Text>
        </TouchableOpacity>
      )}
    </View>
  );
}
