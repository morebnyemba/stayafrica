import React, { useState } from 'react';
import { View, Text, TouchableOpacity, Image, Alert } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import * as ImagePicker from 'expo-image-picker';
import { LinearGradient } from 'expo-linear-gradient';

interface SelfieCaptureProps {
  onCaptureComplete: (imageUrl: string) => void;
  selfieUrl?: string;
}

export function SelfieCapture({ onCaptureComplete, selfieUrl }: SelfieCaptureProps) {
  const [selfieImage, setSelfieImage] = useState<string | undefined>(selfieUrl);
  const [isCapturing, setIsCapturing] = useState(false);

  const requestPermissions = async () => {
    const { status } = await ImagePicker.requestCameraPermissionsAsync();
    if (status !== 'granted') {
      Alert.alert(
        'Permission Required',
        'Camera access is required to take your selfie.',
        [{ text: 'OK' }]
      );
      return false;
    }
    return true;
  };

  const captureSelfie = async () => {
    try {
      setIsCapturing(true);

      Alert.alert(
        'Select Image',
        'Choose how you want to provide your selfie',
        [
          {
            text: 'Take Selfie',
            onPress: async () => {
              const hasPermission = await requestPermissions();
              if (!hasPermission) {
                setIsCapturing(false);
                return;
              }

              const result = await ImagePicker.launchCameraAsync({
                mediaTypes: ImagePicker.MediaTypeOptions.Images,
                allowsEditing: true,
                aspect: [3, 4],
                quality: 0.8,
                cameraType: ImagePicker.CameraType.front,
              });

              if (!result.canceled && result.assets[0]) {
                const uri = result.assets[0].uri;
                setSelfieImage(uri);
                onCaptureComplete(uri);
              }
              setIsCapturing(false);
            },
          },
          {
            text: 'Choose from Library',
            onPress: async () => {
              const result = await ImagePicker.launchImageLibraryAsync({
                mediaTypes: ImagePicker.MediaTypeOptions.Images,
                allowsEditing: true,
                aspect: [3, 4],
                quality: 0.8,
              });

              if (!result.canceled && result.assets[0]) {
                const uri = result.assets[0].uri;
                setSelfieImage(uri);
                onCaptureComplete(uri);
              }
              setIsCapturing(false);
            },
          },
          {
            text: 'Cancel',
            style: 'cancel',
            onPress: () => setIsCapturing(false),
          },
        ]
      );
    } catch (error) {
      console.error('Error capturing selfie:', error);
      Alert.alert('Error', 'Failed to capture selfie. Please try again.');
      setIsCapturing(false);
    }
  };

  return (
    <View>
      <View className="bg-blue-50 rounded-xl p-4 mb-4">
        <View className="flex-row items-start">
          <View className="bg-blue-100 rounded-full p-2 mr-3">
            <Ionicons name="information-circle" size={20} color="#3B82F6" />
          </View>
          <View className="flex-1">
            <Text className="text-blue-900 font-semibold text-sm mb-1">Selfie guidelines:</Text>
            <Text className="text-blue-800 text-xs">
              • Look directly at the camera{'\n'}
              • Remove glasses or hats{'\n'}
              • Good lighting on your face{'\n'}
              • Keep a neutral expression{'\n'}
              • Face should match your ID photo
            </Text>
          </View>
        </View>
      </View>

      <TouchableOpacity
        onPress={captureSelfie}
        disabled={isCapturing}
        className="aspect-[3/4] rounded-2xl overflow-hidden border-2 border-dashed"
        style={{
          borderColor: selfieImage ? '#10B981' : '#D9B168',
          backgroundColor: selfieImage ? 'transparent' : '#f4f1ea',
        }}
      >
        {selfieImage ? (
          <View className="relative w-full h-full">
            <Image source={{ uri: selfieImage }} className="w-full h-full" resizeMode="cover" />
            <View
              className="absolute top-2 right-2 w-8 h-8 rounded-full items-center justify-center"
              style={{ backgroundColor: 'rgba(16, 185, 129, 0.9)' }}
            >
              <Ionicons name="checkmark" size={20} color="#fff" />
            </View>
            <TouchableOpacity
              onPress={captureSelfie}
              className="absolute bottom-4 self-center bg-white rounded-full px-4 py-3"
              style={{
                shadowColor: '#000',
                shadowOffset: { width: 0, height: 2 },
                shadowOpacity: 0.15,
                shadowRadius: 4,
                elevation: 3,
              }}
            >
              <Text className="text-forest font-semibold">Retake Selfie</Text>
            </TouchableOpacity>
          </View>
        ) : (
          <View className="flex-1 items-center justify-center p-4">
            <View className="bg-gold/20 rounded-full p-6 mb-4">
              <Ionicons name="person-outline" size={48} color="#D9B168" />
            </View>
            <Text className="text-forest font-bold text-lg text-center mb-2">
              Take a Selfie
            </Text>
            <Text className="text-moss text-sm text-center mb-4">
              We'll use this to verify your identity
            </Text>
            <View className="bg-gold/10 rounded-full px-4 py-2">
              <Text className="text-gold font-semibold text-sm">Tap to Capture</Text>
            </View>
          </View>
        )}
      </TouchableOpacity>
    </View>
  );
}
