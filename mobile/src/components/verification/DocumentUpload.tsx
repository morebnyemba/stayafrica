import React, { useState } from 'react';
import { View, Text, TouchableOpacity, Image, Alert, Platform } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import * as ImagePicker from 'expo-image-picker';

interface DocumentUploadProps {
  documentType: 'PASSPORT' | 'NATIONAL_ID' | 'DRIVERS_LICENSE';
  onUploadComplete: (frontUrl: string, backUrl?: string) => void;
  frontImageUrl?: string;
  backImageUrl?: string;
}

export function DocumentUpload({
  documentType,
  onUploadComplete,
  frontImageUrl,
  backImageUrl,
}: DocumentUploadProps) {
  const [frontImage, setFrontImage] = useState<string | undefined>(frontImageUrl);
  const [backImage, setBackImage] = useState<string | undefined>(backImageUrl);
  const [isUploading, setIsUploading] = useState(false);

  const requiresBackImage = documentType === 'NATIONAL_ID' || documentType === 'DRIVERS_LICENSE';

  const requestPermissions = async () => {
    const { status } = await ImagePicker.requestCameraPermissionsAsync();
    if (status !== 'granted') {
      Alert.alert(
        'Permission Required',
        'Camera access is required to take photos of your documents.',
        [{ text: 'OK' }]
      );
      return false;
    }
    return true;
  };

  const pickImage = async (side: 'front' | 'back') => {
    try {
      setIsUploading(true);

      Alert.alert(
        'Select Image',
        'Choose how you want to provide your document image',
        [
          {
            text: 'Take Photo',
            onPress: async () => {
              const hasPermission = await requestPermissions();
              if (!hasPermission) {
                setIsUploading(false);
                return;
              }

              const result = await ImagePicker.launchCameraAsync({
                mediaTypes: ImagePicker.MediaTypeOptions.Images,
                allowsEditing: true,
                aspect: [4, 3],
                quality: 0.8,
              });

              if (!result.canceled && result.assets[0]) {
                const uri = result.assets[0].uri;
                if (side === 'front') {
                  setFrontImage(uri);
                  onUploadComplete(uri, backImage);
                } else {
                  setBackImage(uri);
                  onUploadComplete(frontImage!, uri);
                }
              }
              setIsUploading(false);
            },
          },
          {
            text: 'Choose from Library',
            onPress: async () => {
              const result = await ImagePicker.launchImageLibraryAsync({
                mediaTypes: ImagePicker.MediaTypeOptions.Images,
                allowsEditing: true,
                aspect: [4, 3],
                quality: 0.8,
              });

              if (!result.canceled && result.assets[0]) {
                const uri = result.assets[0].uri;
                if (side === 'front') {
                  setFrontImage(uri);
                  onUploadComplete(uri, backImage);
                } else {
                  setBackImage(uri);
                  onUploadComplete(frontImage!, uri);
                }
              }
              setIsUploading(false);
            },
          },
          {
            text: 'Cancel',
            style: 'cancel',
            onPress: () => setIsUploading(false),
          },
        ]
      );
    } catch (error) {
      console.error('Error picking image:', error);
      Alert.alert('Error', 'Failed to pick image. Please try again.');
      setIsUploading(false);
    }
  };

  const UploadBox = ({ side, image }: { side: 'front' | 'back'; image?: string }) => (
    <View className="flex-1">
      <Text className="text-sm font-semibold text-forest mb-2">
        {side === 'front' ? 'Front Side' : 'Back Side'} *
      </Text>
      <TouchableOpacity
        onPress={() => pickImage(side)}
        disabled={isUploading}
        className="aspect-[4/3] rounded-2xl overflow-hidden border-2 border-dashed"
        style={{
          borderColor: image ? '#10B981' : '#D9B168',
          backgroundColor: image ? 'transparent' : '#f4f1ea',
        }}
      >
        {image ? (
          <View className="relative w-full h-full">
            <Image source={{ uri: image }} className="w-full h-full" resizeMode="cover" />
            <View
              className="absolute top-2 right-2 w-8 h-8 rounded-full items-center justify-center"
              style={{ backgroundColor: 'rgba(16, 185, 129, 0.9)' }}
            >
              <Ionicons name="checkmark" size={20} color="#fff" />
            </View>
            <TouchableOpacity
              onPress={() => pickImage(side)}
              className="absolute bottom-2 right-2 bg-white rounded-full px-3 py-2"
              style={{
                shadowColor: '#000',
                shadowOffset: { width: 0, height: 2 },
                shadowOpacity: 0.15,
                shadowRadius: 4,
                elevation: 3,
              }}
            >
              <Text className="text-forest text-xs font-semibold">Change</Text>
            </TouchableOpacity>
          </View>
        ) : (
          <View className="flex-1 items-center justify-center p-4">
            <View className="bg-gold/20 rounded-full p-4 mb-3">
              <Ionicons name="camera-outline" size={32} color="#D9B168" />
            </View>
            <Text className="text-forest font-semibold text-center mb-1">
              Upload {side === 'front' ? 'Front' : 'Back'}
            </Text>
            <Text className="text-moss text-xs text-center">
              Tap to take photo or choose from library
            </Text>
          </View>
        )}
      </TouchableOpacity>
    </View>
  );

  return (
    <View>
      <View className="bg-blue-50 rounded-xl p-4 mb-4">
        <View className="flex-row items-start">
          <View className="bg-blue-100 rounded-full p-2 mr-3">
            <Ionicons name="information-circle" size={20} color="#3B82F6" />
          </View>
          <View className="flex-1">
            <Text className="text-blue-900 font-semibold text-sm mb-1">Tips for best results:</Text>
            <Text className="text-blue-800 text-xs">
              • Ensure good lighting{'\n'}
              • Keep document flat and in focus{'\n'}
              • All corners should be visible{'\n'}
              • No glare or shadows
            </Text>
          </View>
        </View>
      </View>

      {requiresBackImage ? (
        <View className="flex-row gap-3">
          <UploadBox side="front" image={frontImage} />
          <UploadBox side="back" image={backImage} />
        </View>
      ) : (
        <UploadBox side="front" image={frontImage} />
      )}
    </View>
  );
}
