import React, { useState } from 'react';
import {
  View,
  Text,
  ScrollView,
  TouchableOpacity,
  TextInput,
  Alert,
  ActivityIndicator,
  Platform,
} from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { LinearGradient } from 'expo-linear-gradient';
import { DocumentUpload } from './DocumentUpload';
import { SelfieCapture } from './SelfieCapture';
import { GlassmorphicView } from '../common/GlassmorphicView';
import { useRouter } from 'expo-router';
import { apiClient } from '@/services/api-client';
import { logError, logInfo, logApiError } from '@/utils/logger';

type WizardStep = 'document-info' | 'document-upload' | 'selfie' | 'review';

interface VerificationData {
  documentType: 'PASSPORT' | 'NATIONAL_ID' | 'DRIVERS_LICENSE';
  documentNumber: string;
  issuedCountry: string;
  expiryDate: string;
  frontImageUrl: string;
  backImageUrl?: string;
  selfieUrl: string;
}

const COUNTRIES = [
  'United States',
  'United Kingdom',
  'Canada',
  'Australia',
  'South Africa',
  'Nigeria',
  'Kenya',
  'Ghana',
  'Uganda',
  'Tanzania',
  'Zimbabwe',
  'Botswana',
  'Namibia',
  'Zambia',
  'Mozambique',
];

export function VerificationWizard() {
  const router = useRouter();
  const [currentStep, setCurrentStep] = useState<WizardStep>('document-info');
  const [isSubmitting, setIsSubmitting] = useState(false);
  
  const [data, setData] = useState<Partial<VerificationData>>({
    documentType: 'PASSPORT',
    documentNumber: '',
    issuedCountry: '',
    expiryDate: '',
    frontImageUrl: '',
    backImageUrl: '',
    selfieUrl: '',
  });

  const steps: { id: WizardStep; label: string; icon: keyof typeof Ionicons.glyphMap }[] = [
    { id: 'document-info', label: 'Info', icon: 'document-text' },
    { id: 'document-upload', label: 'ID', icon: 'card' },
    { id: 'selfie', label: 'Selfie', icon: 'person' },
    { id: 'review', label: 'Review', icon: 'checkmark-circle' },
  ];

  const currentStepIndex = steps.findIndex((s) => s.id === currentStep);
  const requiresBackImage =
    data.documentType === 'NATIONAL_ID' || data.documentType === 'DRIVERS_LICENSE';

  const canProceedFromInfo =
    data.documentType &&
    data.documentNumber &&
    data.documentNumber.trim().length > 0 &&
    data.issuedCountry &&
    data.issuedCountry.length > 0;

  const canProceedFromUpload =
    data.frontImageUrl && (!requiresBackImage || data.backImageUrl);

  const canProceedFromSelfie = data.selfieUrl && data.selfieUrl.length > 0;

  const handleNext = () => {
    if (currentStep === 'document-info' && canProceedFromInfo) {
      setCurrentStep('document-upload');
    } else if (currentStep === 'document-upload' && canProceedFromUpload) {
      setCurrentStep('selfie');
    } else if (currentStep === 'selfie' && canProceedFromSelfie) {
      setCurrentStep('review');
    }
  };

  const handleBack = () => {
    if (currentStep === 'document-upload') {
      setCurrentStep('document-info');
    } else if (currentStep === 'selfie') {
      setCurrentStep('document-upload');
    } else if (currentStep === 'review') {
      setCurrentStep('selfie');
    }
  };

  const handleSubmit = async () => {
    try {
      setIsSubmitting(true);

      // Submit verification data to API
      const verificationData = {
        document_type: data.documentType,
        document_number: data.documentNumber,
        issued_country: data.issuedCountry,
        expiry_date: data.expiryDate || null,
        front_image_url: data.frontImageUrl,
        back_image_url: data.backImageUrl || null,
        selfie_url: data.selfieUrl,
      };

      await apiClient.post('/users/verification/', verificationData);

      logInfo('Verification submitted successfully');
      
      Alert.alert(
        'Success!',
        "Your verification has been submitted. We'll review your documents within 1-2 business days.",
        [
          {
            text: 'OK',
            onPress: () => router.back(),
          },
        ]
      );
    } catch (error: any) {
      logApiError('/users/verification/', error, { action: 'submit verification' });
      const errorMessage = error?.response?.data?.detail || 
                          error?.response?.data?.message || 
                          'Failed to submit verification. Please try again.';
      Alert.alert('Error', errorMessage);
    } finally {
      setIsSubmitting(false);
    }
  };

  const renderProgressBar = () => (
    <View className="px-4 mb-6">
      <View className="flex-row items-center justify-between">
        {steps.map((step, index) => (
          <View key={step.id} className="flex-row items-center flex-1">
            <View className="items-center">
              <View
                className={`w-10 h-10 rounded-full items-center justify-center ${
                  index <= currentStepIndex
                    ? 'bg-gold'
                    : 'bg-sand-200'
                }`}
                style={
                  index === currentStepIndex
                    ? {
                        shadowColor: '#D9B168',
                        shadowOffset: { width: 0, height: 2 },
                        shadowOpacity: 0.3,
                        shadowRadius: 4,
                        elevation: 3,
                      }
                    : undefined
                }
              >
                {index < currentStepIndex ? (
                  <Ionicons name="checkmark" size={20} color="#122F26" />
                ) : (
                  <Ionicons
                    name={step.icon}
                    size={20}
                    color={index === currentStepIndex ? '#122F26' : '#3A5C50'}
                  />
                )}
              </View>
              <Text
                className={`text-xs mt-1 ${
                  index <= currentStepIndex ? 'text-forest font-semibold' : 'text-moss'
                }`}
              >
                {step.label}
              </Text>
            </View>
            {index < steps.length - 1 && (
              <View
                className={`flex-1 h-0.5 mx-2 ${
                  index < currentStepIndex ? 'bg-gold' : 'bg-sand-200'
                }`}
              />
            )}
          </View>
        ))}
      </View>
    </View>
  );

  const renderDocumentInfo = () => (
    <View className="px-4">
      <Text className="text-2xl font-bold text-forest mb-2">Document Information</Text>
      <Text className="text-moss mb-6">
        Enter your government-issued ID details
      </Text>

      {/* Document Type */}
      <View className="mb-4">
        <Text className="text-sm font-semibold text-forest mb-2">Document Type *</Text>
        <View className="flex-row gap-2">
          {[
            { value: 'PASSPORT', label: 'Passport' },
            { value: 'NATIONAL_ID', label: 'National ID' },
            { value: 'DRIVERS_LICENSE', label: "Driver's License" },
          ].map((type) => (
            <TouchableOpacity
              key={type.value}
              onPress={() => setData({ ...data, documentType: type.value as any })}
              className={`flex-1 py-3 px-3 rounded-xl border-2 ${
                data.documentType === type.value
                  ? 'border-gold bg-gold/10'
                  : 'border-sand-200 bg-white'
              }`}
            >
              <Text
                className={`text-center text-sm font-semibold ${
                  data.documentType === type.value ? 'text-forest' : 'text-moss'
                }`}
              >
                {type.label}
              </Text>
            </TouchableOpacity>
          ))}
        </View>
      </View>

      {/* Document Number */}
      <View className="mb-4">
        <Text className="text-sm font-semibold text-forest mb-2">Document Number *</Text>
        <TextInput
          value={data.documentNumber}
          onChangeText={(text) => setData({ ...data, documentNumber: text })}
          placeholder="Enter document number"
          placeholderTextColor="#94a3b8"
          className="bg-white border-2 border-sand-200 rounded-xl px-4 py-3 text-forest"
          style={{
            fontSize: 16,
          }}
        />
      </View>

      {/* Country & Expiry */}
      <View className="mb-4">
        <Text className="text-sm font-semibold text-forest mb-2">Issued Country *</Text>
        <View className="bg-white border-2 border-sand-200 rounded-xl">
          <TextInput
            value={data.issuedCountry}
            onChangeText={(text) => setData({ ...data, issuedCountry: text })}
            placeholder="Select or type country"
            placeholderTextColor="#94a3b8"
            className="px-4 py-3 text-forest"
            style={{
              fontSize: 16,
            }}
          />
        </View>
      </View>

      <View className="mb-4">
        <Text className="text-sm font-semibold text-forest mb-2">
          Expiry Date (Optional)
        </Text>
        <TextInput
          value={data.expiryDate}
          onChangeText={(text) => setData({ ...data, expiryDate: text })}
          placeholder="YYYY-MM-DD"
          placeholderTextColor="#94a3b8"
          className="bg-white border-2 border-sand-200 rounded-xl px-4 py-3 text-forest"
          style={{
            fontSize: 16,
          }}
        />
      </View>
    </View>
  );

  const renderDocumentUpload = () => (
    <View className="px-4">
      <Text className="text-2xl font-bold text-forest mb-2">Upload Your ID</Text>
      <Text className="text-moss mb-6">
        Take clear photos of your {data.documentType?.toLowerCase().replace('_', ' ')}
      </Text>

      <DocumentUpload
        documentType={data.documentType!}
        frontImageUrl={data.frontImageUrl}
        backImageUrl={data.backImageUrl}
        onUploadComplete={(front, back) => {
          setData({ ...data, frontImageUrl: front, backImageUrl: back });
        }}
      />
    </View>
  );

  const renderSelfie = () => (
    <View className="px-4">
      <Text className="text-2xl font-bold text-forest mb-2">Take a Selfie</Text>
      <Text className="text-moss mb-6">
        We'll use this to verify you're the person in your ID
      </Text>

      <SelfieCapture
        selfieUrl={data.selfieUrl}
        onCaptureComplete={(url) => setData({ ...data, selfieUrl: url })}
      />
    </View>
  );

  const renderReview = () => (
    <View className="px-4">
      <Text className="text-2xl font-bold text-forest mb-2">Review & Submit</Text>
      <Text className="text-moss mb-6">
        Please review your information before submitting
      </Text>

      <View className="bg-white rounded-2xl p-4 mb-4"
        style={{
          shadowColor: '#122F26',
          shadowOffset: { width: 0, height: 2 },
          shadowOpacity: 0.05,
          shadowRadius: 4,
          elevation: 2,
        }}
      >
        <Text className="text-base font-bold text-forest mb-3">Document Information</Text>
        <View className="space-y-2">
          <View className="flex-row justify-between py-2 border-b border-sand-100">
            <Text className="text-moss">Type:</Text>
            <Text className="text-forest font-semibold">
              {data.documentType?.replace('_', ' ')}
            </Text>
          </View>
          <View className="flex-row justify-between py-2 border-b border-sand-100">
            <Text className="text-moss">Number:</Text>
            <Text className="text-forest font-semibold">{data.documentNumber}</Text>
          </View>
          <View className="flex-row justify-between py-2 border-b border-sand-100">
            <Text className="text-moss">Country:</Text>
            <Text className="text-forest font-semibold">{data.issuedCountry}</Text>
          </View>
          {data.expiryDate && (
            <View className="flex-row justify-between py-2">
              <Text className="text-moss">Expiry:</Text>
              <Text className="text-forest font-semibold">{data.expiryDate}</Text>
            </View>
          )}
        </View>
      </View>

      <View className="bg-blue-50 rounded-xl p-4">
        <View className="flex-row items-start">
          <View className="bg-blue-100 rounded-full p-2 mr-3">
            <Ionicons name="information-circle" size={20} color="#3B82F6" />
          </View>
          <Text className="flex-1 text-blue-800 text-sm">
            By submitting, you confirm that all information provided is accurate and that you are
            the person shown in the documents.
          </Text>
        </View>
      </View>
    </View>
  );

  const renderNavigationButtons = () => (
    <View className="flex-row justify-between px-4 pb-6 pt-4">
      <TouchableOpacity
        onPress={handleBack}
        disabled={currentStep === 'document-info'}
        className={`flex-row items-center px-6 py-3 rounded-xl ${
          currentStep === 'document-info' ? 'opacity-30' : ''
        }`}
        style={{
          backgroundColor: '#fff',
          borderWidth: 2,
          borderColor: '#f4f1ea',
        }}
      >
        <Ionicons name="chevron-back" size={20} color="#122F26" />
        <Text className="text-forest font-semibold ml-2">Back</Text>
      </TouchableOpacity>

      {currentStep !== 'review' ? (
        <TouchableOpacity
          onPress={handleNext}
          disabled={
            (currentStep === 'document-info' && !canProceedFromInfo) ||
            (currentStep === 'document-upload' && !canProceedFromUpload) ||
            (currentStep === 'selfie' && !canProceedFromSelfie)
          }
          className={`flex-row items-center px-6 py-3 rounded-xl ${
            ((currentStep === 'document-info' && !canProceedFromInfo) ||
              (currentStep === 'document-upload' && !canProceedFromUpload) ||
              (currentStep === 'selfie' && !canProceedFromSelfie))
              ? 'opacity-30'
              : ''
          }`}
        >
          <LinearGradient
            colors={['#D9B168', '#bea04f']}
            className="flex-row items-center px-6 py-3 rounded-xl"
            style={{
              shadowColor: '#D9B168',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.3,
              shadowRadius: 4,
              elevation: 3,
            }}
          >
            <Text className="text-forest font-bold mr-2">Next</Text>
            <Ionicons name="chevron-forward" size={20} color="#122F26" />
          </LinearGradient>
        </TouchableOpacity>
      ) : (
        <TouchableOpacity
          onPress={handleSubmit}
          disabled={isSubmitting}
          className={isSubmitting ? 'opacity-50' : ''}
        >
          <LinearGradient
            colors={['#10B981', '#059669']}
            className="flex-row items-center px-6 py-3 rounded-xl"
            style={{
              shadowColor: '#10B981',
              shadowOffset: { width: 0, height: 2 },
              shadowOpacity: 0.3,
              shadowRadius: 4,
              elevation: 3,
            }}
          >
            {isSubmitting ? (
              <>
                <ActivityIndicator color="#fff" size="small" />
                <Text className="text-white font-bold ml-2">Submitting...</Text>
              </>
            ) : (
              <>
                <Ionicons name="checkmark-circle" size={20} color="#fff" />
                <Text className="text-white font-bold ml-2">Submit</Text>
              </>
            )}
          </LinearGradient>
        </TouchableOpacity>
      )}
    </View>
  );

  return (
    <View className="flex-1 bg-sand-100">
      <ScrollView className="flex-1" showsVerticalScrollIndicator={false}>
        <LinearGradient
          colors={['#122F26', '#1d392f', '#2d4a40']}
          start={{ x: 0, y: 0 }}
          end={{ x: 1, y: 1 }}
          className="pb-8"
          style={{ paddingTop: Platform.OS === 'ios' ? 50 : 35 }}
        >
          <View className="px-4 mb-4">
            <TouchableOpacity
              onPress={() => router.back()}
              className="w-10 h-10 rounded-xl items-center justify-center mb-4"
              style={{ backgroundColor: 'rgba(255, 255, 255, 0.15)' }}
            >
              <Ionicons name="close" size={24} color="#fff" />
            </TouchableOpacity>
            <Text className="text-3xl font-black text-white tracking-tight">
              Identity Verification
            </Text>
            <Text className="text-sand-200 mt-1">
              Complete all steps to verify your identity
            </Text>
          </View>

          {renderProgressBar()}
        </LinearGradient>

        <View className="py-6">
          {currentStep === 'document-info' && renderDocumentInfo()}
          {currentStep === 'document-upload' && renderDocumentUpload()}
          {currentStep === 'selfie' && renderSelfie()}
          {currentStep === 'review' && renderReview()}
        </View>
      </ScrollView>

      {renderNavigationButtons()}
    </View>
  );
}
