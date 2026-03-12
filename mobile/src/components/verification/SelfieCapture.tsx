import { useState, useRef, useEffect, useCallback } from 'react';
import { View, Text, TouchableOpacity, Image, Alert, StyleSheet, Modal } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import * as ImagePicker from 'expo-image-picker';
import { LinearGradient } from 'expo-linear-gradient';
import { CameraView, useCameraPermissions, CameraCapturedPicture } from 'expo-camera';
import * as FaceDetector from 'expo-face-detector';

interface SelfieCaptureProps {
  onCaptureComplete: (imageUrl: string) => void;
  selfieUrl?: string;
}

type ChallengeStep = 'look-center' | 'turn-left' | 'turn-right' | 'done';

const CHALLENGE_TIMEOUT_MS = 12000;
const POLL_INTERVAL_MS = 400;
const YAW_THRESHOLD = 15;
const CENTER_FRAME_COUNT = 4; // consecutive center frames needed before challenge starts

export function SelfieCapture({ onCaptureComplete, selfieUrl }: SelfieCaptureProps) {
  const [selfieImage, setSelfieImage] = useState<string | undefined>(selfieUrl);
  const [cameraOpen, setCameraOpen] = useState(false);
  const [permission, requestPermission] = useCameraPermissions();

  // Challenge state
  const [challengeStep, setChallengeStep] = useState<ChallengeStep>('look-center');
  const [leftDone, setLeftDone] = useState(false);
  const [rightDone, setRightDone] = useState(false);
  const [timedOut, setTimedOut] = useState(false);
  const [cameraReady, setCameraReady] = useState(false);

  const cameraRef = useRef<CameraView>(null);
  const capturedRef = useRef(false);
  const pollingRef = useRef<ReturnType<typeof setInterval> | null>(null);
  const timeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const centerFrameCountRef = useRef(0);
  const challengeStepRef = useRef<ChallengeStep>('look-center');
  const leftDoneRef = useRef(false);
  const isPollingRef = useRef(false);

  // Keep refs in sync with state
  useEffect(() => { challengeStepRef.current = challengeStep; }, [challengeStep]);
  useEffect(() => { leftDoneRef.current = leftDone; }, [leftDone]);

  const stopPolling = useCallback(() => {
    if (pollingRef.current) {
      clearInterval(pollingRef.current);
      pollingRef.current = null;
    }
    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current);
      timeoutRef.current = null;
    }
  }, []);

  const resetChallenge = useCallback(() => {
    setChallengeStep('look-center');
    setLeftDone(false);
    setRightDone(false);
    setTimedOut(false);
    setCameraReady(false);
    capturedRef.current = false;
    centerFrameCountRef.current = 0;
    challengeStepRef.current = 'look-center';
    leftDoneRef.current = false;
    isPollingRef.current = false;
  }, []);

  // Reset when camera opens
  useEffect(() => {
    if (cameraOpen) {
      resetChallenge();
    }
    return () => stopPolling();
  }, [cameraOpen, resetChallenge, stopPolling]);

  // Start timeout once camera is ready
  useEffect(() => {
    if (cameraReady && cameraOpen) {
      timeoutRef.current = setTimeout(() => setTimedOut(true), CHALLENGE_TIMEOUT_MS);
    }
  }, [cameraReady, cameraOpen]);

  // Auto-capture when challenge completes
  useEffect(() => {
    if (challengeStep === 'done' && !capturedRef.current) {
      capturedRef.current = true;
      stopPolling();
      setTimeout(() => autoCapturePhoto(), 500);
    }
  }, [challengeStep, stopPolling]);

  const runPollFrame = useCallback(async () => {
    if (isPollingRef.current || capturedRef.current || !cameraRef.current) return;
    isPollingRef.current = true;

    try {
      const pic: CameraCapturedPicture = await cameraRef.current.takePictureAsync({
        quality: 0.3,
        skipProcessing: true,
        base64: false,
      }) as CameraCapturedPicture;

      const result = await FaceDetector.detectFacesAsync(pic.uri, {
        mode: FaceDetector.FaceDetectorMode.fast,
        detectLandmarks: FaceDetector.FaceDetectorLandmarks.none,
        runClassifications: FaceDetector.FaceDetectorClassifications.none,
      });

      const step = challengeStepRef.current;

      if (result.faces.length === 0) {
        centerFrameCountRef.current = 0;
        return;
      }

      const face = result.faces[0];
      const yaw = face.yawAngle ?? 0;

      if (step === 'look-center') {
        if (Math.abs(yaw) < YAW_THRESHOLD) {
          centerFrameCountRef.current += 1;
          if (centerFrameCountRef.current >= CENTER_FRAME_COUNT) {
            setChallengeStep('turn-left');
          }
        } else {
          centerFrameCountRef.current = 0;
        }
      } else if (step === 'turn-left' && !leftDoneRef.current) {
        if (yaw < -YAW_THRESHOLD) {
          setLeftDone(true);
          setChallengeStep('turn-right');
        }
      } else if (step === 'turn-right') {
        if (yaw > YAW_THRESHOLD) {
          setRightDone(true);
          setChallengeStep('done');
        }
      }
    } catch {
      // Silent — frame captures can fail transiently
    } finally {
      isPollingRef.current = false;
    }
  }, []);

  const startPolling = useCallback(() => {
    stopPolling();
    pollingRef.current = setInterval(runPollFrame, POLL_INTERVAL_MS);
  }, [runPollFrame, stopPolling]);

  const autoCapturePhoto = async () => {
    if (!cameraRef.current) return;
    try {
      const photo = await cameraRef.current.takePictureAsync({ quality: 0.85 });
      if (photo?.uri) {
        setSelfieImage(photo.uri);
        onCaptureComplete(photo.uri);
      }
    } catch (err) {
      console.error('Capture error:', err);
      Alert.alert('Error', 'Failed to capture photo. Please try again.');
    } finally {
      setCameraOpen(false);
    }
  };

  const openCamera = async () => {
    if (!permission?.granted) {
      const { granted } = await requestPermission();
      if (!granted) {
        Alert.alert('Permission Required', 'Camera access is needed to take your selfie.');
        return;
      }
    }
    setCameraOpen(true);
  };

  const handleChooseFromLibrary = async () => {
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
  };

  const handleCaptureTap = () => {
    Alert.alert('Take Selfie', 'Choose how you want to provide your selfie', [
      { text: 'Live Camera (with liveness check)', onPress: openCamera },
      { text: 'Choose from Library', onPress: handleChooseFromLibrary },
      { text: 'Cancel', style: 'cancel' },
    ]);
  };

  const challengeLabel = () => {
    if (timedOut && challengeStep !== 'done') return "⏱ Time's up — tap 'Capture Anyway'";
    switch (challengeStep) {
      case 'look-center': return 'Look directly at the camera';
      case 'turn-left':   return '← Turn your head slowly left';
      case 'turn-right':  return '→ Now turn your head right';
      case 'done':        return '✓ Liveness confirmed!';
    }
  };

  const ovalColor =
    challengeStep === 'done'      ? '#10B981' :
    challengeStep === 'look-center' ? 'rgba(255,255,255,0.6)' :
    '#D9B168';

  return (
    <View>
      {/* Guidelines */}
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

      {/* Selfie area */}
      <TouchableOpacity
        onPress={handleCaptureTap}
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
              onPress={handleCaptureTap}
              className="absolute bottom-4 self-center bg-white rounded-full px-4 py-3"
              style={{ shadowColor: '#000', shadowOffset: { width: 0, height: 2 }, shadowOpacity: 0.15, shadowRadius: 4, elevation: 3 }}
            >
              <Text className="text-forest font-semibold">Retake Selfie</Text>
            </TouchableOpacity>
          </View>
        ) : (
          <View className="flex-1 items-center justify-center p-4">
            <View className="bg-gold/20 rounded-full p-6 mb-4">
              <Ionicons name="person-outline" size={48} color="#D9B168" />
            </View>
            <Text className="text-forest font-bold text-lg text-center mb-2">Take a Selfie</Text>
            <Text className="text-moss text-sm text-center mb-4">
              We'll use this to verify your identity
            </Text>
            <View className="bg-gold/10 rounded-full px-4 py-2">
              <Text className="text-gold font-semibold text-sm">Tap to Capture</Text>
            </View>
          </View>
        )}
      </TouchableOpacity>

      {/* Live camera modal */}
      <Modal visible={cameraOpen} animationType="slide" statusBarTranslucent>
        <View style={StyleSheet.absoluteFill} className="bg-black">
          <CameraView
            ref={cameraRef}
            style={StyleSheet.absoluteFill}
            facing="front"
            onCameraReady={() => {
              setCameraReady(true);
              startPolling();
            }}
          />

          {/* Oval face guide overlay */}
          <View style={StyleSheet.absoluteFill} pointerEvents="none" className="items-center justify-center">
            <View
              style={{
                width: 220,
                height: 300,
                borderRadius: 110,
                borderWidth: 3,
                borderColor: ovalColor,
              }}
            />
          </View>

          {/* Challenge HUD */}
          <View
            className="absolute bottom-32 left-6 right-6 rounded-2xl p-4 items-center"
            style={{ backgroundColor: 'rgba(0,0,0,0.65)' }}
          >
            <Text className="text-white font-bold text-base text-center mb-3">
              {challengeLabel()}
            </Text>

            {/* Progress indicators */}
            <View className="flex-row items-center" style={{ gap: 12 }}>
              <View className="flex-row items-center" style={{ gap: 6 }}>
                <View
                  className="w-5 h-5 rounded-full items-center justify-center"
                  style={{ backgroundColor: leftDone ? '#10B981' : 'rgba(255,255,255,0.25)' }}
                >
                  {leftDone && <Ionicons name="checkmark" size={12} color="#fff" />}
                </View>
                <Text className="text-white text-xs opacity-80">Left</Text>
              </View>
              <View className="w-8 h-px bg-white opacity-30" />
              <View className="flex-row items-center" style={{ gap: 6 }}>
                <View
                  className="w-5 h-5 rounded-full items-center justify-center"
                  style={{ backgroundColor: rightDone ? '#10B981' : 'rgba(255,255,255,0.25)' }}
                >
                  {rightDone && <Ionicons name="checkmark" size={12} color="#fff" />}
                </View>
                <Text className="text-white text-xs opacity-80">Right</Text>
              </View>
            </View>
          </View>

          {/* Fallback capture button (shown after timeout) */}
          {timedOut && challengeStep !== 'done' && (
            <View className="absolute bottom-16 left-6 right-6">
              <TouchableOpacity onPress={autoCapturePhoto}>
                <LinearGradient
                  colors={['#D9B168', '#bea04f']}
                  className="py-4 rounded-2xl items-center"
                >
                  <Text className="text-forest font-bold">Capture Anyway</Text>
                </LinearGradient>
              </TouchableOpacity>
            </View>
          )}

          {/* Close button */}
          <TouchableOpacity
            onPress={() => { stopPolling(); setCameraOpen(false); }}
            className="absolute top-14 right-5 w-10 h-10 rounded-full items-center justify-center"
            style={{ backgroundColor: 'rgba(0,0,0,0.5)' }}
          >
            <Ionicons name="close" size={24} color="#fff" />
          </TouchableOpacity>
        </View>
      </Modal>
    </View>
  );
}
