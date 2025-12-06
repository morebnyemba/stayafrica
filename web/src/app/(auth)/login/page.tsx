import type { Metadata } from 'next';
import { LoginContent } from '@/components/common/login-content';

export const metadata: Metadata = {
  title: 'Login - StayAfrica',
  description: 'Login to your StayAfrica account',
};

export default function LoginPage() {
  return <LoginContent />;
}
