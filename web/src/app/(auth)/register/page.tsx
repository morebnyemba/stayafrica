import type { Metadata } from 'next';
import { RegisterContent } from '@/components/common/register-content';

export const metadata: Metadata = {
  title: 'Register - StayAfrica',
  description: 'Create your StayAfrica account',
};

export default function RegisterPage() {
  return <RegisterContent />;
}
