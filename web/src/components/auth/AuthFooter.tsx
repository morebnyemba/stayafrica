/**
 * AuthFooter - Shared legal footer for auth pages (Terms & Privacy links)
 */
import Link from 'next/link';

interface AuthFooterProps {
  action: 'signin' | 'signup';
}

export function AuthFooter({ action }: AuthFooterProps) {
  return (
    <div className="mt-6 text-center text-sm text-primary-600 dark:text-sand-400">
      <p>
        By {action === 'signin' ? 'signing in' : 'creating an account'}, you agree to our{' '}
        <Link href="/terms" className="text-secondary-600 dark:text-secondary-400 hover:underline">
          Terms of Service
        </Link>{' '}
        and{' '}
        <Link href="/privacy" className="text-secondary-600 dark:text-secondary-400 hover:underline">
          Privacy Policy
        </Link>
      </p>
    </div>
  );
}
